#' Calculate Revisions in Vintage Data
#'
#' Computes revisions in vintage data based on specified reference points: a fixed reference date,
#' the nth release, or a specified interval. This function allows users to analyze differences between
#' data vintages across time.
#'
#' @param df A data frame containing vintage data. The data frame must include at least the following columns:
#'   - `pub_date`: The publication date of each vintage.
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The observed value for the given vintage and reference period.
#' @param interval A positive integer specifying the lag (in periods) between vintages to compute revisions.
#'   Defaults to `1` if no other parameter is specified.
#' @param nth_release A positive integer or `"latest"`, specifying the release to use as a reference for revisions.
#'   If `"latest"`, the most recent vintage is used.
#' @param ref_date A date specifying the fixed reference publication date to compare all vintages against.
#'
#' @return A data frame (tibble) of class `tbl_revision`, with the following columns:
#'   - `pub_date`: The publication date of the vintage.
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The calculated revision, i.e., the difference between the observed value and the reference value.
#'
#' @details
#' The function supports three mutually exclusive methods for calculating revisions:
#' - **Reference date (`ref_date`)**: Computes revisions relative to a fixed publication date.
#' - **Interval (`interval`)**: Computes revisions relative to vintages published `interval` periods earlier.
#' - **Nth release (`nth_release`)**: Computes revisions relative to the nth vintage release for each reference period.
#'
#' If no method is explicitly specified, `interval = 1` is used by default.
#'
#' Input validation ensures that only one of `ref_date`, `nth_release`, or `interval` is specified.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp , id=="US")
#'
#' # Calculate revisions using an interval of 1
#' revisions_interval <- get_revisions(df, interval = 1)
#'
#' # Calculate revisions using a fixed reference date
#' revisions_date <- get_revisions(df, ref_date = as.Date("2023-02-01"))
#'
#' # Calculate revisions relative to the nth release (2nd release)
#' revisions_nth <- get_revisions(df, nth_release = 1)
#'
#' @export
get_revisions <- function(
  df,
  interval = NULL,
  nth_release = NULL,
  ref_date = NULL
) {
  # Validate inputs
  specified_count <- sum(sapply(
    list(interval, nth_release, ref_date),
    function(x) !is.null(x)
  ))

  # Default interval is 1
  if (specified_count == 0L) {
    interval <- 1
  }

  if (specified_count > 1L) {
    rlang::abort("Specify only one of 'ref_date', 'nth_release' or 'interval'.")
  }

  if (!is.null(ref_date)) {
    ref_date <- tryCatch(as.Date(ref_date), error = function(e) e)
    if (!c("Date") %in% class(ref_date)) {
      rlang::abort("The input 'ref_date' must be a date object.")
    }
  } else if (!is.null(interval)) {
    if (!is.numeric(interval) || interval < 1) {
      rlang::abort("The input 'interval' must be a positive integer.")
    }
  } else if (!is.null(nth_release)) {
    if (is.numeric(nth_release) && nth_release < 0) {
      rlang::abort(
        "The input 'nth_release' must be set to a non-negative integer or 'latest'."
      )
    } else if (is.character(nth_release) && tolower(nth_release) != "latest") {
      rlang::abort(
        "The input 'nth_release' must be set to a non-negative integer or 'latest'."
      )
    }
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  # Check if id column present
  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(id, pub_date, time)

    if (!is.null(ref_date)) {
      # Calculate revisions against the specified reference date
      revisions <- df %>%
        dplyr::inner_join(
          df %>%
            dplyr::filter(pub_date == ref_date) %>%
            dplyr::select(id, time, value_ref = value),
          by = c("id", "time")
        ) %>%
        dplyr::group_by(id, time) %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::ungroup() %>%
        dplyr::select(id, pub_date, time, value)
    } else if (!is.null(interval)) {
      # Calculate revisions relative to estimates published 'interval' periods ago
      revisions <- df %>%
        dplyr::group_by(id, time) %>%
        dplyr::mutate(
          value_ref = dplyr::lag(value, n = interval, order_by = pub_date)
        ) %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select(id, pub_date, time, value)
    } else if (!is.null(nth_release)) {
      # Calculate revisions relative to the nth release
      revisions <- df %>%
        dplyr::inner_join(
          get_nth_release(df, n = nth_release) %>%
            dplyr::select(id, time, value_ref = value),
          by = c("id", "time")
        ) %>%
        dplyr::group_by(id, time) %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select(id, pub_date, time, value)
    }
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(pub_date, time)

    if (!is.null(ref_date)) {
      # Calculate revisions against the specified reference date
      revisions <- df %>%
        dplyr::inner_join(
          df %>%
            dplyr::filter(pub_date == ref_date) %>%
            dplyr::select(time, value_ref = value),
          by = "time"
        ) %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::select(pub_date, time, value)
    } else if (!is.null(interval)) {
      # Calculate revisions relative to estimates published 'interval' periods ago
      revisions <- df %>%
        dplyr::group_by(time) %>%
        dplyr::mutate(
          value_ref = dplyr::lag(value, n = interval, order_by = pub_date)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select(pub_date, time, value)
    } else if (!is.null(nth_release)) {
      # Calculate revisions relative to the nth release
      revisions <- df %>%
        dplyr::inner_join(
          get_nth_release(df, n = nth_release) %>%
            dplyr::select(time, value_ref = value),
          by = "time"
        ) %>%
        dplyr::mutate(value = value_ref - value) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::select(pub_date, time, value)
    }
  }

  revisions <- vintages_assign_class(revisions)
  return(revisions)
}

#' Identify the First Efficient Release in Vintage Data
#'
#' Identifies the first release in a sequence of vintages that is "efficient" relative to the final release.
#' A release is deemed efficient if it satisfies specific conditions of unbiasedness and efficiency,
#' tested using a Mincer-Zarnowitz type linear regression and hypothesis testing.
#'
#' @param df A data frame of class `tbl_release` containing the vintage data.
#'   It must include the columns:
#'   - `time`: The reference period (e.g., quarter or month).
#'   - `value`: The observed value for the given release.
#'   - `release`: The release number or identifier.
#' @param final_release A data frame containing the final release data.
#'   This must include the columns:
#'   - `time`: The reference period.
#'   - `value`: The observed final value for the given period.
#' @param significance A numeric value specifying the significance level for the hypothesis test (default is `0.05`).
#' @param test_all A logical value indicating whether to test all releases, even after finding the first efficient release (default is `FALSE`).
#'
#' @return A list of class `list_eff_rel` with the following elements:
#'   - `e`: The index of the first efficient release. (0 indexed)
#'   - `data`: A long-format data frame containing the vintage data with the final release appended.
#'   - `models`: A list of linear regression models fitted for each release.
#'   - `tests`: A list of hypothesis test results for each release.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates inputs and ensures both `df` and `final_release` are in the correct format.
#' 2. Iteratively tests each release for efficiency using a linear regression model of the form:
#'    \deqn{final = \beta_0 + \beta_1 \cdot release_i + \epsilon}
#'    The null hypothesis for efficiency is:
#'    - \eqn{\beta_0 = 0} (no bias)
#'    - \eqn{\beta_1 = 1} (efficiency)
#'    Uses heteroskedasticity and autocorrelation consistent (HAC) standard errors for robust hypothesis testing.
#' 3. Stops testing when the first efficient release is found (unless `test_all = TRUE`).
#'
#' If no efficient release is found, a warning is issued.
#'
#' @examples
#' # Example data
#' df <- get_nth_release(tsbox::ts_pc(dplyr::filter(reviser::gdp, id=="US")), n = 0:3)
#'
#' final_release <- get_nth_release(tsbox::ts_pc(dplyr::filter(reviser::gdp, id=="US")), n = 10)
#'
#' # Identify the first efficient release
#' result <- get_first_efficient_release(df, final_release, significance = 0.05)
#'
#' # Access the index of the first efficient release
#' result$e
#'
#' @export
get_first_efficient_release <- function(
  df,
  final_release,
  significance = 0.05,
  test_all = FALSE
) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  check <- vintages_check(final_release)
  if (check == "wide") {
    final_release <- vintages_long(final_release)
  }
  final_release <- vintages_assign_class(final_release)

  if (!"tbl_release" %in% class(df)) {
    rlang::abort("The input 'df' must be a 'tbl_release' object.")
  }

  if ("id" %in% colnames(df) & "id" %in% colnames(final_release)) {
    if (!setequal(unique(df$id), unique(final_release$id))) {
      rlang::abort(
        "The 'id' column in 'df' and 'final_release' must have the same values."
      )
    }
  } else if ("id" %in% colnames(df) | "id" %in% colnames(final_release)) {
    rlang::abort(
      "Either both or none of 'df' and 'final_release' must contain an 'id' column."
    )
  }

  df_output <- NULL

  has_id <- "id" %in% colnames(df)
  if (has_id) {
    if (length(unique(df$id)) > 1) {
      has_ids <- TRUE
    } else {
      has_ids <- FALSE
    }
  } else {
    has_ids <- FALSE
  }

  if (has_ids) {
    for (iidd in unique(df$id)) {
      models <- list()
      tests <- list()
      final_release_id <- final_release %>%
        dplyr::filter(id == iidd) %>%
        dplyr::select(time, value) %>%
        dplyr::mutate(release = "final")

      df_id <- df %>%
        dplyr::filter(id == iidd) %>%
        dplyr::select(time, value, release)

      # Ensure data is sorted by pub_date and time
      df_id <- df_id %>%
        dplyr::bind_rows(final_release_id) %>%
        dplyr::arrange(time) %>%
        stats::na.omit()

      es <- unique(df_id$release)
      es <- es[es != "final"]

      df_wide <- vintages_wide(df_id, names_from = "release")

      e_found <- FALSE
      for (i in 1:length(es)) {
        formula <- stats::as.formula(paste0("final ~ ", es[i]))

        model <- stats::lm(formula, data = df_wide)
        hac_se <- sandwich::vcovHAC(model)

        test <- car::linearHypothesis(
          model,
          c("(Intercept) = 0", paste0(es[i], " = 1")),
          vcov = hac_se
        )

        p_value <- test[2, 'Pr(>F)']

        models[[i]] <- model
        tests[[i]] <- test

        if (!test_all) {
          if (p_value > significance) {
            efficient_release <- i - 1
            break
          }
        } else if (test_all & !e_found & p_value > significance) {
          efficient_release <- i - 1
          e_found <- TRUE
        }
      }

      if (i == length(es) & p_value < significance) {
        rlang::warn(paste0(
          "No efficient release found for ",
          iidd,
          ". Please provide further releases!"
        ))
        efficient_release <- NA_real_
      }

      data <- vintages_long(df_wide, names_to = "release")
      data <- vintages_assign_class(data)

      df_output[[iidd]] <- list(
        'e' = efficient_release,
        'data' = data,
        'models' = models,
        'tests' = tests
      )
    }
  } else {
    models <- list()
    tests <- list()

    final_release <- dplyr::select(final_release, time, value) %>%
      dplyr::mutate(release = "final")

    df <- df %>%
      dplyr::select(time, value, release)

    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::bind_rows(final_release) %>%
      dplyr::arrange(time) %>%
      stats::na.omit()

    es <- unique(df$release)
    es <- es[es != "final"]

    df_wide <- vintages_wide(df, names_from = "release")

    models <- list()
    tests <- list()
    e_found <- FALSE
    for (i in 1:length(es)) {
      formula <- stats::as.formula(paste0("final ~ ", es[i]))

      model <- stats::lm(formula, data = df_wide)
      hac_se <- sandwich::vcovHAC(model)

      test <- car::linearHypothesis(
        model,
        c("(Intercept) = 0", paste0(es[i], " = 1")),
        vcov = hac_se
      )

      p_value <- test[2, 'Pr(>F)']

      models[[i]] <- model
      tests[[i]] <- test

      if (!test_all) {
        if (p_value > significance) {
          efficient_release <- i - 1
          break
        }
      } else if (test_all & !e_found & p_value > significance) {
        efficient_release <- i - 1
        e_found <- TRUE
      }
    }

    if (i == length(es) & p_value < significance) {
      rlang::warn(
        "No efficient release found. Please provide further releases!"
      )
      efficient_release <- NA_real_
    }

    data <- vintages_long(df_wide, names_to = "release")
    data <- vintages_assign_class(data)

    df_output <- list(
      'e' = efficient_release,
      'data' = data,
      'models' = models,
      'tests' = tests
    )
  }

  class(df_output) <- c("lst_efficient", class(df_output))
  return(df_output)
}

#' Summary of Efficient Release Models
#'
#' Provides a detailed summary of the regression model and hypothesis test for the first efficient release
#' identified by the `get_first_efficient_release` function.
#'
#' @param object An output object from the `get_first_efficient_release` function.
#'   The object must be of class `list_eff_rel`.
#' @param ... Additional arguments (not used).
#'
#' @details
#' This function prints the following information:
#' - The index of the first efficient release.
#' - A summary of the regression model fitted for the efficient release, which includes coefficients,
#'   R-squared values, and other relevant statistics.
#' - The hypothesis test results for the efficient release, showing the test statistic and p-value
#'   for the null hypothesis of unbiasedness and efficiency.
#'
#' The function assumes the object includes:
#' - `e`: The index of the first efficient release (0-based).
#' - `models`: A list of linear regression models for each release.
#' - `tests`: A list of hypothesis test results corresponding to each release.
#'
#' @return Returns a tibble with the following columns:
#' - `id`: The identifier of the time series (if present in input data).
#' - `e`: The index of the first efficient release.
#' - `alpha`: The intercept coefficient of the regression model.
#' - `beta`: The coefficient of the slope.
#' - `p-value`: The p-value of the joint hypothesis test (alpha = 0 and beta = 1).
#' - `n_tested`: The number of releases tested.
#'
#' @examples
#' # Example usage
#' df <- get_nth_release(tsbox::ts_pc(dplyr::filter(reviser::gdp , id=="US")), n = 1:4)
#'
#' final_release <- get_nth_release(tsbox::ts_pc(dplyr::filter(reviser::gdp, id=="US")), n = 10)
#'
#' # Identify the first efficient release
#' result <- get_first_efficient_release(df, final_release, significance = 0.05)
#' summary(result)
#'
#' @export
summary.lst_efficient <- function(object, ...) {
  is_id_list <- is.null(object$e)
  df_out <- NULL
  if (is_id_list) {
    for (iidd in names(object)) {
      cat("id: ", iidd, "\n")
      if (is.na(object[[iidd]]$e)) {
        cat("No efficient release found!")
      } else {
        cat("Efficient release: ", object[[iidd]]$e, "\n\n")
        cat("Model summary: \n")
        print(summary(object[[iidd]]$models[[object[[iidd]]$e + 1]]))
        cat("\nTest summary: \n")
        print(object[[iidd]]$tests[[object[[iidd]]$e + 1]])
        cat("\n\n")
      }
      if (!is.na(object[[iidd]]$e)) {
        df_out <- dplyr::bind_rows(
          df_out,
          tibble::tibble(
            id = iidd,
            e = object[[iidd]]$e,
            alpha = stats::coef(summary(object[[iidd]]$models[[
              object[[iidd]]$e + 1
            ]]))[1, 1],
            beta = stats::coef(summary(object[[iidd]]$models[[
              object[[iidd]]$e + 1
            ]]))[2, 1],
            p_value = object[[iidd]]$tests[[object[[iidd]]$e + 1]][2, 'Pr(>F)'],
            n_tested = length(object[[iidd]]$tests)
          )
        )
      } else {
        df_out <- dplyr::bind_rows(
          df_out,
          tibble::tibble(
            id = iidd,
            e = NA_real_,
            alpha = stats::coef(summary(object[[iidd]]$models[[length(
              object[[iidd]]$tests
            )]]))[1, 1],
            beta = stats::coef(summary(object[[iidd]]$models[[length(
              object[[iidd]]$tests
            )]]))[2, 1],
            p_value = object[[iidd]]$tests[[length(object[[iidd]]$tests)]][
              2,
              'Pr(>F)'
            ],
            n_tested = length(object[[iidd]]$tests)
          )
        )
      }
    }
  } else {
    if (is.na(object$e)) {
      cat("No efficient release found!")
    } else {
      cat("Efficient release: ", object$e, "\n\n")
      cat("Model summary: \n")
      print(summary(object$models[[object$e + 1]]))
      cat("\nTest summary: \n")
      print(object$tests[[object$e + 1]])
    }
    if (!is.na(object$e)) {
      df_out <- tibble::tibble(
        e = object$e,
        alpha = stats::coef(summary(object$models[[object$e + 1]]))[1, 1],
        beta = stats::coef(summary(object$models[[object$e + 1]]))[2, 1],
        p_value = object$tests[[object$e + 1]][2, 'Pr(>F)'],
        n_tested = length(object$tests)
      )
    } else {
      df_out <- tibble::tibble(
        e = NA_real_,
        alpha = stats::coef(summary(object$models[[length(object$tests)]]))[
          1,
          1
        ],
        beta = stats::coef(summary(object$models[[length(object$tests)]]))[
          2,
          1
        ],
        p_value = object$tests[[length(object$tests)]][2, 'Pr(>F)'],
        n_tested = length(object$tests)
      )
    }
  }

  return(invisible(df_out))
}


#' Revision Analysis Summary Statistics
#'
#' Calculates a comprehensive set of summary statistics and hypothesis tests
#' for revisions between initial and final data releases.
#'
#' @param df A data frame containing the initial data releases. Must include columns:
#'   - `time`: The time variable.
#'   - `value`: The observed values in the initial release.
#'   - Optionally, `release` (release identifier) and `id` (grouping variable).
#' @param final_release A data frame containing the final release data. Must include columns:
#'   - `time`: The time variable (matching the initial release data).
#'   - `value`: The observed values in the final release.
#' @param degree An integer between 1 and 5 specifying the level of detail for the output:
#'    1: Default, descriptive statistics (bias, min/max, standard deviation, noise/signal ratio, correlation).
#'    2: Efficiency tests (bias intercept and slope tests, efficiency tests for intercept and slope).
#'    3: Orthogonality tests (correlation, autocorrelation, Theil's U statistics, seasonality tests).
#'    4: News vs. noise tests (p-values for news and noise tests).
#'    5: Full set of all statistics and tests.
#' @param grouping_var A character string specifying the grouping variable in the data frame. Defaults to
#' `pub_date` or `release` if available.
#'
#' @details
#' This function performs a variety of statistical analyses to understand the nature of revisions between
#' the initial and final data releases. The function:
#' - Checks the input data for consistency and transforms it as necessary.
#' - Merges the initial and final release datasets by their time variable and optional grouping variables (`id` or `release`).
#' - Computes summary statistics such as the mean, standard deviation, and range of the revisions.
#' - Performs hypothesis tests for bias, efficiency, and correlation using robust methods (e.g., Newey-West standard errors).
#' - Includes tests for seasonality, noise, and news effects.
#'
#' Key tests include:
#' - **Bias Tests**: Tests for the presence of mean bias and regression bias.
#' - **Efficiency Tests**: Tests whether revisions are orthogonal to initial values.
#' - **Autocorrelation and Seasonality**: Tests for serial correlation and seasonal patterns in revisions.
#' - **Theil's U Statistics**: Measures predictive accuracy of the initial releases relative to the final values.
#' - **Noise vs. News**: Differentiates between unpredictable errors (noise) and systematic adjustments (news).
#'
#' The function supports grouped calculations based on the presence of `id` or `release` columns in the input.
#'
#' The following statistics and tests are calculated:
#'
#' - **N**: The number of observations in the group.
#' - **Frequency**: The inferred data frequency (e.g., 12 for monthly or 4 for quarterly data).
#' - **Bias (mean)**: The mean revision, testing whether revisions are systematically biased.
#' - **Bias (p-value)**: p-value from a t-test evaluating the significance of the mean revision.
#' - **Bias (robust p-value)**: Newey-West HAC robust p-value for the mean revision test.
#' - **Bias (intercept)**: Intercept of a regression of final values on initial values, testing for systematic bias.
#' - **Bias (intercept p-value)**: p-value for the intercept test.
#' - **Bias (slope)**: Slope of the regression of final values on initial values, testing the alignment between them.
#' - **Bias (slope p-value)**: p-value for the slope test, with the null hypothesis of slope = 1.
#' - **Efficiency (intercept)**: Intercept of the regression of revisions on initial values, testing forecast efficiency.
#' - **Efficiency (intercept p-value)**: p-value for the efficiency intercept test.
#' - **Efficiency (slope)**: Slope of the regression of revisions on initial values, testing forecast efficiency.
#' - **Efficiency (slope p-value)**: p-value for the efficiency slope test, with the null hypothesis of slope = 0.
#' - **Minimum**: The minimum revision in the group.
#' - **Maximum**: The maximum revision in the group.
#' - **Std. Dev.**: The standard deviation of revisions, indicating their variability.
#' - **Noise/Signal**: The ratio of the standard deviation of revisions to the standard deviation of final values.
#' - **Correlation**: The Pearson correlation between revisions and initial values, testing the relationship.
#' - **Correlation (p-value)**: p-value for the significance of the correlation.
#' - **Autocorrelation (1st)**: The first-order autocorrelation of revisions, measuring persistence.
#' - **Autocorrelation (1st p-value)**: p-value for the first-order autocorrelation test.
#' - **Autocorrelation up to 4th (Ljung-Box p-value)**: p-value for the Ljung-Box test for higher-order autocorrelation.
#' - **Theil's U1**: A normalized measure of forecast accuracy, comparing the root mean squared error (RMSE) of revisions to the RMSE of final and initial values.
#' - **Theil's U2**: A measure comparing forecast changes to actual changes.
#' - **Seasonality (Ljung-Box p-value)**: Tests for seasonality in revisions using the Ljung-Box test for lags matching the data frequency.
#' - **Seasonality (Friedman p-value)**: Tests for seasonality in revisions using the Friedman test.
#' - **News test (p-value)**: Tests whether revisions are explained by information in final values.
#' - **Noise test (p-value)**: Tests whether revisions are uncorrelated with initial values.
#'
#' @return A data frame with one row per grouping (if applicable) and columns for summary statistics and test results.
#' The resulting data frame is of class `revision_summary`.
#'
#' @examples
#' # Example usage:
#' df <- get_nth_release( dplyr::filter(reviser::gdp, id=="US"), n = 0:3)
#'
#' final_release <- get_nth_release(dplyr::filter(reviser::gdp, id=="US"), n = "latest")
#'
#' results <- get_revision_analysis(df, final_release)
#' print(results)
#'
#' @export
get_revision_analysis <- function(
  df,
  final_release,
  degree = 1,
  grouping_var = NULL
) {
  # Check degree in 1:5
  if (!degree %in% c(1:5)) {
    rlang::abort("The 'degree' must be an integer between 1 and 3.")
  }

  # Check grouping variable present in both df and final_release
  if (!is.null(grouping_var)) {
    if (
      !grouping_var %in% colnames(df) |
        !grouping_var %in% colnames(final_release)
    ) {
      rlang::abort(
        "The grouping variable must be present in both 'df' and 'final_release'."
      )
    }
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  check <- vintages_check(final_release)
  if (check == "wide") {
    final_release <- vintages_long(final_release)
  }
  final_release <- vintages_assign_class(final_release)

  if (!any(c("tbl_release", "tbl_pubdate") %in% class(df))) {
    rlang::abort("'df' must be a 'tbl_release' or 'tbl_pubdate' object.")
  }

  # Check that id is present in both data.frames or neither
  if ("id" %in% colnames(df) & "id" %in% colnames(final_release)) {
    if (!identical(unique(df$id), unique(final_release$id))) {
      rlang::abort("The same 'id' must be present in 'df' and 'final_release'.")
    }
  } else if ("id" %in% colnames(df) | "id" %in% colnames(final_release)) {
    rlang::abort(
      "Both or none of 'df' and 'final_release' must have an 'id' column."
    )
  }

  # If both pub_date and release columns are present in df, use the release column
  if ("release" %in% colnames(df) & "pub_date" %in% colnames(df)) {
    rlang::warn(
      "Both 'release' and 'pub_date' columns are present in 'df. The 'release' column will be used."
    )
    df <- df %>%
      dplyr::select(-pub_date)
  }

  if (!is.null(grouping_var)) {
    df_var <- grouping_var
  } else if (any(grepl("pub_date", colnames(df)))) {
    df_var <- "pub_date"
  } else if (any(grepl("release", colnames(df)))) {
    df_var <- "release"
  } else {
    rlang::abort("The 'df' object must have a 'release' or 'pub_date' column.")
  }

  final_release <- final_release %>%
    dplyr::rename(final_value = value)

  # Check if df has id column:
  if ("id" %in% colnames(df) & "id" %in% colnames(final_release)) {
    final_release <- dplyr::select(final_release, time, final_value, id)

    df <- df %>%
      dplyr::select(time, value, dplyr::all_of(df_var), id)

    df <- df %>%
      dplyr::left_join(final_release, by = c("time" = "time", "id" = "id")) %>%
      dplyr::arrange(id, time) %>%
      stats::na.omit()
  } else {
    final_release <- dplyr::select(final_release, time, final_value)

    df <- df %>%
      dplyr::select(time, value, df_var)

    df <- df %>%
      dplyr::left_join(final_release, by = c("time" = "time")) %>%
      dplyr::arrange(time) %>%
      stats::na.omit()
  }

  revisions <- df %>%
    dplyr::mutate(
      revision = final_value - value,
    )

  # Function to compute statistics and tests for each group
  compute_stats <- function(data) {
    N <- nrow(data)
    freq <- round(
      1 / ((mean(((as.numeric(diff(unique(data$time))) / 360)), na.rm = TRUE)))
    )
    mean_revision <- mean(data$revision)
    mean_abs_revision <- mean(abs(data$revision))
    min_revision <- min(data$revision)
    max_revision <- max(data$revision)
    std_dev_revision <- stats::sd(data$revision)
    noise_to_signal <- std_dev_revision / stats::sd(data$final_value)
    correlation <- stats::cor(data$revision, data$value, use = "complete.obs")
    autocorrelation <- stats::cor(
      data$revision[-1],
      data$revision[-length(data$revision)],
      use = "complete.obs"
    )

    # Significance test for the Bias (mean) (Newey-West HAC standard errors)
    mean_test_model <- stats::lm(revision ~ 1, data = data)
    # Conventional t test:
    mean_t_stat <- summary(mean_test_model)$coefficients[1, 3]
    mean_p_value <- summary(mean_test_model)$coefficients[1, 4]

    # Newey-West HAC standard errors t test:
    mean_test_se <- sqrt(sandwich::NeweyWest(mean_test_model))
    mean_nw_t_stat <- stats::coef(mean_test_model)[1] / mean_test_se
    mean_nw_p_value <- 2 * (1 - stats::pt(abs(mean_nw_t_stat), df = N - 1)) # Two-sided p-value

    # Significance test for the Bias (intercept and slope)
    bias_test_model <- stats::lm(final_value ~ value, data = data)
    coef_test_intercept <- summary(bias_test_model)$coefficients[1, 1]
    coef_test_slope <- summary(bias_test_model)$coefficients[2, 1]
    coef_p_value_intercept <- summary(bias_test_model)$coefficients[1, 4]
    test <- car::linearHypothesis(bias_test_model, c("value = 1"))
    coef_p_value_slope <- test[2, 'Pr(>F)']

    # Efficiency test
    efficiency_test_model <- stats::lm(revision ~ value, data = data)
    coef_efficiency_intercept <- summary(efficiency_test_model)$coefficients[
      1,
      1
    ]
    coef_efficiency_slope <- summary(efficiency_test_model)$coefficients[2, 1]
    coef_efficiency_p_value_intercept <- summary(
      efficiency_test_model
    )$coefficients[1, 4]
    coef_efficiency_p_value_slope <- summary(
      efficiency_test_model
    )$coefficients[2, 4]

    # Significance test for correlation
    cor_t_stat <- correlation * sqrt((N - 2) / (1 - correlation^2))
    cor_p_value <- 2 * (1 - stats::pt(abs(cor_t_stat), df = N - 2)) # Two-sided p-value

    # Significance test for autocorrelation
    auto_t_stat <- autocorrelation * sqrt((N - 2) / (1 - autocorrelation^2))
    auto_p_value <- 2 * (1 - stats::pt(abs(auto_t_stat), df = N - 2)) # Two-sided p-value

    # Ljung Box:
    ljung_box <- stats::Box.test(
      data$revision,
      lag = 4,
      type = "Ljung-Box",
      fitdf = 1
    )$p.value

    theils_u1 <- sqrt(mean((data$final_value - data$value)^2)) /
      (sqrt(mean(data$final_value^2)) + sqrt(mean(data$value^2)))
    theils_u2 <- sqrt(
      sum(
        (data$value[-1] - data$final_value[-1]) /
          data$final_value[-length(data$final_value)]
      )^2
    ) /
      sqrt(
        sum(
          (data$final_value[-1] - data$final_value[-length(data$final_value)]) /
            data$final_value[-length(data$final_value)]
        )^2
      )

    # Seasonality test
    if (freq == 12) {
      friedman_p_value <- friedman_test(
        data$revision,
        frequency = freq
      )$`p_value`
      ljung_box_seasonality_p_value <- qs_test(
        data$revision,
        lags = c(12, 24)
      )$`p_value`
    } else if (freq == 4) {
      friedman_p_value <- friedman_test(
        data$revision,
        frequency = freq
      )$`p_value`
      ljung_box_seasonality_p_value <- qs_test(
        data$revision,
        lags = c(4, 8)
      )$`p_value`
    } else {
      friedman_p_value <- NA
      ljung_box_seasonality_p_value <- NA
    }

    # Noise test
    noise_test <- stats::lm(revision ~ value, data = data)
    hac_se <- sandwich::vcovHAC(noise_test)
    test <- car::linearHypothesis(
      noise_test,
      c("(Intercept) = 0", "value = 0"),
      vcov = hac_se
    )
    noise_p_value <- test[2, 'Pr(>F)']

    # News test
    news_test <- stats::lm(revision ~ final_value, data = data)
    hac_se <- sandwich::vcovHAC(news_test)
    test <- car::linearHypothesis(
      news_test,
      c("(Intercept) = 0", "final_value = 0"),
      vcov = hac_se
    )
    news_p_value <- test[2, 'Pr(>F)']

    # Computes the fraction of sign changes
    correct_sign <- data %>%
      mutate(early_sign = sign(value), late_sign = sign(final_value)) %>%
      summarise(
        fraction_sign_correct = sum((early_sign - late_sign) == 0) / n(),
        fraction_sign_wrong = 1 - fraction_sign_correct,
        n = n()
      ) %>%
      ungroup() %>%
      pull(fraction_sign_correct)

    # Computes the fraction of changes in the sign of the change in the growth rate
    correct_change <- data %>%
      mutate(
        diff_value = value - lag(value, 1),
        diff_final_value = final_value - lag(final_value, 1)
      ) %>%
      mutate(
        early_sign = sign(diff_value),
        late_sign = sign(diff_final_value)
      ) %>%
      summarise(
        fraction_sign_correct = sum((early_sign - late_sign) == 0, na.rm = T) /
          n(),
        fraction_sign_wrong = 1 - fraction_sign_correct,
        n = n()
      ) %>%
      ungroup() %>%
      pull(fraction_sign_correct)

    tibble::tibble(
      Statistic = c(
        "N",
        "Frequency",
        "Bias (mean)",
        "Bias (p-value)",
        "Bias (robust p-value)",
        "Bias (intercept)",
        "Bias (intercept p-value)",
        "Bias (slope)",
        "Bias (slope p-value)",
        "Efficiency (intercept)",
        "Efficiency (intercept p-value)",
        "Efficiency (slope)",
        "Efficiency (slope p-value)",
        "Minimum",
        "Maximum",
        "MAR",
        "Std. Dev.",
        "Noise/Signal",
        "Correlation",
        "Correlation (p-value)",
        "Autocorrelation (1st)",
        "Autocorrelation (1st p-value)",
        "Autocorrelation up to 4th (Ljung-Box p-value)",
        "Theil's U1",
        "Theil's U2",
        "Seasonality (Ljung-Box p-value)",
        "Seasonality (Friedman p-value)",
        "News test (p-value)",
        "Noise test (p-value)",
        "Fraction of correct sign",
        "Fraction of correct growth rate change"
      ),
      Value = c(
        N,
        freq,
        mean_revision,
        mean_p_value,
        mean_nw_p_value,
        coef_test_intercept,
        coef_p_value_intercept,
        coef_test_slope,
        coef_p_value_slope,
        coef_efficiency_intercept,
        coef_efficiency_p_value_intercept,
        coef_efficiency_slope,
        coef_efficiency_p_value_slope,
        min_revision,
        max_revision,
        mean_abs_revision,
        std_dev_revision,
        noise_to_signal,
        correlation,
        cor_p_value,
        autocorrelation,
        auto_p_value,
        ljung_box,
        theils_u1,
        theils_u2,
        ljung_box_seasonality_p_value,
        friedman_p_value,
        news_p_value,
        noise_p_value,
        correct_sign,
        correct_change
      )
    )
  }

  # if no id or release column present, create a dummy id column
  if (!any(c("id", "release", "pub_date") %in% colnames(revisions))) {
    revisions <- revisions %>%
      dplyr::mutate(id = "release_0")
  }

  # Defing grouping vars
  if (all(c(df_var, "id") %in% colnames(revisions))) {
    grouping_vars <- c("id", df_var)
  } else if ("id" %in% colnames(revisions)) {
    grouping_vars <- c("id")
  } else if (df_var %in% colnames(revisions)) {
    grouping_vars <- c(df_var)
  }

  # Check that there are at least N = 10 observation paires per group
  if (
    any(
      revisions %>%
        dplyr::count(dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::pull(n) <
        8
    )
  ) {
    rlang::abort(
      "There must be at least 8 observations per group to compute the statistics."
    )
  }

  # Apply the computation to each group and combine the results
  results <- revisions %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::group_modify(~ compute_stats(.x)) %>%
    dplyr::ungroup()

  results <- results %>%
    tidyr::pivot_wider(
      names_from = "Statistic",
      values_from = "Value"
    )

  class(results) <- c("revision_summary", class(results))

  if (degree == 1) {
    # Descriptives
    results <- results %>%
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Bias (mean)",
        "Bias (p-value)",
        "Minimum",
        "Maximum",
        "MAR",
        "Std. Dev.",
        "Noise/Signal",
        "Correlation",
        "Correlation (p-value)"
      )
    return(results)
  } else if (degree == 2) {
    # Efficiency
    results <- results %>%
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Bias (intercept)",
        "Bias (intercept p-value)",
        "Bias (slope)",
        "Bias (slope p-value)",
        "Efficiency (intercept)",
        "Efficiency (intercept p-value)",
        "Efficiency (slope)",
        "Efficiency (slope p-value)"
      )
    return(results)
  } else if (degree == 3) {
    # Orthogonality
    results <- results %>%
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "Correlation",
        "Correlation (p-value)",
        "Autocorrelation (1st)",
        "Autocorrelation (1st p-value)",
        "Autocorrelation up to 4th (Ljung-Box p-value)",
        "Theil's U1",
        "Theil's U2",
        "Seasonality (Ljung-Box p-value)",
        "Seasonality (Friedman p-value)"
      )
    return(results)
  } else if (degree == 4) {
    # News and noise tests
    results <- results %>%
      dplyr::select(
        dplyr::all_of(
          grouping_vars
        ),
        "N",
        "News test (p-value)",
        "Noise test (p-value)"
      )
    return(results)
  } else if (degree == 5) {
    return(results)
  }
}


#' Function for Friedman Test used in `get_revision_analysis`
#' @param series vector
#' @param frequency integer
#' @noRd
friedman_test <- function(series, frequency = 12) {
  # First-difference the series
  diff_series <- diff(series)

  # Reshape the series into blocks (years) and treatments (months or quarters)
  n <- length(diff_series)
  n_blocks <- n %/% frequency # Number of years (or periods of 'frequency' data)

  # Reshape the series into a matrix of ranks
  diff_matrix <- matrix(
    diff_series[1:(n_blocks * frequency)],
    nrow = n_blocks,
    ncol = frequency
  )
  ranked_matrix <- apply(diff_matrix, 2, rank)

  # Compute the test statistic
  row_means <- apply(ranked_matrix, 1, mean)
  total_mean <- mean(row_means)

  Q <- sum((row_means - total_mean)^2) / sum((ranked_matrix - total_mean)^2)

  # Degrees of freedom for the chi-squared distribution
  df <- frequency - 1
  p_value <- 1 - stats::pchisq(Q, df)

  return(list(p_value = p_value))
}


#' Function for the QS test used in `get_revision_analysis`
#' @param series vector
#' @param lags vector of seasonal lags
#' @noRd
qs_test <- function(series, lags = c(12, 24)) {
  # First-difference the series
  diff_series <- diff(series)

  # Compute autocorrelations for seasonal lags
  acf_vals <- stats::acf(diff_series, lag.max = max(lags), plot = FALSE)$acf

  # Compute QS statistic
  n <- length(diff_series)
  QS <- sum((pmax(0, acf_vals[lags])^2) * (n * (n + 2) / (n - lags)))

  # Chi-squared distribution with 'k' degrees of freedom
  k <- length(lags)

  # Compare QS statistic to chi-squared critical value
  p_value <- 1 - stats::pchisq(QS, df = k)

  return(list(p_value = p_value))
}


#' Extract the Nth Data Release (Vintage)
#'
#' Filters the input dataset to return the Nth release (or vintage) of data for each time period.
#' The function supports selecting the first, latest, or a specific numbered release.
#'
#' @param df A data frame containing data vintages. The data frame must include the columns
#'           `pub_date` (publication date of the release) and `time` (the corresponding time period for the data).
#' @param n The release number to extract. Accepts:
#'          - A positive integer or vector (e.g., 1 for the first release, 2 for the second, etc.).
#'          - `"first"` to extract the first release.
#'          - `"latest"` to extract the most recent release.
#'          Default is 1 (the first release).
#' @param diagonal Logical. If `TRUE`, the function only returns real first releases.
#'
#' @return A filtered data frame containing only the specified release(s). The resulting data frame is
#'         assigned the class `tbl_release` to indicate its structure. If diagonal
#'         is set to `TRUE`, the function only returns the real first releases. That is historic values for which no
#'         vintages exist are not returned.
#'
#' @details
#' The behavior depends on the value of `n`:
#' - **Non-negative integer**: The function retrieves the Nth release for each time period (e.g., 0 = first release, 1 = second release, etc.).
#' - **"first"**: Retrieves the first release for each time period (via `get_first_release`).
#' - **"latest"**: Retrieves the most recent release for each time period (via `get_latest_release`).
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Get the first release (n = 0)
#' first_release <- get_nth_release(df, n = 0)
#'
#' # Get the latest release
#' latest_release <- get_nth_release(df, n = "latest")
#'
#' # Get the second release (n = 1)
#' second_release <- get_nth_release(df, n = 1)
#'
#' @export
get_nth_release <- function(df, n = 0, diagonal = FALSE) {
  # Validate inputs
  if (is.numeric(n) && any(n < 0)) {
    rlang::abort("The input 'n' must be >= 0, 'first', or 'latest'.")
  } else if (is.character(n) && !tolower(n) %in% c("first", "latest")) {
    rlang::abort("The input 'n' must be  >= 0, 'first', or 'latest'.")
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(id, pub_date, time)

    if (is.numeric(n)) {
      n <- n + 1
      # Get the nth release(s)
      nth_release <- df %>%
        dplyr::group_by(time, id) %>%
        dplyr::mutate(release = paste0("release_", (1:dplyr::n() - 1))) %>%
        dplyr::slice(n) %>%
        dplyr::ungroup()
    } else if (tolower(n) == "latest") {
      # Get the latest release
      nth_release <- get_latest_release(df)
    } else if (tolower(n) == "first") {
      # Get the first release
      nth_release <- get_first_release(df)
    }
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(pub_date, time)

    if (is.numeric(n)) {
      n <- n + 1
      # Get the nth release(s)
      nth_release <- df %>%
        dplyr::group_by(time) %>%
        dplyr::mutate(release = paste0("release_", (1:dplyr::n() - 1))) %>%
        dplyr::slice(n) %>%
        dplyr::ungroup()
    } else if (tolower(n) == "latest") {
      # Get the latest release
      nth_release <- get_latest_release(df)
    } else if (tolower(n) == "first") {
      # Get the first release
      nth_release <- get_first_release(df)
    }
  }

  if (diagonal) {
    df <- df %>%
      dplyr::filter(
        !dplyr::lead(pub_date) == (pub_date)
      )
  }

  # Add the class only if it is not already present
  nth_release <- vintages_assign_class(nth_release)
  return(nth_release)
}

#' Extract the First Data Release (Vintage)
#'
#' Filters the input dataset to return the earliest release (or vintage) for each time period.
#'
#' @param df A data frame containing data vintages. The data frame must include the columns
#'           `pub_date` (publication date of the release) and `time` (the corresponding time period for the data).
#' @param diagonal Logical. If `TRUE`, the function only returns real first releases.
#'
#' @return A filtered data frame containing only the first release(s). The resulting data frame is
#'         assigned the class `tbl_release` to indicate its structure.
#'
#' @details
#' For each time period, the function identifies the release with the earliest publication date (`pub_date`).
#' A new column `release` is added and labels all rows in the resulting data frame as `release_0`. If diagonal
#' is set to `TRUE`, the function only returns the real first releases. That is historic values for which no
#' vintages exist are not returned.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Get the first release for each time period
#' first_release <- get_first_release(df)
#'
#' @export
get_first_release <- function(df, diagonal = FALSE) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(id, pub_date, time)

    df <- df %>%
      dplyr::group_by(id, time) %>%
      dplyr::mutate("release" = "release_0") %>%
      dplyr::filter(pub_date == min(pub_date)) %>%
      dplyr::ungroup()
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(pub_date, time)

    df <- df %>%
      dplyr::group_by(time) %>%
      dplyr::mutate("release" = "release_0") %>%
      dplyr::filter(pub_date == min(pub_date)) %>%
      dplyr::ungroup()
  }

  if (diagonal) {
    df <- df %>%
      dplyr::filter(
        !dplyr::lead(pub_date) == (pub_date)
      )
  }

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Extract the Latest Data Release (Vintage)
#'
#' Filters the input dataset to return the most recent release (or vintage) for each time period.
#'
#' @param df A data frame containing data vintages. The data frame must include the columns
#'           `pub_date` (publication date of the release) and `time` (the corresponding time period for the data).
#'
#' @return A filtered data frame containing only the most recent release(s). The resulting data frame is
#'         assigned the class `tbl_release` to indicate its structure.
#'
#' @details
#' For each time period, the function identifies the release with the latest publication date (`pub_date`)
#' and adds a column `release` that labels the release as `release_N`, where `N` is the release index (zero indexed).
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Get the latest release for each time period
#' latest_release <- get_latest_release(df)
#'
#' @export
get_latest_release <- function(df) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  if ("id" %in% colnames(df)) {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(id, pub_date, time)
    df <- df %>%
      dplyr::group_by(id, time) %>%
      dplyr::mutate("release" = paste0("release_", dplyr::n())) %>%
      dplyr::filter(pub_date == max(pub_date)) %>%
      dplyr::ungroup()
  } else {
    # Ensure data is sorted by pub_date and time
    df <- df %>%
      dplyr::arrange(pub_date, time)
    df <- df %>%
      dplyr::group_by(time) %>%
      dplyr::mutate("release" = paste0("release_", dplyr::n())) %>%
      dplyr::filter(pub_date == max(pub_date)) %>%
      dplyr::ungroup()
  }

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Extract Vintage Values from a Data Frame
#'
#' Some statistical agencies make a final revision of their data after a certain
#' period of time in a give month in the year. This function extracts values
#' from a given month or quarter a specified  number of years after the
#' initial release.
#'
#' @param df A data frame containing columns `pub_date` (publication date) and `time` (observation date).
#' @param month An optional parameter specifying the target month as a name ("July") or an integer (7). Cannot be used with `quarter`.
#' @param quarter An optional parameter specifying the target quarter (1-4). Cannot be used with `month`.
#' @param years The number of years after `pub_date` for which the values should be extracted.
#'
#' @return A filtered data frame containing values matching the specified criteria.
#' @examples
#' df <- dplyr::filter(reviser::gdp, id=="US")
#' dta <- get_fixed_release(df, month = "July", years = 3)
#' dta <- get_fixed_release(df, month = 7, years = 3)
#' dta <- get_fixed_release(df, quarter = 3, years = 3)
#'
#' @export
get_fixed_release <- function(df, years, month = NULL, quarter = NULL) {
  # Ensure only one of month or quarter is specified
  if (!is.null(month) && !is.null(quarter)) {
    rlang::abort("Specify either a month or a quarter, not both.")
  }

  # Ensure years is numeric
  if (!is.numeric(years)) {
    rlang::abort("'years' must be a numeric value.")
  }
  # Ensure the month is in numeric format if provided
  if (!is.null(month)) {
    if (is.character(month)) {
      month <- match(tolower(month), tolower(month.name))
      if (is.na(month)) rlang::abort("Invalid 'month' name")
    }
  }
  # Ensure quarter is in numeric format if provided
  if (!is.null(quarter)) {
    if (!quarter %in% 1:4) {
      rlang::abort("Invalid quarter number. Must be between 1 and 4.")
    }
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }
  df <- vintages_assign_class(df)

  # Define quarter months if quarter is specified
  if (!is.null(quarter)) {
    quarter_months <- list(
      `1` = c(1, 2, 3),
      `2` = c(4, 5, 6),
      `3` = c(7, 8, 9),
      `4` = c(10, 11, 12)
    )
    target_month <- quarter_months[[as.character(quarter)]][1]
  } else if (!is.null(month)) {
    target_month <- month
  }

  df <- df %>%
    dplyr::mutate(
      target_date = dplyr::if_else(
        lubridate::month(time) > target_month,
        lubridate::make_date(
          lubridate::year(time) + years + 1,
          target_month,
          1
        ),
        lubridate::make_date(lubridate::year(time) + years, target_month, 1)
      )
    ) %>%
    dplyr::filter(
      lubridate::year(pub_date) == lubridate::year(target_date) &
        lubridate::month(pub_date) == lubridate::month(target_date)
    ) %>%
    dplyr::select(-target_date)

  # Add the class only if it is not already present
  df <- vintages_assign_class(df)
  return(df)
}

#' Get Data Releases for a Specific Date
#'
#' Filters the input dataset to return the releases corresponding to a specific time period (date).
#'
#' @param df A data frame containing data vintages. The data frame must include the columns
#'           `pub_date` (publication date of the release) and `time` (the corresponding time period for the data).
#' @param date A Date object specifying the time period (date) for which releases should be retrieved.
#'
#' @return A data frame containing the releases for the specified date. The returned data frame will include
#'         the same structure as the input, filtered to only include rows matching the `date` in the `time` column.
#'
#' @details
#' This function filters the input data based on the specified `date` in the `time` column. The input dataset
#' must have the `pub_date` and `time` columns, with `time` being the period to match against the given `date`.
#' If the dataset is in wide format, it will first be transformed into long format using the helper function
#' `vintages_long`.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Get releases for a specific date
#' date <- as.Date("2020-04-01")
#' releases_on_date <- get_releases_by_date(df, date)
#'
#' @export
get_releases_by_date <- function(df, date) {
  date <- tryCatch(as.Date(date), error = function(e) e)

  # Validate inputs
  if (!c("Date") %in% class(date)) {
    rlang::abort("The input 'date' must be a Date object.")
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }

  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)

  # Get all releases on the specified date
  releases <- df %>%
    dplyr::filter(time == date)

  releases <- vintages_assign_class(releases)

  return(releases)
}


#' Calculate the Number of Days Between Period End and First Release
#'
#' Computes the number of days between the publication date (`pub_date`) of a release and the time period
#' (`time`) end date for each record in the dataset.
#'
#' @param df A data frame containing data vintages. The data frame must include the columns
#'           `pub_date` (publication date of the release) and `time` (the corresponding time period for the data).
#'
#' @return A data frame with an additional column `days_to_release` representing the number of days between
#'         the publication date (`pub_date`) and the time period (`time`) for each release.
#'
#' @details
#' The function calculates the difference between `pub_date` and `time` for each row in the dataset.
#' The result is expressed as the number of days between the release publication date and the corresponding
#' time period end. If the dataset is in wide format, it will first be transformed into long format using
#' `vintages_long`.
#'
#' @examples
#' # Example data
#' df <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Calculate days to release
#' df_with_days <- get_days_to_release(df)
#'
#' @export
get_days_to_release <- function(df) {
  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df)
  }

  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)

  # Get the number of days between the release date and the period end date
  df <- df %>%
    dplyr::mutate(
      days_to_release = as.numeric(difftime(pub_date, time, units = "days"))
    )

  df <- vintages_assign_class(df)

  return(df)
}
