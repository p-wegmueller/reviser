#' get revisions
#'
#' @export
get_revisions <- function(df, interval = NULL, nth_release = NULL, ref_date = NULL) {
  # Validate inputs
  specified_count <- sum(sapply(list(interval, nth_release, ref_date), function(x) !is.null(x)))
  
  # Default interval is 1
  if (specified_count == 0L) {
    interval <- 1
  }
  
  if (specified_count > 1L) {
    rlang::abort("Specify only one of 'ref_date', 'nth_release' or 'interval'.")
  }
  
  if (!is.null(ref_date)) {
    ref_date <- as.Date(ref_date)
    if (!is.Date(ref_date)) {
      rlang::abort("The input 'ref_date' must be a date object.")
    }
  } else if (!is.null(interval)) {
    if (!is.numeric(interval) || interval < 1) {
      rlang::abort("The input 'interval' must be a positive integer.")
    }
  } else if (!is.null(nth_release)) {
    if (is.numeric(nth_release) && nth_release < 1) {
      rlang::abort("The input 'nth_release' must be set to a positive integer or 'latest'.")
    } else if (is.character(nth_release) && tolower(nth_release) != "latest") {
      rlang::abort("The input 'nth_release' must be set to a positive integer or 'latest'.")
    }
  }
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)
  
  if (!is.null(ref_date)) {
    # Calculate revisions against the specified reference date
    revisions <- data %>%
      dplyr::inner_join(
        data %>%
          dplyr::filter(pub_date == ref_date) %>%
          dplyr::select(time, value_ref = value),
        by = "time"
      ) %>%
      dplyr::mutate(value = value - value_ref) %>%
      dplyr::select(pub_date, time, value)
    
  } else if (!is.null(interval)) {
    # Calculate revisions relative to estimates published 'interval' periods ago
    revisions <- df %>%
      dplyr::group_by(time) %>%
      dplyr::mutate(
        value_ref = dplyr::lag(value, n = interval, order_by = pub_date)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = value - value_ref) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::select(pub_date, time, value)
  } else if (!is.null(nth_release)) {
    # Calculate revisions relative to the nth release
    revisions <- df %>%
      dplyr::inner_join(
        get_nth_release(df, n = nth_release) %>%
          dplyr::select(time, value_ref = value),
        by = "time"
      )
      dplyr::mutate(value = value - value_ref) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::select(pub_date, time, value)
  }
  class(revisions) <- c("tbl_rev", class(revisions))
  return(revisions)
}

#' get first efficient release using Mincer Zarnowitz regression
#'
#' @param df 
#'
#' @return filtered df only with first data vintage
#' @export
#'
get_first_efficient_release <- function(df, final_release, significance=0.05) {
  
  
  if(!"tbl_nth_rel" %in% class(df)) {
    rlang::abort("The input 'df' must be a 'tbl_nth_rel' object.")
  }
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  check <- vintages_check(final_release)
  if (check=="wide") {
    final_release <- vintages_long(final_release)
  }
  
  # Check for unique dates in the 'time' column
  if (any(duplicated(final_release$time))) {
    rlang::abort("The 'time' column in 'final_release' must have no duplicates.")
  }
  
  final_release <- dplyr::select(final_release, time, value) %>%
    dplyr::mutate(release = "final")
  
  df <- df %>%
    dplyr::select(time, value, release) %>%
    dplyr::mutate(release = paste0("release_", release))
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    bind_rows(final_release) %>%
    dplyr::arrange(time) %>%
    na.omit()
  
  es <- unique(df$release)
  es <- es[es != "final"]
  
  df_wide <- vintages_wide(df, names_from = "release")
  
  models <- list()
  tests <- list()
  for (i in 1:length(es)) {
    formula <- as.formula(paste0("final ~ ", es[i]))
    
    
    model <- stats::lm(formula, data = df_wide)
    hac_se <- sandwich::vcovHAC(model)

    test <- car::linearHypothesis(model, c("(Intercept) = 0", paste0(es[i], " = 1")), vcov = hac_se)

    p_value <- test[2, 'Pr(>F)']
    
    models[[i]] <- model
    tests[[i]] <- test
    
    if (p_value < significance) {
      break
    }
    
  }
  
  if (i == length(es) && p_value > significance) {
    rlang::warn("No efficient release found. Please provide further releases!")
  }
  
  data <- vintages_long(df_wide, names_to = "release")
  
  df_output <- list('e' = (i), 'data' = data, 'models' = models, 'tests' = tests)
  class(df_output) <- c("list_eff_rel", class(df_output))
  return(df_output)
}

#' summary of efficient release models
#'
#' @param output from get_first_efficient_release 
#'
#' @export
#'
summary.list_eff_rel <- function(object) {
  cat("Efficient release: ", object$e, "\n\n")
  cat("Model summary: \n")
  print(summary(object$models[[object$e]]))
  cat("\nTest summary: \n")
  print(object$tests[[object$e]])
}


#' calculate summary statistics
#' 
#' requires input to have initial and revision col
#' 
#' @param df
#'
#' @return dataframe with 1 row and summary stats
#' @export
get_revision_summary <- function(df, final_release) {
  
  if(!"tbl_nth_rel" %in% class(df)) {
    rlang::abort("The input 'df' must be a 'tbl_nth_rel' object.")
  }
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  check <- vintages_check(final_release)
  if (check=="wide") {
    final_release <- vintages_long(final_release)
  }
  
  # Check for unique dates in the 'time' column
  if (any(duplicated(final_release$time))) {
    rlang::abort("The 'time' column in 'final_release' must have no duplicates.")
  }
  
  final_release <- dplyr::select(final_release, time, value) %>%
    dplyr::mutate(release = "final")
  
  df <- df %>%
    dplyr::select(time, value, release) %>%
    dplyr::mutate(release = paste0("release_", release))
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    bind_rows(final_release) %>%
    dplyr::arrange(time) %>%
    na.omit()
  
  es <- unique(df$release)
  es <- es[es != "final"]
  
  df_wide <- vintages_wide(df, names_from = "release")
  df_wide_rev <- get_revisions(df_wide, )
  
  
  noise <- (sd(df_i[["revision"]])**2)/(sd(df_i[["initial"]])**2)
  print("noise")
  print(sd(df_i[["revision"]])**2)
  print("signal")
  print(sd(df_i[["initial"]])**2)
  print("-----")
  
  corr <- cor(df_i[["initial"]], df_i[["revision"]])
  summary_df <- data.frame(
    NoObs = length(df_i[["revision"]]),
    Mean = mean(df_i[["revision"]]),
    Min = min(df_i[["revision"]]),
    Max = max(df_i[["revision"]]),
    SD = sd(df_i[["revision"]]),
    Noise = noise,
    Correlation = corr
  )
  return(summary_df)
}


#' get nth vintage
#'
#' @param df 
#'
#' @return filtered df only with first data vintage
#' @export
#'
get_nth_release <- function(df, n = 1) {
  # Validate inputs
  if (is.numeric(n) && any(n < 1)) {
    rlang::abort("The input 'n' must be set to a positive integer, 'first', or 'latest'.")
  } else if (is.character(n) && !tolower(n) %in% c("first", "latest")) {
    rlang::abort("The input 'n' must be set to a positive integer, 'first', or 'latest'.")
  }
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)
  
  if (is.numeric(n)) {
  # Get the nth release(s)
  nth_release <- df %>%
    dplyr::group_by(time) %>%
    dplyr::slice(n) %>%
    dplyr::mutate(release = as.character((1:dplyr::n())-1)) %>%
    dplyr::ungroup()
  } else if (tolower(n) == "latest") {
    # Get the latest release
    nth_release <- get_latest_release(df)
  } else if (tolower(n) == "first") {
    # Get the first release
    nth_release <- get_first_release(df)
  }
    
  # Add the class only if it is not already present
  if (!"tbl_nth_rel" %in% class(nth_release)) {
    class(nth_release) <- c("tbl_nth_rel", class(nth_release))
  }
  return(nth_release)
}

#' get first vintage
#'
#' @param df 
#'
#' @return filtered df only with first data vintage
#' @export
#'
get_first_release <- function(df) {
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)
  
  df <- df %>%
    dplyr::group_by(time) %>%
    dplyr::filter(pub_date == min(pub_date)) %>%
    dplyr::ungroup()
  
  # Add the class only if it is not already present
  if (!"tbl_nth_rel" %in% class(df)) {
    class(df) <- c("tbl_nth_rel", class(df))
  }
  return(df)
}

#' get latest vintage
#'
#' @param df 
#' @param col_rel_date 
#' @param col_val_date 
#'
#' @return filtered df only with latest data vintage
#' @export
#'
get_latest_release <- function(df) {
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)
  df <- df %>%
    dplyr::group_by(time) %>%
    dplyr::filter(pub_date == max(pub_date)) %>%
    dplyr::ungroup()
  
  # Add the class only if it is not already present
  if (!"tbl_nth_rel" %in% class(df)) {
    class(df) <- c("tbl_nth_rel", class(df))
  }
  return(df)
}

#' get all releases of a single observation date:
get_releases_by_date <- function(df, date) {
  # Validate inputs
  if (!is.Date(date)) {
    rlang::abort("The input 'date' must be a Date object.")
  }
  
  check <- vintages_check(df)
  if (check=="wide") {
    df <- vintages_long(df)
  }
  
  # Ensure data is sorted by pub_date and time
  df <- df %>%
    dplyr::arrange(pub_date, time)
  
  # Get all releases on the specified date
  releases <- df %>%
    dplyr::filter(time == date)
    
  return(releases)
}


#' get the number of days between first release and period end:
get_days_to_release <- function(df) {

  check <- vintages_check(df)
  if (check=="wide") {
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
  
  return(df)
}
