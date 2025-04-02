#' Generalized Kishor-Koenig Model for Nowcasting
#'
#' Implements a generalized Kishor-Koenig (KK) model for nowcasting and forecasting
#' with state-space models, allowing for multiple vintages of data, efficient estimation,
#' and Kalman filtering and smoothing.
#'
#' @param df A data frame containing the time series data in either "long" or "wide" format.
#'           It must include columns for the time index and the different release vintages.
#' @param e An integer indicating the number of data vintages to include in the model.
#'          Must be greater than 0.
#' @param h An integer specifying the forecast horizon. Default is 0, which implies no forecasts.
#'          Must be greater than or equal to 0.
#' @param model A string specifying the type of model to use. Options are:
#'
#'  - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.
#'  - "Howrey": Howrey's simplified framework.
#'  - "Classical": Classical model without vintage effects.
#' @param method A string specifying the estimation method to use. Options are "SUR" (default) and "OLS".
#' @param solver_options An optional list to control the behaviour of the
#'  underlying [systemfit::nlsystemfit()] and [stats::nlm()] solvers:
#'
#' - **trace**: An integer controlling the level of output for the optimization procedure.
#'              Default is 0 (minimal output).
#' - **maxiter**: An integer specifying the maximum number of iterations for the optimization procedure. Default is 1000.
#' - **startvals**: A list of starting values for the optimization procedure (must match the number of parameters of the model).
#' - **solvtol**: Tolerance for detecting linear dependencies in the columns of
#'    X in the qr function calls (See [systemfit::nlsystemfit()]). Default is .Machine$double.eps.
#' - **gradtol**: A a positive scalar giving the tolerance at which the scaled
#'    gradient is considered close enough to zero to terminate the algorithm (See [stats::nlm()]). Default is 1e-6.
#' -  **steptol**: A positive scalar providing the minimum allowable relative step length (See [stats::nlm()]). Default is 1e-6.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{filtered_z}{A tibble of filtered latent state variables based on the Kalman filter.}
#'   \item{filtered_y}{A tibble of filtered observed variables based on the Kalman filter.}
#'   \item{smoothed_z}{A tibble of smoothed latent state variables obtained using the Kalman smoother.}
#'   \item{smoothed_y}{A tibble of smoothed observed variables obtained using the Kalman smoother.}
#'   \item{forecast_z}{A tibble of forecasted latent state variables. Returned only if \code{h > 0}.}
#'   \item{forecast_y}{A tibble of forecasted observed variables. Returned only if \code{h > 0}.}
#'   \item{kk_model_mat}{A list of KK model matrices, such as transition and observation matrices.}
#'   \item{ss_model_mat}{A list of state-space model matrices derived from the KK model.}
#'   \item{params}{Estimated model parameters, including covariance terms.}
#'   \item{fit}{The fitted model object from the SUR estimation procedure.}
#'   \item{e}{The number of the efficient release (0-indexed).}
#' }
#'
#' @examples
#' # Example usage:
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id=="US")
#'       ),
#'       start = "1980-01-01"
#'      ),
#'      n = 0:1
#'    )
#' df <- na.omit(dplyr::select(df, -id))
#'
#' e <- 1  # Number of efficient release
#' h <- 2  # Forecast horizon
#' model_result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")
#'
#' model_result$params
#'
#' @details
#' The function supports multiple models, including the full Kishor-Koenig framework, Howrey's model,
#' and a classical approach. It handles data preprocessing, estimation of system equations using
#' Seemingly Unrelated Regressions (SUR), and application of the Kalman filter and smoother.
#'
#' The function requires well-structured input data with multiple vintages. The time series must
#' be regular, and the function automatically checks and transforms the data if needed.
#' @import dplyr
#' @importFrom KFAS SSModel SSMcustom
#' @export
kk_nowcast <- function(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  method = "SUR",
  solver_options = list()
) {
  # Default solver options
  default_solver_options <- list(
    trace = 0,
    maxiter = 1000,
    startvals = NULL,
    solvtol = .Machine$double.eps,
    gradtol = 1e-6,
    steptol = 1e-6
  )

  # Check solver options input is list
  if (!is.list(solver_options)) {
    rlang::abort("'solver_options' must be a list!")
  }

  # Check solver options input names are valid
  if (
    length(setdiff(names(solver_options), names(default_solver_options))) > 0
  ) {
    rlang::abort(
      "Invalid solver options provided. Valid options are: ",
      paste(names(default_solver_options), collapse = ", ")
    )
  }

  # Update default options with user-provided options
  if (length(solver_options) > 0) {
    for (name in names(solver_options)) {
      default_solver_options[[name]] <- solver_options[[name]]
    }
  }

  # Check input e
  if (e == 0) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }

  # Check horizon h
  if (h < 0) {
    rlang::abort("The horizon 'h' must be at least 0!")
  }

  # Check model input
  if (!model %in% c("Kishor-Koenig", "KK", "Howrey", "Classical")) {
    rlang::abort(
      "'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!"
    )
  }

  # Check start values
  # if provided startvalues must be numeric vector
  if (!is.null(default_solver_options$startvals)) {
    if (!is.numeric(default_solver_options$startvals)) {
      rlang::abort(
        "'startvals' must be a named, numeric vector! E.g. c(F0 = 0.2)."
      )
    }
  }
  # check vector is named
  if (!is.null(default_solver_options$startvals)) {
    if (is.null(names(default_solver_options$startvals))) {
      rlang::abort(
        "'startvals' must be a named, numeric vector! E.g. c(F0 = 0.2)."
      )
    }
  }

  # KK input matrices
  n_param_mat <- dplyr::if_else(
    model %in% c("Kishor-Koenig", "KK"),
    1 + e + e^2,
    dplyr::if_else(
      model == "Howrey",
      1 + e^2,
      1 # Classical
    )
  )

  # Define model matrices
  kk_mat_sur <- kk_matrices(e = e, model = model, type = "character")

  # KK cov matrices
  n_param_cov <- e + 1

  n_param <- n_param_mat + n_param_cov

  if (!is.null(default_solver_options$startvals)) {
    if (length(default_solver_options$startvals) != n_param) {
      rlang::abort(paste0(
        "The length of 'startvals' must be ",
        n_param,
        " if 'model' = ",
        model,
        " and e = ",
        e
      ))
    } else {
      start_mat <- kk_matrices(
        e = e,
        model = model,
        params = default_solver_options$startvals,
        type = "numeric"
      )$params[1:n_param_mat]
    }
  } else {
    start_mat <- rep(0.4, n_param_mat)
    names(start_mat) <- names(kk_mat_sur$params)[1:n_param_mat]
  }

  # Check start values for the expected value of the pre-sample state vector m0
  # and the covariance matrix C0
  m0 <- 0
  C0 <- 1e-6

  # Both must be numeric
  if (!is.numeric(m0) || !is.numeric(C0)) {
    rlang::abort("Both 'm0' and 'C0' must be numeric!")
  }
  if (length(m0) == 1) {
    m0 <- rep(m0, (e + 1) * 2)
  } else if (length(m0) != (e + 1) * 2) {
    rlang::abort(
      paste0("The length of 'm0' must be 1 or ", (e + 1) * 2),
      " if 'e' = ",
      e
    )
  }

  if (length(C0) == 1) {
    C0 <- diag(C0, (e + 1) * 2)
  } else if (length(C0) == (e + 1) * 2) {
    C0 <- diag(C0)
  } else if (all(dim(C0) == c((e + 1) * 2, (e + 1) * 2))) {
    C0 <- C0
  } else {
    rlang::abort(paste0(
      "'C0' must be a vector of length 1 or ",
      (e + 1) * 2,
      " or a ",
      (e + 1) * 2,
      "x",
      (e + 1) * 2,
      " matrix if 'e' = ",
      e
    ))
  }

  # Check data input
  check <- vintages_check(df)
  if (check == "long") {
    df <- vintages_wide(df, names_from = "release")
  }

  # Define state and observable variable names
  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  y_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  equations <- kk_equations(
    kk_mat_sur = kk_mat_sur
  )

  sur_data <- kk_arrange_data(
    df = df,
    e = e
  )

  if (method == "SUR") {
    fit <- systemfit::nlsystemfit(
      equations,
      method = "SUR",
      data = sur_data,
      startvals = start_mat,
      print.level = default_solver_options$trace,
      maxiter = default_solver_options$maxiter,
      solvtol = default_solver_options$solvtol,
      steptol = default_solver_options$steptol,
      gradtol = default_solver_options$gradtol
    )
    parm_cov <- (diag(fit$rcov))
    names(parm_cov) <- c("v0", paste0("eps", (e - 1):0))
    params <- c(fit$b, parm_cov)

    # bring params in the right order
    params <- kk_matrices(
      e = e,
      model = model,
      params = params,
      type = "numeric"
    )$params
  } else if (method == "OLS") {
    ols_estim <- kk_ols_estim(
      equations = equations,
      model = model,
      data = sur_data
    )

    fit <- ols_estim$fit

    params <- ols_estim$params
    # bring params in the right order
    params <- kk_matrices(
      e = e,
      model = model,
      params = params,
      type = "numeric"
    )$params
  }

  kk_mat_hat <- kk_matrices(
    e = e,
    model = model,
    params = params,
    type = "numeric"
  )

  sur_ss_mat <- kk_to_ss(
    II = kk_mat_hat$II,
    FF = kk_mat_hat$FF,
    GG = kk_mat_hat$GG,
    R = kk_mat_hat$R,
    H = kk_mat_hat$H,
    epsilon = 1e-6
  )

  Y <- array(NA, c(nrow(df), e + 1))
  for (j in (e):0) {
    Y[, (e + 1) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", j)]), j)
  }
  Y <- stats::na.omit(tibble::tibble(as.data.frame(Y)))
  colnames(Y) <- y_names
  Ymat <- as.matrix(Y)

  # Create the SSM object
  model_kfas <- SSModel(
    Ymat ~
      -1 +
        SSMcustom(
          Z = sur_ss_mat$Z,
          T = sur_ss_mat$Tmat,
          R = diag(1, nrow = nrow(sur_ss_mat$W)),
          Q = sur_ss_mat$W,
          a1 = m0,
          P1 = C0,
          index = c(1:ncol(Ymat))
        ),
    H = sur_ss_mat$V
  )

  # Run the Kalman filter
  kalman <- KFAS::KFS(model_kfas)

  # Filtered states
  filtered_z <- tibble::tibble(as.data.frame(kalman$att[,
    1:((e + 1))
  ])) %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(z_names), z_names))

  filtered_y <- (kalman$att[, 1:(e + 1)] +
    kalman$att[, (e + 2):(2 * (e + 1))]) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(y_names), y_names))

  # Smoothed states
  smoothed_z <- tibble::tibble(as.data.frame(kalman$alphahat[
    1:nrow(kalman$alphahat),
    1:((e + 1))
  ])) %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(z_names), z_names))

  smoothed_y <- (kalman$alphahat[, 1:(e + 1)] +
    kalman$alphahat[, (e + 2):(2 * (e + 1))]) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(y_names), y_names))

  if (h > 0) {
    frequency <- unique((round(as.numeric(diff(df$time)) / 30)))
    if (length(frequency) > 1) {
      rlang::abort(
        "The time series seems not to be regular, please provide a regular time series!"
      )
    }

    forecast_dates <- seq.Date(
      df$time[nrow(df)],
      by = paste0(frequency, " months"),
      length.out = (h + 1)
    )[2:(h + 1)]

    # Forecast
    forecast <- array(NA, c(h + 1, (2 * (e + 1))))
    forecast[2, ] <- sur_ss_mat$Tmat %*%
      kalman$att[nrow(kalman$att), ]
    if (h > 1) {
      for (hh in 2:(h)) {
        forecast[hh + 1, ] <- sur_ss_mat$Tmat %*% forecast[hh, ]
      }
    }

    # Forecasted states
    forecast_z <- tibble::tibble(as.data.frame(forecast[
      1:(h + 1),
      1:(e + 1)
    ])) %>%
      stats::na.omit() %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(
        time,
        !!!stats::setNames(seq_along(z_names), z_names)
      )

    # Forecasted observations
    forecast_y <- tibble::tibble(as.data.frame(forecast[
      1:(h + 1),
    ])) %>%
      stats::na.omit()
    forecast_y <- forecast_y %>%
      dplyr::mutate(dplyr::across(
        .cols = 1:(ncol(forecast_y) - (e + 1)), # Columns to sum (e.g., 1 to n-e)
        .fns = ~ . +
          forecast_y[[
            which(names(forecast_y) == dplyr::cur_column()) + e
          ]],
        .names = "obs_{col}" # Name of the new column
      )) %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(dplyr::contains("obs_"), time) %>%
      dplyr::select(
        time,
        !!!stats::setNames(seq_along(y_names), y_names)
      )
  } else {
    forecast_z <- forecast_y <- NULL
  }

  # Remove the parameters from the model matrices
  kk_mat_hat$params <- NULL

  results <- list(
    filtered_z = filtered_z,
    filtered_y = filtered_y,
    smoothed_z = smoothed_z,
    smoothed_y = smoothed_y,
    forecast_z = forecast_z,
    forecast_y = forecast_y,
    kk_model_mat = kk_mat_hat,
    ss_model_mat = sur_ss_mat,
    params = params,
    fit = fit,
    e = e
  )
  class(results) <- c("kk_model", class(results))

  return(results)
}

#' @title Create Equations for Kishor-Koenig (KK) Models
#'
#' @description
#' This function generates a list of formula objects representing the equations
#' for the Kishor-Koenig (KK) models based on the provided KK matrix structure.
#'
#' @param kk_mat_sur A list containing the KK matrix structure, including matrices
#'   `FF`, `II`, and `GG`.
#'
#' @return A list of formula objects representing the equations of the KK model.
#'
#' @details
#' The function constructs the equations based on the dimensions of the input
#' matrices and generates formulas for each equation. It utilizes lagged variables
#' and matrix operations to form the relationships.
#'
#' @keywords internal
#' @noRd
kk_equations <- function(kk_mat_sur) {
  e <- dim(kk_mat_sur$II)[1] - 1

  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  y_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  lhs1 <- z_names
  rhs1 <- kk_mat_sur$FF %mx% z_lag_names

  lhs2 <- (y_names)
  rhs2 <- (((kk_mat_sur$II %diff% kk_mat_sur$GG) %prod% kk_mat_sur$FF) %mx%
    (y_lag_names)) %sum%
    (kk_mat_sur$GG %mx% z_names)

  equations <- list()
  formula <- stats::as.formula(paste0(lhs1[e + 1], " ~ ", rhs1[e + 1]))
  equations[[paste0("eq", 1)]] <- formula
  eq <- 2
  for (i in 2:(e + 1)) {
    formula <- stats::as.formula(paste0(lhs2[i], " ~ ", rhs2[i]))
    equations[[paste0("eq", eq)]] <- formula
    eq <- eq + 1
  }
  return(equations)
}

#' @title Arrange Data for Kishor-Koenig (KK) Models
#'
#' @description
#' This function arranges the input data frame into a format suitable for estimating
#' Kishor-Koenig (KK) models. It generates lagged variables and combines them
#' into a data frame.
#'
#' @param df A data frame containing the original data.
#' @param e An integer indicating the efficient release.
#'
#' @return A data frame with lagged variables, prepared for KK model estimation.
#'
#' @details
#' The function creates lagged versions of the release variables up to the specified
#' lag `e`. It constructs variables named `release_e_lag_e:0`, `release_e_lag_(e+1):1`,
#' `release_e:0_lag_e:0`, and `release_e:0_lag_(e+1):1`. The function then combines
#' these variables into a single data frame, removing rows with missing values.
#'
#' @keywords internal
#' @noRd
kk_arrange_data <- function(df, e) {
  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  y_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  # Arrange data
  z <- array(NA, c(nrow(df), e + 1))
  z_lag <- array(NA, c(nrow(df), e + 1))
  for (j in (e):0) {
    z[, (e + 1) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", e)]), j)
    z_lag[, (e + 1) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", e)]),
      j + 1
    )
  }
  z <- tibble::tibble(as.data.frame(z))
  colnames(z) <- z_names
  z_lag <- tibble::tibble(as.data.frame(z_lag))
  colnames(z_lag) <- z_lag_names

  y <- array(NA, c(nrow(df), e))
  y_lag <- array(NA, c(nrow(df), e))
  for (j in (e - 1):0) {
    y[, (e) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", j)]), j)
    y_lag[, (e) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", j)]),
      j + 1
    )
  }

  y <- tibble::tibble(as.data.frame(y))
  y_lag <- tibble::tibble(as.data.frame(y_lag))
  colnames(y) <- c(paste0("release_", (e - 1):0, "_lag_", (e - 1):0))
  colnames(y_lag) <- c(paste0("release_", (e - 1):0, "_lag_", e:1))

  data <- cbind(z, y, y_lag) %>% stats::na.omit()

  return(data)
}

#' @title Estimate Generalized Kishor-Koenig (KK) Models via OLS
#'
#' @description
#' This function estimates the parameters of generalized Kishor-Koenig (KK) models
#' using ordinary least squares (OLS). It processes a set of equations, extracts
#' necessary information, and fits linear models. This function is intended for
#' internal use within the package.
#'
#' @param equations A list of formula objects representing the equations of the
#'   KK model.
#' @param data A data frame containing the variables used in the equations.
#' @param model A character string specifying the model type. Must be one of
#'   "KK", "Kishor-Koenig", "Howrey", or "Classical". Defaults to "KK".
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item{\code{params}: A named numeric vector containing the estimated
#'           parameters (coefficients and variances).}
#'     \item{\code{fit}: A list of fitted \code{lm} model objects.}
#'   }
#'
#' @details
#' This function is designed to handle different variations of the KK model, including
#' "Classical", "Howrey", and the standard "KK" or "Kishor-Koenig" models.
#' It extracts coefficients and variances based on the specified model type.
#'
#' @keywords internal
#' @noRd
kk_ols_estim <- function(equations, data, model = "KK") {
  n_eq <- length(equations)
  e <- n_eq - 1

  # Remove all brackets and the term "F0" from eq 1
  eq1 <- gsub("\\(|\\)", "", deparse(equations[[1]]))
  eq1 <- gsub("F0", "", eq1)
  eq1 <- gsub("\\*", "", eq1)
  formula <- paste0(eq1, " -1")

  F0_mod <- stats::lm(
    stats::as.formula(formula),
    data = data
  )

  fit <- list()

  fit[[1]] <- F0_mod

  F0 <- stats::coef(F0_mod)
  var_v <- summary(F0_mod)$sigma^2

  ols_coeffs <- c("F0" = as.numeric(F0))
  ols_vars <- c("v0" = as.numeric(var_v))

  for (ii in 2:n_eq) {
    eq <- deparse(equations[[ii]])
    # Step 1: Extract the signs
    signs <- unlist(regmatches(
      eq,
      gregexpr("(?<=[^\\w])[-+]", eq, perl = TRUE)
    ))

    # Step 2: Extract the variable names
    variables <- unlist(regmatches(
      eq,
      gregexpr("release_[^ )]+", eq)
    ))

    # Step 3: Extract the parameters
    pars <- unlist(regmatches(
      eq,
      gregexpr("G\\d+_\\d+", eq, perl = TRUE)
    ))

    # Step 4: Combine signs and variable names
    # Handle cases where the first term might not have an explicit sign
    if (length(signs) < (length(variables) - 1)) {
      signs <- c("+", signs) # Assume first term has implicit '+'
    }

    if (model == "Classical") {
      sig2 <- var(data[[variables[1]]] - data[[variables[2]]])
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
    } else if (model == "Howrey") {
      # Check length of extracted elements (always same in Howrey) except last equation
      if (
        length(signs) != (length(variables) - 1) ||
          length(signs) != length(pars) && !ii == n_eq
      ) {
        rlang::abort(
          "Error: Incorrect number of elements extracted from equation"
        )
      }

      uniq_pars <- unique(pars)

      # Create transformed regressors based on gs mapping
      regressors <- stats::setNames(
        vector("list", length(uniq_pars)),
        uniq_pars
      )

      for (i in seq_along(pars)) {
        g <- pars[i]
        var <- variables[i + 1]
        sign <- ifelse(signs[i] == "+", 1, -1)

        if (is.null(regressors[[g]])) {
          regressors[[g]] <- sign * data[[var]]
        } else {
          regressors[[g]] <- regressors[[g]] + sign * data[[var]]
        }
      }

      # Convert to data frame
      df_regressors <- as.data.frame(regressors)

      # Add dependent variable
      if (ii == n_eq) {
        df_regressors[[variables[1]]] <- data[[variables[1]]] -
          data[[variables[length(variables)]]]
      } else {
        df_regressors[[variables[1]]] <- data[[variables[1]]]
      }

      # Construct formula
      formula <- stats::as.formula(paste(
        variables[1],
        "~",
        paste0(
          paste(names(df_regressors)[-ncol(df_regressors)], collapse = " + "),
          " -1"
        )
      ))

      # Estimate model
      modeli <- stats::lm(formula, data = df_regressors)
      ols_coeffs <- c(ols_coeffs, modeli$coefficients)
      sig2 <- summary(modeli)$sigma^2
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
      fit[[ii]] <- modeli
    } else if (model %in% c("KK", "Kishor-Koenig")) {
      # Check length of extracted elements (always same in KK) except last equation
      if (
        length(signs) != (length(variables) - 1) ||
          length(signs) != length(pars) && !ii == n_eq
      ) {
        rlang::abort(
          "Error: Incorrect number of elements extracted from equation"
        )
      }

      # Unique coefficients to estimate
      uniq_pars <- unique(pars)

      # Create transformed regressors based on gs mapping
      regressors <- stats::setNames(
        vector("list", length(uniq_pars)),
        uniq_pars
      )

      for (i in seq_along(pars)) {
        g <- pars[i]
        var <- variables[i + 1]
        sign <- ifelse(signs[i] == "+", 1, -1)

        if (is.null(regressors[[g]])) {
          if (i == 1 & ii == n_eq) {
            regressors[[g]] <- sign * data[[var]] * ols_coeffs["F0"]
          } else {
            regressors[[g]] <- sign * data[[var]]
          }
        } else {
          regressors[[g]] <- regressors[[g]] + sign * data[[var]]
        }
      }

      # Convert to data frame
      df_regressors <- as.data.frame(regressors)
      # Add dependent variable
      if (ii == n_eq) {
        df_regressors[[variables[1]]] <- data[[variables[1]]] -
          data[[variables[2]]] * ols_coeffs["F0"]
      } else {
        df_regressors[[variables[1]]] <- data[[variables[1]]]
      }

      # Construct formula
      formula <- stats::as.formula(paste(
        variables[1],
        "~",
        paste0(
          paste(names(df_regressors)[-length(df_regressors)], collapse = " + "),
          " -1"
        )
      ))

      # Estimate model
      modeli <- stats::lm(formula, data = df_regressors)
      ols_coeffs <- c(ols_coeffs, modeli$coefficients)
      sig2 <- summary(modeli)$sigma^2
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
      fit[[ii]] <- modeli
    }
  }
  params <- c(ols_coeffs, ols_vars)
  return(list(params = params, fit = fit))
}

#' Create Matrices for the generalized Kishor-Koenig (KK) model
#'
#' Constructs the matrices \eqn{I}, \eqn{F}, \eqn{G}, \eqn{R}, and \eqn{H} used in
#' state-space models, specifically for the Kishor-Koenig (KK), Howrey, or Classical frameworks.
#'
#' @param e Integer. The number of efficiency gaps (lags) in the model. Must be greater than 0.
#' @param model Character. Specifies the type of model to use. Options are:
#'   \describe{
#'     \item{"Kishor-Koenig" or "KK"}{Uses the Kishor-Koenig framework with \eqn{e \times (e+1)} parameters for the \eqn{G} matrix.}
#'     \item{"Howrey"}{Uses the Howrey framework with \eqn{e \times e} parameters for the \eqn{G} matrix.}
#'     \item{"Classical"}{Uses a diagonal identity matrix for \eqn{G}.}
#'   }
#' @param params Numeric vector (optional). A vector of parameters to initialize the matrices. If \code{NULL}, default values are used:
#'   \describe{
#'     \item{\code{type = "numeric"}}{A vector of params must be supplied.}
#'     \item{\code{type = "character"}}{Initializes named parameters as \code{NA_real_}.}
#'   }
#'   If provided, the length of \code{params} must match the number of parameters required by the specified model.
#' @param type Character. Specifies the type of matrices returned. Options are:
#'   \describe{
#'     \item{"numeric"}{Returns numeric matrices with parameter values.}
#'     \item{"character"}{Returns character matrices with parameter names. If \code{params} is provided, it is ignored.}
#'   }
#'
#' @return A list containing the following components:
#'   \describe{
#'     \item{\code{II}}{Identity matrix (\eqn{I}). Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{FF}}{State transition matrix (\eqn{F}). Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{GG}}{Control matrix (\eqn{G}). Size depends on the model and \code{e}.}
#'     \item{\code{R}}{State noise covariance matrix (\eqn{R}). Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{H}}{Observation noise covariance matrix (\eqn{H}). Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{params}}{The vector of parameters used to construct the matrices, including their names.}
#'   }
#'
#' @examples
#' # Example 1: Kishor-Koenig model with character matrices
#' matrices <- kk_matrices(e = 3, model = "KK", type = "character")
#' str(matrices)
#'
#' # Example 2: Kishor-Koenig model with e = 2
#' params <- rep(0.1, 17)
#' names(params) <- names(matrices$params)
#' matrices <- kk_matrices(e = 3, params = params,  model = "KK", type = "numeric")
#' str(matrices)
#'
#' @export
kk_matrices <- function(e, model, params = NULL, type = "numeric") {
  # Start param count
  ii <- 1

  # Check input e
  if (e == 0) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }

  # Check model input
  if (!model %in% c("Kishor-Koenig", "KK", "Howrey", "Classical")) {
    rlang::abort(
      "'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!"
    )
  }

  # Check type input
  if (!type %in% c("numeric", "character")) {
    rlang::abort("'type' must be one of 'numeric' or 'character'!")
  }

  # Check params input
  if (!is.null(params) && type == "character") {
    rlang::warn(
      "If argument 'type' is 'character', argument 'params' is ignored!"
    )
  }

  if (is.null(params) && type == "numeric") {
    rlang::abort(
      "If argument 'type' is 'numeric', argument 'params' must be provided!"
    )
  }

  # Check params are named
  if (!is.null(params) && !all(!is.na(names(params)))) {
    rlang::abort("All parameters must be named!")
  }

  # Check params input
  n_param_mat <- dplyr::if_else(
    model %in% c("Kishor-Koenig", "KK"),
    1 + e + e^2,
    dplyr::if_else(
      model == "Howrey",
      1 + e^2,
      1 # Classical
    )
  )

  n_param_cov <- e + 1

  n_param <- n_param_mat + n_param_cov

  if (!is.null(params) & length(params) != n_param) {
    rlang::abort(paste0(
      "'params' must have length ",
      n_param,
      ", not ",
      length(params)
    ))
  }

  if (is.null(params) && type == "numeric") {
    params <- rep(1e-1, n_param)
  } else if (is.null(params) && type == "character") {
    params <- rep(NA_real_, n_param)
  }

  # Define I matrix
  II <- diag(e + 1)

  # Define F matrix
  FF <- array(0, c(e + 1, e + 1))
  FF[1:(e), 2:(e + 1)] <- diag(e)
  if (type == "numeric") {
    FF[e + 1, e + 1] <- params[[paste0("F0")]]
  } else if (type == "character") {
    FF[e + 1, e + 1] <- "F0"
    names(params) <- c("F0")
  }
  ii <- ii + 1

  # Define G matrix
  GG <- diag(e + 1)

  if (model %in% c("Kishor-Koenig", "KK")) {
    # e * e+1 params for G
    for (i in 1:e) {
      for (j in 1:(e + 1)) {
        if (type == "numeric") {
          GG[i + 1, j] <- params[[paste0("G", e - i, "_", e - j + 1)]]
        } else if (type == "character") {
          GG[i + 1, j] <- paste0("G", e - i, "_", e - j + 1)
          names(params)[ii] <- c(paste0("G", e - i, "_", e - j + 1))
        }
        ii <- ii + 1
      }
    }
  } else if (model == "Howrey") {
    # e * e params for G
    for (i in 1:e) {
      for (j in 1:e) {
        if (type == "numeric") {
          GG[i + 1, j] <- params[[paste0("G", e - i, "_", e - j + 1)]]
        } else if (type == "character") {
          GG[i + 1, j] <- paste0("G", e - i, "_", e - j + 1)
          names(params)[ii] <- c(paste0("G", e - i, "_", e - j + 1))
        }

        ii <- ii + 1
      }
    }
  } else if (model == "Classical") {
    GG <- diag(e + 1)
  } else {
    rlang::abort("'model' not recognized")
  }

  # Get Variance-covariance matrices
  # State noise covariance
  R <- array(0, c(e + 1, e + 1))
  if (type == "numeric") {
    R[e + 1, e + 1] <- params[[paste0("v0")]]
  } else if (type == "character") {
    R[e + 1, e + 1] <- "v0"
    names(params)[ii] <- c(paste0("v0"))
  }
  ii <- ii + 1

  # Observation noise covariance
  H <- array(0, c(e + 1, e + 1))
  for (jj in 2:(e + 1)) {
    if (type == "numeric") {
      H[jj, jj] <- params[[paste0("eps", e + 1 - jj)]]
    } else if (type == "character") {
      H[jj, jj] <- paste0("eps", e + 1 - jj)
      names(params)[ii] <- c(paste0("eps", e + 1 - jj))
    }

    ii <- ii + 1
  }

  if (type == "character") {
    II <- apply(II, c(1, 2), as.character)
    FF <- apply(FF, c(1, 2), as.character)
    GG <- apply(GG, c(1, 2), as.character)
    R <- apply(R, c(1, 2), as.character)
    H <- apply(H, c(1, 2), as.character)
  } else if (type == "numeric") {
    II <- apply(II, c(1, 2), as.numeric)
    FF <- apply(FF, c(1, 2), as.numeric)
    GG <- apply(GG, c(1, 2), as.numeric)
    R <- apply(R, c(1, 2), as.numeric)
    H <- apply(H, c(1, 2), as.numeric)
  }

  # Sort params to ensure consistency
  params <- c(
    params["F0"],
    params[grep("G", names(params))][order(names(params[grep(
      "G",
      names(params)
    )]))],
    params["v0"],
    params[grep("eps", names(params))][order(names(params[grep(
      "eps",
      names(params)
    )]))]
  )

  return(list(II = II, FF = FF, GG = GG, R = R, H = H, params = params))
}


#' Cast Generalized Kishor-Koenig Matrices into State-Space Form
#'
#' Transforms the generalized Kishor-Koenig (KK) matrices into the state-space representation required for Kalman filtering and smoothing.
#'
#' @param II Matrix. The identity matrix used in the observation and state transition matrices.
#' @param FF Matrix. The state transition matrix defining how the state evolves over time.
#' @param GG Matrix. The control matrix, representing the influence of the state on observations.
#' @param R Matrix. The state noise covariance matrix.
#' @param H Matrix. The observation noise covariance matrix.
#' @param epsilon Numeric. A small positive number to ensure numerical stability in covariance matrices (default: \code{1e-6}).
#'
#' @return A list containing the state-space matrices:
#'   \describe{
#'     \item{\code{Z}}{The observation matrix.}
#'     \item{\code{Tmat}}{The state transition matrix for the augmented state-space model.}
#'     \item{\code{V}}{The observation noise covariance matrix.}
#'     \item{\code{W}}{The state noise covariance matrix.}
#'   }
#'
#' @examples
#' # Define example matrices
#' II <- diag(3)
#' FF <- matrix(c(0.9, 0.1, 0, 0.1, 0.8, 0.1, 0, 0.1, 0.9), nrow = 3)
#' GG <- matrix(c(0.8, 0.2, 0, 0.2, 0.7, 0.1, 0, 0.1, 0.8), nrow = 3)
#' R <- diag(3) * 0.01
#' H <- diag(3) * 0.02
#'
#' # Generate state-space matrices
#' ss_matrices <- kk_to_ss(II, FF, GG, R, H)
#' str(ss_matrices)
#'
#' @export
kk_to_ss <- function(II, FF, GG, R, H, epsilon = 1e-6) {
  # Get efficient release, e
  e <- nrow(FF) - 1

  # Observation matrix Z
  Z <- cbind(II, II)

  # State transition matrix
  Tmat <- rbind(
    cbind(FF, array(0, c(e + 1, e + 1))),
    cbind(array(0, c(e + 1, e + 1)), (II - GG) %*% FF)
  )

  # Covariance matrices
  V <- array(0, c(e + 1, e + 1))
  W <- array(0, c(2 * (e + 1), 2 * (e + 1)))
  v_t_2 <- R[1:(e + 1), 1:(e + 1)]
  W[1:(e + 1), 1:(e + 1)] <- v_t_2
  W[(1:(e + 1)), ((e + 2):(2 * (e + 1)))] <- -v_t_2 %*% t(II - GG)
  W[((e + 2):(2 * (e + 1))), 1:(e + 1)] <- -(II - GG) %*% v_t_2
  W[((e + 2):(2 * (e + 1))), ((e + 2):(2 * (e + 1)))] <- H[
    1:(e + 1),
    1:(e + 1)
  ] +
    (II - GG) %*% v_t_2 %*% t(II - GG)

  for (jj in c(1:e, e + 2)) {
    W[jj, jj] <- epsilon
  }

  return(list(Z = Z, Tmat = Tmat, V = V, W = W))
}

#' Jacobs van Norde State Space Model for Nowcasting
#'
#' Coming Soon!
#'
# jvn_nowcast <- function() {
#   # Create a function to nowcast the JVN model
#   print("Coming soon!")
#   # R port of the Matlab code for State Space Model estimation using Kalman Filter
#
#   # Model specifications
#   uni <- 1 # Univariate model
#   crossCorrNoise <- 1
#   crossCorrNews <- 1
#   spill <- 0
#   stat <- 1 # Stationarity check
#   const <- 1 # Constant in state equation
#   dum <- 0 # Dummies for comprehensive revisions
#   nVAR <- 1 # Number of variables in VAR
#   q <- 1 # Lag length
#
#   # Data Loading and Preparation
#   data_gdp_i <- read_csv("DataDiagonals-GRRGDI.csv")
#   data_gdp_e <- read_csv("DataDiagonals-GRRGDP.csv")
#
#   # Ensure data_gdp_i and data_gdp_e are available and have the same structure as in Matlab
#   ydata <- cbind(
#     data_gdp_i[1:58, 2], # Adjusted index for R (1-based)
#     data_gdp_i[1:58, 13],
#     data_gdp_i[1:58, 25],
#     data_gdp_e[1:58, 2],
#     data_gdp_e[1:58, 4],
#     data_gdp_e[1:58, 13],
#     data_gdp_e[1:58, 25]
#   ) %>%
#     as.matrix() # Convert to matrix
#   # Define State Space Model Matrices
#   n1 <- 3
#   n2 <- 4
#   n <- n1 + n2 # Number of observables
#   Ks <- 2 * n # Number of shocks
#
#   # Z matrix
#   Z <- matrix(0, nrow = n, ncol = n + n + nVAR)
#   Z[, 1:nVAR] <- kronecker(matrix(1, 1, nVAR), matrix(1, n, 1))
#   Z[, nVAR + 1:n] <- diag(n)
#   Z[, nVAR + n + 1:n] <- diag(n)
#   Z <- Z[, -c(n1 + 1, n1 + n2 + 1 + nVAR)] # Remove columns
#
#   # R matrix
#   R1 <- matrix(1, 1, n1)
#   R2 <- matrix(1, 1, n2)
#
#   Vl1 <- matrix(0, n1, n1)
#   Vl2 <- matrix(0, n2, n2)
#
#   for (ix in 1:n1) {
#     for (jx in 1:n1) {
#       if (jx > ix) {
#         Vl1[ix, jx] <- 1
#       }
#     }
#   }
#
#   for (ix in 1:n2) {
#     for (jx in 1:n2) {
#       if (jx > ix) {
#         Vl2[ix, jx] <- 1
#       }
#     }
#   }
#
#   R <- rbind(
#     cbind(R1, R2, matrix(0, 1, Ks - n)),
#     cbind(-Vl1 %*% diag(c(R1)), matrix(0, n1, n2 + n)),
#     cbind(matrix(0, n2, n1), -Vl2 %*% diag(c(R2)), matrix(0, n2, n)),
#     cbind(matrix(0, n, n), diag(n))
#   )
#   R <- R[-c(n1 + 1, n1 + n2 + 1), ]
#   R <- R[, -c(n1 + 1)]
#
#   Ks <- ncol(R)
#
#   # Model Building
#
#   build_model <- function(ydata, Z, R, q, const, nVAR) {
#     T <- nrow(ydata)
#     K <- ncol(R)
#     M <- K * q
#
#     # Transition matrix (Tea) - simplified for clarity, needs proper VAR estimation
#     Tea <- matrix(0, nrow = M, ncol = M)
#     if (const == 1) {
#       Tea[1, 1] <- 0 # Placeholder for constant
#     }
#     if (q >= 1) {
#       Tea[const + 1:K, const + 1:K] <- diag(K) # Placeholder for AR coefficients
#     }
#
#     # State equation
#     state_equation <- SSModel(
#       y = ydata,
#       Z = Z,
#       H = matrix(0, nrow = n, ncol = n), # H is 0
#       T = Tea,
#       R = R,
#       Q = diag(Ks), # Placeholder for Q, estimated later
#       a1 = matrix(0, M, 1),
#       P1 = diag(M)
#     )
#
#     return(state_equation)
#   }
#
#   # Create the model
#   model <- build_model(ydata, Z, R, q, const, nVAR)
#
#   # Kalman Filter Estimation
#   # In KFAS, parameters to be estimated are in the 'params' argument
#   # We need to map our Q and Tea (VAR coefficients) into 'params'
#
#   # Define a function to update the model with new parameters
#   update_model_params <- function(model, params, K, q, const, Ks) {
#     # Update Tea (transition matrix) - this is a simplified version
#     if (const == 1) {
#       model$T[1, 1] <- params[1] # Placeholder for constant
#     }
#     if (q >= 1) {
#       model$T[const + 1:K, const + 1:K] <- diag(K) # Placeholder for AR coefficients
#     }
#
#     # Update Q (covariance matrix)
#     Q_params_count <- Ks # Number of parameters in Q (diagonal elements)
#     Q_params <- params[seq_len(Q_params_count) + max(1, const)]
#     diag(model$Q) <- Q_params
#
#     return(model)
#   }
#
#   # Define the number of parameters to estimate
#   n_params <- Ks + const # For Q (diagonal) and constant in Tea
#
#   # Initial parameter values
#   initial_params <- c(rep(0.1, max(1, const)), rep(0.1, Ks)) # Initial values for Tea and Q
#
#   # Log-likelihood function for optimization
#   loglik_ssm <- function(params, model, K, q, const, Ks) {
#     model <- update_model_params(model, params, K, q, const, Ks)
#     -logLik(model)
#   }
#
#   # Optimization using optim (Nelder-Mead is a robust choice)
#   fit <- optim(
#     initial_params,
#     loglik_ssm,
#     model = model,
#     K = Ks,
#     q = q,
#     const = const,
#     Ks = Ks,
#     control = list(maxit = 1000)
#   )
#
#   # Estimated parameters
#   estimated_params <- fit$par
#
#   # Update the model with estimated parameters
#   model <- update_model_params(model, estimated_params, Ks, q, const, Ks)
#
#   # Kalman Filtering and Smoothing
#   # Use KFAS functions for filtering and smoothing
#   filtered_states <- KFS(model)
#
#   # Results
#   print("Estimated Parameters:")
#   print(estimated_params)
#
#   # Extract smoothed states
#   smoothed_states <- KFS(
#     model,
#     filtering = FALSE,
#     smoothing = c("state", "mean", "variance", "disturbance")
#   )
#   alpha_hat <- smoothed_states$a
#   # Further analysis (e.g., plotting, inference) can be done with the filtered/smoothed states
# }
