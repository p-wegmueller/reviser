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
#'              - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.
#'              - "Howrey": Howrey's simplified framework.
#'              - "Classical": Classical model without vintage effects.
#'
#' @param trace An integer controlling the level of output for the optimization procedure.
#'              Default is 0 (minimal output).
#' @param maxiter An integer specifying the maximum number of iterations for the optimization procedure.
#' @param startvals A list of starting values for the optimization procedure.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{forecast_states}{A tibble of forecasted state variables, including the forecast dates. Returned only if \code{h > 0}.}
#'   \item{filtered_states}{A tibble of filtered state variables based on the Kalman filter.}
#'   \item{observations}{A tibble of the observed variables used in the model.}
#'   \item{forecast_observation}{A tibble of forecasted observations. Returned only if \code{h > 0}.}
#'   \item{smoothed_states}{A tibble of smoothed state variables based on the Kalman smoother.}
#'   \item{kk_model_mat}{A list of KK model matrices (e.g., transition and observation matrices).}
#'   \item{ss_model_mat}{A list of state-space model matrices derived from the KK model.}
#'   \item{params}{Estimated model parameters, including covariance terms.}
#'   \item{fit}{The fitted model object from the SUR estimation procedure.}
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
#' @export
kk_nowcast <- function(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  trace = 0,
  maxiter = 1000,
  startvals = NULL
) {
  start_mat <- 0.4
  start_cov <- 0.4
  m0 <- 0
  C0 <- 1e-6

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

  # KK cov matrices
  n_param_cov <- e + 1

  n_param <- n_param_mat + n_param_cov

  if (length(start_mat) == 1 && is.numeric(start_mat)) {
    start_mat <- rep(start_mat, n_param_mat)
  }

  if (length(start_cov) == 1 && is.numeric(start_cov)) {
    start_cov <- rep(start_cov, n_param_cov)
  }

  if (!is.null(startvals)) {
    if (length(startvals) != n_param_mat) {
      rlang::abort(paste0(
        "The length of 'startvals' must be ",
        n_param_mat,
        " if 'model' = ",
        model,
        " and e = ",
        e
      ))
    } else {
      start_mat <- startvals
    }
  }

  if (length(c(start_mat, start_cov)) != n_param) {
    rlang::abort(paste0(
      "The length of 'start_mat' must be 1 or ",
      n_param_mat,
      " and the length of 'start_cov' must be 1 or ",
      n_param_cov,
      " if 'e' = ",
      e
    ))
  }

  # Check start values for the expected value of the pre-sample state vector m0
  # and the covariance matrix C0

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

  state_names <- c(paste0("release_", e, "_lag_", (e):0))
  state_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  observable_names <- c(paste0("release_", e:0, "_lag_", e:0))
  observable_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  # Define matrices
  kk_mat_sur <- kk_matrices(e = e, model = model, type = "character")

  X_names <- state_names
  X_lag_names <- state_lag_names
  Y_names <- observable_names
  Y_lag_names <- observable_lag_names

  # Define equations
  lhs1 <- X_names
  rhs1 <- kk_mat_sur$FF %mx% X_lag_names

  lhs2 <- Y_names
  rhs2 <- ((kk_mat_sur$II %diff% kk_mat_sur$GG) %prod% kk_mat_sur$FF) %mx%
    Y_lag_names %sum%
    (kk_mat_sur$GG %mx% X_names)

  equations <- list()
  formula <- stats::as.formula(paste0(lhs1[e + 1], " ~ ", rhs1[e + 1]))
  equations[[paste0("eq", 1)]] <- formula
  eq <- 2
  for (i in 2:(e + 1)) {
    formula <- stats::as.formula(paste0(lhs2[i], " ~ ", rhs2[i]))
    equations[[paste0("eq", eq)]] <- formula
    eq <- eq + 1
  }

  # Arrange data
  X <- array(NA, c(nrow(df), e + 1))
  X_lag <- array(NA, c(nrow(df), e + 1))
  for (j in (e):0) {
    X[, (e + 1) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", e)]), j)
    X_lag[, (e + 1) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", e)]),
      j + 1
    )
  }
  X <- tibble::tibble(as.data.frame(X))
  colnames(X) <- state_names
  X_lag <- tibble::tibble(as.data.frame(X_lag))
  colnames(X_lag) <- state_lag_names

  Y <- array(NA, c(nrow(df), e))
  Y_lag <- array(NA, c(nrow(df), e))
  for (j in (e - 1):0) {
    Y[, (e) - j] <- dplyr::lag(dplyr::pull(df[paste0("release_", j)]), j)
    Y_lag[, (e) - j] <- dplyr::lag(
      dplyr::pull(df[paste0("release_", j)]),
      j + 1
    )
  }

  Y <- tibble::tibble(as.data.frame(Y))
  Y_lag <- tibble::tibble(as.data.frame(Y_lag))
  colnames(Y) <- c(paste0("release_", (e - 1):0, "_lag_", (e - 1):0))
  colnames(Y_lag) <- c(paste0("release_", (e - 1):0, "_lag_", e:1))

  sur_data <- cbind(X, Y, Y_lag) %>% stats::na.omit()

  names(start_mat) <- names(kk_mat_sur$params)[1:n_param_mat]

  fit <- systemfit::nlsystemfit(
    equations,
    method = "SUR",
    data = sur_data,
    startvals = start_mat,
    print.level = trace,
    maxiter = maxiter
  )

  params <- c(fit$b, (diag(fit$rcov)))

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
  colnames(Y) <- observable_names
  Ymat <- as.matrix(Y)

  kalman <- kalman_filter_smoother(
    Y,
    FF = sur_ss_mat$Z,
    V = sur_ss_mat$V,
    GG = sur_ss_mat$Tmat,
    W = sur_ss_mat$W,
    m0 = m0,
    C0 = C0
  )

  # Filtered states
  filtered_states <- tibble::tibble(as.data.frame(kalman$filtered_states[
    1:nrow(kalman$filtered_states),
    1:((e + 1))
  ])) %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(state_names), state_names))

  # Smoothed states
  smoothed_states <- tibble::tibble(as.data.frame(kalman$smoothed_states[
    1:nrow(kalman$smoothed_states),
    1:((e + 1))
  ])) %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    dplyr::select(time, !!!stats::setNames(seq_along(state_names), state_names))

  # Observations
  observations <- Y %>%
    dplyr::mutate(time = df$time[(e + 1):(nrow(df))]) %>%
    # select time and up to release_e
    dplyr::select(time, dplyr::everything())

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
      kalman$filtered_states[nrow(kalman$filtered_states), ]
    if (h > 1) {
      for (hh in 2:(h)) {
        forecast[hh + 1, ] <- sur_ss_mat$Tmat %*% forecast[hh, ]
      }
    }

    # Forecasted states
    forecast_states <- tibble::tibble(as.data.frame(forecast[
      1:(h + 1),
      1:(e + 1)
    ])) %>%
      stats::na.omit() %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(
        time,
        !!!stats::setNames(seq_along(state_names), state_names)
      )

    # Forecasted observations
    forecast_observation <- tibble::tibble(as.data.frame(forecast[
      1:(h + 1),
    ])) %>%
      stats::na.omit()
    forecast_observation <- forecast_observation %>%
      dplyr::mutate(dplyr::across(
        .cols = 1:(ncol(forecast_observation) - (e + 1)), # Columns to sum (e.g., 1 to n-e)
        .fns = ~ . +
          forecast_observation[[
            which(names(forecast_observation) == dplyr::cur_column()) + e
          ]],
        .names = "obs_{col}" # Name of the new column
      )) %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(dplyr::contains("obs_"), time) %>%
      dplyr::select(
        time,
        !!!stats::setNames(seq_along(observable_names), observable_names)
      )
  } else {
    forecast_states <- NULL
    forecast_observation <- NULL
  }

  params <- kk_mat_hat$params

  # Remove the parameters from the model
  kk_mat_hat$params <- NULL

  results <- list(
    forecast_states = forecast_states,
    filtered_states = filtered_states,
    observations = observations,
    forecast_observation = forecast_observation,
    smoothed_states = smoothed_states,
    kk_model_mat = kk_mat_hat,
    ss_model_mat = sur_ss_mat,
    params = params,
    fit = fit,
    e = e
  )
  class(results) <- c("kk_model", class(results))

  return(results)
}


#' Create Matrices for the Kishor-Koenig (KK) or Related Models
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
#'     \item{\code{type = "numeric"}}{Initializes parameters to \code{1e-1}.}
#'     \item{\code{type = "character"}}{Initializes parameters as \code{NA_real_}.}
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
#' # Example 1: Kishor-Koenig model with e = 2
#' matrices <- kk_matrices(e = 2, model = "KK", type = "numeric")
#' str(matrices)
#'
#' # Example 2: Howrey model with custom parameters
#' custom_params <- c(0.5, 0.3, 0.2, 0.1, 0.2, 0.25, 0.05, 0.01)
#' matrices <- kk_matrices(e = 2, model = "Howrey", params = custom_params, type = "numeric")
#' str(matrices)
#'
#' # Example 3: Classical model with character matrices
#' matrices <- kk_matrices(e = 3, model = "Classical", type = "character")
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
  FF[e + 1, e + 1] <- ifelse(type == "numeric", params[ii], "F0")
  names(params) <- c("F0")
  ii <- ii + 1

  # Define G matrix
  GG <- diag(e + 1)

  if (model %in% c("Kishor-Koenig", "KK")) {
    # e * e+1 params for G
    for (i in 1:e) {
      for (j in 1:(e + 1)) {
        GG[i + 1, j] <- ifelse(
          type == "numeric",
          params[ii],
          paste0("G", e - i, "_", e - j + 1)
        )
        names(params)[ii] <- c(paste0("G", e - i, "_", e - j + 1))
        ii <- ii + 1
      }
    }
  } else if (model == "Howrey") {
    # e * e params for G
    for (i in 1:e) {
      for (j in 1:e) {
        GG[i + 1, j] <- ifelse(
          type == "numeric",
          params[ii],
          paste0("G", e - i, "_", e - j + 1)
        )
        names(params)[ii] <- c(paste0("G", e - i, "_", e - j + 1))
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
  R[e + 1, e + 1] <- ifelse(type == "numeric", (params[ii]), "v0")
  names(params)[ii] <- c(paste0("v0"))
  ii <- ii + 1

  # Observation noise covariance
  H <- array(0, c(e + 1, e + 1))
  for (jj in 2:(e + 1)) {
    H[jj, jj] <- ifelse(
      type == "numeric",
      (params[ii]),
      paste0("eps", e + 1 - jj)
    )
    names(params)[ii] <- c(paste0("eps", e + 1 - jj))
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
#' @seealso \code{\link{kalman_filter_smoother}}
#' @noRd
kk_to_ss <- function(II, FF, GG, R, H, epsilon = 1e-6) {
  # Get efficient release, e
  e <- nrow(FF) - 1

  # Observation matrix Z
  # Z <- cbind(GG, (II - GG) %*% FF)
  Z <- cbind(II, II)

  # State transition matrix T
  # Tmat <- rbind(
  #   cbind(FF, matrix(0, e+1, e+1)),
  #   Z
  # )

  Tmat <- rbind(
    cbind(FF, array(0, c(e + 1, e + 1))),
    cbind(array(0, c(e + 1, e + 1)), (II - GG) %*% FF)
  )

  # Covariance matrices
  # V <- array(0,c(e+1, e+1))
  #
  # State noise covariance (x_t and y_t-1)
  # W <- array(0,c(2*(e+1), 2*(e+1)))
  #
  # for (jj in 2:(e+1)) {
  #   V[jj,jj] <- H[jj,jj]  # e param for V0
  #   W[jj+e+1,jj+e+1] <- H[jj,jj]  # same e param for W0
  # }
  #
  # V[1,1] <- epsilon
  #
  # W[e+1,e+1] <- R[e+1,e+1]  # 1 param for W0

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


#' Kalman Filter and Smoother for State-Space Models
#'
#' Implements the Kalman filter and smoother for a given state-space model. It estimates the state variables over time and provides both filtered and smoothed estimates.
#'
#' @param Y Matrix. The observed data, where each row corresponds to an observation and each column to a variable.
#' @param FF Matrix. The observation matrix (\eqn{F}) that maps states to observations.
#' @param V Matrix. The observation noise covariance matrix.
#' @param GG Matrix. The state transition matrix (\eqn{G}).
#' @param W Matrix. The state noise covariance matrix.
#' @param m0 Numeric vector. The initial state mean vector.
#' @param C0 Matrix. The initial state covariance matrix.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{filtered_states}}{Matrix of filtered state estimates at each time point.}
#'     \item{\code{filtered_covariances}}{Array of filtered state covariance matrices.}
#'     \item{\code{smoothed_states}}{Matrix of smoothed state estimates.}
#'     \item{\code{smoothed_covariances}}{Array of smoothed state covariance matrices.}
#'   }
#'
#' @examples
#' # Simulate observations and state-space model parameters
#' Y <- matrix(rnorm(100), ncol = 2)
#' FF <- diag(2)
#' V <- diag(2) * 0.1
#' GG <- diag(2) * 0.9
#' W <- diag(2) * 0.05
#' m0 <- c(0, 0)
#' C0 <- diag(2) * 0.1
#'
#' # Apply Kalman filter and smoother
#' results <- kalman_filter_smoother(Y, FF, V, GG, W, m0, C0)
#' str(results)
#'
#' @noRd
kalman_filter_smoother <- function(Y, FF, V, GG, W, m0, C0) {
  # Dimensions
  n <- nrow(Y) # Number of observations (time points)
  p <- ncol(Y) # Dimension of observations
  m <- nrow(GG) # Dimension of the state vector

  # Storage for results
  a <- matrix(0, nrow = n + 1, ncol = m) # Filtered state estimates
  P <- array(0, dim = c(m, m, n + 1)) # Filtered state covariances
  a[1, ] <- m0 # Initial state mean
  P[,, 1] <- C0 # Initial state covariance

  at <- matrix(0, nrow = n, ncol = m) # Predicted states
  Pt <- array(0, dim = c(m, m, n)) # Predicted covariances

  # Filtering
  for (t in 1:n) {
    # Predict
    at[t, ] <- GG %*% a[t, ]
    Pt[,, t] <- GG %*% P[,, t] %*% t(GG) + W

    # Update
    vt <- Y[t, ] - FF %*% at[t, ] # Prediction error
    Ft <- FF %*% Pt[,, t] %*% t(FF) + V # Prediction covariance
    Kt <- Pt[,, t] %*% t(FF) %*% solve(Ft) # Kalman gain

    a[t + 1, ] <- at[t, ] + Kt %*% t(vt) # Updated state estimate
    P[,, t + 1] <- Pt[,, t] - Kt %*% FF %*% Pt[,, t] # Updated covariance
  }

  # Remove the initial state estimate
  a_filtered <- a[-1, ]
  P_filtered <- P[,, -1]

  # Smoothing
  a_smooth <- matrix(0, nrow = n, ncol = m) # Smoothed state estimates
  P_smooth <- array(0, dim = c(m, m, n)) # Smoothed state covariances

  a_smooth[n, ] <- a_filtered[n, ]
  P_smooth[,, n] <- P_filtered[,, n]

  for (t in (n - 1):1) {
    Jt <- P_filtered[,, t] %*% t(GG) %*% solve(Pt[,, t + 1]) # Smoothing gain
    a_smooth[t, ] <- a_filtered[t, ] + Jt %*% (a_smooth[t + 1, ] - at[t + 1, ])
    P_smooth[,, t] <- P_filtered[,, t] +
      Jt %*% (P_smooth[,, t + 1] - Pt[,, t + 1]) %*% t(Jt)
  }

  # Results
  list(
    filtered_states = a_filtered,
    filtered_covariances = P_filtered,
    smoothed_states = a_smooth,
    smoothed_covariances = P_smooth
  )
}
