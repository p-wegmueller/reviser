#' Jacobs-Van Norden Model for Data Revisions
#'
#' Implements the Jacobs & Van Norden (2011) state-space model for analyzing
#' data revisions, allowing for news and noise components in measurement errors.
#'
#' @param df A matrix or data frame where each column represents a different 
#'   vintage estimate for the same time period. Rows are time periods.
#' @param ar_order Integer specifying the AR order for true values (default = 2).
#' @param h Integer specifying the forecast horizon (default = 0).
#' @param include_news Logical, whether to include news component in measurement 
#'   error (default = TRUE).
#' @param include_noise Logical, whether to include noise component in measurement 
#'   error (default = TRUE).
#' @param include_spillovers Logical, whether to include spillover effects 
#'   (default = FALSE).
#' @param spillover_news Logical, whether spillovers apply to news component 
#'   (default = TRUE).
#' @param spillover_noise Logical, whether spillovers apply to noise component 
#'   (default = TRUE).
#' @param method A string specifying the estimation method to use. Options are
#'  Maximum likelihood ("MLE") for now.
#' @param solver_options List of options for the optimizer:
#'   - trace: Integer controlling output level (default = 0)
#'   - maxiter: Maximum iterations (default = 1000)
#'   - startvals: Named vector of starting values (optional)
#'   - transform_se: T/F whether standard errors should be constrained to be 
#'     positive in optimization.
#'.  - method: String specifying optimization method (default = "BFGS").
#'
#' @return A list of class 'jvn_model' with components:
#' \describe{
#'   \item{filtered_true}{Filtered estimates of true values}
#'   \item{smoothed_true}{Smoothed estimates of true values}
#'   \item{forecast_true}{Forecasted true values (if h > 0)}
#'   \item{forecast_vintages}{Forecasted vintage values (if h > 0)}
#'   \item{jvn_model_mat}{Model matrices (Z, T, R, H, Q)}
#'   \item{params}{Estimated parameters with standard errors}
#'   \item{fit}{Optimization results}
#'   \item{loglik}{Log-likelihood value}
#'   \item{aic}{Akaike Information Criterion}
#'   \item{bic}{Bayesian Information Criterion}
#'   \item{data}{Input data}
#' }
#'
#' @references Jacobs, Jan P.A.M. and Van Norden, Simon, "Modeling Data 
#' Revisions: Measurement Error and Dynamics of 'True' Values", Journal of 
#' Econometrics, 2011.
#'
#' @examples
#' # Simulate data
#' set.seed(123)
#' n_obs <- 150
#' n_vint <- 4
#' y_true <- arima.sim(n = n_obs, list(ar = c(0.4, 0.1)), sd = 0.9)
#' data <- matrix(NA, n_obs, n_vint)
#' for(v in 1:n_vint) {
#'   data[, v] <- y_true + rnorm(n_obs, 0, 0.3 / v)
#' }
#' 
#' # Estimate model
#' result <- jvn_nowcast(
#'   df = data,
#'   ar_order = 2,
#'   h = 12,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' 
#' @export
jvn_nowcast <- function(df,
                        e,
                        ar_order = 1,
                        h = 0,
                        include_news = TRUE,
                        include_noise = TRUE,
                        include_spillovers = FALSE,
                        spillover_news = TRUE,
                        spillover_noise = TRUE,
                        method = "MLE",
                        alpha = 0.05,
                        solver_options = list()) {
  
  # Default solver options
  default_solver_options <- list(
    trace = 0,
    method = "BFGS",
    maxiter = 1000,
    transform_se = TRUE,
    startvals = NULL
  )
  
  # Check ar order is integer and > 0:
  if (ar_order <= 0) {
    rlang::abort("'ar_order' must be > 0!")
  } else (
    ar_order <- round(ar_order)
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
  
  # Check data input
  check <- vintages_check(df)
  if (check == "long") {
    df <- vintages_wide(df, names_from = "release")
  }
  
  # Arrange data
  df_intern <- df[,1:(e+1)]
  z_names <- colnames(df_intern)[2:(e+1)]
  
  Ymat <- as.matrix(dplyr::select(df_intern, -time))
  rownames(Ymat) <- as.character(df$time)
  
  # Build model structure
  model_struct <- jvn_matrices(
    n_obs = nrow(df_intern),
    n_vint = ncol(df_intern)-1,
    ar_order = ar_order,
    include_news = include_news,
    include_noise = include_noise,
    include_spillovers = include_spillovers,
    spillover_news = spillover_news,
    spillover_noise = spillover_noise
  )

  # Initialize parameters
  if (!is.null(default_solver_options$startvals)) {
    if (length(default_solver_options$startvals) != model_struct$n_params) {
      rlang::abort(paste0(
        "The length of 'startvals' must be ",
        model_struct$n_params
      ))
    }
    init_params <- default_solver_options$startvals
  } else {
    init_params <- jvn_init_params(
      model_struct, 
      transform_se = default_solver_options$transform_se
      )
  }
  
  if (default_solver_options$trace > 0) {
    cat("Estimating JVN model with", model_struct$n_params, "parameters...\n")
    cat("AR order:", ar_order, "\n")
    cat("News:", include_news, " Noise:", include_noise, 
        " Spillovers:", include_spillovers, "\n\n")
  }
  
  # Optimize
  opt_result <- stats::optim(
    par = init_params,
    fn = jvn_negloglik,
    model_struct = model_struct,
    data = Ymat,
    transform_se = default_solver_options$transform_se,
    method = default_solver_options$method,
    control = list(
      trace = default_solver_options$trace,
      maxit = default_solver_options$maxiter
    ),
    hessian = FALSE
  )
  
  # Extract results (raw scale from optimizer)
  params_raw <- opt_result$par
  loglik <- -opt_result$value
  
  # Calculate initial standard errors (on the scale the optimizer used)
  # Calculate high-precision Hessian
  precise_hessian <- numDeriv::hessian(
    func = jvn_negloglik,
    x = params_raw,           
    model_struct = model_struct,              
    data = Ymat,
    transform_se = default_solver_options$transform_se
  )
  se_raw <- tryCatch({
    sqrt(diag(MASS::ginv(precise_hessian)))
  }, error = function(e) {
    rep(NA, length(params_raw))
  })
  
  # Initialize final objects
  params <- params_raw
  se <- se_raw
  
  # Apply transformation and Delta Method
  if (default_solver_options$transform_se) {
    # List of indices to transform
    idx_to_transform <- c(model_struct$param_info$sigma_e_idx, 
                          model_struct$param_info$sigma_nu_idx, 
                          model_struct$param_info$sigma_zeta_idx)
    
    # Remove any NULLs
    idx_to_transform <- unlist(idx_to_transform)
    
    for (i in idx_to_transform) {
      if (!is.na(i)) {
        # The value on the original scale: exp(log_sigma)
        params[i] <- exp(params_raw[i])
        
        # The Delta Method: SE_original = |d/dx exp(x)| * SE_log
        # SE_original = exp(params_raw[i]) * se_raw[i]
        se[i] <- params[i] * se_raw[i]
      }
    }
  }
  
  
  # Update matrices with estimated parameters
  model_struct <- jvn_update_matrices(model_struct, params)
  
  # Create parameter table
  param_table <- jvn_param_table(params, se, model_struct$param_info)
  
  # Calculate information criteria
  n_params <- length(params)
  n_obs <- nrow(Ymat) * ncol(Ymat)
  aic <- -2 * loglik + 2 * n_params
  bic <- -2 * loglik + log(n_obs) * n_params
  
  # Calculate forecasts if h > 0
  if (h > 0) {
    frequency <- unique((round(as.numeric(diff(df$time)) / 30)))
    if (length(frequency) > 1) {
      rlang::abort(
        "The time series seems not to be regular, 
        please provide a regular time series!"
      )
    }
    
    forecast_dates <- seq.Date(
      df$time[nrow(df)],
      by = paste0(frequency, " months"),
      length.out = (h + 1)
    )[2:(h + 1)]
    
    output_dates <- c(as.Date(rownames(Ymat)), forecast_dates)
    
    # Create extended data by appending NAs
    Ymat <- rbind(Ymat, matrix(NA, h, dim(Ymat)[2]))
    
  } else {
    output_dates <- c(as.Date(rownames(Ymat)))
  }
  
  # Build KFAS model
  kfas_model <- KFAS::SSModel(
    Ymat ~ -1 +
      SSMcustom(
        Z = model_struct$Z,
        T = model_struct$Tmat,
        R = model_struct$R,
        Q = model_struct$Q,
        a1 = c(0.2, rep(0, model_struct$m-1)),
        P1inf = diag(c(1, rep(0, model_struct$m-1)), model_struct$m),
        P1 = diag(c(0, rep(1, model_struct$m-1)), model_struct$m)
      ),
    H = model_struct$H
  )
  
  # Run Kalman filter and smoother
  kalman <- KFAS::KFS(kfas_model)
  
  # Number of state variables
  n_states <- length(z_names)
  n_total <- length(output_dates)
  
  # Initialize list to store results
  state_results <- list()
  
  # Loop through each state variable
  for (i in 1:n_states) {
    
    # Extract filtered estimates
    filtered_est <- kalman$att[, i]
    filtered_se <- sqrt(kalman$Ptt[i, i, ])
    
    # Extract smoothed estimates
    smoothed_est <- kalman$alphahat[, i]
    smoothed_se <- sqrt(kalman$V[i, i, ])
    
    # Create filtered data frame
    filtered_df <- dplyr::tibble(
      time = output_dates,
      state = paste0("state_",i),#z_names[i],
      estimate = filtered_est,
      lower = filtered_est - qnorm(1-alpha/2) * filtered_se,
      upper = filtered_est + qnorm(1-alpha/2) * filtered_se,
      filter = "filtered",
      sample = dplyr::if_else(output_dates %in% forecast_dates, 
                              "out_of_sample",
                              "in_sample")
    )
    
    # Create smoothed data frame
    smoothed_df <- dplyr::tibble(
      time = output_dates,
      state = paste0("state_",i), #z_names[i],
      estimate = smoothed_est,
      lower = smoothed_est - qnorm(1-alpha/2) * smoothed_se,
      upper = smoothed_est + qnorm(1-alpha/2) * smoothed_se,
      filter = "smoothed",
      sample = dplyr::if_else(output_dates %in% forecast_dates, 
                              "out_of_sample",
                              "in_sample")
    )
    
    # Combine filtered and smoothed
    state_results[[i]] <- dplyr::bind_rows(filtered_df, smoothed_df)
  }
  
  # Combine all states into one data frame
  states_long <- dplyr::bind_rows(state_results)
  
  # Optional: Convert to tibble if using tidyverse
  states_long <- dplyr::as_tibble(states_long) %>%
    arrange(filter, state, time)
  
  # Prepare results
  results <- list(
    states = states_long,
    jvn_model_mat = list(
      Z = model_struct$Z,
      Tmat = model_struct$Tmat,
      R = model_struct$R,
      H = model_struct$H,
      Q = model_struct$Q
    ),
    params = param_table,
    fit = opt_result,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = opt_result$convergence,
    data = df
  )
  
  class(results) <- c("jvn_model", class(results))
  return(results)
}


#' Build JVN Model Matrices in State-Space Form
#' 
#' Constructs the state-space matrices Z, T, R, H, Q according to the 
#' Jacobs & Van Norden (2011) specification.
#' 
#' @keywords internal
#' @noRd
jvn_matrices <- function(n_obs, n_vint, ar_order, include_news, include_noise,
                         include_spillovers, spillover_news, spillover_noise) {
  
  # State dimensions following paper notation
  # State vector: [ỹ_t, ỹ_{t-1}, ..., ỹ_{t-p+1}, ν_t, ζ_t]
  state_dim <- ar_order  # For AR(p) we need p lags
  news_dim <- if (include_news) n_vint else 0
  noise_dim <- if (include_noise) n_vint else 0
  m <- state_dim + news_dim + noise_dim
  
  # Number of shocks
  n_eta <- 1 + news_dim + noise_dim
  
  # ===== Measurement Matrix Z =====
  # KFAS requires: (p x m) matrix or (p x m x 1) array for time-invariant case
  Z <- matrix(0, n_vint, m)
  Z[, 1] <- 1  # All vintages depend on true value ỹ_t
  
  if (include_news) {
    news_start <- ar_order + 1
    news_end <- ar_order + n_vint
    Z[, news_start:news_end] <- diag(n_vint)  # Add news component
  }
  
  if (include_noise) {
    noise_start <- ar_order + news_dim + 1
    noise_end <- ar_order + news_dim + n_vint
    Z[, noise_start:noise_end] <- diag(n_vint)  # Add noise component
  }
  
  # ===== Transition Matrix T =====
  # KFAS requires: (m x m) matrix or (m x m x 1) array for time-invariant case
  Tmat <- matrix(0, m, m)
  
  # AR dynamics for true value (first row)
  # ỹ_t = ρ_1*ỹ_{t-1} + ... + ρ_p*ỹ_{t-p} + shocks
  # Will be filled with AR coefficients (ρ_1, ..., ρ_p)
  
  # Companion form for AR lags (if ar_order > 1)
  if (ar_order > 1) {
    for (i in 2:ar_order) {
      Tmat[i, i - 1] <- 1  # ỹ_{t-i+1} = ỹ_{t-i}
    }
  }
  
  # ===== R Matrix (Error Loading Matrix) =====
  # KFAS requires: (m x r) matrix or (m x r x 1) array for time-invariant case
  # η_t = [η_{et}, η_{ν1t}, ..., η_{νlt}, η_{ζ1t}, ..., η_{ζlt}]'
  
  R <- matrix(0, m, n_eta)
  
  # First state (true value ỹ_t) gets:
  # - AR shock (σ_e)
  # - ALL news shocks (σ_{ν1}, ..., σ_{νl}) 
  R[1, 1] <- 1  # Placeholder for σ_e, will be filled during update
  
  if (include_news) {
    # Paper equation: ỹ_{t+1} = ... + R_3 * η_{νt}
    # where R_3 = [σ_{ν1}, σ_{ν2}, ..., σ_{νl}]
    R[1, 2:(n_vint + 1)] <- 1  # Placeholders for σ_ν, filled during update
    
    # News states get: -U_1 · diag(R_3) · η_{νt}
    # U_1 is upper triangular matrix with 1s on and above diagonal
    # This creates the cumulative news structure
    news_start <- ar_order + 1
    for (i in 1:n_vint) {
      for (j in i:n_vint) {
        # ν_{jt} gets shock from η_{νit} for all i <= j
        R[news_start + j - 1, 1 + i] <- -1  # Placeholder, will be -σ_{νi}
      }
    }
  }
  
  if (include_noise) {
    # Noise states get independent shocks
    # Paper: R_4 = diag(σ_{ζ1}, ..., σ_{ζl})
    noise_start <- ar_order + news_dim + 1
    shock_start <- 1 + news_dim + 1
    for (i in 1:n_vint) {
      R[noise_start + i - 1, shock_start + i - 1] <- 1  # Placeholder for σ_ζ
    }
  }
  
  # ===== H and Q matrices =====
  H <- matrix(0, n_vint, n_vint)  # No measurement error
  Q <- diag(n_eta)  # Identity matrix (shocks are standard normal)
  
  # ===== Parameter Information =====
  param_info <- list(
    ar_order = ar_order,
    n_vint = n_vint,
    include_news = include_news,
    include_noise = include_noise,
    include_spillovers = include_spillovers,
    spillover_news = spillover_news,
    spillover_noise = spillover_noise,
    ar_coef_idx = 1:ar_order,
    sigma_e_idx = ar_order + 1,
    sigma_nu_idx = if (include_news) {
      (ar_order + 2):(ar_order + 1 + n_vint)
    } else NULL,
    sigma_zeta_idx = if (include_noise) {
      start <- ar_order + 2 + news_dim
      start:(start + n_vint - 1)
    } else NULL,
    spill_nu_idx = if (include_news && include_spillovers && spillover_news) {
      start <- ar_order + 2 + news_dim + noise_dim
      start:(start + n_vint - 1)
    } else NULL,
    spill_zeta_idx = if (include_noise && include_spillovers && spillover_noise) {
      start <- ar_order + 2 + news_dim + noise_dim +
        (if (include_news && include_spillovers && spillover_news) n_vint else 0)
      start:(start + n_vint - 1)
    } else NULL
  )
  
  # Count total parameters
  n_params <- ar_order + 1 + news_dim + noise_dim
  if (include_spillovers) {
    if (include_news && spillover_news) n_params <- n_params + n_vint
    if (include_noise && spillover_noise) n_params <- n_params + n_vint
  }
  
  list(
    Z = Z,
    Tmat = Tmat,
    R = R,
    H = H,
    Q = Q,
    param_info = param_info,
    n_params = n_params,
    m = m
  )
}


#' Update Model Matrices with Estimated Parameters
#' @keywords internal
#' @noRd
jvn_update_matrices <- function(model_struct, params) {
  info <- model_struct$param_info
  Tmat <- model_struct$Tmat
  R <- model_struct$R
  
  # Update AR coefficients in first row of T
  Tmat[1, 1:info$ar_order] <- params[info$ar_coef_idx]
  
  # Update AR shock standard deviation
  R[1, 1] <- params[info$sigma_e_idx]
  
  # Update news shocks
  if (info$include_news) {
    sigma_nu <- params[info$sigma_nu_idx]
    
    # Update first row: true value gets all news shocks
    R[1, 2:(info$n_vint + 1)] <- sigma_nu
    
    # Update news state rows: -U_1 · diag(R_3) structure
    news_start <- info$ar_order + 1
    for (i in 1:info$n_vint) {
      for (j in i:info$n_vint) {
        R[news_start + j - 1, 1 + i] <- -sigma_nu[i]
      }
    }
  }
  
  # Update noise shocks
  if (info$include_noise) {
    sigma_zeta <- params[info$sigma_zeta_idx]
    noise_start <- info$ar_order + (if (info$include_news) info$n_vint else 0) + 1
    shock_start <- 1 + (if (info$include_news) info$n_vint else 0) + 1
    for (i in 1:info$n_vint) {
      R[noise_start + i - 1, shock_start + i - 1] <- sigma_zeta[i]
    }
  }
  
  # Update spillover effects for news
  if (info$include_news && info$include_spillovers && info$spillover_news) {
    spill_nu <- params[info$spill_nu_idx]
    news_start <- info$ar_order + 1
    news_end <- info$ar_order + info$n_vint
    diag(Tmat[news_start:news_end, news_start:news_end]) <- spill_nu
  }
  
  # Update spillover effects for noise
  if (info$include_noise && info$include_spillovers && info$spillover_noise) {
    spill_zeta <- params[info$spill_zeta_idx]
    noise_start <- info$ar_order + (if (info$include_news) info$n_vint else 0) + 1
    noise_end <- noise_start + info$n_vint - 1
    diag(Tmat[noise_start:noise_end, noise_start:noise_end]) <- spill_zeta
  }
  
  model_struct$Tmat <- Tmat
  model_struct$R <- R
  model_struct
}


#' Negative Log-Likelihood Function
#' @keywords internal
#' @noRd
jvn_negloglik <- function(params, model_struct, data, transform_se = TRUE) {
  
  param_info <- model_struct$param_info
  
  if (transform_se) {
    if (!is.null(param_info$sigma_e_idx)) {
      params[param_info$sigma_e_idx] <- exp(params[param_info$sigma_e_idx])
    }
    
    if (!is.null(param_info$sigma_nu_idx)) {
      params[param_info$sigma_nu_idx] <- exp(params[param_info$sigma_nu_idx])
    }
    
    if (!is.null(param_info$sigma_zeta_idx)) {
      params[param_info$sigma_zeta_idx] <- exp(params[param_info$sigma_zeta_idx])
    }
  }
  
  # Update matrices with current parameter values
  model_struct <- jvn_update_matrices(model_struct, params)
  
  tryCatch(
    {
      # Build KFAS model
      kfas_model <- KFAS::SSModel(
        data ~ -1 +
          SSMcustom(
            Z = model_struct$Z,
            T = model_struct$Tmat,
            R = model_struct$R,
            Q = model_struct$Q,
            a1 = c(0.2, rep(0, model_struct$m-1)),
            P1inf = diag(c(1, rep(0, model_struct$m-1)), model_struct$m),
            P1 = diag(c(0, rep(1, model_struct$m-1)), model_struct$m)
          ),
        H = model_struct$H
      )
      
      # Calculate log-likelihood
      ll <- stats::logLik(kfas_model, check.model = FALSE)
      
      if (is.finite(ll)) {
        return(-as.numeric(ll))
      } else {
        return(1e10)
      }
    },
    error = function(e) {
      cat("Error in likelihood calculation:", e$message, "\n")
      return(1e10)
    }
  )
}

#' Initialize parameters
#' @keywords internal
#' @noRd
jvn_init_params <- function(model_struct, transform_se = TRUE) {
  info <- model_struct$param_info
  params <- numeric(model_struct$n_params)
  
  # AR coefficients: decreasing values that sum to < 1
  params[info$ar_coef_idx] <- seq(0.5, 0.1, length.out = info$ar_order)
  
  # AR shock standard deviation
  params[info$sigma_e_idx] <- 1.5
  
  # News shock standard deviations (decreasing)
  if (!is.null(info$sigma_nu_idx)) {
    params[info$sigma_nu_idx] <- seq(1.5, 0.1, length.out = info$n_vint)
  }
  
  # Noise shock standard deviations (decreasing)
  if (!is.null(info$sigma_zeta_idx)) {
    params[info$sigma_zeta_idx] <- seq(1.5, 0.1, length.out = info$n_vint)
  }
  
  # Spillover parameters (small positive values)
  if (!is.null(info$spill_nu_idx)) {
    params[info$spill_nu_idx] <- rep(0.5, info$n_vint)
  }
  
  if (!is.null(info$spill_zeta_idx)) {
    params[info$spill_zeta_idx] <- rep(0.5, info$n_vint)
  }
  
  # Take log of standard deviations
  if (transform_se) {
    if (!is.null(info$sigma_e_idx)) {
      params[info$sigma_e_idx] <- log(params[info$sigma_e_idx])
    }
    
    if (!is.null(info$sigma_nu_idx)) {
      params[info$sigma_nu_idx] <- log(params[info$sigma_nu_idx])
    }
    
    if (!is.null(info$sigma_zeta_idx)) {
      params[info$sigma_zeta_idx] <- log(params[info$sigma_zeta_idx])
    }
  }
  
  params
}

#' Create Parameter Table
#' @keywords internal
#' @noRd
jvn_param_table <- function(params, se, param_info) {
  param_names <- character(length(params))
  idx <- 1
  
  # AR coefficients
  for (i in 1:param_info$ar_order) {
    param_names[idx] <- paste0("rho_", i)
    idx <- idx + 1
  }
  
  # AR shock
  param_names[idx] <- "sigma_e"
  idx <- idx + 1
  
  # News shocks
  if (param_info$include_news) {
    for (i in 1:param_info$n_vint) {
      param_names[idx] <- paste0("sigma_nu_", i)
      idx <- idx + 1
    }
  }
  
  # Noise shocks
  if (param_info$include_noise) {
    for (i in 1:param_info$n_vint) {
      param_names[idx] <- paste0("sigma_zeta_", i)
      idx <- idx + 1
    }
  }
  
  # News spillovers
  if (param_info$include_spillovers && param_info$include_news &&
      param_info$spillover_news) {
    for (i in 1:param_info$n_vint) {
      param_names[idx] <- paste0("T_nu_", i)
      idx <- idx + 1
    }
  }
  
  # Noise spillovers
  if (param_info$include_spillovers && param_info$include_noise &&
      param_info$spillover_noise) {
    for (i in 1:param_info$n_vint) {
      param_names[idx] <- paste0("T_zeta_", i)
      idx <- idx + 1
    }
  }
  
  data.frame(
    Parameter = param_names,
    Estimate = params,
    Std.Error = se,
    row.names = NULL
  )
}


#' Summary Method for JVN Model
#' @export
summary.jvn_model <- function(object, ...) {
  cat("\n=== Jacobs-Van Norden Model ===\n\n")
  cat("Convergence:", ifelse(object$convergence == 0, "Success", "Failed"), "\n")
  cat("Log-likelihood:", round(object$loglik, 2), "\n")
  cat("AIC:", round(object$aic, 2), "\n")
  cat("BIC:", round(object$bic, 2), "\n\n")
  cat("Parameter Estimates:\n")
  #print(object$params, digits = 2, row.names = FALSE)
  df_print <- object$params
  df_print$Estimate <- sprintf("%.3f", df_print$Estimate)
  df_print$Std.Error <- sprintf("%.3f", df_print$Std.Error)
  print(df_print, row.names = FALSE, quote = FALSE)
  cat("\n")
  invisible(object)
}


#' Print Method for JVN Model
#' @export
print.jvn_model <- function(x, ...) {
  summary.jvn_model(x, ...)
}