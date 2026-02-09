#' Jacobs-Van Norden Model for Data Revisions
#'
#' Implements the Jacobs & Van Norden (2011) state-space model for analyzing
#' data revisions, allowing for news and noise components in measurement errors.
#'
#' @param df A matrix or data frame where each column represents a different
#'   vintage estimate for the same time period. Rows are time periods.
#' @param e An integer indicating the number of data vintages to include in the
#'  model. Must be greater than 0.
#' @param ar_order Integer specifying AR order for true values (default = 2).
#' @param h Integer specifying the forecast horizon (default = 0).
#' @param include_news Logical, whether to include news component in
#'   measurement error (default = TRUE).
#' @param include_noise Logical, whether to include noise component in
#'   measurement error (default = TRUE).
#' @param include_spillovers Logical, whether to include spillover effects
#'   (default = FALSE).
#' @param spillover_news Logical, whether spillovers apply to news component
#'   (default = TRUE).
#' @param spillover_noise Logical, whether spillovers apply to noise component
#'   (default = TRUE).
#' @param method A string specifying the estimation method to use. Options are
#'  Maximum likelihood ("MLE") for now.
#' @param alpha Significance level for confidence intervals (default = 0.05).
#' @param solver_options List of options for the optimizer:
#'   - trace: Integer controlling output level (default = 0)
#'   - maxiter: Maximum iterations (default = 1000)
#'   - startvals: Named vector of starting values (optional)
#'   - transform_se: T/F whether standard errors should be constrained to be
#'     positive in optimization.
#' .  - method: String specifying optimization method (default = "L-BFGS-B").
#'   - se_method: Method for standard error calculation (default = "hessian")
#'   - n_starts: Number of random starting points for multi-start optimization
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
#' gdp_growth <- dplyr::filter(
#'   tsbox::ts_pc(
#'     reviser::gdp
#'   ), id %in% c("EA"),
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp_growth <- tidyr::drop_na(gdp_growth)
#' df <- get_nth_release(gdp_growth, n = 0:4)
#'
#' # Estimate model
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 3,
#'   ar_order = 2,
#'   h = 4,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#'
#' @family revision nowcasting
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
    method = "L-BFGS-B",
    maxiter = 1000,
    transform_se = TRUE,
    startvals = NULL,
    se_method = "hessian",
    n_starts = 1
  )

  # Check ar order is integer and > 0:
  if (ar_order <= 0) {
    rlang::abort("'ar_order' must be > 0!")
  } else {
    ar_order <- round(ar_order)
  }

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

  # Reject lists with multiple IDs
  if (is.list(check)) {
    if (length(check) > 1) {
      rlang::abort(paste0(
        "'df' must contain a single ID, but ", length(check),
        " IDs were provided: ", paste(names(check), collapse = ", ")
      ))
    }
    # Extract single data frame from list
    df <- df[[1]]
    check <- check[[1]]
  }

  # Convert long to wide if needed
  if (check == "long") {
    if ("id" %in% colnames(df) && length(unique(df$id)) > 1) {
      rlang::abort(paste0(
        "'df' contains ", length(unique(df$id)), " different IDs. ",
        "Filter to a single ID first."
      ))
    }
    df <- suppressWarnings(vintages_wide(df, names_from = "release"))
    # Handle if vintages_wide returns a list
    if (is.list(df) && !is.data.frame(df)) df <- df[[1]]
  }

  # Arrange data
  df_intern <- df[, 1:(e + 1)]

  y_mat <- as.matrix(dplyr::select(df_intern, -"time"))
  rownames(y_mat) <- as.character(df$time)

  # Build model structure
  model_struct <- jvn_matrices(
    n_obs = nrow(df_intern),
    n_vint = ncol(df_intern) - 1,
    ar_order = ar_order,
    include_news = include_news,
    include_noise = include_noise,
    include_spillovers = include_spillovers,
    spillover_news = spillover_news,
    spillover_noise = spillover_noise
  )

  state_names <- model_struct$state_names

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
      y_mat,
      transform_se = default_solver_options$transform_se
    )
  }

  if (default_solver_options$trace > 0) {
    cat("Estimating JVN model with", model_struct$n_params, "parameters...\n")
    cat("AR order:", ar_order, "\n")
    cat(
      "News:", include_news, " Noise:", include_noise,
      " Spillovers:", include_spillovers, "\n\n"
    )
  }

  # ===== OPTIMIZATION =====

  # Check if multi-start is requested
  n_starts <- if (!is.null(default_solver_options$n_starts)) {
    max(1, default_solver_options$n_starts)
  } else {
    1
  }

  if (n_starts > 1 && default_solver_options$trace > 0) {
    cat("\nUsing multi-start optimization with", n_starts, "starting points\n")
    cat("Method:", default_solver_options$method, "\n\n")
  }

  # Storage for multi-start results
  all_results <- vector("list", n_starts)
  all_values <- numeric(n_starts)

  # Run optimization from multiple starting points
  for (start_idx in 1:n_starts) {
    if (n_starts > 1 && default_solver_options$trace > 0) {
      cat("=== Starting point", start_idx, "of", n_starts, "===\n")
    }

    # Generate starting values
    if (start_idx == 1) {
      # First start: use default initialization
      current_init <- init_params
    } else {
      # Subsequent starts: perturb around default
      current_init <- init_params + stats::rnorm(length(init_params), 0, 0.5)

      # Ensure perturbations respect constraints
      if (default_solver_options$transform_se) {
        # For log-transformed SDs, keep them reasonable
        sd_indices <- c(
          model_struct$param_info$sigma_e_idx,
          model_struct$param_info$sigma_nu_idx,
          model_struct$param_info$sigma_zeta_idx
        )
        sd_indices <- unlist(sd_indices)
        # Keep log(sigma) between log(0.01) and log(10)
        current_init[sd_indices] <- pmax(
          pmin(current_init[sd_indices], log(100)), log(0.001)
        )
      }

      # For AR coefficients, ensure stationarity
      ar_indices <- model_struct$param_info$ar_coef_idx
      current_init[ar_indices] <- pmax(
        pmin(current_init[ar_indices], 0.9), -0.9
      )
      # For spillover parameters, keep between 0 and 1
      if (!is.null(model_struct$param_info$spill_nu_idx)) {
        current_init[model_struct$param_info$spill_nu_idx] <-
          pmax(
            pmin(current_init[model_struct$param_info$spill_nu_idx], 0.9), -0.9
          )
      }
      if (!is.null(model_struct$param_info$spill_zeta_idx)) {
        current_init[model_struct$param_info$spill_zeta_idx] <-
          pmax(pmin(current_init[
            model_struct$param_info$spill_zeta_idx
          ], 0.9), -0.9)
      }
    }

    # Determine optimization method
    opt_method <- default_solver_options$method

    # ===== TWO-STEP METHOD =====
    if (opt_method == "two-step") {
      if (default_solver_options$trace > 0) {
        cat("Step 1: Nelder-Mead (global search)...\n")
      }

      opt_result_nm <- stats::optim(
        par = current_init,
        fn = jvn_negloglik,
        model_struct = model_struct,
        data = y_mat,
        transform_se = default_solver_options$transform_se,
        method = "Nelder-Mead",
        control = list(
          trace = max(0, default_solver_options$trace),
          maxit = 500
        )
      )

      if (default_solver_options$trace > 0) {
        cat("Step 2: BFGS (local refinement)...\n")
      }
      current_result <- stats::optim(
        par = opt_result_nm$par,
        fn = jvn_negloglik,
        model_struct = model_struct,
        data = y_mat,
        transform_se = default_solver_options$transform_se,
        method = "BFGS",
        control = list(
          trace = max(0, default_solver_options$trace),
          maxit = default_solver_options$maxiter
        ),
        hessian = FALSE
      )

      # ===== L-BFGS-B METHOD =====
    } else if (opt_method == "L-BFGS-B") {
      # Set bounds for parameters
      n_params <- model_struct$n_params
      lower_bounds <- rep(-Inf, n_params)
      upper_bounds <- rep(Inf, n_params)

      # For log-transformed standard deviations
      if (default_solver_options$transform_se) {
        param_info <- model_struct$param_info

        # log(sigma) bounds: log(0.001) to log(100)
        sd_indices <- c(
          param_info$sigma_e_idx,
          param_info$sigma_nu_idx,
          param_info$sigma_zeta_idx
        )
        sd_indices <- unlist(sd_indices)

        lower_bounds[sd_indices] <- log(0.001)
        upper_bounds[sd_indices] <- log(100)
      }

      # AR coefficients: ensure stationarity
      ar_indices <- model_struct$param_info$ar_coef_idx
      lower_bounds[ar_indices] <- -0.9
      upper_bounds[ar_indices] <- 0.9

      # Spillover parameters: between 0 and 0.99
      if (!is.null(model_struct$param_info$spill_nu_idx)) {
        lower_bounds[model_struct$param_info$spill_nu_idx] <- -0.9
        upper_bounds[model_struct$param_info$spill_nu_idx] <- 0.9
      }
      if (!is.null(model_struct$param_info$spill_zeta_idx)) {
        lower_bounds[model_struct$param_info$spill_zeta_idx] <- -0.9
        upper_bounds[model_struct$param_info$spill_zeta_idx] <- 0.9
      }

      current_result <- stats::optim(
        par = current_init,
        fn = jvn_negloglik,
        model_struct = model_struct,
        data = y_mat,
        transform_se = default_solver_options$transform_se,
        method = "L-BFGS-B",
        lower = lower_bounds,
        upper = upper_bounds,
        control = list(
          trace = max(0, default_solver_options$trace),
          maxit = default_solver_options$maxiter
        ),
        hessian = FALSE
      )

      # ===== NLMINB METHOD =====
    } else if (opt_method == "nlminb") {
      # Set bounds
      n_params <- model_struct$n_params
      lower_bounds <- rep(-Inf, n_params)
      upper_bounds <- rep(Inf, n_params)

      if (default_solver_options$transform_se) {
        param_info <- model_struct$param_info
        sd_indices <- c(
          param_info$sigma_e_idx,
          param_info$sigma_nu_idx,
          param_info$sigma_zeta_idx
        )
        sd_indices <- unlist(sd_indices)
        lower_bounds[sd_indices] <- log(0.001)
        upper_bounds[sd_indices] <- log(100)
      }

      # AR stationarity
      ar_indices <- model_struct$param_info$ar_coef_idx
      lower_bounds[ar_indices] <- -0.9
      upper_bounds[ar_indices] <- 0.9

      # Spillovers
      if (!is.null(model_struct$param_info$spill_nu_idx)) {
        lower_bounds[model_struct$param_info$spill_nu_idx] <- -0.9
        upper_bounds[model_struct$param_info$spill_nu_idx] <- 0.9
      }
      if (!is.null(model_struct$param_info$spill_zeta_idx)) {
        lower_bounds[model_struct$param_info$spill_zeta_idx] <- -0.9
        upper_bounds[model_struct$param_info$spill_zeta_idx] <- 0.9
      }

      opt_result_nlminb <- stats::nlminb(
        start = current_init,
        objective = jvn_negloglik,
        model_struct = model_struct,
        data = y_mat,
        transform_se = default_solver_options$transform_se,
        lower = lower_bounds,
        upper = upper_bounds,
        control = list(
          trace = max(0, default_solver_options$trace),
          eval.max = default_solver_options$maxiter * 2,
          iter.max = default_solver_options$maxiter
        )
      )

      # Convert nlminb output to optim format
      current_result <- list(
        par = opt_result_nlminb$par,
        value = opt_result_nlminb$objective,
        convergence = opt_result_nlminb$convergence,
        message = opt_result_nlminb$message
      )

      # ===== STANDARD METHODS (BFGS, Nelder-Mead, etc.) =====
    } else {
      current_result <- stats::optim(
        par = current_init,
        fn = jvn_negloglik,
        model_struct = model_struct,
        data = y_mat,
        transform_se = default_solver_options$transform_se,
        method = opt_method,
        control = list(
          trace = max(0, default_solver_options$trace),
          maxit = default_solver_options$maxiter
        ),
        hessian = FALSE
      )
    }

    # Store results
    all_results[[start_idx]] <- current_result
    all_values[start_idx] <- current_result$value

    if (n_starts > 1 && default_solver_options$trace > 0) {
      cat("Negative log-likelihood:", round(current_result$value, 4), "\n")
      cat("Convergence:", current_result$convergence, "\n\n")
    }
  }

  # Select best result from all starting points
  best_idx <- which.min(all_values)
  opt_result <- all_results[[best_idx]]

  if (n_starts > 1 && default_solver_options$trace > 0) {
    cat("=== Multi-start Summary ===\n")
    cat("Best result from starting point", best_idx, "\n")
    cat("Negative log-likelihoods across starts:\n")
    for (i in 1:n_starts) {
      marker <- if (i == best_idx) " <- BEST" else ""
      cat(sprintf("  Start %d: %.4f%s\n", i, all_values[i], marker))
    }
    cat("\n")
  }

  # Extract results (raw scale from optimizer)
  params_raw <- opt_result$par
  loglik <- -opt_result$value

  # ===== STANDARD ERROR CALCULATION =====
  se_method <- default_solver_options$se_method

  if (se_method == "hessian") {
    # ===== HESSIAN METHOD =====
    
    # We wrap the whole logic in a list-returning tryCatch
    se_result <- suppressWarnings(tryCatch(
      {
        # Calculate high-precision Hessian
        precise_hessian <- numDeriv::hessian(
          func = jvn_negloglik,
          x = params_raw,
          model_struct = model_struct,
          data = y_mat,
          transform_se = default_solver_options$transform_se,
          method.args = list(eps = 1e-4, d = 0.01, r = 6)
        )
        
        # Check condition number
        cond_num <- tryCatch(
          kappa(precise_hessian, exact = FALSE), error = function(e) Inf
          )
        
        if (!is.finite(cond_num) || cond_num > 1e10) {
          msg <- paste0(
            "Hessian is poorly conditioned", 
            if (is.finite(cond_num)) {
              paste0(" (cond = ", 
                     format(cond_num, scientific = TRUE, digits = 2), 
                     ")") } else {
                       ""},
            ". SEs may be unreliable.")
          
          # Return a list instead of updating external variables
          list(se = rep(NA, length(params_raw)), warning = msg, failed = TRUE)
          
        } else {
          # Try to invert
          fisher_info <- tryCatch({
            solve(precise_hessian)
          }, error = function(e) {
            ridge <- 1e-6 * mean(abs(diag(precise_hessian)))
            if (default_solver_options$trace > 0) {
              cat("Adding ridge regularization (\u03BB =", 
                  format(ridge, scientific = TRUE), ")\n")
            }
            tryCatch(solve(
              precise_hessian + ridge * diag(nrow(precise_hessian))
              ), error = function(e2) NULL)
          })
          
          if (is.null(fisher_info)) {
            list(
              se = rep(NA, length(params_raw)), 
              warning = "Failed to invert Hessian matrix.", 
              failed = TRUE
            )
          } else {
            se_calc <- sqrt(diag(fisher_info))
            n_nan <- sum(is.nan(se_calc))
            n_large <- sum(se_calc > 1e3, na.rm = TRUE)
            
            problem_msg <- NULL
            has_failed <- FALSE
            
            if ((n_nan + n_large) > 0) {
              problem_msg <- paste0((n_nan + n_large), 
                                    " parameter(s) have problematic SEs")
              has_failed <- TRUE
            }
            
            se_calc[is.nan(se_calc)] <- NA
            list(se = se_calc, warning = problem_msg, failed = has_failed)
          }
        }
      },
      error = function(e) {
        # No more <<- operator! We return the list directly from the handler.
        list(
          se = rep(NA, length(params_raw)), 
          warning = paste0("Hessian calculation failed: ", e$message), 
          failed = TRUE
        )
      }
    ))
    
    # Extract results back into your local variables
    se_raw         <- se_result$se
    se_warning     <- se_result$warning
    hessian_failed <- se_result$failed
    se_method_used <- "hessian"
    
    # Issue warning if Hessian was problematic
    if (hessian_failed && !is.null(se_warning)) {
      warning(se_warning, call. = FALSE)
    }
  }

  if (default_solver_options$trace > 0) {
    cat("Standard error method used:", se_method_used, "\n")
    if (!is.null(se_warning) && default_solver_options$trace > 1) {
      cat("Warning:", se_warning, "\n")
    }
  }

  # Initialize final objects
  params <- params_raw
  se <- se_raw

  # Apply transformation and Delta Method
  if (default_solver_options$transform_se) {
    # List of indices to transform
    idx_to_transform <- c(
      model_struct$param_info$sigma_e_idx,
      model_struct$param_info$sigma_nu_idx,
      model_struct$param_info$sigma_zeta_idx
    )

    # Remove any NULLs
    idx_to_transform <- unlist(idx_to_transform)

    for (i in idx_to_transform) {
      if (!is.na(i)) {
        # The value on the original scale: exp(log_sigma)
        params[i] <- exp(params_raw[i])

        # The Delta Method: SE_original = |d/dx exp(x)| * SE_log
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
  n_obs <- nrow(y_mat) * ncol(y_mat)
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

    output_dates <- c(as.Date(rownames(y_mat)), forecast_dates)

    # Create extended data by appending NAs
    y_mat <- rbind(y_mat, matrix(NA, h, dim(y_mat)[2]))
  } else {
    output_dates <- c(as.Date(rownames(y_mat)))
    forecast_dates <- as.Date(character(0))
  }

  # Build KFAS model
  kfas_model <- KFAS::SSModel(
    y_mat ~ -1 +
      SSMcustom(
        Z = model_struct$Z,
        T = model_struct$Tmat,
        R = model_struct$R,
        Q = model_struct$Q,
        a1 = c(0.2, rep(0, model_struct$m - 1)),
        P1inf = diag(c(1, rep(0, model_struct$m - 1)), model_struct$m),
        P1 = diag(c(0, rep(1, model_struct$m - 1)), model_struct$m)
      ),
    H = model_struct$H
  )

  # Run Kalman filter and smoother
  kalman <- KFAS::KFS(kfas_model)

  # Number of state variables
  n_states <- length(state_names)

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
      state = state_names[i],
      estimate = filtered_est,
      lower = filtered_est - stats::qnorm(1 - alpha / 2) * filtered_se,
      upper = filtered_est + stats::qnorm(1 - alpha / 2) * filtered_se,
      filter = "filtered",
      sample = dplyr::if_else(output_dates %in% forecast_dates,
        "out_of_sample",
        "in_sample"
      )
    )

    # Create smoothed data frame
    smoothed_df <- dplyr::tibble(
      time = output_dates,
      state = state_names[i],
      estimate = smoothed_est,
      lower = smoothed_est - stats::qnorm(1 - alpha / 2) * smoothed_se,
      upper = smoothed_est + stats::qnorm(1 - alpha / 2) * smoothed_se,
      filter = "smoothed",
      sample = dplyr::if_else(output_dates %in% forecast_dates,
        "out_of_sample",
        "in_sample"
      )
    )

    # Combine filtered and smoothed
    state_results[[i]] <- dplyr::bind_rows(filtered_df, smoothed_df)
  }

  # Combine all states into one data frame
  states_long <- dplyr::bind_rows(state_results)

  # Optional: Convert to tibble if using tidyverse
  states_long <- dplyr::as_tibble(states_long) %>%
    dplyr::arrange(.data$filter, .data$state, .data$time)

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
  # State vector:
  # $\tilde{y}_t, \tilde{y}_{t-1}, \ldots, \tilde{y}_{t-p+1}, \nu_t, \zeta_t$
  state_dim <- ar_order # For AR(p) we need p lags
  news_dim <- if (include_news) n_vint else 0
  noise_dim <- if (include_noise) n_vint else 0
  m <- state_dim + news_dim + noise_dim

  # Number of shocks
  n_eta <- 1 + news_dim + noise_dim

  # ===== Measurement Matrix Z =====
  # KFAS requires: (p x m) matrix or (p x m x 1) array for time-invariant case
  Z <- matrix(0, n_vint, m)
  Z[, 1] <- 1 # All vintages depend on true value $\tilde{y}_t$

  if (include_news) {
    news_start <- ar_order + 1
    news_end <- ar_order + n_vint
    Z[, news_start:news_end] <- diag(n_vint) # Add news component
  }

  if (include_noise) {
    noise_start <- ar_order + news_dim + 1
    noise_end <- ar_order + news_dim + n_vint
    Z[, noise_start:noise_end] <- diag(n_vint) # Add noise component
  }

  # ===== Transition Matrix T =====
  # KFAS requires: (m x m) matrix or (m x m x 1) array for time-invariant case
  Tmat <- matrix(0, m, m)

  # AR dynamics for true value (first row)
  # $\tilde{y}_t =
  # \rho_1 \tilde{y}_{t-1} + \ldots + \rho_p \tilde{y}_{t-p} + \text{shocks}$
  # Will be filled with AR coefficients $(\rho_1, \ldots, \rho_p)$

  # Companion form for AR lags (if ar_order > 1)
  if (ar_order > 1) {
    for (i in 2:ar_order) {
      Tmat[i, i - 1] <- 1 # $\tilde{y}_{t-i+1} = \tilde{y}_{t-i}$
    }
  }

  # ===== R Matrix (Error Loading Matrix) =====
  # KFAS requires: (m x r) matrix or (m x r x 1) array for time-invariant case
  # $\eta_t =
  # [\eta_{e t}, \eta_{\nu 1 t}, \ldots,
  # \eta_{\nu l t}, \eta_{\zeta 1 t}, \ldots, \eta_{\zeta l t}]'$

  R <- matrix(0, m, n_eta)

  # First state (true value $\tilde{y}_t$) gets:
  # - AR shock $(\sigma_e)$
  # - ALL news shocks $(\sigma_{\nu 1}, \ldots, \sigma_{\nu l})$
  R[1, 1] <- 1 # Placeholder for $\sigma_e$, will be filled during update

  if (include_news) {
    # Paper equation: $\tilde{y}_{t+1} = \ldots + R_3 \eta_{\nu t}$
    # where $R_3 = [\sigma_{\nu 1}, \sigma_{\nu 2}, \ldots, \sigma_{\nu l}]$

    # Placeholders for $\sigma_{\nu}$, filled during update
    R[1, 2:(n_vint + 1)] <- 1

    # News states get: $-U_1 · diag(R_3) \eta_{\nu t}$
    # $U_1$ is upper triangular matrix with 1s on and above diagonal
    # This creates the cumulative news structure
    news_start <- ar_order + 1
    for (i in 1:n_vint) {
      for (j in i:n_vint) {
        # $\nu_{j t}$ gets shock from $\eta_{\nu i t}$ for all $i \le j$
        # Placeholder, will be $-\sigma_{\nu i}$
        R[news_start + j - 1, 1 + i] <- -1
      }
    }
  }

  if (include_noise) {
    # Noise states get independent shocks
    # Paper: $R_4 = \mathrm{diag}(\sigma_{\zeta 1}, \ldots, \sigma_{\zeta l})$
    noise_start <- ar_order + news_dim + 1
    shock_start <- 1 + news_dim + 1
    for (i in 1:n_vint) {
      # Placeholder for $\sigma_{\zeta}$
      R[noise_start + i - 1, shock_start + i - 1] <- 1
    }
  }

  # ===== H and Q matrices =====
  H <- matrix(0, n_vint, n_vint) # No measurement error
  Q <- diag(n_eta) # Identity matrix (shocks are standard normal)

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
    } else {
      NULL
    },
    sigma_zeta_idx = if (include_noise) {
      start <- ar_order + 2 + news_dim
      start:(start + n_vint - 1)
    } else {
      NULL
    },
    spill_nu_idx = if (include_news && include_spillovers && spillover_news) {
      start <- ar_order + 2 + news_dim + noise_dim
      start:(start + n_vint - 1)
    } else {
      NULL
    },
    spill_zeta_idx =
      if (include_noise && include_spillovers && spillover_noise) {
        start <- ar_order + 2 + news_dim + noise_dim +
          (if (include_news && include_spillovers && spillover_news) {
            n_vint
          } else {
                  0})
        start:(start + n_vint - 1)
      } else {
        NULL
      }
  )

  # Count total parameters
  n_params <- ar_order + 1 + news_dim + noise_dim
  if (include_spillovers) {
    if (include_news && spillover_news) n_params <- n_params + n_vint
    if (include_noise && spillover_noise) n_params <- n_params + n_vint
  }

  # State names
  # 1. True Value Labels ($\tilde{y}_t, \tilde{y}_{t-1}, \ldots$)
  true_names <- paste0("true_lag_", 0:(ar_order - 1))

  # 2. News Labels ($\nu_1, \nu_2, \ldots$)
  news_names <- NULL
  if (include_news) {
    news_names <- paste0("news_vint", 1:n_vint)
  }

  # 3. Noise Labels ($\zeta_1, \zeta_2, \ldots$)
  noise_names <- NULL
  if (include_noise) {
    noise_names <- paste0("noise_vint", 1:n_vint)
  }

  # Combine in the order they appear in the state vector
  state_names <- c(true_names, news_names, noise_names)

  list(
    Z = Z,
    Tmat = Tmat,
    R = R,
    H = H,
    Q = Q,
    param_info = param_info,
    n_params = n_params,
    state_names = state_names,
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

    # Update news state rows: -U_1 x diag(R_3) structure
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
    noise_start <- info$ar_order +
      (if (info$include_news) info$n_vint else 0) + 1
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
    noise_start <- info$ar_order +
      (if (info$include_news) info$n_vint else 0) + 1
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
      params[param_info$sigma_zeta_idx] <- exp(
        params[param_info$sigma_zeta_idx]
      )
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
            a1 = c(0.2, rep(0, model_struct$m - 1)),
            P1inf = diag(c(1, rep(0, model_struct$m - 1)), model_struct$m),
            P1 = diag(c(0, rep(1, model_struct$m - 1)), model_struct$m)
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

#' Initialize Parameters with Data-Driven Starting Values
#' @keywords internal
#' @noRd
jvn_init_params <- function(model_struct, data, transform_se = TRUE) {
  info <- model_struct$param_info
  params <- numeric(model_struct$n_params)
  n_vint <- info$n_vint
  ar_order <- info$ar_order

  # Remove NA rows for estimation
  data_clean <- data[stats::complete.cases(data), , drop = FALSE]

  if (nrow(data_clean) < ar_order + 10) {
    warning("Insufficient data for smart initialization. Using default values.")
    return(jvn_init_params(model_struct, transform_se))
  }

  # ===== 1. AR COEFFICIENTS AND SHOCK from final vintage =====
  final_vintage <- data_clean[, n_vint]

  ar_fit <- tryCatch(
    {
      stats::ar(
        final_vintage,
        aic = FALSE,
        order.max =
          ar_order,
        method = "ols"
      )
    },
    error = function(e) {
      y <- final_vintage[(ar_order + 1):length(final_vintage)]
      X <- matrix(0, length(y), ar_order)
      for (i in 1:ar_order) {
        X[, i] <- final_vintage[(ar_order + 1 - i):(length(final_vintage) - i)]
      }
      lm_fit <- stats::lm(y ~ X - 1)
      list(ar = stats::coef(lm_fit), var.pred = summary(lm_fit)$sigma^2)
    }
  )

  params[info$ar_coef_idx] <- ar_fit$ar
  params[info$sigma_e_idx] <- sqrt(ar_fit$var.pred)

  # ===== 2. NEWS SHOCKS from revision variances =====
  if (info$include_news) {
    # Calculate vintage-to-vintage revisions
    revision_vars <- numeric(n_vint)

    for (v in 1:(n_vint - 1)) {
      revisions <- data_clean[, v + 1] - data_clean[, v]
      revision_vars[v] <- stats::var(revisions, na.rm = TRUE)
    }
    # Assume smaller for final
    revision_vars[n_vint] <- revision_vars[n_vint - 1] * 0.5

    # Take square root and ensure decreasing pattern
    sigma_nu_init <- sqrt(pmax(revision_vars, 0.01))

    # Force monotonic decrease
    for (v in 2:n_vint) {
      sigma_nu_init[v] <- min(sigma_nu_init[v], sigma_nu_init[v - 1] * 0.9)
    }

    params[info$sigma_nu_idx] <- pmax(sigma_nu_init, 0.05)
  }

  # ===== 3. NOISE SHOCKS from cross-vintage deviations =====
  if (info$include_noise) {
    # Measure noise as deviation from cross-vintage mean
    vintage_mean <- rowMeans(data_clean, na.rm = TRUE)

    sigma_zeta_init <- numeric(n_vint)
    for (v in 1:n_vint) {
      deviations <- data_clean[, v] - vintage_mean
      sigma_zeta_init[v] <- stats::sd(deviations, na.rm = TRUE)
    }

    # Force monotonic decrease (later vintages should be less noisy)
    for (v in 2:n_vint) {
      sigma_zeta_init[v] <- min(
        sigma_zeta_init[v], sigma_zeta_init[v - 1] * 0.85
      )
    }

    params[info$sigma_zeta_idx] <- pmax(sigma_zeta_init, 0.05)
  }

  # ===== 4. SPILLOVER PARAMETERS from AR(1) on revisions/residuals =====
  if (info$include_spillovers) {
    if (info$include_news && info$spillover_news) {
      spillover_nu <- numeric(n_vint)

      for (v in 1:(n_vint - 1)) {
        revisions <- data_clean[, v + 1] - data_clean[, v]
        ar1 <- tryCatch(
          {
            stats::ar(
              revisions, aic = FALSE, order.max = 1, method = "ols"
            )$ar[1]
          },
          error = function(e) 0.3
        )
        spillover_nu[v] <- ar1
      }
      spillover_nu[n_vint] <- mean(spillover_nu[1:(n_vint - 1)])

      params[info$spill_nu_idx] <- pmax(pmin(spillover_nu, 0.9), 0.1)
    }

    if (info$include_noise && info$spillover_noise) {
      spillover_zeta <- numeric(n_vint)

      for (v in 1:n_vint) {
        residuals <- diff(data_clean[, v])
        ar1 <- tryCatch(
          {
            stats::ar(
              residuals, aic = FALSE, order.max = 1, method = "ols"
            )$ar[1]
          },
          error = function(e) 0.2
        )
        spillover_zeta[v] <- ar1 * 0.7 # Dampen
      }

      params[info$spill_zeta_idx] <- pmax(pmin(spillover_zeta, 0.8), 0.05)
    }
  }

  # ===== 5. TRANSFORMATIONS =====
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

  # ===== 6. VALIDATION =====
  if (any(!is.finite(params))) {
    warning("Some starting values are non-finite. Falling back to defaults.")
    return(jvn_init_params(model_struct, transform_se))
  }

  # Ensure AR stationarity
  ar_sum <- sum(params[info$ar_coef_idx])
  if (abs(ar_sum) >= 1) {
    params[info$ar_coef_idx] <- params[info$ar_coef_idx] * 0.9 / abs(ar_sum)
  }

  return(params)
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
#'
#' @description Computes and displays a summary of the results from a
#' Jacobs-Van Norden (JVN) model fit, including convergence status,
#' information criteria, and parameter estimates.
#'
#' @param object An object of class \code{jvn_model}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The function returns the input \code{object} invisibly.
#' @method summary jvn_model
#' @examples
#' gdp <- dplyr::filter(
#'   tsbox::ts_pc(
#'     reviser::gdp
#'   ), id %in% c("EA"),
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp <- tidyr::drop_na(gdp)
#' df <- get_nth_release(gdp, n = 0:4)
#'
#' # Estimate model
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 3,
#'   ar_order = 2,
#'   h = 4,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' summary(result)
#'
#' @family revision nowcasting
#' @export
summary.jvn_model <- function(object, ...) {
  cat("\n=== Jacobs-Van Norden Model ===\n\n")
  cat("Convergence:", ifelse(
    object$convergence == 0, "Success", "Failed"
  ), "\n")
  cat("Log-likelihood:", round(object$loglik, 2), "\n")
  cat("AIC:", round(object$aic, 2), "\n")
  cat("BIC:", round(object$bic, 2), "\n\n")

  cat("Parameter Estimates:\n")
  df_print <- object$params
  df_print$Estimate <- sprintf("%.3f", df_print$Estimate)
  df_print$Std.Error <- sprintf("%.3f", df_print$Std.Error)
  print(df_print, row.names = FALSE, quote = FALSE)

  cat("\n")
  invisible(object)
}


#' Print Method for JVN Model
#'
#' @description Default print method for \code{jvn_model} objects.
#' Wraps the \code{summary} method for a consistent output.
#'
#' @param x An object of class \code{jvn_model}.
#' @param ... Additional arguments passed to \code{summary.jvn_model}.
#'
#' @return The function returns the input \code{x} invisibly.
#' @method print jvn_model
#' @examples
#' gdp <- dplyr::filter(
#'   tsbox::ts_pc(
#'     reviser::gdp
#'   ), id %in% c("EA"),
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp <- tidyr::drop_na(gdp)
#' df <- get_nth_release(gdp, n = 0:4)
#'
#' # Estimate model
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 3,
#'   ar_order = 2,
#'   h = 4,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' result
#'
#' @family revision nowcasting
#' @export
print.jvn_model <- function(x, ...) {
  summary.jvn_model(x, ...)
}

#' Plot JVN Model Results
#'
#' @param x An object of class 'jvn_model'
#' @param state String. The name of the state to visualize.
#' @param type String. Type of estimate to plot: "filtered" or "smoothed".
#' @param ... Additional arguments passed to theme_reviser.
#'
#' @return A ggplot2 object visualizing the specified state estimates.
#' @examples
#' gdp <- dplyr::filter(
#'   tsbox::ts_pc(
#'     reviser::gdp
#'   ), id %in% c("EA"),
#'   time >= min(pub_date),
#'   time <= as.Date("2020-01-01")
#' )
#' gdp <- tidyr::drop_na(gdp)
#' df <- get_nth_release(gdp, n = 0:4)
#'
#' # Estimate model
#' result <- jvn_nowcast(
#'   df = df,
#'   e = 3,
#'   ar_order = 2,
#'   h = 4,
#'   include_news = TRUE,
#'   include_noise = TRUE
#' )
#' plot(result)
#'
#' @family revision nowcasting
#' @export
plot.jvn_model <- function(x, state = "true_lag_0", type = "filtered", ...) {
  # Forward to the base method with JVN defaults
  plot.revision_model(x, state = state, type = type, ...)
}
