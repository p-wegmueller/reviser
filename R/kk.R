#' Generalized Kishor-Koenig Model for Nowcasting
#'
#' Implements a generalized Kishor-Koenig (KK) model for nowcasting and
#' forecasting with state-space models, allowing for multiple vintages of data,
#' efficient estimation, and Kalman filtering and smoothing.
#'
#' @param df A data frame containing the time series data in either "long" or
#' "wide" format.  It must include columns for the time index and the different
#' release vintages.
#' @param e An integer indicating the number of data vintages to include in the
#'  model. Must be greater than 0.
#' @param h An integer specifying the forecast horizon. Default is 0, which
#' implies no forecasts. Must be greater than or equal to 0.
#' @param model A string specifying the type of model to use. Options are:
#'
#'  - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.
#'  - "Howrey": Howrey's simplified framework.
#'  - "Classical": Classical model without vintage effects.
#' @param method A string specifying the estimation method to use. Options are
#' "SUR" (default), Maximum likelihood ("MLE") and "OLS".
#' @param alpha Significance level for confidence intervals (default = 0.05).
#' @param solver_options An optional list to control the behaviour of the
#'  underlying [systemfit::nlsystemfit()], [stats::optim()] and [stats::nlm()]
#'  solvers:
#'
#' - **trace**: An integer controlling the level of output for the
#' optimization procedure. Default is 0 (minimal output).
#' - **maxiter**: An integer specifying the maximum number of iterations for
#' the optimization procedure. Default is 1000.
#' - **startvals**: A list of starting values for the optimization
#' procedure (must match the number of parameters of the model).
#' - **solvtol**: Tolerance for detecting linear dependencies in the columns of
#'    X in the qr function calls (See [systemfit::nlsystemfit()]).
#'    Default is .Machine$double.eps.
#' - **gradtol**: A a positive scalar giving the tolerance at which the scaled
#'    gradient is considered close enough to zero to terminate the
#'    algorithm (See [stats::nlm()]). Default is 1e-6.
#' -  **steptol**: A positive scalar providing the minimum allowable relative
#' step length (See [stats::nlm()]). Default is 1e-6.
#' - **transform_se**: T/F whether standard errors should be constrained to be
#'     positive in optimization.
#' - **method**: String specifying optimization method (default = "L-BFGS-B").
#' - **se_method**: Method for standard error calculation (default = "hessian")
#' - **n_starts**: Number of random starting points for multi-start optimization
#'
#' @return A list with the following components:
#' \describe{
#'   \item{states}{A tibble containing filtered and smoothed state estimates.}
#'   \item{kk_model_mat}{A list of KK model matrices, such as transition
#'   and observation matrices.}
#'   \item{ss_model_mat}{A list of state-space model matrices derived
#'   from the KK model.}
#'   \item{model}{The KFAS state-space model object.}
#'   \item{params}{Estimated model parameters with standard errors.}
#'   \item{fit}{The fitted model object from the estimation procedure.}
#'   \item{loglik}{Log-likelihood value (MLE only).}
#'   \item{aic}{Akaike Information Criterion (MLE only).}
#'   \item{bic}{Bayesian Information Criterion (MLE only).}
#'   \item{convergence}{Convergence status.}
#'   \item{e}{The number of the efficient release (0-indexed).}
#'   \item{data}{The input data in wide format.}
#' }
#'
#'
#' @examples
#' # Example usage:
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1 # Number of efficient release
#' h <- 2 # Forecast horizon
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")
#'
#' result$params
#'
#' @references Kishor, N. Kundan and Koenig, Evan F., "VAR Estimation and
#' Forecasting When Data Are Subject to Revision", Journal of Business and
#' Economic Statistics, 2012.
#' @srrstats {G1.0} academic literature.
#'
#' @details
#' The function supports multiple models, including the full Kishor-Koenig
#' framework, Howrey's model, and a classical approach. It handles data
#' preprocessing, estimation of system equations using Seemingly Unrelated
#' Regressions (SUR), and application of the Kalman filter. This is
#' the first openly available implementation of the Kishor-Koenig model (See
#' the vignette \code{vignette("nowcasting_revisions")} for more details).
#' @srrstats {G1.1} first implementation of a novel algorithm
#' @srrstats {G1.3} Terminology explained here and in vignette.
#' @srrstats {G2.3b} use `tolower()`
#' @srrstats {TS4.3} return data contains time colum
#' @srrstats {TS4.6} Time Series Software which implements or otherwise
#' enables forecasting should return either:
#' @srrstats {TS4.6b} filtered/forecasted point estimates
#' @srrstats {TS4.6c} Error indication for forecast estimates (confidence intervals)
#' @srrstats {TS4.7c} Distinguishes model vs forecast values (sample column)
#' @srrstats {TS4.7} forecast values and models separately returned
#' @srrstats {TS4.7a} only forecast values returned
#' @srrstats {TS4.7b} forecast values and models separately returned
#' @srrstats {G5.9a} `.Machine$double.eps` to data does not meaningfully
#' change results
#'
#' @family revision nowcasting
#' @export
kk_nowcast <- function(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  method = "SUR",
  alpha = 0.05,
  solver_options = list()
) {
  # Default solver options
  default_solver_options <- list(
    trace = 0,
    maxiter = 1000,
    startvals = NULL,
    solvtol = .Machine$double.eps,
    gradtol = 1e-6,
    steptol = 1e-6,
    transform_se = TRUE,
    method = "L-BFGS-B",
    se_method = "hessian",
    n_starts = 1
  )

  model <- tolower(model)
  method <- tolower(method)

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
  if (!model %in% tolower(c("Kishor-Koenig", "KK", "Howrey", "Classical"))) {
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
    model %in% tolower(c("Kishor-Koenig", "KK")),
    1 + e + e^2,
    dplyr::if_else(
      model == tolower("Howrey"),
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

  # Check data input
  check <- vintages_check(df)

  # Reject lists with multiple IDs
  if (is.list(check)) {
    if (length(check) > 1) {
      rlang::abort(paste0(
        "'df' must contain a single ID, but ",
        length(check),
        " IDs were provided: ",
        paste(names(check), collapse = ", ")
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
        "'df' contains ",
        length(unique(df$id)),
        " different IDs. ",
        "Filter to a single ID first."
      ))
    }
    df <- suppressWarnings(vintages_wide(df, names_from = "release"))
    # Handle if vintages_wide returns a list
    if (is.list(df) && !is.data.frame(df)) df <- df[[1]]
  }

  # Define state and observable variable names
  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))

  n_states <- length(c(z_names, y_names))

  # Define equations
  equations <- kk_equations(
    kk_mat_sur = kk_mat_sur
  )

  # Arrange data
  sur_data <- kk_arrange_data(
    df = df,
    e = e
  )

  # Create observable variable matrix
  Ymat <- sur_data %>%
    dplyr::select(dplyr::all_of(y_names)) %>%
    as.matrix()

  # TODO: ar_order specifiable
  ar_order <- 1
  if (default_solver_options$trace > 0) {
    cat(
      "Estimating",
      model,
      "model with",
      n_param,
      "parameters...\n"
    )
    cat("Estimation:", method, "\n")
    cat("AR order:", ar_order, "\n")
  }

  # Initialize variables for results
  params <- NULL
  fit <- NULL
  loglik <- NULL
  aic <- NULL
  bic <- NULL
  convergence <- NULL
  params_raw <- NULL
  se_raw <- NULL

  # Estimation of parameters with SUR or OLS
  if (method == tolower("SUR")) {
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

    # Extract standard errors from SUR
    # For nlsystemfit objects, use the coefCov component
    if (!is.null(fit$se)) {
      se_raw <- fit$se
    } else {
      # Fallback: approximate SEs
      se_raw <- rep(NA, length(fit$b))
      rlang::warn(
        "Could not extract standard errors from SUR estimation",
        call. = FALSE
      )
    }

    # Add variance SEs
    # For variance parameters from residual covariance matrix,
    # use asymptotic SE formula: SE(sigma^2) = sigma^2 * sqrt(2/(n-k))
    # where n = number of observations, k = number of parameters
    n_obs <- nrow(sur_data)
    k_params <- length(fit$b)
    var_se <- parm_cov * sqrt(2 / (n_obs - k_params))
    se_raw <- c(se_raw, var_se)

    convergence <- fit$convergence
  } else if (method == tolower("OLS")) {
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

    # Extract standard errors from OLS
    se_raw <- numeric(length(params))
    idx <- 1
    for (i in seq_along(fit)) {
      if (!is.null(fit[[i]])) {
        model_se <- summary(fit[[i]])$coefficients[, "Std. Error"]
        se_raw[idx:(idx + length(model_se) - 1)] <- model_se
        idx <- idx + length(model_se)
      }
    }

    # Add variance SEs using asymptotic formula
    # For variance parameters: SE(sigma^2) = sigma^2 * sqrt(2/(n-k))
    var_idx <- grep("v0|eps", names(params))
    if (length(var_idx) > 0) {
      # Use the first OLS model to get n and k (representative)
      n_obs <- nrow(fit[[1]]$model)
      k_params <- length(fit[[1]]$coefficients)
      for (i in var_idx) {
        se_raw[i] <- params[i] * sqrt(2 / (n_obs - k_params))
      }
    }

    convergence <- 0
  } else if (method == tolower("MLE")) {
    # 1. Define the Negative Log-Likelihood Function
    kk_negloglik <- function(p, e, model, Ymat, n_states, transform_se) {
      # Map vector p back to named parameters
      param_names <- names(start_mat_mle)
      names(p) <- param_names

      # Transform variance parameters if needed
      if (transform_se) {
        var_idx <- grep("v0|eps", param_names)
        p[var_idx] <- exp(p[var_idx])
      }

      # Ensure variance parameters are positive
      var_idx <- grep("v0|eps", names(p))
      if (any(p[var_idx] <= 0)) {
        return(1e10)
      }

      tryCatch(
        {
          k_mat <- kk_matrices(
            e = e,
            model = model,
            params = p,
            type = "numeric"
          )
          ss_mat <- kk_to_ss(k_mat$FF, k_mat$GG, k_mat$V, k_mat$W)

          mod <- KFAS::SSModel(
            Ymat ~ -1 +
              SSMcustom(
                Z = ss_mat$Z,
                T = ss_mat$Tmat,
                R = ss_mat$R,
                Q = ss_mat$Q,
                a1 = c(rep(0.2, n_states / 2), rep(0, n_states / 2)),
                P1inf = diag(
                  c(rep(1, n_states / 2), rep(0, n_states / 2)),
                  n_states
                ),
                P1 = diag(
                  c(rep(0, n_states / 2), rep(1, n_states / 2)),
                  n_states
                ),
                index = seq_len(ncol(Ymat))
              ),
            H = ss_mat$H
          )

          # Return negative log-likelihood for minimization
          ll <- stats::logLik(mod, check.model = FALSE)

          if (is.finite(ll)) {
            return(-as.numeric(ll))
          } else {
            return(1e10)
          }
        },
        error = function(e) {
          return(1e10)
        }
      )
    }

    # 2. Prepare starting values
    if (is.null(default_solver_options$startvals)) {
      # Use OLS estimates as smart starting values if none provided
      ols_init <- kk_ols_estim(equations, sur_data, model)$params
      start_mat_mle <- ols_init
    } else {
      start_mat_mle <- default_solver_options$startvals
    }

    # Log-transform variances if needed
    if (default_solver_options$transform_se) {
      var_idx <- grep("v0|eps", names(start_mat_mle))
      start_mat_mle[var_idx] <- log(pmax(start_mat_mle[var_idx], 1e-6))
    }

    # Check if multi-start is requested
    n_starts <- if (!is.null(default_solver_options$n_starts)) {
      max(1, default_solver_options$n_starts)
    } else {
      1
    }

    if (n_starts > 1 && default_solver_options$trace > 0) {
      cat(
        "\nUsing multi-start optimization with",
        n_starts,
        "starting points\n"
      )
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
        current_init <- start_mat_mle
      } else {
        # Perturb around default
        current_init <- start_mat_mle +
          stats::rnorm(length(start_mat_mle), 0, 0.5)

        # Ensure perturbations respect constraints
        if (default_solver_options$transform_se) {
          var_idx <- grep("v0|eps", names(start_mat_mle))
          current_init[var_idx] <- pmax(
            pmin(current_init[var_idx], log(100)),
            log(0.001)
          )
        }

        # For F0 and G parameters, keep reasonable
        non_var_idx <- grep("F0|G", names(start_mat_mle))
        current_init[non_var_idx] <- pmax(
          pmin(current_init[non_var_idx], 0.9),
          -0.9
        )
      }

      # Determine optimization method
      opt_method <- default_solver_options$method

      if (opt_method == "two-step") {
        if (default_solver_options$trace > 0) {
          cat("Step 1: Nelder-Mead (global search)...\n")
        }

        opt_result_nm <- stats::optim(
          par = current_init,
          fn = kk_negloglik,
          e = e,
          model = model,
          Ymat = Ymat,
          n_states = n_states,
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
          fn = kk_negloglik,
          e = e,
          model = model,
          Ymat = Ymat,
          n_states = n_states,
          transform_se = default_solver_options$transform_se,
          method = "BFGS",
          control = list(
            trace = max(0, default_solver_options$trace),
            maxit = default_solver_options$maxiter
          ),
          hessian = FALSE
        )
      } else if (opt_method == "L-BFGS-B") {
        # Set bounds for parameters
        n_params_opt <- length(current_init)
        lower_bounds <- rep(-Inf, n_params_opt)
        upper_bounds <- rep(Inf, n_params_opt)

        if (default_solver_options$transform_se) {
          var_idx <- grep("v0|eps", names(start_mat_mle))
          lower_bounds[var_idx] <- log(0.001)
          upper_bounds[var_idx] <- log(100)
        }

        # F0 and G parameters
        non_var_idx <- grep("F0|G", names(start_mat_mle))
        lower_bounds[non_var_idx] <- -0.99
        upper_bounds[non_var_idx] <- 0.99

        current_result <- stats::optim(
          par = current_init,
          fn = kk_negloglik,
          e = e,
          model = model,
          Ymat = Ymat,
          n_states = n_states,
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
      } else if (opt_method == "nlminb") {
        # Set bounds
        n_params_opt <- length(current_init)
        lower_bounds <- rep(-Inf, n_params_opt)
        upper_bounds <- rep(Inf, n_params_opt)

        if (default_solver_options$transform_se) {
          var_idx <- grep("v0|eps", names(start_mat_mle))
          lower_bounds[var_idx] <- log(0.001)
          upper_bounds[var_idx] <- log(100)
        }

        non_var_idx <- grep("F0|G", names(start_mat_mle))
        lower_bounds[non_var_idx] <- -0.99
        upper_bounds[non_var_idx] <- 0.99

        opt_result_nlminb <- stats::nlminb(
          start = current_init,
          objective = kk_negloglik,
          e = e,
          model = model,
          Ymat = Ymat,
          n_states = n_states,
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
      } else {
        current_result <- stats::optim(
          par = current_init,
          fn = kk_negloglik,
          e = e,
          model = model,
          Ymat = Ymat,
          n_states = n_states,
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
    convergence <- opt_result$convergence
    fit <- opt_result

    # ===== STANDARD ERROR CALCULATION =====
    se_method <- default_solver_options$se_method

    if (se_method == "hessian") {
      # Calculate high-precision Hessian
      se_result <- suppressWarnings(tryCatch(
        {
          precise_hessian <- numDeriv::hessian(
            func = kk_negloglik,
            x = params_raw,
            e = e,
            model = model,
            Ymat = Ymat,
            n_states = n_states,
            transform_se = default_solver_options$transform_se,
            method.args = list(eps = 1e-4, d = 0.01, r = 6)
          )

          # Check condition number
          cond_num <- tryCatch(
            kappa(precise_hessian, exact = FALSE),
            error = function(e) Inf
          )

          if (!is.finite(cond_num) || cond_num > 1e10) {
            msg <- paste0(
              "Hessian is poorly conditioned",
              if (is.finite(cond_num)) {
                paste0(
                  " (cond = ",
                  format(cond_num, scientific = TRUE, digits = 2),
                  ")"
                )
              } else {
                ""
              },
              ". SEs may be unreliable."
            )

            list(se = rep(NA, length(params_raw)), warning = msg, failed = TRUE)
          } else {
            # Try to invert
            fisher_info <- tryCatch(
              {
                solve(precise_hessian)
              },
              error = function(e) {
                ridge <- 1e-6 * mean(abs(diag(precise_hessian)))
                if (default_solver_options$trace > 0) {
                  cat(
                    "Adding ridge regularization (\u03BB =",
                    format(ridge, scientific = TRUE),
                    ")\n"
                  )
                }
                tryCatch(
                  solve(
                    precise_hessian + ridge * diag(nrow(precise_hessian))
                  ),
                  error = function(e2) NULL
                )
              }
            )

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
                problem_msg <- paste0(
                  (n_nan + n_large),
                  " parameter(s) have problematic SEs"
                )
                has_failed <- TRUE
              }

              se_calc[is.nan(se_calc)] <- NA
              list(se = se_calc, warning = problem_msg, failed = has_failed)
            }
          }
        },
        error = function(e) {
          list(
            se = rep(NA, length(params_raw)),
            warning = paste0("Hessian calculation failed: ", e$message),
            failed = TRUE
          )
        }
      ))

      se_raw <- se_result$se
      se_warning <- se_result$warning
      hessian_failed <- se_result$failed
      se_method_used <- "hessian"

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
      var_idx <- grep("v0|eps", names(params_raw))

      for (i in var_idx) {
        if (!is.na(i)) {
          # The value on the original scale: exp(log_sigma)
          params[i] <- exp(params_raw[i])

          # The Delta Method: SE_original = |d/dx exp(x)| * SE_log
          se[i] <- params[i] * se_raw[i]
        }
      }
    }

    # Ensure parameter order is consistent
    params <- kk_matrices(
      e = e,
      model = model,
      params = params,
      type = "numeric"
    )$params

    # Calculate information criteria
    n_obs <- nrow(Ymat) * ncol(Ymat)
    aic <- -2 * loglik + 2 * n_param
    bic <- -2 * loglik + log(n_obs) * n_param
  }

  # Create model matrices with estimated parameters
  kk_mat_hat <- kk_matrices(
    e = e,
    model = model,
    params = params,
    type = "numeric"
  )

  # Cast model matrices to state space form
  sur_ss_mat <- kk_to_ss(
    FF = kk_mat_hat$FF,
    GG = kk_mat_hat$GG,
    V = kk_mat_hat$V,
    W = kk_mat_hat$W,
    epsilon = 1e-6
  )

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
    forecast_dates <- as.Date(character(0))
  }

  # Create the SSM object
  model_kfas <- KFAS::SSModel(
    Ymat ~
      -1 +
      SSMcustom(
        Z = sur_ss_mat$Z,
        T = sur_ss_mat$Tmat,
        R = sur_ss_mat$R,
        Q = sur_ss_mat$Q,
        a1 = c(rep(0.2, n_states / 2), rep(0, n_states / 2)),
        P1inf = diag(c(rep(1, n_states / 2), rep(0, n_states / 2)), n_states),
        P1 = diag(c(rep(0, n_states / 2), rep(1, n_states / 2)), n_states),
        index = c(seq_len(dim(Ymat)[2]))
      ),
    H = sur_ss_mat$H
  )

  # Run the Kalman filter
  kalman <- KFAS::KFS(model_kfas)

  # Number of state variables
  n_states <- length(z_names)

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
      state = z_names[i],
      estimate = filtered_est,
      lower = filtered_est - stats::qnorm(1 - alpha / 2) * filtered_se,
      upper = filtered_est + stats::qnorm(1 - alpha / 2) * filtered_se,
      filter = "filtered",
      sample = dplyr::if_else(
        output_dates %in% forecast_dates,
        "out_of_sample",
        "in_sample"
      )
    )

    # Create smoothed data frame
    smoothed_df <- dplyr::tibble(
      time = output_dates,
      state = z_names[i],
      estimate = smoothed_est,
      lower = smoothed_est - stats::qnorm(1 - alpha / 2) * smoothed_se,
      upper = smoothed_est + stats::qnorm(1 - alpha / 2) * smoothed_se,
      filter = "smoothed",
      sample = dplyr::if_else(
        output_dates %in% forecast_dates,
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

  # Create parameter table with standard errors
  if (method == tolower("MLE")) {
    param_table <- data.frame(
      Parameter = names(params),
      Estimate = params,
      Std.Error = se,
      row.names = NULL
    )
  } else if (method == tolower("SUR") || method == tolower("OLS")) {
    param_table <- data.frame(
      Parameter = names(params),
      Estimate = params,
      Std.Error = se_raw,
      row.names = NULL
    )
  }

  # Remove the parameters from the model matrices
  kk_mat_hat$params <- NULL

  results <- list(
    states = states_long,
    kk_model_mat = kk_mat_hat,
    ss_model_mat = sur_ss_mat,
    model = model_kfas,
    params = param_table,
    fit = fit,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = convergence,
    e = e,
    data = df
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
#' @param kk_mat_sur A list containing the KK matrix structure,
#' including matrices `FF`, `II`, and `GG`.
#'
#' @return A list of formula objects representing the equations of the KK model.
#'
#' @details
#' The function constructs the equations based on the dimensions of the input
#' matrices and generates formulas for each equation. It utilizes lagged
#' variables and matrix operations to form the relationships.
#'
#' @keywords internal
#' @noRd
kk_equations <- function(kk_mat_sur) {
  e <- dim(kk_mat_sur$FF)[1] - 1

  II <- diag(e + 1)

  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))
  y_names <- c(paste0("release_", e:0, "_lag_", e:0))
  y_lag_names <- c(paste0("release_", e:0, "_lag_", (e + 1):1))

  lhs1 <- z_names
  rhs1 <- kk_mat_sur$FF %mx% z_lag_names

  lhs2 <- (y_names)
  rhs2 <- (((II %diff% kk_mat_sur$GG) %prod% kk_mat_sur$FF) %mx%
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
#' This function arranges the input data frame into a format suitable for
#' estimating  Kishor-Koenig (KK) models. It generates lagged variables and
#' combines them into a data frame.
#'
#' @param df A data frame containing the original data.
#' @param e An integer indicating the efficient release.
#'
#' @return A data frame with lagged variables, prepared for KK model estimation.
#'
#' @details
#' The function creates lagged versions of the release variables up to
#' the specified lag `e`. It constructs variables named `release_e_lag_e:0`,
#' `release_e_lag_(e+1):1`, `release_e:0_lag_e:0`, and
#' `release_e:0_lag_(e+1):1`. The function then combines these variables into
#' a single data frame, removing rows with missing values.
#'
#' @keywords internal
#' @noRd
kk_arrange_data <- function(df, e) {
  dates <- df$time
  z_names <- c(paste0("release_", e, "_lag_", (e):0))
  z_lag_names <- c(paste0("release_", e, "_lag_", (e + 1):1))

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

  data <- cbind(z, y, y_lag)
  rownames(data) <- dates
  data <- data %>% tidyr::drop_na()

  return(data)
}

#' @title Estimate Generalized Kishor-Koenig (KK) Models via OLS
#'
#' @description
#' This function estimates the parameters of generalized KK models
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
#' @srrstats {G2.4b} convert  via `as.numeric()`
#'
#' @details
#' This function is designed to handle different variations of the KK model,
#' including "Classical", "Howrey", and the "KK" or "Kishor-Koenig" models.
#' It extracts coefficients and variances based on the specified model type.
#'
#' @keywords internal
#' @noRd
kk_ols_estim <- function(equations, data, model = "KK") {
  model <- tolower(model)
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

    if (model == tolower("Classical")) {
      sig2 <- var(data[[variables[1]]] - data[[variables[2]]])
      names(sig2) <- paste0("eps", e - ii + 1)
      ols_vars <- c(ols_vars, sig2)
    } else if (model == tolower("Howrey")) {
      # Check length of extracted elements
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
    } else if (model %in% tolower(c("KK", "Kishor-Koenig"))) {
      # Check length of extracted elements
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
          if (i == 1 && ii == n_eq) {
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
#' Constructs the matrices \eqn{I}, \eqn{F}, \eqn{G}, \eqn{V}, and \eqn{W}
#' used in state-space models, specifically for the Kishor-Koenig (KK), Howrey,
#'  or Classical frameworks.
#'
#' @param e Integer. The number of efficiency gaps (lags) in the model. Must
#' be greater than 0.
#' @param model Character. Specifies the type of model to use. Options are:
#'   \describe{
#'     \item{"Kishor-Koenig" or "KK"}{Uses the Kishor-Koenig framework with
#'     \eqn{e \times (e+1)} parameters for the \eqn{G} matrix.}
#'     \item{"Howrey"}{Uses the Howrey framework with \eqn{e \times e}
#'     parameters for the \eqn{G} matrix.}
#'     \item{"Classical"}{Uses a diagonal identity matrix for \eqn{G}.}
#'   }
#' @param params Numeric vector (optional). A vector of parameters to
#' initialize the matrices. If \code{NULL}, default values are used:
#'   \describe{
#'     \item{\code{type = "numeric"}}{A vector of params must be supplied.}
#'     \item{\code{type = "character"}}{Initializes named parameters
#'     as \code{NA_real_}.}
#'   }
#'   If provided, the length of \code{params} must match the number of
#'   parameters required by the specified model.
#' @param type Character. Specifies the type of matrices returned. Options are:
#'   \describe{
#'     \item{"numeric"}{Returns numeric matrices with parameter values.}
#'     \item{"character"}{Returns character matrices with parameter names.
#'     If \code{params} is provided, it is ignored.}
#'   }
#'
#' @return A list containing the following components:
#'   \describe{
#'     \item{\code{FF}}{State transition matrix (\eqn{F}). Size: \eqn{(e+1)
#'      \times (e+1)}.}
#'     \item{\code{GG}}{Control matrix (\eqn{G}). Size depends on the model
#'     and \code{e}.}
#'     \item{\code{V}}{State noise covariance matrix (\eqn{V}).
#'     Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{W}}{Observation noise covariance matrix (\eqn{W}).
#'     Size: \eqn{(e+1) \times (e+1)}.}
#'     \item{\code{params}}{The vector of parameters used to construct the
#'     matrices, including their names.}
#'   }
#'
#' @details The generalized Kishor-Koenig model consists of the following
#' equations:
#'
#' **State Equation:**
#' \deqn{ z_t = F z_{t-1} + \nu_t, \quad \nu_t \sim N(0, V)}
#'
#' **Observation Equation:**
#' \deqn{y_t = (I - G) F y_{t-1} + G z_t +
#'  \epsilon_t, \quad \epsilon_t \sim N(0, W)}
#'
#' where:
#' - \eqn{z_t} is the state vector.
#' - \eqn{y_t} is the observed data.
#' - \eqn{F} is the state transition matrix.
#' - \eqn{G} is the control matrix.
#' - \eqn{V} is the state noise covariance matrix.
#' - \eqn{W} is the observation noise covariance matrix.
#'
#' @srrstats {G2.4b} convert via via `as.numeric()`
#' @srrstats {G2.4c} convert via via `as.character()`
#' @srrstats {G3.0} Uses epsilon for numerical stability (epsilon parameter)
#'
#' @examples
#' # Example 1: Kishor-Koenig model with character matrices
#' matrices <- kk_matrices(e = 3, model = "KK", type = "character")
#' str(matrices)
#'
#' # Example 2: Kishor-Koenig model with e = 2
#' params <- rep(0.1, 17)
#' names(params) <- names(matrices$params)
#' matrices <- kk_matrices(
#'   e = 3, params = params, model = "KK", type = "numeric"
#' )
#' str(matrices)
#'
#' @keywords internal
#' @noRd
kk_matrices <- function(e, model, params = NULL, type = "numeric") {
  model <- tolower(model)

  # Start param count
  ii <- 1

  # Check input e
  if (e == 0) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }

  # Check model input
  if (!model %in% tolower(c("Kishor-Koenig", "KK", "Howrey", "Classical"))) {
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
    model %in% tolower(c("Kishor-Koenig", "KK")),
    1 + e + e^2,
    dplyr::if_else(
      model == tolower("Howrey"),
      1 + e^2,
      1 # Classical
    )
  )

  n_param_cov <- e + 1

  n_param <- n_param_mat + n_param_cov

  if (!is.null(params) && length(params) != n_param) {
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

  if (model %in% tolower(c("Kishor-Koenig", "KK"))) {
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
  } else if (model == tolower("Howrey")) {
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
  } else if (model == tolower("Classical")) {
    GG <- diag(e + 1)
  } else {
    rlang::abort("'model' not recognized")
  }

  # Get Variance-covariance matrices
  # State noise covariance
  V <- array(0, c(e + 1, e + 1))
  if (type == "numeric") {
    V[e + 1, e + 1] <- params[[paste0("v0")]]
  } else if (type == "character") {
    V[e + 1, e + 1] <- "v0"
    names(params)[ii] <- c(paste0("v0"))
  }
  ii <- ii + 1

  # Observation noise covariance
  W <- array(0, c(e + 1, e + 1))
  for (jj in 2:(e + 1)) {
    if (type == "numeric") {
      W[jj, jj] <- params[[paste0("eps", e + 1 - jj)]]
    } else if (type == "character") {
      W[jj, jj] <- paste0("eps", e + 1 - jj)
      names(params)[ii] <- c(paste0("eps", e + 1 - jj))
    }

    ii <- ii + 1
  }

  if (type == "character") {
    FF <- apply(FF, c(1, 2), as.character)
    GG <- apply(GG, c(1, 2), as.character)
    V <- apply(V, c(1, 2), as.character)
    W <- apply(W, c(1, 2), as.character)
  } else if (type == "numeric") {
    FF <- apply(FF, c(1, 2), as.numeric)
    GG <- apply(GG, c(1, 2), as.numeric)
    V <- apply(V, c(1, 2), as.numeric)
    W <- apply(W, c(1, 2), as.numeric)
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

  return(list(FF = FF, GG = GG, V = V, W = W, params = params))
}


#' Cast Generalized Kishor-Koenig Matrices into State-Space Form
#'
#' Transforms the generalized Kishor-Koenig (KK) matrices into the state-space
#' representation required for Kalman filtering and smoothing.
#'
#' @param FF Matrix. The state transition matrix defining how the state
#' evolves over time.
#' @param GG Matrix. The control matrix, representing the influence of the
#' state on observations.
#' @param V Matrix. The state noise covariance matrix.
#' @param W Matrix. The observation noise covariance matrix.
#' @param epsilon Numeric. A small positive number to ensure numerical
#' stability in covariance matrices (default: \code{1e-6}).
#'
#' @return A list containing the state-space matrices:
#'   \describe{
#'     \item{\code{Z}}{The observation matrix.}
#'     \item{\code{Tmat}}{The state transition matrix for the augmented
#'     state-space model.}
#'     \item{\code{H}}{The observation noise covariance matrix.}
#'     \item{\code{Q}}{The state noise covariance matrix.}
#'     \item{\code{R}}{The control matrix.}
#'   }
#' @srrstats {G3.0} Numerical stability with epsilon parameter
#' @details The state space model is represented by the following equations:
#'
#' **State Equation:**
#' \deqn{\alpha_{t+1} = T \alpha_t + R \eta_t, \quad \eta_t \sim N(0, Q)}
#'
#' **Observation Equation:**
#' \deqn{y_t = Z \alpha_t + \epsilon_t, \quad \epsilon_t \sim N(0, H)}
#'
#' where:
#' - \eqn{\alpha_t} is the state vector.
#' - \eqn{y_t} is the observed data.
#' - \eqn{T} is the state transition matrix.
#' - \eqn{R} is the control matrix.
#' - \eqn{Q} is the covariance matrix of the state disturbances \eqn{\eta_t}.
#' - \eqn{Z} is the observation matrix.
#' - \eqn{H} is the covariance matrix of the observation disturbances
#' \eqn{\epsilon_t}.
#'
#' @examples
#' # Define example matrices
#' II <- diag(3)
#' FF <- matrix(c(0.9, 0.1, 0, 0.1, 0.8, 0.1, 0, 0.1, 0.9), nrow = 3)
#' GG <- matrix(c(0.8, 0.2, 0, 0.2, 0.7, 0.1, 0, 0.1, 0.8), nrow = 3)
#' V <- diag(3) * 0.01
#' W <- diag(3) * 0.02
#'
#' # Generate state-space matrices
#' ss_matrices <- kk_to_ss(FF, GG, V, W)
#' str(ss_matrices)
#'
#' @keywords internal
#' @noRd
kk_to_ss <- function(FF, GG, V, W, epsilon = 1e-6) {
  # Get efficient release, e
  e <- nrow(FF) - 1

  II <- diag(e + 1)

  # Observation matrix Z
  Z <- cbind(II, II)

  # State transition matrix
  Tmat <- rbind(
    cbind(FF, array(0, c(e + 1, e + 1))),
    cbind(array(0, c(e + 1, e + 1)), (II - GG) %*% FF)
  )

  # Covariance matrices
  R <- diag(2 * (e + 1))
  H <- array(0, c(e + 1, e + 1))
  Q <- array(0, c(2 * (e + 1), 2 * (e + 1)))
  v_t_2 <- V[1:(e + 1), 1:(e + 1)]
  Q[1:(e + 1), 1:(e + 1)] <- v_t_2
  Q[(1:(e + 1)), ((e + 2):(2 * (e + 1)))] <- -v_t_2 %*% t(II - GG)
  Q[((e + 2):(2 * (e + 1))), 1:(e + 1)] <- -(II - GG) %*% v_t_2
  Q[((e + 2):(2 * (e + 1))), ((e + 2):(2 * (e + 1)))] <- W[
    1:(e + 1),
    1:(e + 1)
  ] +
    (II - GG) %*% v_t_2 %*% t(II - GG)

  for (jj in c(1:e, e + 2)) {
    Q[jj, jj] <- epsilon
  }

  return(list(Z = Z, Tmat = Tmat, H = H, Q = Q, R = R))
}


#' Summary Method for KK Model
#'
#' @description Computes and displays a summary of the results from a
#' Kishor-Koenig (KK) model fit, including convergence status,
#' information criteria, and parameter estimates.
#'
#' @param object An object of class \code{kk_model}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The function returns the input \code{object} invisibly.
#' @method summary kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1
#' h <- 2
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig", method = "MLE")
#' summary(result)
#'
#' @family revision nowcasting
#' @export
summary.kk_model <- function(object, ...) {
  cat("\n=== Kishor-Koenig Model ===\n\n")

  if (!is.null(object$convergence)) {
    cat(
      "Convergence:",
      ifelse(
        object$convergence == 0,
        "Success",
        "Failed"
      ),
      "\n"
    )
  }

  if (!is.null(object$loglik)) {
    cat("Log-likelihood:", round(object$loglik, 2), "\n")
  }

  if (!is.null(object$aic)) {
    cat("AIC:", round(object$aic, 2), "\n")
  }

  if (!is.null(object$bic)) {
    cat("BIC:", round(object$bic, 2), "\n")
  }

  cat("\nParameter Estimates:\n")
  df_print <- object$params
  df_print$Estimate <- sprintf("%.3f", df_print$Estimate)
  df_print$Std.Error <- sprintf("%.3f", df_print$Std.Error)
  print(df_print, row.names = FALSE, quote = FALSE)

  cat("\n")
  invisible(object)
}


#' Print Method for KK Model
#'
#' @description Default print method for \code{kk_model} objects.
#' Wraps the \code{summary} method for a consistent output.
#'
#' @param x An object of class \code{kk_model}.
#' @param ... Additional arguments passed to \code{summary.kk_model}.
#'
#' @return The function returns the input \code{x} invisibly.
#' @method print kk_model
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1
#' h <- 2
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig", method = "MLE")
#' result
#'
#' @family revision nowcasting
#' @export
print.kk_model <- function(x, ...) {
  summary.kk_model(x, ...)
}


#' Plot Kishor-Koenig Model Results
#'
#' @param x An object of class 'kk_model'
#' @param state String. The name of the state to visualize.
#' @param type String. Type of estimate to plot: "filtered" or "smoothed".
#' @param ... Additional arguments passed to theme_reviser.
#'
#' @return A ggplot2 object visualizing the specified state estimates.
#' @examples
#' df <- get_nth_release(
#'   tsbox::ts_span(
#'     tsbox::ts_pc(
#'       dplyr::filter(reviser::gdp, id == "US")
#'     ),
#'     start = "1980-01-01"
#'   ),
#'   n = 0:1
#' )
#' df <- dplyr::select(df, -c("id", "pub_date"))
#' df <- na.omit(df)
#'
#' e <- 1 # Number of efficient release
#' h <- 2 # Forecast horizon
#' result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")
#'
#' plot(result)
#'
#' @family revision nowcasting
#' @export
plot.kk_model <- function(x, state = NULL, type = "filtered", ...) {
  if (is.null(state)) {
    state <- x$states[x$states$filter == type, ]$state[1]
  }
  # Forward to the base method with KK defaults
  plot.revision_model(x, state = state, type = type, ...)
}
