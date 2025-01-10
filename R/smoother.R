#' Generalized Kishor-Koenig model nowcasting
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return MLE of state space model - Kalman filter and smoother
#' @export
#' TODO: Allow for more lags, add constant and trend, or choose wit IC
#' TODO: function for start_values grid search
kk_nowcast <- function(
    df, 
    e, 
    h=1, 
    model = "Kishor-Koenig", 
    estim = "MLE", 
    start_mat = 0.4, 
    start_cov = 0.4,
    m0 = 0,
    C0 = 1e-6,
    trace = 0
    ) {
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
    rlang::abort("'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!")
  }
  
  # Check estimation input
  if (!estim %in% c("MLE", "SUR")) {
    rlang::abort("'estim' must be one of 'MLE' or 'SUR'!")
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
    ))
  
  # KK cov matrices
  n_param_cov <- e + 1
  
  n_param <- n_param_mat + n_param_cov
  
  
  if (length(start_mat) == 1 && is.numeric(start_mat)) {
    start_mat <- rep(start_mat, n_param_mat)
  }
  
  if (length(start_cov) == 1 && is.numeric(start_cov)) {
    start_cov <- rep(start_cov, n_param_cov)
  }
  
  if (length(c(start_mat, start_cov))  != n_param) {
    rlang::abort(paste0("The length of 'start_mat' must be 1 or ", n_param_mat ," and the length of 'start_cov' must be 1 or ", n_param_cov, " if 'e' = ", e))
  }
  
  # Check start values forthe expected value of the pre-sample state vector m0 
  # and the covariance matrix C0
  
  # Both must be numeric
  if (!is.numeric(m0) || !is.numeric(C0)) {
    rlang::abort("Both 'm0' and 'C0' must be numeric!")
  }
  
  if (length(m0) == 1) {
    m0 <- rep(m0, (e+1)*2)
  } else if (length(m0) != (e+1)*2) {
    rlang::abort(paste0("The length of 'm0' must be 1 or ", (e+1)*2), " if 'e' = ", e)
  }
  
  if (length(C0) == 1) {
    C0 <- diag(C0, (e+1)*2)
  } else if (length(C0) == (e+1)*2) {
    C0 <- diag(C0)
  } else if (all(dim(C0) == c((e+1)*2, (e+1)*2))) {
    C0 <- C0
  } else {
    rlang::abort(paste0("'C0' must be a vector of length 1 or ", (e+1)*2, " or a ", (e+1)*2, "x", (e+1)*2, " matrix if 'e' = ", e))
  }
  
  
  # Check data input
  check <- vintages_check(df)
  if (check=="long") {
    df <- vintages_wide(df, names_from = "release")
  }
  
  state_names <- c(paste0("release_", e, "_lag_", (e):0))
  state_lag_names <- c(paste0("release_", e, "_lag_", (e+1):1))
  observable_names <- c(paste0("release_", e:0, "_lag_", e:0))
  observable_lag_names <- c(paste0("release_", e:0, "_lag_", (e+1):1))
  
  if (estim == "SUR") {
    # Define matrices
    kk_mat_sur <- kk_matrices(e=e, model=model, type = "character")
  
    X_names <- state_names
    X_lag_names <- state_lag_names
    Y_names <- observable_names
    Y_lag_names <- observable_lag_names
    
    # Define equations
    lhs1 <- X_names
    rhs1 <- kk_mat_sur$FF %mx% X_lag_names
    
    lhs2 <- Y_names
    rhs2 <- ((kk_mat_sur$II %diff% kk_mat_sur$GG) %prod% kk_mat_sur$FF ) %mx% Y_lag_names %sum% (kk_mat_sur$GG %mx% X_names)
  
    equations <- list()
    formula <- as.formula(paste0(lhs1[e+1], " ~ ", rhs1[e+1]))
    equations[[paste0("eq", 1)]] <- formula
    eq <- 2
    for (i in 2:(e+1)) {
      formula <- as.formula(paste0( lhs2[i], " ~ ", rhs2[i]))
      equations[[paste0("eq", eq)]] <- formula
      eq <- eq + 1
    }
    
    # Arrange data
    X <- array(NA, c(nrow(df), e + 1))
    X_lag <- array(NA, c(nrow(df), e + 1))
    for (j in (e):0) {
      X[, (e+1) - j] <- lag(dplyr::pull(df[paste0("release_",e)]), j)
      X_lag[, (e+1) - j] <- lag(dplyr::pull(df[paste0("release_",e)]), j+1)
    }
    X <- tibble::as_tibble(X)
    colnames(X) <- state_names
    X_lag <- tibble::as_tibble(X_lag)
    colnames(X_lag) <- state_lag_names
    
    Y <- array(NA, c(nrow(df), e ))
    Y_lag <- array(NA, c(nrow(df), e ))
    for (j in (e-1):0) {
        Y[, (e) - j] <- lag(dplyr::pull(df[paste0("release_",j)]), j)  
        Y_lag[, (e) - j] <- lag(dplyr::pull(df[paste0("release_",j)]), j+1)  
    }
    
    Y <- tibble::as_tibble(Y)
    Y_lag <- tibble::as_tibble(Y_lag)
    colnames(Y) <- c(paste0("release_", (e-1):0, "_lag_", (e-1):0))
    colnames(Y_lag) <- c(paste0("release_", (e-1):0, "_lag_", e:1))
    
    sur_data <- cbind(X, Y, Y_lag) %>% na.omit()
    
    names(start_mat) <- names(kk_mat_sur$params)[1:n_param_mat]
    
    fit <- systemfit::nlsystemfit(
      equations, 
      method = "SUR", 
      data = sur_data, 
      startvals = start_mat,
      print.level = trace
      )
    
    params <- c(fit$b, diag(fit$rcov))
    
    kk_mat_hat <- kk_matrices(e=e, model=model, params = params, type = "numeric")
    
    sur_ss_mat <- kk_to_ss(
      II = kk_mat_hat$II,
      FF=kk_mat_hat$FF, 
      GG=kk_mat_hat$GG, 
      R=kk_mat_hat$R, 
      H=kk_mat_hat$H,
      epsilon = 1e-6
      )
    
    # Build the model
    dlm_mod <- dlm::dlm(
      m0 = m0, 
      C0 = C0, 
      FF = sur_ss_mat$Z, 
      V = sur_ss_mat$V,
      GG = sur_ss_mat$Tmat, 
      W = sur_ss_mat$W
    )
  
  } else if (estim == "MLE") {
    # Define the system matrices for dlm
    buildModel <- function(params, e, model, m0, C0) {
      
      kk_mat_hat <- kk_matrices(e=e, model=model, params = params, type = "numeric")
      
      mle_ss_mat <- kk_to_ss(
        II=kk_mat_hat$II,
        FF=kk_mat_hat$FF, 
        GG=kk_mat_hat$GG, 
        R=kk_mat_hat$R, 
        H=kk_mat_hat$H,
        epsilon = 1e-6
      )
      
      # Build the model
      dlmMod <- dlm::dlm(
        m0 = m0, 
        C0 = C0, 
        FF = mle_ss_mat$Z, 
        V = mle_ss_mat$V,
        GG = mle_ss_mat$Tmat, 
        W = mle_ss_mat$W
      )
      return(dlmMod)
    }
    
    Y <- array(NA, c(nrow(df), e+1))
    for (j in (e):0) {
      Y[, (e+1) - j] <- lag(dplyr::pull(df[paste0("release_",j)]), j)  
    }
    Y <- na.omit(tibble::as_tibble(Y))
    colnames(Y) <- observable_names
    Y <- as.matrix(Y)

    fit <- dlm::dlmMLE(
      Y, 
      parm = c(start_mat, start_cov), 
      build = buildModel, 
      e = e,
      model = model,
      m0 = m0, 
      C0 = C0,
      control = list(trace = trace, REPORT = 2)
      )
    params <- c(fit$par[1:n_param_mat], exp(fit$par[(n_param_mat+1):n_param]))
    kk_mat_hat <- kk_matrices(e = e, model = model, params = params, type = "numeric")
    dlm_mod <- buildModel(fit$par, e, model = model, m0 = m0, C0 = C0)
  } 
  
  Y <- array(NA, c(nrow(df), e+1))
  for (j in (e):0) {
    Y[, (e+1) - j] <- lag(dplyr::pull(df[paste0("release_",j)]), j)  
  }
  Y <- na.omit(tibble::as_tibble(Y))
  colnames(Y) <- observable_names
  Ymat <- as.matrix(Y)
  
  filtered <- dlm::dlmFilter(Ymat, dlm_mod)
  smoothed <- dlm::dlmSmooth(filtered)
  
  # Filtered states
  filtered_states <- tibble::as_tibble(filtered$m[2:nrow(filtered$m),1:(e+1)]) %>%
    dplyr::mutate(time = df$time[2:(nrow(df)-1)]) %>%
    dplyr::select(time, !!!setNames(seq_along(state_names), state_names) )

  # Smoothed states
  smoothed_states <- tibble::as_tibble(smoothed$s[2:nrow(smoothed$s),1:(e+1)]) %>%
    dplyr::mutate(time = df$time[2:(nrow(df)-1)]) %>%
    dplyr::select(time, !!!setNames(seq_along(state_names), state_names) )
  
  # Observations
  observations <- Y  %>%
    dplyr::mutate(time = df$time[2:(nrow(df)-1)]) %>%
    # select time and up to release_e
    dplyr::select(time, dplyr::everything()) 
  
  if (h > 0) {
    frequency <- unique((round(as.numeric(diff(df$time))/30)))
    if(length(frequency) > 1) {
      rlang::abort("The time series seems not to be regular, please provide a regular time series!")
    }
    
    forecast_dates <- seq.Date(df$time[nrow(df)], by = paste0(frequency, " months"), length.out=(h+1))[2:(h+1)]
    
    forecast <- dlm::dlmForecast(filtered, nAhead = h)
    
    # Forecasted states
    forecast_states <- tibble::as_tibble(forecast$a) %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(time, !!!setNames(seq_along(state_names), state_names) )
    
    # forecast observations
    forecast_observation <- tibble::as_tibble(forecast$f) %>%
      dplyr::mutate(time = forecast_dates) %>%
      dplyr::select(time, !!!setNames(seq_along(observable_names), observable_names) )
  } else {
    forecast_states <- NULL
    forecast_observation <- NULL
  }
  
  params <- kk_mat_hat$params 
  
  # Remove the parameters from the model
  kk_mat_hat$params  <- NULL
  dlm_mod$m0 <- NULL
  dlm_mod$C0 <- NULL

  return(list(
    forecast_states = forecast_states,
    filtered_states = filtered_states,
    observations = observations,
    forecast_observation = forecast_observation,
    smoothed_states = smoothed_states,
    kk_model_mat = kk_mat_hat,
    params = params,
    ss_model_mat = dlm_mod,
    auxiliary = list(
      fit = fit,
      filtered = filtered, 
      smoothed = smoothed
    )))
}


#' Create matrices to estimate Kishor-Koenig model 
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return MLE of state space model - Kalman filter and smoother
#' @export
#'
kk_matrices <- function(e, model, params = NULL, type = "numeric") {
  # Start param count
  ii <- 1
  
  # Check input e
  if (e == 0) {
    rlang::abort("The initial release is already efficient, 'e' is equal to 0!")
  }
  
  # Check model input
  if (!model %in% c("Kishor-Koenig", "KK", "Howrey", "Classical")) {
    rlang::abort("'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!")
  }
  
  # Check type input
  if (!type %in% c("numeric", "character")) {
    rlang::abort("'type' must be one of 'numeric' or 'character'!")
  }
  
  # Check params input
  if (!is.null(params) && type == "character") {
    rlang::warn("If argument 'type' is 'character', argument 'params' is ignored!")
  }
  
  # Check params input
  n_param_mat <- dplyr::if_else(
    model %in% c("Kishor-Koenig", "KK"),  
    1 + e + e^2, 
    dplyr::if_else(
      model == "Howrey", 
      1 + e^2, 
      1 # Classical
    ))
  
  n_param_cov <- e + 1
  
  n_param <- n_param_mat + n_param_cov
  
  if (!is.null(params) & length(params) != n_param) {
    rlang::abort(paste0("'params' must have length ", n_param, ", not ", length(params)))
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
  FF[1:(e), 2:(e+1)] <- diag(e)
  FF[e+1, e+1] <- ifelse(type == "numeric", params[ii], "F0")
  names(params) <- c("F0")
  ii <- ii + 1
  
  # Define G matrix
  GG <- diag(e + 1)
  
  if (model %in% c("Kishor-Koenig", "KK")) {
    # e * e+1 params for G
    for (i in 1:e) {
      for (j in 1:(e+1)) {
        GG[i+1, j] <- ifelse(type == "numeric", params[ii], paste0("G", e-i, e-j+1))
        names(params)[ii] <- c(paste0("G", e-i, e-j+1))
        ii <- ii + 1
      }
    }
  } else if (model == "Howrey") {
    # e * e params for G
    for (i in 1:e) {
      for (j in 1:e) {
        GG[i+1, j] <- ifelse(type == "numeric", params[ii], paste0("G", e-i, e-j+1))
        names(params)[ii] <- c(paste0("G", e-i, e-j+1))
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
  R[e + 1, e + 1] <- ifelse(type == "numeric", exp(params[ii]), "v0")
  names(params)[ii] <- c(paste0("v0"))
  ii <- ii + 1
  
  # Observation noise covariance
  H <- array(0,c(e + 1, e + 1))
  for (jj in 2:(e+1)) {
    H[jj, jj] <-  ifelse(type == "numeric", exp(params[ii]), paste0("eps", e+1-jj)) 
    names(params)[ii] <- c(paste0("eps", e+1-jj))
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


#' Cast Generalized Kishor-Koenig matrices into state space form 
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return MLE of state space model - Kalman filter and smoother
#' @export
#'
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
  Tmat <- rbind(cbind(
    FF, array(0, c(e+1, e+1))), 
    array(0, c(e+1, e+1)), (II - GG) %*% FF
  )
  
  # Covariance matrices
  # V <- array(0,c(e+1, e+1))
  # 
  # # State noise covariance (x_t and y_t-1)
  # W <- array(0,c(2*(e+1), 2*(e+1)))
  # 
  # for (jj in 2:(e+1)) {
  #   V[jj,jj] <- H[jj,jj]  # e param for V0
  #   W[jj+e+1,jj+e+1] <- H[jj,jj]  # same e param for W0
  # }
  # 
  # V[1,1] <- epsilon
  # 
  # W[1:e,1:e] <- diag(e)*epsilon
  # W[e+1,e+1] <- R[e+1,e+1]  # 1 param for W0
  # W[e+2,e+2] <- epsilon
  
  V <- array(0,c(e+1, e+1))
  W <- array(0,c(2*(e+1), 2*(e+1)))
  v_t_2 <- R[1:(e+1),1:(e+1)]
  W[1:(e+1), 1:(e+1)] <- v_t_2
  W[(1:(e+1)), ((e+2):(2*(e+1)))] <- -v_t_2*t((II-GG))
  W[((e+2):(2*(e+1))), 1:(e+1)] <- -(II-GG)*v_t_2
  W[((e+2):(2*(e+1))), ((e+2):(2*(e+1)))] <- H[1:(e+1),1:(e+1)]
  
  
  
  return(list(Z = Z, Tmat = Tmat, V = V, W = W))
}


#' Cast state space matrices back into Generalized Kishor-Koenig form
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return MLE of state space model - Kalman filter and smoother
#' @export
#'
ss_to_kk <- function(Z, Tmat, V, W) {
  e <- nrow(Z) - 1
  FF <- Tmat[1:(e+1), 1:(e+1)]
  GG <- Z[, 1:(e+1)]
  Q <- V[1:(e+1), 1:(e+1)]
  H <- W[(e+2):(2*(e+1)), (e+2):(2*(e+1))]
  return(list(FF = FF, GG = GG, Q = Q, H = H))
}

