#' @import tidyverse
#' @import tsbox
#' @import dlm
#' @import sandwich
#' @import car
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ReviseR is loading, along with the packages: tidyverse, tsbox, dlm, car, sandwhich.")
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(tsbox))
  suppressPackageStartupMessages(library(dlm))
  suppressPackageStartupMessages(library(sandwich))
  suppressPackageStartupMessages(library(car))
}

#' get first release series ("realtime")
#'
#' @param df dataframe with revisions, longformat
#' @param col_rel_date name of column with release dates (date format)
#' @param col_val_date name of column with observation dates (date format)
#'
#' @return filtered df with every first release for obs col_val_date
#' @export
#'
get_realtime_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    filter({{ col_rel_date }} == min({{ col_rel_date }})) %>%
    ungroup()
  return(df_adj)
}

#' get revisions
#'
#' @param df 
#' @param col_rel_date 
#' @param col_val_date 
#' @param i 
#'
#' @return obtain revisions from data frame
#' @export 
#'
get_revision <- function(df, col_rel_date, col_val_date, i) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    arrange({{ col_rel_date }}) %>%
    slice(i) %>%
    ungroup()
  return(df_adj)
}

#' get final revision series
#'
#' @param df dataframe with revisions, longformat
#' @param col_rel_date name of column with release dates (date format)
#' @param col_val_date name of column with observation dates (date format)
#' @param p_month number of months to be added to release until "final" revision 
#' 
#' @return filtered df only with first release + p_month
#' @export
#'
get_final_revision_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    # collect first release date
    mutate(REL_Date_First = min({{ col_rel_date }})) %>% 
    # select final release for quarterly data
    filter( year({{ col_rel_date }}) == year(REL_Date_First) + 4 & 
              (month({{ col_rel_date }}) == 8 | month({{ col_rel_date }}) == 9)) %>%
    ungroup()
  
  df_adj <- df_adj %>%
    group_by({{ col_val_date }}) %>%
    arrange({{ col_rel_date }}) %>%
    slice(1)
  
  return(df_adj)
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
get_latest_vintage_series <- function(df, col_rel_date, col_val_date) {
  df_adj <- df %>%
    group_by({{ col_val_date }}) %>%
    filter({{ col_rel_date }} == max({{ col_rel_date }})) %>%
    ungroup()
  return(df_adj)
}

#' calculate summary statistics
#' 
#' requires input to have initial and revision col
#' 
#' @param df_i 
#'
#' @return dataframe with 1 row and summary stats
#' @export
#'
create_summary_stats <- function(df_i) {
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


#' Mincer-Zarnowitz regression
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return Statistics from MZ-regression
#' @export
#'
MincerZarnowitz <- function(df_full, df_final, significance) {
  df_final <- df_final %>%
    rename(value="final") %>% 
    select(time, value) %>%
    ts_long() %>%
    mutate(id = 'final')
  
  e = 0 
  p_value = 0
  all_releases <- data.frame(id = character(0), time = numeric(0), value = numeric(0))
  
  while (p_value <= significance) {
    
    e <- e+1
    
    revision <- get_revision(df_full, pub_date, time, e) %>%
      rename(value="diff") %>% 
      select(time, value) %>%
      filter(time >= ymd("1995-04-01") & time <= ymd("2017-12-31")) %>%
      ts_long() %>%
      mutate(id = paste0("release_", e))
    
    all_releases <- ts_bind(all_releases, revision)
    
    df_all <- ts_bind(all_releases, df_final) %>%
      ts_wide()
    
    model_formula <- as.formula(paste("final ~ release_", e, sep = ""))
    model <- lm(model_formula, data = df_all)
    hac_se <- vcovHAC(model)
    print(summary(model))
    test <- linearHypothesis(model, c("(Intercept) = 0", paste0("release_", e, " = 1")), vcov = hac_se)
    print(test)
    p_value <- test[2, 'Pr(>F)']
  }
  df_output <- list('e' = e-1, 'coef' = model$coefficients, 'p_value' = p_value, 'releases' = df_all)
  return(df_output)
}


#' Univariate simple Kalman filter
#'
#' @param df_full 
#' @param df_final 
#' @param significance 
#'
#' @return MLE of state space model - Kalman filter
#' @export
#'
# 
KalmanNowcastPrecision <- function(x, y, m0, C0, initial_params) {
  # TODO: mention imprecision in paper (how mo and c0 were estimtaed)
  fn <- function(param) {
    dlm(FF = 1, V = exp(param[1]), GG = param[2], W= exp(param[3]), m0 = m0, C0 = C0)
  }
  # Initial parameter values (what are reasonable values?)
  err <- numeric(length(y))
  for (i in 1:length(y)) {
    y_i <- y[1:i]
    fit <- dlmMLE(y_i, initial_params, build = fn)
    # Check convergence and log-likelihood
    print(paste("Convergence: ", fit$convergence))
    print(paste("Log-likelihood: ", fit$value))
    # Update state estimates
    x_hat <- dlmFilter(y_i, fn(fit$par))$m
    x_i_hat <- x_hat[length(x_hat)]
    # Update error calculation
    err[i] <- (x[i] - x_i_hat)^2
  }
  return(err)
}