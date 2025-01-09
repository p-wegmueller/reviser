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

