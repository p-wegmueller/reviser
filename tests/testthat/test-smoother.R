#' @srrstats {G5.4} statistical algorithms produce expected results
#' @srrstats {G5.4a} correctness of the implementation, tested against
#' simple, trivial cases

# Create example data
df <- get_nth_release(
  tsbox::ts_span(
    tsbox::ts_pc(
      dplyr::filter(reviser::gdp, id == "US")
    ),
    start = "1980-01-01"
  ),
  n = 0:10
)

df_final <- get_nth_release(
  tsbox::ts_span(
    tsbox::ts_pc(
      dplyr::filter(reviser::gdp, id == "US")
    ),
    start = "1980-01-01"
  ),
  n = 11
)

df_kk <- get_first_efficient_release(df, df_final)$data


# Test suite for kk_nowcast
test_that("kk_nowcast returns a list of class kk_model", {
  result <- kk_nowcast(df_kk, e = 1, h = 0, model = "Kishor-Koenig")
  expect_s3_class(result, "kk_model")
})

test_that("kk_nowcast returns expected components", {
  result <- kk_nowcast(df_kk, e = 1, h = 1, model = "Kishor-Koenig")
  expect_named(
    result,
    c(
      "filtered_z",
      "filtered_y",
      "smoothed_z",
      "smoothed_y",
      "forecast_z",
      "forecast_y",
      "kk_model_mat",
      "ss_model_mat",
      "params",
      "fit",
      "e",
      "data"
    )
  )
})

test_that("kk_nowcast handles different models", {
  expect_silent(kk_nowcast(df_kk, e = 1, h = 0, model = "Kishor-Koenig"))
  expect_silent(kk_nowcast(df_kk, e = 1, h = 0, model = "Howrey"))
  expect_silent(kk_nowcast(df_kk, e = 1, h = 0, model = "Classical"))
})

test_that("kk_nowcast handles forecast horizon h > 0", {
  result_forecast <- kk_nowcast(df_kk, e = 1, h = 2, model = "Kishor-Koenig")
  expect_s3_class(result_forecast$forecast_z, "tbl_df")
  expect_s3_class(result_forecast$forecast_y, "tbl_df")
  expect_equal(nrow(result_forecast$forecast_z), 2)
  expect_equal(nrow(result_forecast$forecast_y), 2)
})

test_that("kk_nowcast throws error for invalid e and h", {
  expect_error(
    kk_nowcast(df_kk, e = 0),
    "The initial release is already efficient, 'e' is equal to 0!"
  )
  expect_error(
    kk_nowcast(df_kk, e = 1, h = -1),
    "The horizon 'h' must be at least 0!"
  )
})

test_that("kk_nowcast throws error for invalid model", {
  expect_error(
    kk_nowcast(df_kk, e = 1, model = "InvalidModel"),
    "'model' must be one of 'Kishor-Koenig', 'KK', 'Howrey', or 'Classical'!"
  )
})

test_that("kk_nowcast handles solver_options", {
  options <- list(trace = 0, maxiter = 500)
  expect_silent(kk_nowcast(df_kk, e = 1, solver_options = options))
})

test_that("kk_nowcast throws error for invalid solver_options", {
  expect_error(
    kk_nowcast(df_kk, e = 1, solver_options = list(invalid_option = 1)),
    "Invalid solver options provided."
  )
  expect_error(
    kk_nowcast(df_kk, e = 1, solver_options = "not_a_list"),
    "'solver_options' must be a list!"
  )
})

test_that("kk_nowcast handles startvals", {
  n_params <- ifelse(
    "Kishor-Koenig" %in% c("Kishor-Koenig", "KK"),
    1 + 1 + 1^2,
    ifelse("Howrey" == "Howrey", 1 + 1^2, 1)
  ) +
    (1 + 1)
  start_values <- setNames(rep(0.1, n_params), paste0("p", 1:n_params))
  expect_silent(kkmod <- kk_nowcast(df_kk, e = 1))
  start_values_vec <- kkmod$params
  expect_silent(kk_nowcast(
    df_kk,
    e = 1,
    solver_options = list(startvals = start_values_vec)
  ))
})

test_that("kk_nowcast throws error for invalid startvals", {
  n_params <- ifelse(
    "Kishor-Koenig" %in% c("Kishor-Koenig", "KK"),
    1 + 1 + 1^2,
    ifelse("Howrey" == "Howrey", 1 + 1^2, 1)
  ) +
    (1 + 1)
  expect_error(
    kk_nowcast(
      df_kk,
      e = 1,
      solver_options = list(startvals = rep(0.1, n_params - 1))
    ),
    paste0("'startvals' must be a named, numeric vector!")
  )
  expect_error(
    kk_nowcast(df_kk, e = 1, solver_options = list(startvals = 1:n_params)),
    "'startvals' must be a named, numeric vector!"
  )
  start_values_unnamed <- rep(0.1, n_params)
  expect_error(
    kk_nowcast(
      df_kk,
      e = 1,
      solver_options = list(startvals = start_values_unnamed)
    ),
    "'startvals' must be a named, numeric vector!"
  )
})

test_that("kk_nowcast handles irregular time series for forecasting", {
  df_irregular <- df_kk
  df_irregular$time[3] <- df_irregular$time[3] + 10 # Introduce irregularity
  expect_error(
    kk_nowcast(df_irregular, e = 1, h = 2),
    "The time series seems not to be regular"
  )
})

# Test suite for summary.kk_model
test_that("summary.kk_model is a generic function", {
  expect_true(is.function(summary))
})


test_that("summary.kk_model returns a list of metrics", {
  kk_result <- kk_nowcast(df_kk, e = 1)
  summary_result <- summary(kk_result)
  expect_type(summary_result, "list")
  expect_named(
    summary_result,
    c(
      "final_release_metrics",
      "true_efficient_release_metrics",
      "final_release_relative_metrics",
      "true_efficient_release_relative_metrics"
    )
  )
  expect_s3_class(summary_result$final_release_metrics, "data.frame")
  expect_s3_class(summary_result$true_efficient_release_metrics, "data.frame")
  expect_s3_class(summary_result$final_release_relative_metrics, "data.frame")
  expect_s3_class(
    summary_result$true_efficient_release_relative_metrics,
    "data.frame"
  )
})

test_that("summary.kk_model calculates MSE, RMSE, and MAE", {
  kk_result <- kk_nowcast(df_kk, e = 1)
  summary_result <- summary(kk_result)
  expect_named(summary_result$final_release_metrics, c("MSE", "RMSE", "MAE"))
  expect_named(
    summary_result$true_efficient_release_metrics,
    c("MSE", "RMSE", "MAE")
  )
  expect_named(
    summary_result$final_release_relative_metrics,
    c("MSE", "RMSE", "MAE")
  )
  expect_named(
    summary_result$true_efficient_release_relative_metrics,
    c("MSE", "RMSE", "MAE")
  )
})

test_that("summary.kk_model prints formatted output", {
  kk_result <- kk_nowcast(df_kk, e = 1)
  expect_output(summary(kk_result))
})
