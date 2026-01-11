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

df_jvn <- get_first_efficient_release(df, df_final)$data


# Test suite for kk_nowcast
test_that("jvn_nowcast returns a list of class kk_model", {
  result <- jvn_nowcast(df_jvn, e = 2, h = 0)
  expect_s3_class(result, "jvn_model")
})

test_that("jvn_nowcast returns expected components", {
  result <- jvn_nowcast(df_jvn, e = 2, h = 0)
  expect_named(
    result,
    c("states", "jvn_model_mat", "params", "fit", "loglik", "aic", 
      "bic", "convergence", "data")
  )
})
