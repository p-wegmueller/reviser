source("test-data.R")

test_that("interval works", {
  data <- get_revisions(df, interval = 1)
  expect_equal(
    as.numeric(na.omit(round(data$value, 2))), round(rev_interval, 2)
    )
})

test_that("latest works", {
  data <- get_revisions(df, nth_release = "latest")
  expect_equal(as.numeric(na.omit(round(data$value, 2))), round(rev_last, 2))
})

test_that("first works", {
  data <- get_revisions(df, nth_release = 0)
  expect_equal(as.numeric(na.omit(round(data$value, 2))), round(rev_first, 2))
})


test_that("first rel works", {
  data <- get_nth_release(df, n = 0)
  expect_equal(round(data$value, 2), round(first_rel, 2))
})

test_that("second rel works", {
  data <- get_nth_release(df, n = 1)
  expect_equal(round(data$value, 2), round(second_rel, 2))
})

test_that("fixed rel works", {
  data <- get_fixed_release(df, years = 0, month = "April")
  expect_equal(round(data$value, 2), round(fixed_rel, 2))
})

test_that("rel by date works", {
  data <- get_releases_by_date(df, date = as.Date("2022-10-01"))
  expect_equal(round(data$value, 2), round(rel_by_date, 2))
})


# Create example data
df_initial <- data.frame(
  time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 40),
  value = rnorm(40),
  release = rep(paste0("release_", 1:4), each = 10)
)

df_final <- data.frame(
  time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 40),
  value = rnorm(40, mean = 0.1),
  release = "final"
)

df_initial_grouped <- data.frame(
  time = rep(seq(as.Date("2020-01-01"), by = "quarter", length.out = 10), 2),
  value = rnorm(20),
  id = rep(c("US", "CA"), each = 10),
  release = rep(paste0("release_", 1:2), each = 10)
)

df_final_grouped <- data.frame(
  time = rep(seq(as.Date("2020-01-01"), by = "quarter", length.out = 10), 2),
  value = rnorm(20, mean = 0.1),
  id = rep(c("US", "CA"), each = 10),
  release = "final"
)

# Test suite for get_revision_analysis
test_that("get_revision_analysis returns a data frame", {
  results <- get_revision_analysis(df_initial, df_final)
  expect_s3_class(results, "data.frame")
})

test_that("get_revision_analysis returns object of class revision_summary", {
  results <- get_revision_analysis(df_initial, df_final)
  expect_s3_class(results, "revision_summary")
})

test_that("get_revision_analysis handles degree parameter correctly", {
  results_deg1 <- get_revision_analysis(df_initial, df_final, degree = 1)
  expect_true(all(c("N", "Bias (mean)", "MAR", "Std. Dev.", "Noise/Signal") %in% names(results_deg1)))
  expect_false(any(c("Correlation", "Theil's U1") %in% names(results_deg1)))
  
  results_deg2 <- get_revision_analysis(df_initial, df_final, degree = 2)
  expect_true(all(c("Correlation", "Autocorrelation (1st)") %in% names(results_deg2)))
  expect_false(any(c("News test Intercept", "Theil's U1") %in% names(results_deg2)))
  
  results_deg3 <- get_revision_analysis(df_initial, df_final, degree = 3)
  expect_true(all(c("News test Intercept", "Noise test Intercept") %in% names(results_deg3)))
  expect_false(any(c("Theil's U1", "Fraction of correct sign") %in% names(results_deg3)))
  
  results_deg4 <- get_revision_analysis(df_initial, df_final, degree = 4)
  expect_true(all(c("Theil's U1", "Fraction of correct sign", "Seasonality (Friedman p-value)") %in% names(results_deg4)))
  expect_false(any(c("News test Intercept", "Correlation") %in% names(results_deg4)))
  
  results_deg5 <- get_revision_analysis(df_initial, df_final, degree = 5)
  expected_cols_deg5 <- c("N", "Frequency", "Bias (mean)", "Bias (p-value)", "Bias (robust p-value)", "Minimum", "Maximum", "10Q", "Median", "90Q", "MAR", "Std. Dev.", "Noise/Signal", "Correlation", "Correlation (p-value)", "Autocorrelation (1st)", "Autocorrelation (1st p-value)", "Autocorrelation up to 1yr (Ljung-Box p-value)", "Theil's U1", "Theil's U2", "Seasonality (Friedman p-value)", "News test Intercept", "News test Intercept (std.err)", "News test Intercept (p-value)", "News test Coefficient", "News test Coefficient (std.err)", "News test Coefficient (p-value)", "News joint test (p-value)", "Noise test Intercept", "Noise test Intercept (std.err)", "Noise test Intercept (p-value)", "Noise test Coefficient", "Noise test Coefficient (std.err)", "Noise test Coefficient (p-value)", "Noise joint test (p-value)", "Fraction of correct sign", "Fraction of correct growth rate change")
  expect_true(all(expected_cols_deg5 %in% names(results_deg5)))
})

test_that("get_revision_analysis throws error for invalid degree", {
  expect_error(get_revision_analysis(df_initial, df_final, degree = 0), "The 'degree' must be an integer between 1 and 5.")
  expect_error(get_revision_analysis(df_initial, df_final, degree = 6), "The 'degree' must be an integer between 1 and 5.")
  expect_error(get_revision_analysis(df_initial, df_final, degree = 2.5), "The 'degree' must be an integer between 1 and 5.")
})

test_that("get_revision_analysis handles grouping_var correctly", {
  results_grouped <- get_revision_analysis(df_initial_grouped, df_final_grouped, grouping_var = "id")
  expect_true("id" %in% names(results_grouped))
  expect_equal(nrow(results_grouped), length(unique(df_initial_grouped$id)))
  
  results_grouped_release <- get_revision_analysis(df_initial_grouped, df_final_grouped, grouping_var = "release")
  expect_true("release" %in% names(results_grouped_release))
  expect_equal(nrow(results_grouped_release), length(unique(df_initial_grouped$release)))
})

test_that("get_revision_analysis throws error if grouping_var not in data frames", {
  expect_error(get_revision_analysis(df_initial, df_final, grouping_var = "non_existent_var"),
               "The grouping variable must be present in 'df' and 'final_release'.")
})

test_that("get_revision_analysis handles missing grouping_var and infers from column names", {
  df_initial_pubdate <- df_initial 
  df_final_pubdate <- df_final
  
  results_pubdate <- get_revision_analysis(df_initial_pubdate, df_final_pubdate)
  expect_false("pub_date" %in% names(results_pubdate))
  
  results_release_inferred <- get_revision_analysis(df_initial, df_final)
  expect_true("release" %in% names(results_release_inferred))
})

test_that("get_revision_analysis handles data with only pub_date", {
  df_initial_pubdate_only <- df_initial %>% select(-release) %>%  mutate(pub_date = as.Date("2029-10-01"))
  df_final_pubdate_only <- df_final #%>% rename(time = as_date(time))
  results_pubdate_only <- get_revision_analysis(df_initial_pubdate_only, df_final_pubdate_only)
  expect_true("pub_date" %in% names(results_pubdate_only))
})

test_that("get_revision_analysis throws error if neither release nor pub_date is present and no grouping_var is given", {
  df_no_grouping <- df_initial %>% select(-release)
  df_final_no_grouping <- df_final
  expect_error(get_revision_analysis(df_no_grouping, df_final_no_grouping),
               "One or more column names in the 'wide format' are not")
})

test_that("get_revision_analysis requires 'id' to be present in both or neither data frames", {
  df_initial_id <- df_initial %>% mutate(id = "US")
  df_final_no_id <- df_final %>% select(-any_of("id"))
  expect_error(get_revision_analysis(df_initial_id, df_final_no_id),
               "Both or none of 'df' and 'final_release' must have an 'id' column.")
  
  df_initial_no_id <- df_initial %>% select(-any_of("id"))
  df_final_id <- df_final %>% mutate(id = "US")
  expect_error(get_revision_analysis(df_initial_no_id, df_final_id),
               "Both or none of 'df' and 'final_release' must have an 'id' column.")
})

test_that("get_revision_analysis requires identical 'id' values in both data frames", {
  df_initial_diff_id <- df_initial %>% mutate(id = "US")
  df_final_diff_id <- df_final_grouped
  expect_error(get_revision_analysis(df_initial_diff_id, df_final_diff_id),
               "The same 'id' must be present in 'df' and 'final_release'.")
})

test_that("get_revision_analysis handles cases with no 'id' column", {
  df_initial_no_id <- df_initial %>% select(-any_of("id"))
  df_final_no_id <- df_final %>% select(-any_of("id"))
  results_no_id <- get_revision_analysis(df_initial_no_id, df_final_no_id)
  expect_true(!any(grepl("id", names(results_no_id))))
})

test_that("get_revision_analysis requires at least 8 observations per group", {
  df_small_group <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 5),
    value = rnorm(5),
    release = "release_1",
    id = "A"
  )
  df_final_small_group <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 5),
    value = rnorm(5, mean = 0.1),
    release = "final",
    id = "A"
  )
  expect_error(get_revision_analysis(df_small_group, df_final_small_group, grouping_var = "id"),
               "Need at least 8 observations per group to compute the statistics.")
  
  df_mixed_group_size <- bind_rows(
    df_initial_grouped %>% filter(id == "US"),
    df_small_group %>% mutate(id = "C")
  )
  df_final_mixed_group_size <- bind_rows(
    df_final_grouped %>% filter(id == "US"),
    df_final_small_group %>% mutate(id = "C")
  )
  expect_error(get_revision_analysis(df_mixed_group_size, df_final_mixed_group_size, grouping_var = "id"),
               "Need at least 8 observations per group to compute the statistics.")
})


test_that("friedman_test returns a list with p_value", {
  series <- rnorm(48)
  result <- friedman_test(series, frequency = 12)
  expect_type(result, "list")
  expect_named(result, "p_value")
  expect_type(result$p_value, "double")
})

test_that("friedman_test handles different frequencies", {
  series_monthly <- rnorm(60)
  result_monthly <- friedman_test(series_monthly, frequency = 12)
  expect_type(result_monthly$p_value, "double")
  
  series_quarterly <- rnorm(40)
  result_quarterly <- friedman_test(series_quarterly, frequency = 4)
  expect_type(result_quarterly$p_value, "double")
})
