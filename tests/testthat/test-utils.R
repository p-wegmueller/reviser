source("test-data.R")

test_that("vintages_wide works without id column", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01", "2020-02-01")),
    pub_date = as.Date(c(
      "2020-01-10",
      "2020-01-20",
      "2020-01-10",
      "2020-01-20"
    )),
    value = c(1.1, 1.2, 1.3, 1.4)
  )
  result <- vintages_wide(df)
  expect_s3_class(result, "tbl_pubdate")
  expect_true("2020-01-10" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("vintages_wide works with id column", {
  df <- dplyr::tibble(
    id = c("A", "A", "B", "B"),
    time = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
    pub_date = as.Date(c(
      "2020-01-10",
      "2020-01-20",
      "2020-01-10",
      "2020-01-20"
    )),
    value = c(1, 2, 3, 4)
  )
  result <- vintages_wide(df)
  expect_type(result, "list")
  expect_named(result, c("A", "B"))
  expect_s3_class(result$A, "tbl_pubdate")
  expect_equal(nrow(result$A), 1)
  expect_equal(
    result$A[1, "2020-01-10"]$`2020-01-10`,
    1
  )
})

test_that("vintages_wide throws error on missing columns", {
  df <- dplyr::tibble(
    time = as.Date("2020-01-01"),
    value = 1.0
  )
  expect_error(
    vintages_wide(df),
    "One or more column names in the 'wide format' are not"
  )
})

test_that("vintages_wide warns and ignores extra columns", {
  df <- dplyr::tibble(
    time = as.Date("2020-01-01"),
    pub_date = as.Date("2020-01-10"),
    value = 1.0,
    extra = "ignore me"
  )
  expect_warning(
    result <- vintages_wide(df),
    "Ignoring columns"
  )
  expect_s3_class(result, "tbl_pubdate")
})

test_that("vintages_wide handles names_from = 'release'", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-01-01")),
    release = c(1, 2),
    value = c(100, 200)
  )
  result <- vintages_wide(df, names_from = "release")
  expect_s3_class(result, "tbl_release")
  expect_true("1" %in% names(result))
})

test_that("vintages_wide returns input if already wide", {
  wide_df <- dplyr::tibble(
    time = as.Date("2020-01-01"),
    `2020-01-10` = 1.0
  )
  class(wide_df) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
  expect_warning(
    result <- vintages_wide(wide_df),
    "already in wide format"
  )
  expect_identical(result, wide_df)
})

test_that("vintages_wide handles multiple rows per pub_date", {
  df <- dplyr::tibble(
    time = rep(as.Date(c("2020-01-01", "2020-02-01")), each = 2),
    pub_date = as.Date(c(
      "2020-01-10",
      "2020-01-10",
      "2020-02-10",
      "2020-02-10"
    )),
    value = c(1, 2, 3, 4)
  )
  expect_warning(
    result <- vintages_wide(df),
    "are not uniquely identified"
  )
  expect_equal(ncol(result), 3) # time + one pub_date
  expect_s3_class(result, "tbl_pubdate")
})

test_that("vintages_wide works with missing values", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-02-01")),
    pub_date = as.Date(c("2020-01-10", "2020-01-10")),
    value = c(NA, 1.5)
  )
  result <- vintages_wide(df)
  expect_true(any(is.na(result)))
})
