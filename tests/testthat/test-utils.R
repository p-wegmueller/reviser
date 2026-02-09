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
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01")),
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
  expect_equal(nrow(result$A), 2)
  expect_equal(
    result$A[1, "2020-01-10"]$`2020-01-10`,
    1
  )
})

test_that("vintages_wide throws error on missing columns", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-02-01")),
    value = c(1.0, 1.0)
  )
  expect_error(
    vintages_wide(df),
    "One or more column names in the 'wide"
  )
})

test_that("vintages_wide warns and ignores extra columns", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    pub_date = as.Date(c("2020-01-10", "2020-01-10")),
    value = c(1.0, 1.0),
    extra = c("ignore me", "please")
  )
  expect_warning(
    result <- vintages_wide(df),
    "Ignoring columns"
  )
  expect_s3_class(result, "tbl_pubdate")
})

test_that("vintages_wide handles names_from = 'release'", {
  df <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01")),
    release = c(1, 2, 1, 2),
    value = c(100, 200, 150, 250)
  )
  result <- vintages_wide(df, names_from = "release")
  expect_s3_class(result, "tbl_release")
  expect_true("1" %in% names(result))
})

test_that("vintages_wide returns input if already wide", {
  wide_df <- dplyr::tibble(
    time = c(as.Date("2020-01-01"), as.Date("2021-01-01")),
    `2020-01-10` = 1.0,
    `2021-01-10` = 2.0
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


test_that("vintages_long works on wide data frame with pub_date", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    `2020-02-01` = c(1.1, 1.2),
    `2020-05-01` = c(1.4, NA)
  )
  result <- vintages_long(wide_df, names_to = "pub_date")
  expect_s3_class(result, "tbl_pubdate")
  expect_equal(nrow(result), 3)
  expect_equal(colnames(result), c("time", "pub_date", "value"))
  expect_true(all(result$pub_date %in% as.Date(c("2020-02-01", "2020-05-01"))))
})

test_that("vintages_long works on wide data frame with release", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    release_0 = c(1.1, 1.2),
    release_1 = c(1.4, NA)
  )
  result <- vintages_long(wide_df, names_to = "release")
  expect_s3_class(result, "tbl_release")
  expect_equal(nrow(result), 3)
  expect_equal(colnames(result), c("time", "release", "value"))
})

test_that("vintages_long retains NA values when keep_na = TRUE", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    `2020-02-01` = c(1.1, NA),
    `2020-05-01` = c(1.4, 1.5)
  )
  result <- vintages_long(wide_df, names_to = "pub_date", keep_na = TRUE)
  expect_equal(nrow(result), 4)
  expect_true(any(is.na(result$value)))
})

test_that("vintages_long handles input as list of wide data frames", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    `2020-02-01` = c(1.1, 1.2),
    `2020-05-01` = c(1.4, 1.5)
  )
  wide_list <- list(
    A = wide_df,
    B = wide_df
  )
  result <- vintages_long(wide_list, names_to = "pub_date")
  expect_s3_class(result, "tbl_pubdate")
  expect_true("id" %in% colnames(result))
  expect_equal(unique(result$id), c("A", "B"))
  expect_equal(nrow(result), 8)
})

test_that("vintages_long errors on invalid names_to", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01")),
    x = 1
  )
  expect_error(
    vintages_long(wide_df, names_to = "something_else"),
    "'names_to' must be one of 'pub_date' or 'release'"
  )
})

test_that("vintages_long errors on non-logical keep_na", {
  wide_df <- tibble::tibble(
    time = as.Date(c("2020-01-01")),
    x = 1
  )
  expect_error(
    vintages_long(wide_df, names_to = "pub_date", keep_na = "yes"),
    "'keep_na' argument must be logical"
  )
})

test_that("vintages_long returns early for already long input", {
  long_df <- tibble::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01")),
    pub_date = as.Date(c("2020-02-01", "2020-05-01")),
    value = c(1.1, 1.4)
  )
  result <- suppressWarnings(vintages_long(long_df, names_to = "pub_date"))
  expect_s3_class(result, "tbl_pubdate")
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("time", "pub_date", "value"))
})
