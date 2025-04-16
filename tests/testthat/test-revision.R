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
