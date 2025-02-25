df <- dplyr::tibble(
  pub_date = as.Date(c(
    rep("2022-10-25",1), 
    rep("2023-01-25",2), 
    rep("2023-04-25",3), 
    rep("2023-07-25",4)
  )),
  time = as.Date(c(
    "2022-07-01",
    "2022-07-01", "2022-10-01",
    "2022-07-01", "2022-10-01", "2023-01-01",
    "2022-07-01", "2022-10-01", "2023-01-01", "2023-04-01"
    )),
  values = c(
    110,
    111.0, 106.2,
    111.5, 106.8, 108.3,
    111.3, 107.1, 108.6, 113.2
  )
)

# Revision if interval = 1
rev_interval <- -as.numeric(na.omit(c(
  NA, 1.0, NA, 0.5, 0.6, NA, -0.2, 0.3, 0.3, NA
)))

# Revision compared to latest vintage
rev_last <- -c(
    -1.3, -0.3, -0.9, 0.2, -0.3, -0.3, 0, 0, 0, 0
  ) 

# Revision compared to first vintage
rev_first <- -c(
    0, 1, 0, 1.5, 0.6, 0, 1.3, 0.9, 0.3, 0
  )

# Get first release
first_rel <- c(
  110, 106.2, 108.3, 113.2
)

# Get secons release
second_rel <- c(
  111, 106.8, 108.6
)

fixed_rel <- c(
  111.5, 106.8, 108.3
)

rel_by_date <- c(
  106.2, 106.8, 107.1
)


test_that("interval works", {
  data <- get_revisions(df, interval=1)
  expect_equal(round(data$value,2), round(rev_interval,2))
})

test_that("latest works", {
  data <- get_revisions(df, nth_release="latest")
  expect_equal(round(data$value,2), round(rev_last,2))
})

test_that("first works", {
  data <- get_revisions(df, nth_release=0)
  expect_equal(round(data$value,2), round(rev_first,2))
})


test_that("first rel works", {
  data <- get_nth_release(df, n = 0)
  expect_equal(round(data$value,2), round(first_rel,2))
})

test_that("second rel works", {
  data <- get_nth_release(df, n = 1)
  expect_equal(round(data$value,2), round(second_rel,2))
})

test_that("fixed rel works", {
  data <- get_fixed_release(df, years = 0, month = "April")
  expect_equal(round(data$value,2), round(fixed_rel,2))
})

test_that("rel by date works", {
  data <- get_releases_by_date(df, date = as.Date("2022-10-01"))
  expect_equal(round(data$value,2), round(rel_by_date,2))
})
