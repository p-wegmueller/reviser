df <- dplyr::tibble(
  pub_date = as.Date(c(
    rep("2022-10-25", 1),
    rep("2023-01-25", 2),
    rep("2023-04-25", 3),
    rep("2023-07-25", 4)
  )),
  time = as.Date(c(
    "2022-07-01",
    "2022-07-01",
    "2022-10-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01"
  )),
  values = c(
    110,
    111.0,
    106.2,
    111.5,
    106.8,
    108.3,
    111.3,
    107.1,
    108.6,
    113.2
  )
)

# Revision if interval = 1
rev_interval <- -as.numeric(na.omit(c(
  NA,
  1.0,
  NA,
  0.5,
  0.6,
  NA,
  -0.2,
  0.3,
  0.3,
  NA
)))

# Revision compared to latest vintage
rev_last <- -c(
  -1.3,
  -0.3,
  -0.9,
  0.2,
  -0.3,
  -0.3,
  0,
  0,
  0,
  0
)

# Revision compared to first vintage
rev_first <- -c(
  0,
  1,
  0,
  1.5,
  0.6,
  0,
  1.3,
  0.9,
  0.3,
  0
)

# Get first release
first_rel <- c(
  110,
  106.2,
  108.3,
  113.2
)

# Get secons release
second_rel <- c(
  111,
  106.8,
  108.6
)

fixed_rel <- c(
  111.5,
  106.8,
  108.3
)

rel_by_date <- c(
  106.2,
  106.8,
  107.1
)


data_quarterly_update <- dplyr::tibble(
  pub_date = as.Date(c(
    rep("2022-10-25", 5),
    rep("2023-01-25", 6),
    rep("2023-04-25", 7),
    rep("2023-07-25", 8),
    rep("2023-10-25", 9),
    rep("2024-01-25", 10),
    rep("2024-04-25", 11),
    rep("2024-07-25", 12)
  )),
  time = as.Date(c(
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2024-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2024-04-01",
    "2024-07-01"
  )),
  values = c(
    100,
    105,
    102,
    107,
    110,
    100.5,
    105.3,
    102.1,
    107.4,
    111.0,
    106.2,
    100.2,
    105.1,
    102.0,
    107.6,
    111.5,
    106.8,
    108.3,
    100.0,
    105.0,
    102.2,
    107.9,
    111.3,
    107.1,
    108.6,
    113.2,
    99.8,
    104.8,
    102.4,
    108.1,
    111.8,
    107.4,
    109.0,
    113.5,
    117.4,
    99.5,
    104.5,
    102.6,
    108.4,
    111.6,
    107.7,
    109.3,
    113.8,
    117.8,
    120.1,
    99.2,
    104.3,
    102.8,
    108.6,
    112.0,
    108.0,
    109.6,
    114.1,
    118.2,
    120.5,
    123.0,
    99.0,
    104.0,
    103.0,
    108.9,
    112.3,
    108.3,
    110.0,
    114.4,
    118.5,
    120.8,
    123.4,
    128.1
  )
)

data_quarterly_update_irregular <- dplyr::tibble(
  pub_date = as.Date(c(
    rep("2022-10-25", 5),
    rep("2023-01-25", 6),
    rep("2023-04-25", 6),
    rep("2023-07-25", 8),
    rep("2023-10-25", 9),
    rep("2024-01-25", 10),
    rep("2024-04-25", 11),
    rep("2024-07-25", 12)
  )),
  time = as.Date(c(
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2024-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2023-10-01",
    "2024-01-01",
    "2024-04-01",
    "2024-07-01"
  )),
  values = c(
    100,
    105,
    102,
    107,
    110,
    100.5,
    105.3,
    102.1,
    107.4,
    111.0,
    106.2,
    100.2,
    105.1,
    102.0,
    107.6,
    111.5,
    106.8,
    100.0,
    105.0,
    102.2,
    107.9,
    111.3,
    107.1,
    108.6,
    113.2,
    99.8,
    104.8,
    102.4,
    108.1,
    111.8,
    107.4,
    109.0,
    113.5,
    117.4,
    99.5,
    104.5,
    102.6,
    108.4,
    111.6,
    107.7,
    109.3,
    113.8,
    117.8,
    120.1,
    99.2,
    104.3,
    102.8,
    108.6,
    112.0,
    108.0,
    109.6,
    114.1,
    118.2,
    120.5,
    123.0,
    99.0,
    104.0,
    103.0,
    108.9,
    112.3,
    108.3,
    110.0,
    114.4,
    118.5,
    120.8,
    123.4,
    128.1
  )
)


data_monthly_update <- dplyr::tibble(
  pub_date = as.Date(c(
    rep("2022-10-25", 5),
    rep("2022-11-25", 5),
    rep("2022-12-25", 5),
    rep("2023-01-25", 6),
    rep("2023-02-25", 6),
    rep("2023-03-25", 6),
    rep("2023-04-25", 7),
    rep("2023-05-25", 7),
    rep("2023-06-25", 7),
    rep("2023-07-25", 8),
    rep("2023-08-25", 8),
    rep("2023-09-25", 8)
  )),
  time = as.Date(c(
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01",
    "2021-10-01",
    "2022-01-01",
    "2022-04-01",
    "2022-07-01",
    "2022-10-01",
    "2023-01-01",
    "2023-04-01",
    "2023-07-01"
  )),
  values = c(
    100,
    105,
    102,
    107,
    110,
    100.2,
    105.4,
    102,
    107.1,
    110.4,
    100.4,
    105.3,
    102.1,
    107.3,
    110.9,
    100.5,
    105.3,
    102.1,
    107.4,
    111.0,
    106.4,
    100.5,
    105.3,
    102.1,
    107.4,
    111.3,
    106.1,
    100.5,
    105.3,
    102.1,
    107.4,
    111.5,
    106.2,
    100.4,
    105.1,
    102.2,
    107.6,
    111.5,
    106.8,
    108.3,
    100.4,
    105.1,
    102.0,
    107.6,
    111.5,
    106.8,
    108.3,
    100.2,
    105.1,
    102.1,
    107.6,
    111.5,
    106.8,
    108.3,
    100.1,
    105.1,
    102.2,
    107.9,
    111.3,
    107.1,
    108.6,
    113.2,
    100.0,
    105.1,
    102.2,
    107.9,
    111.3,
    107.1,
    108.6,
    113.2,
    100.0,
    105.0,
    102.2,
    107.9,
    111.3,
    107.1,
    108.6,
    113.2
  )
)
