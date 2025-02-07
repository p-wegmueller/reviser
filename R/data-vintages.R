#' Vintages Data
#'
#' A collection of real-time datasets.
#'
#' * [gdp_us]: Quarterly Vintages (Billions of real dollars, seasonally adjusted)
#'
#' @section Datasets:
#' * [gdp_us]:
#'   * Source: [Federal Reserve Bank Philadelphia](https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/real-time-data/data-files/xlsx/routputqvqd.xlsx?la=en&hash=33BCFABC01F5579AECBBFC39447EC6C9)
#'   * Timeframe: Q1 1970 - Q2 2024
#'   * Real-Time Vintages: January 1980 - October 2024
#'   * Frequency: Quarterly
#'   * Format: A tibble with quarterly observations and 3 variables:
#'     * `time`: Date of the observation.
#'     * `pub_date`: Publication date of the vintage
#'     * `values`: Numeric, real GDP (seasonally adjusted).
#'
#' * [gdp_uk]:
#'   * Source: [Bank of England](https://www.bankofengland.co.uk/statistics/gdp-real-time-database)
#'   * Timeframe: Q1 1955 - Q2 2016
#'   * Real-Time Vintages: March 1976 - September 2016
#'   * Frequency: Quarterly
#'   * Format: A tibble with quarterly observations and 3 variables:
#'     * `time`: Date of the observation.
#'     * `pub_date`: Publication date of the vintage
#'     * `values`: Numeric, real GDP (seasonally adjusted).
#'
#' @examples
#' # Load [gdp_us]
#' data(gdp_us)
#' @format NULL
"gdp_us"

#' @rdname gdp_us
#' @format NULL
"gdp_uk"