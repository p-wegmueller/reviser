#' Vintages Data
#'
#' A collection of datasets containing economic indicators for Switzerland.
#'
#' * [gdp]: Quarterly Vintages (Billions of real dollars, seasonally adjusted)
#'
#' @section Datasets:
#' * [gdp]:
#'   * Source: [Federal Reserve Bank Philadelphia](https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/real-time-data/data-files/xlsx/routputqvqd.xlsx?la=en&hash=33BCFABC01F5579AECBBFC39447EC6C9)
#'   * Timeframe: Q1 1970 - Q2 2024
#'   * Real-Time Vintage: January 1980 - October 2024
#'   * Frequency: Quarterly
#'   * Format: A tibble with quarterly observations and 2 variables:
#'     * `time`: Date, the quarter and year of the observation.
#'     * `values`: Numeric, the real GDP (seasonally adjusted).
#'
#'
#' @examples
#' # Load and plot [gdp]
#' data(gdp)
#' @format NULL
"gdp"