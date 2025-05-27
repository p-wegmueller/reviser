#' Vintages Data
#'
#' A collection of real-time datasets.
#'
#' * [gdp]: Quarterly Vintages (Billions of real dollars, seasonally adjusted)
#' * Timeframe: Q1 1980 - Q4 2024
#' * Real-Time Vintages: Q4 2002 - Q4 2024
#' * Frequency: Quarterly
#' * Format: A tibble with quarterly observations and 3 variables:
#'     * `time`: Date of the observation.
#'     * `pub_date`: Publication date of the vintage
#'     * `values`: Numeric, real GDP (seasonally adjusted).
#'     * `id`: Country code
#'
#' @section Sources:
#' * All the data is from the realtime database of Indergand and Leist (2014).
#' **Countries**:
#' * CHE:
#'   * Switzerland
#'   * Source: SECO
#'
#' * US:
#'   * United States
#'   * Sources: FRED, OECD
#'
#' * EA:
#'   * Euro Area
#'   * Sources: Eurostat, OECD
#'
#' * JP:
#'   * Japan
#'   * Sources: Cabinet Office (Japan), OECD
#'
#' @srrstats {G1.0} academic literatre
#'
#' @references Indergand, R., Leist, S. A Real-Time Data Set for Switzerland.
#' Swiss J Economics Statistics 150, 331–352 (2014).
#'  \url{https://doi.org/10.1007/BF03399410}
#'
#' @examples
#' # Load [gdp] dataset
#' data(gdp)
#' @family dataset
#' @format NULL
"gdp"
