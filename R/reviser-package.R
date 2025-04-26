#' @description
#' **reviser** is an R package designed for working with time-series vintages
#' data. The package provides tools to clean, visualize, and analyze time-series
#' revisions. To learn more about reviser, start with the vignettes:
#' `browseVignettes(package = "reviser")`
#' The package will be actively developed and maintained.
#' @srrstats {G1.2} Life Cycle Statement
#' @srrstats {G1.4} Using roxygen2 for function documentation
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(
  ".",
  "id",
  "n",
  "final_value",
  "pub_date",
  "release",
  "time",
  "value",
  "value_ref",
  "target_date",
  "diff_final_value",
  "fraction_sign_correct",
  "late_sign",
  "diff_value",
  "early_sign",
  "realtime_period"
))

# Import from KFAS necessary to overcome a bug when defining the model
#' @importFrom KFAS SSModel SSMcustom
#' @importFrom magrittr %>%
#' @importFrom calculus %mx% %diff% %sum% %prod%
#' @srrstats {G1.4} roxygen2 is used for all documentation.
#' @srrstats {G1.4a} All internal functions are also documented
NULL
