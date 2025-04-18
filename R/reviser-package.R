#' @description
#' To learn more about reviser, start with the vignettes:
#' `browseVignettes(package = "reviser")`
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
NULL
