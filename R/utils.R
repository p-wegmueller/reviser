#' Convert Vintages Data to Long Format
#'
#' Converts a vintages dataset from wide format to long format, optionally
#' adding `id` if the input is a list of data frames. The long format contains
#' one row per combination of `time` and `names_to` (e.g., `pub_date` or
#' `release`), with values stored in a single `value` column.
#'
#' @param df A data frame, tibble, or list of data frames containing vintages
#' data in wide format.
#' @param names_to The name of the column to create from the wide-format
#' column names. Must be either `"pub_date"` (default) or `"release"`.
#' @param keep_na Logical. If `TRUE`, retains rows with `NA` values in the
#' `value` column. Default is `FALSE`.
#'
#' @return A long-format data frame or tibble. If the input is a list of
#' wide-format data frames, the output will be a single combined long-format
#' data frame.
#'
#' @srrstats {G2.0} Implements assertions on types of inputs through parameter
#' validation
#' @srrstats {G2.3a} Uses parameter validation to restrict `names_to` to
#' allowed values
#' @srrstats {G2.8} Provides appropriate conversion routines for tabular data
#' @srrstats {G2.9} Issues diagnostic messages for data conversion (warning when
#' already long format)
#' @srrstats {G2.14} Provides option to specify how to handle missing data via
#' `keep_na` parameter
#' @srrstats {TS1.1} Explicitly documents input data types and classes
#' @srrstats {TS1.2} Validates inputs via `vintages_check()`
#' @srrstats {TS2.0} Warns for possibly implicit missings
#' @srrstats {TS4.2} Explicitly documents return value types and classes
#'
#' @examples
#' # Example wide-format data
#' long_data <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Convert to wide format
#' wide_data <- vintages_wide(long_data)
#'
#' # Example list of wide-format data frames
#' wide_list <- list(
#'   A = wide_data$US,
#'   B = wide_data$US
#' )
#'
#' # Convert list to long format
#' long_data <- vintages_long(wide_list, names_to = "pub_date")
#'
#' @family helpers
#' @export
vintages_long <- function(df, names_to = "pub_date", keep_na = FALSE) {
  names_to <- tolower(names_to)

  # check names_to one of "pub_date" or "release",
  if (!names_to %in% c("pub_date", "release")) {
    rlang::abort(
      paste0(
        "'names_to' must be one of 'pub_date' or 'release', not ",
        names_to
      )
    )
  }

  # Check keep_na is logical
  if (!is.logical(keep_na)) {
    rlang::abort("'keep_na' argument must be logical.")
  }

  # If input is a list of wide data.frames
  if ("list" %in% class(df)) {
    long_list <- lapply(names(df), function(id) {
      check <- vintages_check(df[[id]])
      if (check == "long") {
        rlang::warn("The input data is already in long format.")
        return(df[[id]])
      }
      long_df_tmp <- df[[id]] %>%
        tidyr::pivot_longer(
          cols = -time,
          names_to = names_to,
          values_to = "value"
        ) %>%
        dplyr::mutate(id = id)

      if (keep_na) {
        return(long_df_tmp)
      } else {
        long_df_tmp <- long_df_tmp %>%
          dplyr::filter(!is.na(value))
      }
    })
    # Combine into a single data.frame
    long_df <- dplyr::bind_rows(long_list)
    long_df <- vintages_assign_class(long_df)
    return(long_df)
  } else {
    check <- vintages_check(df)
    if (check == "long") {
      rlang::warn("The input data is already in long format.")
      df <- vintages_assign_class(df)
      return(df)
    }
    # If input is a single wide data.frame
    long_df <- df %>%
      tidyr::pivot_longer(
        cols = -time,
        names_to = names_to,
        values_to = "value"
      )

    if (names_to == "pub_date") {
      long_df <- long_df %>%
        dplyr::mutate(pub_date = as.Date(pub_date)) %>%
        dplyr::arrange(pub_date, time) # Ensure data is sorted
    } else if (names_to == "release") {
      long_df <- long_df %>%
        dplyr::arrange(time) # Ensure data is sorted by time
    }

    if (keep_na) {
      long_df <- vintages_assign_class(long_df)
      return(long_df)
    } else {
      long_df <- long_df %>%
        dplyr::filter(!is.na(value))
      message("There might be implicit NA values in the 'value' column. ",
              "Consider setting 'keep_na = TRUE' if you want to retain them.")
    }
    long_df <- vintages_assign_class(long_df)
    return(long_df)
  }
}

#' Convert Vintages Data to Wide Format
#'
#' Converts a vintages dataset from long format to wide format, optionally
#' grouping by `id` if present. The wide format uses one column per unique
#' value of the `names_from` parameter, with observation dates (`time`) as
#' rows and values (`value`) as cell contents.
#'
#' @param df A data frame or tibble containing vintages data in long format.
#' @param names_from The name of the column whose unique values will be used
#' as column names in the wide format. Defaults to `"pub_date"`.
#' Other: `"release"`.
#'
#' @return If an `id` column is present, the function returns a named list of
#' wide-format data frames, one for each unique `id`. Otherwise, it returns a
#' single wide-format data frame.
#'
#' @srrstats {G2.0} Implements assertions on types of inputs through required
#' column checks
#' @srrstats {G2.8} Provides appropriate conversion routines
#' @srrstats {G2.9} Issues diagnostic messages for data conversion (warnings for
#' ignored columns)
#' @srrstats {G2.14} Handles missing data appropriately through pivot operations
#' @srrstats {TS1.1} Explicitly documents input data types and classes
#' @srrstats {TS1.2} Validates inputs via `vintages_check()` and column
#' verification
#' @srrstats {TS4.0b} Returns data in a consistent class format with specific
#' class assignment
#' @srrstats {TS4.2} Explicitly documents return value types and classes
#'
#' @examples
#' # Example wide-format data
#' long_data <- dplyr::filter(reviser::gdp, id=="US")
#'
#' # Convert to wide format
#' wide_data <- vintages_wide(long_data)
#'
#' # Example list of wide-format data frames
#' wide_list <- list(
#'   A = wide_data$US,
#'   B = wide_data$US
#' )
#'
#' # Convert list to long format
#' long_data1 <- vintages_long(wide_data, names_to = "pub_date")
#' long_data2 <- vintages_long(wide_list, names_to = "pub_date")
#'
#' @family helpers
#' @export
vintages_wide <- function(df, names_from = "pub_date") {
  names_from <- tolower(names_from)
  # check names_from one of "pub_date" or "release",
  if (!names_from %in% c("pub_date", "release")) {
    rlang::abort(
      paste0(
        "'names_from' must be one of 'pub_date' or 'release', not ",
        names_from
      )
    )
  }

  df <- standardize_val_col(df)

  check <- vintages_check(df)
  if (check == "wide") {
    rlang::warn("The input data is already in wide format.")
    return(df)
  }

  # Check required columns
  required_cols <- c("time", names_from, "value")
  if (!all(required_cols %in% colnames(df))) {
    rlang::abort(paste0(
      "The input 'df' must contain the columns: 'time', '",
      names_from,
      "', and 'value'."
    ))
  }
  addidtional_columns <- setdiff(colnames(df), c(required_cols, "id"))
  if (length(addidtional_columns) > 0) {
    rlang::warn(paste0(
      "Ignoring columns: ",
      paste0(addidtional_columns, collapse = ", ")
    ))
  }

  id_present <- "id" %in% colnames(df)
  if (id_present) {
    n_id <- df$id %>%
      unique() %>%
      length()
    df <- df %>%
      dplyr::select(dplyr::all_of(c("id", required_cols)))
  } else {
    n_id <- 0
    df <- df %>%
      dplyr::select(dplyr::all_of(required_cols))
  }

  if (n_id > 0) {
    # Split data by id and create a named list of wide data.frames
    wide_list <- df %>%
      split(.$id) %>%
      lapply(function(sub_df) {
        sub_df <- sub_df %>%
          dplyr::select(time, dplyr::all_of(names_from), value) %>%
          tidyr::pivot_wider(
            names_from = dplyr::all_of(names_from),
            values_from = value
          )
        if (names_from == "pub_date") {
          class(sub_df) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
        } else if (names_from == "release") {
          class(sub_df) <- c("tbl_release", "tbl_df", "tbl", "data.frame")
        }
        sub_df
      })
    return(wide_list)
  } else {
    # Convert to wide format
    wide_df <- df %>%
      tidyr::pivot_wider(
        names_from = dplyr::all_of(names_from),
        values_from = value
      )

    if (names_from == "pub_date") {
      class(wide_df) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
    } else if (names_from == "release") {
      class(wide_df) <- c("tbl_release", "tbl_df", "tbl", "data.frame")
    }
    return(wide_df)
  }
}


#' Check if Data is in Valid Vintages Format
#'
#' Validates whether the provided data frame is in a proper format for vintages
#'  analysis, determining whether it is in "long" or "wide" format. Throws an
#'  error if the format is invalid.
#'
#' @param df A data frame or tibble containing data vintages. The data frame
#' must have specific columns depending on whether it is in long or wide format.
#'
#' @return A string indicating the format of the data:
#' - `"long"` if the data frame is in long format.
#' - `"wide"` if the data frame is in wide format.
#'
#' @srrstats {G1.4a} All internal functions are also documented
#' @srrstats {G2.0} Implements assertions on length of input by checking if the
#' input data frame contains required columns.
#' @srrstats {G2.0a} Documentation explicitly states expectations on input
#' structure through parameter documentation.
#' @srrstats {G2.1} Implements assertions on types of inputs by checking if
#' input is a data frame and if columns contain properly formatted dates.
#' @srrstats {G2.1a} Documentation explicitly states expectations on data types
#' for all required columns.
#' @srrstats {G2.2} Appropriately restricts input to a single data frame,
#' preventing multivariate input where univariate is expected.
#' @srrstats {G2.8} Software provides appropriate conversion or dispatch
#' routines as part of initial pre-processing to ensure that all other
#' sub-functions of a  package receive inputs of a single defined class or type.
#' @srrstats {G2.9} Software issues diagnostic messages for type conversion in
#' which information is lost or added.
#' @srrstats {G2.10} Software ensures that extraction or filtering of single
#' columns from tabular inputs behaves consistently regardless of the class of
#' tabular data.
#' @srrstats {G2.11} Handle list columns
#' @srrstats {G2.12} Handle list columns
#' @srrstats {G2.13} Implements appropriate checks for missing data in critical
#' columns like 'time' and 'pub_date'.
#' @srrstats {G5.2a} unique and descriptive errors.
#' @srrstats {G5.8} Includes edge condition tests to ensure appropriate behavior
#' with invalid inputs.
#' @srrstats {TS1.1}  documents the types and classes of input data
#' @srrstats {TS1.2} implements validation routines that inputs are of
#' acceptable classes.
#' @srrstats {TS1.3}  pre-processing routine to validate input data and
#' transform it to a uniform type.
#' @srrstats {TS1.4} date column always there
#' @srrstats {TS4.2} type and class of all return values are documented.
#'
#' @examples
#' # Example of long format data
#' long_data <- tibble::tibble(
#'   time = seq.Date(
#'     as.Date("2020-01-01"), as.Date("2020-08-01"), by = "month"
#'   ),
#'   pub_date = seq.Date(
#'     as.Date("2020-01-15"), as.Date("2020-08-15"), by = "month"
#'   ),
#'   value = rnorm(8)
#' )
#' vintages_check(long_data) # Should return "long"
#'
#' # Example of wide format data
#' wide_data <- tibble::tibble(
#'   time = seq.Date(
#'     as.Date("2020-01-01"), as.Date("2020-08-01"), by = "month"
#'     ),
#'   `2020-01-15` = rnorm(8),
#'   `2020-02-15` = rnorm(8)
#' )
#' vintages_check(wide_data) # Should return "wide"
#' @keywords internal
#' @noRd
vintages_check <- function(df) {
  # Helper: Check for simple (scalar, non-list) columns
  check_simple_columns <- function(df) {
    issues <- lapply(names(df), function(col) {
      column <- df[[col]]
      
      # G2.12: Check for list columns
      if (is.list(column) && !is.data.frame(column)) {
        return(paste0("Column '", col, "' is a list column."))
      }
      
      # G2.11: Check for non-atomic or multi-valued entries
      non_atomic <- which(!vapply(column, is.atomic, logical(1)))
      if (length(non_atomic) > 0) {
        return(paste0("Column '", col, "' has non-atomic elements."))
      }
      
      not_scalar <- which(vapply(column, function(x) length(x) != 1, logical(1)))
      if (length(not_scalar) > 0) {
        return(paste0("Column '", col, "' has elements that are not scalar values."))
      }
      
      return(NULL)
    })
    
    issues <- unlist(issues)
    if (length(issues) > 0) {
      rlang::abort(paste("Invalid columns detected:\n", paste(issues, collapse = "\n")))
    }
    return(TRUE)
  }
  
  # === Begin Main Checks ===
  # Ensure it's a data.frame or tibble
  if (!is.data.frame(df)) {
    rlang::abort("The provided object is not a data.frame or tibble.")
  }
  
  # Perform column structure check (G2.11 and G2.12)
  check_simple_columns(df)
  
  # Check for required "time" column
  if (!"time" %in% colnames(df)) {
    rlang::abort("The 'time' column is missing in the data.frame.")
  }
  
  # Validate date format for "time"
  if (!all(!is.na(as.Date(df$time, format = "%Y-%m-%d")))) {
    rlang::abort("The 'time' column contains values that are not in the '%Y-%m-%d' format.")
  }
  
  # Check for long format
  long_format <- all(c("pub_date", "value") %in% colnames(df)) ||
    all(c("pub_date", "values") %in% colnames(df)) ||
    all(c("release", "value") %in% colnames(df)) ||
    all(c("release", "values") %in% colnames(df))
  
  if (long_format) {
    if ("pub_date" %in% colnames(df)) {
      if (!all(!is.na(as.Date(df$pub_date, format = "%Y-%m-%d")))) {
        rlang::abort("The 'pub_date' column contains values that are not in '%Y-%m-%d' format.")
      }
    }
    
    # Possibly add similar validation for 'release' if needed
    return("long")
  }
  
  # Check for wide format
  wide_format <- setdiff(colnames(df), "time")
  if (length(wide_format) > 0) {
    if (
      all(!is.na(as.Date(wide_format, format = "%Y-%m-%d"))) ||
      all(grepl("release|final", wide_format))
    ) {
      return("wide")
    } else {
      rlang::abort("One or more column names in the 'wide format' are not labeled correctly.")
    }
  }
  
  rlang::abort("The data.frame does not conform to either 'long format' or 'wide format'.")
}


#' Assign class to vintages tibble depending on columns
#' @param df data.frame
#'
#' @srrstats {G2.0} Implements assertions on input by operating on column
#' presence
#' @srrstats {G2.1} Implicitly handles input type by preserving the original
#' class structure
#' @srrstats {G2.8} Provides appropriate conversion routines to ensure
#' consistent class structure
#' @srrstats {TS1.0} Uses and relies on explicit class systems for time series
#' data
#' @srrstats {TS1.2} Implements validation routines by checking column presence
#' @srrstats {TS1.3} Functions as part of a pre-processing routine that
#' transforms data to uniform type
#' @srrstats {TS4.0b} Ensures return values have consistent class-defined format
#' @srrstats {TS4.2} Contributes to documenting type and class of return values
#'
#' @keywords internal
#' @noRd
vintages_assign_class <- function(df) {
  classes <- class(df) # Get the existing classes

  # Define column-class mappings
  col_class_map <- list(
    "pub_date" = "tbl_pubdate",
    "release" = "tbl_release"
  )

  # Loop through the mapping and update classes
  for (col in names(col_class_map)) {
    if (col %in% names(df)) {
      classes <- union(col_class_map[[col]], classes) # Add class
    } else {
      classes <- setdiff(classes, col_class_map[[col]]) # Remove class
    }
  }

  df <- standardize_val_col(df)
  class(df) <- classes # Assign updated classes
  return(df)
}


#' Standardize the time series data frame
#' Value/s column is renamed to `value`
#' @param df data.frame
#'
#' @srrstats {G2.0} Implements assertions on columns by using dplyr::any_of
#' @srrstats {G2.8} Provides conversion routines to ensure consistent column
#' naming
#' @srrstats {G2.9} Performs column name standardization with appropriate
#' handling
#' @srrstats {G2.10} Ensures consistent behavior when extracting columns
#' regardless of input class
#' @srrstats {TS1.3} Contributes to pre-processing routine by standardizing
#' column names
#' @srrstats {TS1.4} Maintains time- and date-based components by standardizing
#' names without modifying data
#' @srrstats {TS4.0} Helps ensure return values are in a consistent format
#'
#' @keywords internal
#' @noRd
standardize_val_col <- function(df) {
  df %>%
    dplyr::rename(value = dplyr::any_of(c("value", "values"))) %>%
    suppressMessages()
}
