#' Convert Vintages Data to Long Format
#'
#' Converts a vintages dataset from wide format to long format, optionally adding `id` if the input 
#' is a list of data frames. The long format contains one row per combination of `time` and `names_to` 
#' (e.g., `pub_date` or `release`), with values stored in a single `value` column.
#'
#' @param df A data frame, tibble, or list of data frames containing vintages data in wide format.
#' @param names_to The name of the column to create from the wide-format column names. Must be either 
#'        `"pub_date"` (default) or `"release"`.
#' @param keep_na Logical. If `TRUE`, retains rows with `NA` values in the `value` column. Default is `FALSE`.
#'
#' @return A long-format data frame or tibble. If the input is a list of wide-format data frames, the output 
#'         will be a single combined long-format data frame.
#'
#'
#' @examples
#' # Example wide-format data
#' long_data <- reviser::gdp_us
#'
#' # Convert to wide format
#' wide_data <- vintages_wide(long_data)
#'
#' # Example list of wide-format data frames
#' wide_list <- list(
#'   A = wide_data,
#'   B = wide_data
#' )
#' 
#' # Convert list to long format
#' long_data <- vintages_long(wide_list, names_to = "pub_date")
#'
#' @export
vintages_long <- function(df, names_to = "pub_date", keep_na = FALSE) {
  
  # Check names_to argument is either 'pub_date' or ' release'
  if (!names_to %in% c("pub_date", "release")) {
    rlang::abort("'names_to' argument must be either 'pub_date' or 'release'")
  }
  
  # Check keep_na is logical
  if (!is.logical(keep_na)) {
    rlang::abort("'keep_na' argument must be logical.")
  }
  
  
  # If input is a list of wide data.frames
  if ("list" %in% class(df) ) {
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
      dplyr::arrange(pub_date, time) # Ensure data is sorted by pub_date and time 
    } else  if (names_to == "release") {
      long_df <- long_df %>%
        dplyr::arrange(time) # Ensure data is sorted by time 
    }
    
    if (keep_na) {
      long_df <- vintages_assign_class(long_df)
      return(long_df)
    } else {
      long_df <- long_df %>%
        dplyr::filter(!is.na(value)) 
    }
    long_df <- vintages_assign_class(long_df)
    return(long_df)
  }
}

#' Convert Vintages Data to Wide Format
#'
#' Converts a vintages dataset from long format to wide format, optionally grouping by `id` if present. 
#' The wide format uses one column per unique value of the `names_from` parameter, with observation dates 
#' (`time`) as rows and values (`value`) as cell contents.
#'
#' @param df A data frame or tibble containing vintages data in long format.
#' @param names_from The name of the column whose unique values will be used as column names in the wide format. 
#'        Defaults to `"pub_date"`. Other: `"release"`.
#'
#' @return If an `id` column is present, the function returns a named list of wide-format data frames, 
#'         one for each unique `id`. Otherwise, it returns a single wide-format data frame.
#'
#'
#' @examples
#' # Example wide-format data
#' long_data <- reviser::gdp_us
#'
#' # Convert to wide format
#' wide_data <- vintages_wide(long_data)
#'
#' # Example list of wide-format data frames
#' wide_list <- list(
#'   A = wide_data,
#'   B = wide_data
#' )
#' 
#' # Convert list to long format
#' long_data1 <- vintages_long(wide_data, names_to = "pub_date")
#' long_data2 <- vintages_long(wide_list, names_to = "pub_date")
#'
#' @export
vintages_wide <- function(df, names_from = "pub_date") {
  
  df <- standardize_val_col(df)
  
  check <- vintages_check(df)
  if (check == "wide") {
    rlang::warn("The input data is already in wide format.")
    return(df)
  }
  
  # Check required columns
  required_cols <- c("time", names_from, "value")
  if (!all(required_cols %in% colnames(df))) {
    rlang::abort(paste0("The input 'df' must contain the columns: 'time', '",names_from ,"', and 'value'."))
  } 
  addidtional_columns <- setdiff(colnames(df), c(required_cols, "id"))
  if (length(addidtional_columns) > 0) {
    rlang::warn(paste0("Ignoring columns: ", paste0(addidtional_columns, collapse = ", ")))
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
  
  if (n_id > 0 ) {
    # Split data by id and create a named list of wide data.frames
    wide_list <- df %>%
      split(.$id) %>%
      lapply(function(sub_df) {
        sub_df <- sub_df %>%
          dplyr::select(time, dplyr::all_of(names_from), value) %>%
          tidyr::pivot_wider(names_from = names_from, values_from = value)
        if (names_from == "pub_date") {
          class(sub_df) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
        } else  if (names_from == "release") {
          class(sub_df) <- c("tbl_release", "tbl_df", "tbl", "data.frame")
        } 
        sub_df
      })
    return(wide_list)
  } else {
    # Convert to wide format
    wide_df <- df %>%
      tidyr::pivot_wider(names_from = names_from, values_from = value)
    
    if (names_from == "pub_date") {
      class(wide_df) <- c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
    } else  if (names_from == "release") {
      class(wide_df) <- c("tbl_release", "tbl_df", "tbl", "data.frame")
    } 
    return(wide_df)
  }
}

#' Rename Columns to Align with Package Standards
#'
#' Renames columns in a data frame or tibble to align with the conventions used in this package. 
#' Converts the renamed columns to the appropriate data types.
#'
#' @param df A data frame or tibble containing the data to be renamed.
#' @param col_time Optional. The name of the column to be renamed as `time`. 
#'        The `time` column represents observation dates and will be converted to `Date` format.
#' @param col_pub_date Optional. The name of the column to be renamed as `pub_date`. 
#'        The `pub_date` column represents release dates and will be converted to `Date` format.
#' @param col_value Optional. The name of the column to be renamed as `value`. 
#'        The `value` column represents the observed values and will be converted to numeric.
#' @param col_id Optional. The name of the column to be renamed as `id`.
#' @param col_release Optional. The name of the column to be renamed as `release`.
#'        The `id` column is used as an identifier and will be converted to character format.
#'
#' @return A data frame or tibble with the renamed columns and their respective data types converted 
#'         (if specified). The original class of the input object is preserved.
#'
#' @details
#' The function checks the validity of the input data frame and ensures that at least one column 
#' is specified for renaming. If a column is renamed, it is also converted to the expected data type:
#' - `time` and `pub_date` are converted to `Date`.
#' - `value` is converted to numeric.
#' - `release` and `id` are converted to character.
#'
#' @examples
#' # Example data
#' data <- tibble::tibble(
#'   observation_date = seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"),
#'   release_date = seq.Date(as.Date("2020-01-15"), as.Date("2020-06-15"), by = "month"),
#'   observed_value = rnorm(6),
#'   identifier = rep("A", 6)
#' )
#'
#' # Rename columns
#' renamed_data <- vintages_rename(
#'   data,
#'   col_time = observation_date,
#'   col_pub_date = release_date,
#'   col_value = observed_value,
#'   col_id = identifier
#' )
#'
#' @export
vintages_rename <- function(
    df, 
    col_time = NULL, 
    col_pub_date = NULL, 
    col_value = NULL, 
    col_release = NULL,
    col_id = NULL
) {
  # Check if input is a data.frame or tibble
  if (!is.data.frame(df)) {
    rlang::abort("The input 'df' must be a data.frame or tibble.")
  }
  
  df <- vintages_assign_class(df)
  
  # Ensure column inputs can handle both quoted and unquoted names
  col_time <- rlang::enquo(col_time)
  col_pub_date <- rlang::enquo(col_pub_date)
  col_value <- rlang::enquo(col_value)
  col_release <- rlang::enquo(col_release)
  col_id <- rlang::enquo(col_id)
  
  # Check that at least one column is provided
  if (rlang::quo_is_null(col_time) && rlang::quo_is_null(col_pub_date) && rlang::quo_is_null(col_value) &&
      rlang::quo_is_null(col_release) && rlang::quo_is_null(col_id)) {
    rlang::abort("At least one column must be specified for renaming.")
  }
  
  # Check that specified columns exist in df
  provided_cols <- c(
    if (!rlang::quo_is_null(col_time)) rlang::quo_name(col_time),
    if (!rlang::quo_is_null(col_pub_date)) rlang::quo_name(col_pub_date),
    if (!rlang::quo_is_null(col_value)) rlang::quo_name(col_value),
    if (!rlang::quo_is_null(col_release)) rlang::quo_name(col_release),
    if (!rlang::quo_is_null(col_id)) rlang::quo_name(col_id)
  )
  
  missing_cols <- setdiff(provided_cols, colnames(df))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue("The following specified columns are not in 'df': {paste(missing_cols, collapse = ', ')}"))
  }
  
  # Rename and mutate based on provided inputs
  if (!rlang::quo_is_null(col_time)) {
    df <- df %>%
      dplyr::rename(time = !!col_time) %>%
      dplyr::mutate(time = as.Date(time))
  }
  
  if (!rlang::quo_is_null(col_pub_date)) {
    df <- df %>%
      dplyr::rename(pub_date = !!col_pub_date) %>%
      dplyr::mutate(pub_date = as.Date(pub_date))
  }
  
  if (!rlang::quo_is_null(col_value)) {
    df <- df %>%
      dplyr::rename(value = !!col_value) %>%
      dplyr::mutate(value = as.numeric(value))
  }
  
  if (!rlang::quo_is_null(col_release)) {
    df <- df %>%
      dplyr::rename(release = !!col_release) %>%
      dplyr::mutate(release = paste0("release_", as.character(release)))
  }
  
  if (!rlang::quo_is_null(col_id)) {
    df <- df %>%
      dplyr::rename(id = !!col_id) %>%
      dplyr::mutate(id = as.character(id))
  }
  
  df <- vintages_assign_class(df)
  return(df)
}




#' Check if Data is in Valid Vintages Format
#'
#' Validates whether the provided data frame is in a proper format for vintages analysis, determining 
#' whether it is in "long" or "wide" format. Throws an error if the format is invalid.
#'
#' @param df A data frame or tibble containing data vintages. The data frame must have specific columns 
#'           depending on whether it is in long or wide format.
#'
#' @return A string indicating the format of the data:
#' - `"long"` if the data frame is in long format.
#' - `"wide"` if the data frame is in wide format.
#'
#' @examples
#' # Example of long format data
#' long_data <- tibble::tibble(
#'   time = seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"),
#'   pub_date = seq.Date(as.Date("2020-01-15"), as.Date("2020-06-15"), by = "month"),
#'   value = rnorm(6)
#' )
#' vintages_check(long_data) # Should return "long"
#'
#' # Example of wide format data
#' wide_data <- tibble::tibble(
#'   time = seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"),
#'   `2020-01-15` = rnorm(6),
#'   `2020-02-15` = rnorm(6)
#' )
#' vintages_check(wide_data) # Should return "wide"
#' @keywords internal
#' @noRd
vintages_check <- function(df) {

  # Check if the object is a data.frame or tibble
  if (!is.data.frame(df)) {
    rlang::abort("The provided object is not a data.frame or tibble.")
  }
  
  # Check if the object contains the "time" column
  if (!"time" %in% colnames(df)) {
    rlang::abort("The 'time' column is missing in the data.frame.")
  }
  
  # Check if "time" is in the correct date format
  if (!all(!is.na(as.Date(df$time, format = "%Y-%m-%d")))) {
    rlang::abort("The 'time' column contains values that are not in the '%Y-%m-%d' format.")
  }
  
  # Check for "long format"
  long_format <- all(c("pub_date", "value") %in% colnames(df)) ||
    all(c("pub_date", "values") %in% colnames(df)) ||  
    all(c("release", "value") %in% colnames(df)) || 
    all(c("release", "values") %in% colnames(df)) 
  
  if (long_format) {
    if("pub_date" %in% colnames(df)) {
    # Check if "pub_date" is in the correct date format
    if (!all(!is.na(as.Date(df$pub_date, format = "%Y-%m-%d")))) {
      rlang::abort("The 'pub_date' column contains values that are not in '%Y-%m-%d' format.")
    } 
    }
    
    if("release" %in% colnames(df)) {
    }
    return("long")
  }
  
  # Check for "wide format"
  wide_format <- setdiff(colnames(df), "time")
  #if (length(wide_format) == 1) {
  #  rlang::abort("Did you forget to add pub_date or release column?")
  #}
  if (length(wide_format) > 0) {
    if (all(!is.na(as.Date(wide_format, format = "%Y-%m-%d"))) | all(grepl("release|final", wide_format))) {
      return("wide")
    } else {
      rlang::abort("One or more column names in the 'wide format' are not labeled correctly.")
    }
  }
  rlang::abort("The data.frame does not conform to either 'long format' or 'wide format'.")
}

#' Assign class to vintages tibble depending on columns
#' @param df data.frame
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
      classes <- union(col_class_map[[col]], classes) # Add class if column exists
    } else {
      classes <- setdiff(classes, col_class_map[[col]]) # Remove class if column is absent
    }
  }
  
  df <- standardize_val_col(df)
  class(df) <- classes # Assign updated classes
  return(df)
}


#' Standardize the time series data frame
#' Value/s column is renamed to `value`
#' @param df data.frame
#' @keywords internal
#' @noRd
standardize_val_col <- function(df) {
  df %>%
    dplyr::rename(value = dplyr::any_of(c("value", "values"))) %>%
    suppressMessages()
}


