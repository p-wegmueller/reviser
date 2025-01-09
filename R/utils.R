
#' Function to put the vintages data in long format
#' @export
vintages_long <- function(df, names_to = "pub_date", keep_na = FALSE) {
  
  # Check names_to argument is either 'pub_date' or ' release'
  if (!names_to %in% c("pub_date", "release")) {
    rlang::abort("'names_to' argument must be either 'pub_date' or 'release'")
  }
  
  classes <- class(df)
  
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
        dplyr::mutate(pub_date = as.Date(pub_date), id = id) %>% 
        dplyr::arrange(pub_date, time) # Ensure data is sorted by pub_date and time 
      if (keep_na) {
        class(long_df_tmp) <- classes # Restore original class
        return(long_df_tmp)
      } else {
        long_df_tmp <- long_df_tmp %>%
          dplyr::filter(!is.na(value)) 
      }
    })
    # Combine into a single data.frame
    long_df <- dplyr::bind_rows(long_list)
    return(long_df)
  } else {
    check <- vintages_check(df)
    if (check == "long") {
      rlang::warn("The input data is already in long format.")
      class(df) <- classes # Restore original class
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
      class(long_df) <- classes # Restore original class
      return(long_df)
    } else {
      long_df <- long_df %>%
        dplyr::filter(!is.na(value)) 
    }
    class(long_df) <- classes # Restore original class
    return(long_df)
  }
}

#' Function to put the vintages data to wide format
#' @export
vintages_wide <- function(df, names_from = "pub_date") {
  classes <- class(df)
  
  df <- standardize_val_col(df)
  
  check <- vintages_check(df)
  if (check == "wide") {
    rlang::warn("The input data is already in wide format.")
    class(df) <- classes # Restore original class
    return(df)
  }
  
  # Check required columns
  required_cols <- c("time", names_from, "value")
  if (!all(required_cols %in% colnames(df))) {
    rlang::abort("The input 'df' must contain the columns: 'time', 'pub_date', and 'value'.")
  } 
  addidtional_columns <- setdiff(colnames(df), c(required_cols, "id"))
  if (length(addidtional_columns) > 0) {
    rlang::warn(paste0("Ignoring columns: ", paste0(addidtional_columns, collapse = ", ")))
    
    df <- df %>%
      dplyr::select(dplyr::all_of(required_cols))
  }
  
  id_present <- "id" %in% colnames(df)
  if (id_present) {
    n_id <- df$id %>%
      unique() %>%
      nrow()
  } else {
    n_id <- 0
  }
  
  if ( n_id > 0 ) {
    # Split data by id and create a named list of wide data.frames
    wide_list <- df %>%
      split(.$id) %>%
      lapply(function(sub_df) {
        sub_df <- sub_df %>%
          dplyr::select(time, pub_date, value) %>%
          tidyr::pivot_wider(names_from = names_from, values_from = value)
        class(sub_df) <- classes # Restore original class
        sub_df
      })
    return(wide_list)
  } else {
    # Convert to wide format
    wide_df <- df %>%
      tidyr::pivot_wider(names_from = names_from, values_from = value)
    
    class(wide_df) <- classes # Restore original class
    return(wide_df)
  }
}

#' Function to rename columns in accordance with the package
#' 
#' @param df data.frame or tibble
#' @param col_pub_date name of column with release dates (date format)
#' @param col_time name of column with observation dates (date format)
#' @param col_value name of column with values
#' @param col_id name of column with id
#' @export
vintages_rename <- function(
    df, 
    col_time = NULL, 
    col_pub_date = NULL, 
    col_value = NULL, 
    col_id = NULL
) {
  # Check if input is a data.frame or tibble
  if (!is.data.frame(df)) {
    rlang::abort("The input 'df' must be a data.frame or tibble.")
  }
  
  classes <- class(df) 
  
  # Check if at least one input column is provided
  if (is.null(col_time) && is.null(col_pub_date) && is.null(col_value) && is.null(col_id)) {
    rlang::abort("At least one column must be specified for renaming.")
  }
  
  # Rename and mutate based on provided inputs
  if (!is.null(col_time)) {
    df <- df %>%
      dplyr::rename(time = {{ col_time }}) %>%
      dplyr::mutate(time = as.Date(time))
  }
  
  if (!is.null(col_pub_date)) {
    df <- df %>%
      dplyr::rename(pub_date = {{ col_pub_date }}) %>%
      dplyr::mutate(pub_date = as.Date(pub_date))
  }
  
  if (!is.null(col_value)) {
    df <- df %>%
      dplyr::rename(value = {{ col_value }}) %>%
      dplyr::mutate(value = as.numeric(value))
  }
  
  if (!is.null(col_id)) {
    df <- df %>%
      dplyr::rename(id = {{ col_id }}) %>%
      dplyr::mutate(id = as.character(id))
  }
  
  class(df) <- classes # Restore original class
  return(df)
}



#' Standardize the time series data frame
#' Value/s column is renamed to `value`
#' @param df data.frame
#' @noRd
standardize_val_col <- function(df) {
  df %>%
    dplyr::rename(value = dplyr::any_of(c("value", "values"))) %>%
    suppressMessages()
}




#' Check data is in valid vintages format
#' @export
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
    all(c("release", "value") %in% colnames(df)) 
  
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
  if (length(wide_format) == 1) {
    rlang::abort("Did you forget to add pub_date or release column?")
  }
  if (length(wide_format) > 1) {
    if (all(!is.na(as.Date(wide_format, format = "%Y-%m-%d")))) {
      return("wide")
    } else {
      #rlang::abort("One or more column names in the 'wide format' do not match the '%Y-%m-%d' format.")
      return("wide")
    }
  }
  
  rlang::abort("The data.frame does not conform to either 'long format' or 'wide format'.")
}



