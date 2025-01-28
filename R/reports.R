

#' @export
generate_basic_report <- function(data, 
                                output_file = "report.pdf", 
                                title = "Title of my basic report on data revisions", 
                                author = "Author(s)", 
                                include_logo = TRUE, 
                                logo_path = NULL, 
                                template_file = NULL) {
  
  
  # Check vailidity of inputs
  # Validate `data`
  if (missing(data) || !is.data.frame(data)) {
    rlang::abort("The 'data' argument must be provided and must be a data frame or tibble.")
  }
  
  check <- vintages_check(data)
  if (check=="wide") {
    data <- vintages_long(data)
  }
  
  # Validate `output_file`
  if (!is.character(output_file) || nchar(output_file) == 0) {
    rlang::abort("The 'output_file' argument must be a non-empty string.")
  }
  
  # Validate `title`
  if (!is.character(title)) {
    rlang::abort("The 'title' argument must be a string.")
  }
  
  # Validate `author`
  if (!is.character(author)) {
    rlang::abort("The 'author' argument must be a string.")
  }
  
  # Validate `include_logo`
  if (!is.logical(include_logo)) {
    rlang::abort("The 'include_logo' argument must be a single logical value (TRUE or FALSE).")
  }
  
  # Path to the default skeleton
  template_dir <- system.file("rmarkdown/templates/basic_report/skeleton", package = "reviser")
  skeleton_file <- file.path(template_dir, "skeleton.Rmd")
  default_template <- file.path(template_dir, "template.tex")
  
  # Handle logo inclusion
  if (include_logo) {
    if (is.null(logo_path)) {
      logo_path <- file.path(template_dir, "logo.png")
    }
    # Copy the logo to a temporary directory to ensure accessibility
    temp_logo_path <- file.path(tempdir(), basename(logo_path))
    file.copy(logo_path, temp_logo_path, overwrite = TRUE)
  } else {
    temp_logo_path <- NULL
  }
  
  # Validate `logo_path` if `include_logo` is TRUE
  if (include_logo) {
    if (!file.exists(logo_path)) {
      rlang::abort("The file specified in 'logo_path' does not exist.")
    }
  }
  
  # Validate `template_file` if provided
  if (!is.null(template_file)) {
    if (!is.character(template_file) || nchar(template_file) == 0) {
      rlang::abort("The 'template_file' argument must be a non-empty string if provided.")
    }
    if (!file.exists(template_file)) {
      rlang::abort("The file specified in 'template_file' does not exist.")
    }
  }
  
  
  # Use a custom template if provided, otherwise use the default
  if (!is.null(template_file)) {
    custom_template <- file.path(tempdir(), "custom_template.tex")
    file.copy(template_file, custom_template, overwrite = TRUE)
  } else {
    custom_template <- default_template
  }
  
  
  # Define the full path for the output file in the current working directory
  output_path <- file.path(getwd(), output_file)
  
  # Render the report
  rmarkdown::render(
    input = skeleton_file,
    output_file = output_path,
    params = list(
      data = data,
      report_title = title,
      report_author = author,
      logo_path = ifelse(include_logo, temp_logo_path, "NA")
    ),
    output_format = rmarkdown::pdf_document(
      latex_engine = "xelatex",
      template = custom_template
    )
  )
  
  message("Report successfully generated at: ", output_path)
}
