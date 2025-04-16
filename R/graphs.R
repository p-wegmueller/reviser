#' Plot Vintages Data
#'
#' A flexible function to visualize vintage data using various plot types such
#' as line plots, point plots, bar plots, or boxplots. The function ensures
#' that input data is validated and appropriately transformed before plotting.
#'
#' @param df A data frame containing the vintage data to be plotted. Must
#' include at least two columns:
#' one for time (`time`) and one for value (`value`).
#' @param type A character string specifying the type of plot to create.
#' Options are:
#' \itemize{
#'   \item{ "line": Line plot (default).}
#'   \item{ "point": Scatter plot.}
#'   \item{ "bar": Bar plot.}
#'   \item{ "boxplot": Boxplot.}
#'   }
#' @param dim_col A character string specifying the column name in `df` that
#' represents publication dates or other grouping dimensions (e.g. `"release"`).
#' Defaults to `"pub_date"`.
#' @param time_col A character string specifying the column name in `df` that
#' represents the time variable. Defaults to `"time"`.
#' @param title A character string specifying the title of the plot.
#' Defaults to an empty string.
#' @param subtitle A character string specifying the subtitle of the plot.
#' Defaults to an empty string.
#' @param ylab A character string specifying the label for the y-axis.
#' Defaults to an empty string.
#'
#' @return A ggplot2 plot object representing the specified vintage
#' data visualization.
#'
#' @details
#' The `plot_vintages` function is designed to handle data frames in both
#' wide and long formats. It ensures
#' that the provided data frame includes the necessary columns for plotting.
#' If the `dim_col` column contains
#' more than 30 unique values, only the most recent 30 are plotted.
#' Additionally, the function supports
#' custom themes and color scales using `scale_color_reviser`,
#' `scale_fill_reviser`, and `theme_reviser`.
#'
#' The function raises an error if:
#' \itemize{
#'  \item{The `type` argument is not one of `"line"`, `"point"`, `"bar"`,
#'  or `"boxplot"`.}
#'  \item{The specified `dim_col` is not a column in `df`.}
#'  \item{`title`, `subtitle`, or `ylab` are not character strings.}
#' }
#'
#' @seealso [theme_reviser()], [scale_color_reviser()], [scale_fill_reviser()]
#' @examples
#' # Example data
#' df <- data.frame(
#'   time = rep(seq.Date(from = as.Date("2022-01-01"),
#'   by = "month", length.out = 12), 3),
#'   value = runif(36, 50, 100),
#'   pub_date = rep(c("2022-01-05", "2022-02-07", "2022-03-03"), each = 12)
#' )
#'
#' # Line plot
#' plot_vintages(
#'   df,
#'   type = "line",
#'   dim_col = "pub_date",
#'   title = "Line plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Point plot
#' plot_vintages(
#'   df,
#'   type = "point",
#'   dim_col = "pub_date",
#'   title = "Scatter plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Bar plot
#' plot_vintages(
#'   df,
#'   type = "bar",
#'   dim_col = "pub_date",
#'   title = "Bar plot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' # Boxplot
#' plot_vintages(
#'   df,
#'   type = "boxplot",
#'   dim_col = "pub_date",
#'   title = "Boxplot",
#'   subtitle = "Randomly generated data"
#'   )
#'
#' @export
plot_vintages <- function(
  df,
  type = "line",
  dim_col = "pub_date",
  time_col = "time",
  title = "",
  subtitle = "",
  ylab = ""
) {
  # Check type input
  if (!type %in% c("line", "point", "bar", "boxplot")) {
    rlang::abort(
      "The 'type' argument must be either 'line', 'point', 'bar' or 'boxplot'."
    )
  }

  check <- vintages_check(df)
  if (check == "wide") {
    df <- vintages_long(df, keep_na = FALSE)
  }

  # Check 'dim_col' is column name of 'df'
  if (!dim_col %in% colnames(df)) {
    rlang::abort(
      paste0(
        "The column ",
        dim_col,
        " is not found in 'df'."
      )
    )
  }

  # Check 'time_col' is column name of 'df'
  if (!time_col %in% colnames(df)) {
    rlang::abort(
      paste0(
        "The column ",
        time_col,
        " is not found in 'df'."
      )
    )
  }

  # Check that 'time_col' is of date format
  if (!inherits(df[[time_col]], "Date")) {
    rlang::abort(
      "The 'time_col' argument must be of class 'Date'."
    )
  }

  # Check title and subtitle are character strings
  if (!is.character(title) || !is.character(subtitle) || !is.character(ylab)) {
    rlang::abort(
      "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
    )
  }

  dim_col <- as.name(dim_col)
  time_col <- as.name(time_col)

  if (ncol(df) <= 1L) {
    rlang::abort("'df' must have at least two columns.")
  }

  n <- length(unique(df[[dim_col]]))

  df <- df %>%
    dplyr::group_by(time) %>%
    dplyr::arrange(dplyr::desc(abs(value)), .by_group = TRUE) %>%
    dplyr::ungroup()

  if (n > 1) {
    if (class(df[[dim_col]]) %in% c("Date", "integer", "numeric")) {
      df[[dim_col]] <- as.character(df[[dim_col]])
    }

    if (n > 30) {
      rlang::warn(
        paste0(n, " time series supplied. Showing recent 30.")
      )
      df <- df %>%
        dplyr::filter(!!dim_col %in% utils::tail(unique(!!dim_col), 30))
    }
  }

  p <- ggplot2::ggplot()

  if (n == 1L) {
    if (type == "line") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x = !!time_col, y = value), data = df) +
        scale_color_reviser()
    } else if (type == "point") {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(x = !!time_col, y = value),
          data = df
        ) +
        scale_color_reviser()
    } else if (type == "bar") {
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(x = !!time_col, y = value),
          data = df,
          position = "identity",
          stat = "identity"
        ) +
        scale_color_reviser() +
        scale_fill_reviser()
    } else if (type == "boxplot") {
      rlang::abort(
        "'type' boxplot not supported if 'dim_col' contains one unique value."
      )
    } else {
      rlang::abort("Invalid 'type' argument. Must be either 'line' or 'point'.")
    }
  } else {
    if (type == "line") {
      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(x = !!time_col, y = value, color = !!dim_col),
          data = df
        ) +
        scale_color_reviser()
    } else if (type == "point") {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(x = !!time_col, y = value, color = !!dim_col),
          data = df
        ) +
        scale_color_reviser()
    } else if (type == "bar") {
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(
            x = !!time_col,
            y = value,
            color = !!dim_col,
            fill = !!dim_col
          ),
          position = "identity",
          stat = "identity",
          data = df
        ) +
        scale_color_reviser() +
        scale_fill_reviser()
    } else if (type == "boxplot") {
      p <- p +
        ggplot2::geom_boxplot(
          ggplot2::aes(x = !!time_col, y = value, fill = factor(!!time_col)),
          data = df
        ) +
        scale_fill_reviser() +
        theme_reviser(legend.position = "none")
    } else {
      rlang::abort("Invalid 'type' argument. Must be either 'line' or 'point'.")
    }
  }

  # labels and title
  p <- p + ggplot2::ylab(ylab)
  if (!missing("title")) {
    if (missing("subtitle")) subtitle <- NULL
    p <- p + ggplot2::ggtitle(label = title, subtitle = subtitle)
  }

  if (!type == "boxplot") {
    if (n > 5) {
      p <- p +
        theme_reviser(legend.position = "right", legend.direction = "vertical")
    } else {
      p <- p +
        theme_reviser(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
    }
  }
  p
}


#' Custom Visualization Theme and Color Scales for Reviser
#'
#' These functions provide a custom visualization theme and color scales for
#' use with ggplot2, inspired by the `tsbox` package.
#'
#' @param base_size Numeric. The base font size for the theme. Default is 12.
#' @param legend.position Character. Position of the legend.
#' Default is "bottom".
#' @param legend.direction Character. Direction of the legend.
#' Default is "horizontal".
#' @param ... Additional arguments passed to the ggplot2 scale functions.
#'
#' @return A customized ggplot2 theme, color palette, or scale.
#'
#' @details
#' \itemize{
#' \item{`theme_reviser`: Defines a minimal theme with custom adjustments for
#' axis titles, plot titles, subtitles, captions, and legend positioning.}
#' \item{`colors_reviser`: Provides a predefined set of colors, including a
#' soft black, a palette suitable for colorblind readers, and additional
#' colors for extended use.}
#' \item{`scale_color_reviser`: A ggplot2 color scale that uses the custom
#' `colors_reviser` palette.}
#' \item{`scale_fill_reviser`: A ggplot2 fill scale that uses the custom
#' `colors_reviser` palette.}
#'}
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_reviser() +
#'   scale_color_reviser()
#'
#' @export
theme_reviser <- function(
  base_size = 12,
  legend.position = "bottom",
  legend.direction = "horizontal"
) {
  half_line <- base_size / 2
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = "grey10",
        margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)
      ),
      plot.title = ggplot2::element_text(
        color = "grey10",
        face = "bold",
        margin = ggplot2::margin(t = half_line * 2, b = half_line * 0.7),
        hjust = 0,
        size = ggplot2::rel(1.2)
      ),
      plot.subtitle = ggplot2::element_text(
        color = "grey10",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.9),
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        color = "grey50",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.8)
      ),
      panel.grid = ggplot2::element_line(linewidth = 0.2),
      axis.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.7)
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.9)
      ),
      legend.position = legend.position,
      legend.direction = legend.direction
    )
}


#' @export
#' @name theme_reviser
colors_reviser <- function() {
  c(
    # A soft black
    "#4D4D4D",
    # colorblindr
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#E69F00",
    "#56B4E9",
    "#CC79A7",
    "#F0E442",
    "#999999",
    # Additional Colors
    "#8D0808",
    "#461E78",
    "#4AFFF0",
    "#34BDCC",
    "#4F61A1",
    "#440A4F",
    "#C3FBC4",
    "#85F9D6",
    "#79C7AD",
    "#A6CC7A",
    "#DFFF7B",
    "#8D7B88",
    "#4E414F",
    "#BAADB5",
    "#2D2538",
    "#837A80",
    "#FFF68F",
    "#800080",
    "#F8B1CC",
    "#C29BFF",
    "#FFD700",
    "#FF6347"
  )
}

#' @export
#' @name theme_reviser
scale_color_reviser <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = scales::manual_pal(colors_reviser()),
    ...
  )
}

#' @export
#' @name theme_reviser
scale_fill_reviser <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = scales::manual_pal(colors_reviser()),
    ...
  )
}
