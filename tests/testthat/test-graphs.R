# Create example data
df_plot <- data.frame(
  time = rep(
    seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
    3
  ),
  value = runif(36, 50, 100),
  pub_date = rep(c("2022-01-05", "2022-02-07", "2022-03-03"), each = 12)
)
df_plot$pub_date <- as.Date(df_plot$pub_date)

df_plot_wide <- vintages_wide(df_plot)

df_single <- data.frame(
  time = seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12),
  value = runif(12, 50, 100)
)

# Test suite for plot_vintages
test_that("plot_vintages returns a ggplot object", {
  p <- plot_vintages(df_plot)
  expect_s3_class(p, "ggplot")
})

test_that("plot_vintages handles 'line' type correctly", {
  p <- plot_vintages(df_plot, type = "line", dim_col = "pub_date")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("plot_vintages handles 'point' type correctly", {
  p <- plot_vintages(df_plot, type = "point", dim_col = "pub_date")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomPoint"))))
})

test_that("plot_vintages handles 'bar' type correctly", {
  p <- plot_vintages(df_plot, type = "bar", dim_col = "pub_date")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar"))))
})

test_that("plot_vintages handles 'boxplot' type correctly", {
  p <- plot_vintages(df_plot, type = "boxplot", dim_col = "pub_date")
  expect_true(any(sapply(
    p$layers,
    function(l) inherits(l$geom, "GeomBoxplot")
  )))
})

test_that("plot_vintages throws error for invalid 'type'", {
  expect_error(
    plot_vintages(df_plot, type = "histogram"),
    "The 'type' argument must be either 'line', 'point', 'bar' or 'boxplot'."
  )
})

test_that("plot_vintages throws error if 'dim_col' is not found", {
  expect_error(
    plot_vintages(df_plot, dim_col = "non_existent_col"),
    "The column non_existent_col is not found in 'df'."
  )
})

test_that("plot_vintages throws error if 'time_col' is not found", {
  expect_error(
    plot_vintages(df_plot, time_col = "date"),
    "The column date is not found in 'df'."
  )
})

test_that("plot_vintages throws error if 'time_col' is not Date", {
  df_wrong_time <- df_plot %>% mutate(time = as.character(time))
  expect_error(
    plot_vintages(df_wrong_time, time_col = "time"),
    "The 'time_col' argument must be of class 'Date'."
  )
})

test_that("plot_vintages throws error if title, subtitle, or ylab are not characters", {
  expect_error(
    plot_vintages(df_plot, title = 123),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
  expect_error(
    plot_vintages(df_plot, subtitle = TRUE),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
  expect_error(
    plot_vintages(df_plot, ylab = factor("value")),
    "The 'title', 'subtitle', and 'ylab' arguments must be character strings."
  )
})

test_that("plot_vintages handles wide format data", {
  p <- plot_vintages(df_plot_wide)
  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("plot_vintages warns and filters if dim_col has more than 30 unique values", {
  df_many_vintages <- df_plot %>%
    mutate(pub_date = as.Date(pub_date) + rep(1:40, length.out = n()))
  expect_warning(
    plot_vintages(df_many_vintages, dim_col = "pub_date"),
    "36 time series supplied. Showing recent 30."
  )
  p <- suppressWarnings(
    plot_vintages(df_many_vintages, dim_col = "pub_date")
  )
  n_colors <- length(unique(ggplot2::ggplot_build(p)$data[[1]]$colour))
  expect_lte(n_colors, 30)
})


# Test suite for theme_reviser
test_that("theme_reviser returns a ggplot theme object", {
  th <- theme_reviser()
  expect_s3_class(th, "theme")
})

test_that("theme_reviser allows customization of legend position and direction", {
  th_bottom_horiz <- theme_reviser(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
  expect_equal(th_bottom_horiz$legend.position, "bottom")
  expect_equal(th_bottom_horiz$legend.direction, "horizontal")

  th_top_vert <- theme_reviser(
    legend.position = "top",
    legend.direction = "vertical"
  )
  expect_equal(th_top_vert$legend.position, "top")
  expect_equal(th_top_vert$legend.direction, "vertical")
})

# Test suite for colors_reviser
test_that("colors_reviser returns a character vector of colors", {
  cols <- colors_reviser()
  expect_type(cols, "character")
  expect_true(all(grepl("^#([A-Fa-f0-9]{6})$", cols)))
})

# Test suite for scale_color_reviser
test_that("scale_color_reviser returns a discrete color scale", {
  sc <- scale_color_reviser()
  expect_s3_class(sc, "ScaleDiscrete")
  expect_equal(sc$aesthetics, "colour")
})

# Test suite for scale_fill_reviser
test_that("scale_fill_reviser returns a discrete fill scale", {
  sf <- scale_fill_reviser()
  expect_s3_class(sf, "ScaleDiscrete")
  expect_equal(sf$aesthetics, "fill")
})

test_that("scale_color_reviser uses colors_reviser palette", {
  sc <- scale_color_reviser()
  expected_colors <- colors_reviser()
  # Extract the palette function and test it (implementation detail, might change)
  palette_func <- sc$palette(length(expected_colors))
  expect_identical(palette_func, expected_colors)
})

test_that("scale_fill_reviser uses colors_reviser palette", {
  sf <- scale_fill_reviser()
  expected_colors <- colors_reviser()
  # Extract the palette function and test it (implementation detail, might change)
  palette_func <- sf$palette(length(expected_colors))
  expect_identical(palette_func, expected_colors)
})
