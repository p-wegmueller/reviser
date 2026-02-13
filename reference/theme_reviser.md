# Custom Visualization Theme and Color Scales for Reviser

These functions provide a custom visualization theme and color scales
for use with ggplot2, inspired by the `tsbox` package.

## Usage

``` r
theme_reviser(
  base_size = 12,
  legend.position = "bottom",
  legend.direction = "horizontal"
)

colors_reviser()

scale_color_reviser(...)

scale_fill_reviser(...)
```

## Arguments

- base_size:

  Numeric. The base font size for the theme. Default is 12.

- legend.position:

  Character. Position of the legend. Default is "bottom".

- legend.direction:

  Character. Direction of the legend. Default is "horizontal".

- ...:

  Additional arguments passed to the ggplot2 scale functions.

## Value

A customized ggplot2 theme, color palette, or scale.

## Details

- `theme_reviser`: Defines a minimal theme with custom adjustments for
  axis titles, plot titles, subtitles, captions, and legend positioning.

- `colors_reviser`: Provides a predefined set of colors, including a
  soft black, a palette suitable for colorblind readers, and additional
  colors for extended use.

- `scale_color_reviser`: A ggplot2 color scale that uses the custom
  `colors_reviser` palette.

- `scale_fill_reviser`: A ggplot2 fill scale that uses the custom
  `colors_reviser` palette.

## See also

Other revision graphs:
[`plot.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_pubdate.md),
[`plot.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md),
[`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md)

Other revision graphs:
[`plot.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_pubdate.md),
[`plot.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md),
[`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md)

Other revision graphs:
[`plot.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_pubdate.md),
[`plot.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md),
[`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md)

Other revision graphs:
[`plot.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_pubdate.md),
[`plot.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md),
[`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md)

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  theme_reviser() +
  scale_color_reviser()

```
