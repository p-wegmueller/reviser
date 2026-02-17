# Plot Method for Publication Date Vintages

Plot Method for Publication Date Vintages

## Usage

``` r
# S3 method for class 'tbl_pubdate'
plot(x, ...)
```

## Arguments

- x:

  An object of class `tbl_pubdate`.

- ...:

  Additional arguments passed to plot_vintages.

## Value

A ggplot2 object.

## See also

Other revision graphs:
[`plot.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/plot.tbl_release.md),
[`plot_vintages()`](https://p-wegmueller.github.io/reviser/reference/plot_vintages.md),
[`theme_reviser()`](https://p-wegmueller.github.io/reviser/reference/theme_reviser.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
plot(df)
```
