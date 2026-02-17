# Summary Method for Publication Date Vintages

Summary Method for Publication Date Vintages

## Usage

``` r
# S3 method for class 'tbl_pubdate'
summary(object, ...)
```

## Arguments

- object:

  An object of class `tbl_pubdate`.

- ...:

  Additional arguments (not used).

## Value

The function returns a summary tibble invisibly.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
wide_data <- vintages_wide(df)
summary(wide_data$US)
#> 
#> === Vintages Data Summary (Publication Date Format) ===
#> 
#> Time periods: 179 
#> Time range: 1980-01-01 to 2024-07-01 
#> 
#> Number of vintages: 89 
#> Publication dates:
#>   Earliest: 2002-10-01 
#>   Latest: 2024-10-01 
#> 
#> Missing values: 3916 of 15931 (24.58%) 
```
