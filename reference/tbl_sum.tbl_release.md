# Tibble Summary for Release Vintages

Provides a custom header for objects of class `tbl_release` when
printed.

## Usage

``` r
# S3 method for class 'tbl_release'
tbl_sum(x, ...)
```

## Arguments

- x:

  An object of class `tbl_release`.

- ...:

  Additional arguments (unused).

## Value

A named character vector where names are labels and values are the
corresponding information.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
release_data <- get_nth_release(df, n = 0:3)
pillar::tbl_sum(release_data)
#> Vintages data (release format)                         Format 
#>                             ""                         "long" 
#>                   Time periods                       Releases 
#>                          "179"                            "4" 
#>                            IDs 
#>                            "1" 
```
