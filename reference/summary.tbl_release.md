# Summary Method for Release Vintages

Summary Method for Release Vintages

## Usage

``` r
# S3 method for class 'tbl_release'
summary(object, ...)
```

## Arguments

- object:

  An object of class `tbl_release`.

- ...:

  Additional arguments (not used).

## Value

The function returns a summary tibble invisibly.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
# Long format
release_data <- get_nth_release(df, n = 0:3)
summary(release_data)
#> 
#> === Vintages Data Summary (Release Format) ===
#> 
#> Format: long 
#> Time periods: 179 
#> Time range: 1980-01-01 to 2024-07-01 
#> Number of IDs: 1 
#> IDs: US 
#> 
#> Number of releases: 4 
#> Releases: release_0, release_1, release_2, release_3 
#> 
#> Missing values: 0 of 710 (0%) 

# Wide format
wide_release <- vintages_wide(release_data, names_from = "release")
#> Warning: Ignoring columns: pub_date
summary(wide_release$US)
#> 
#> === Vintages Data Summary (Release Format) ===
#> 
#> Format: wide 
#> Time periods: 179 
#> Time range: 1980-01-01 to 2024-07-01 
#> 
#> Number of releases: 4 
#> Releases: release_0, release_1, release_2, release_3 
#> 
#> Missing values: 6 of 716 (0.84%) 
```
