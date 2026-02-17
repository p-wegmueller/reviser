# Print Method for Release Vintages

Print method for objects of class `tbl_release`.

## Usage

``` r
# S3 method for class 'tbl_release'
print(x, ...)
```

## Arguments

- x:

  An object of class `tbl_release`.

- ...:

  Additional arguments passed to the next print method.

## Value

The input `x` is returned invisibly.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
release_data <- get_nth_release(df, n = 0:3)
print(release_data)
#> # Vintages data (release format):
#> # Format:                         long
#> # Time periods:                   179
#> # Releases:                       4
#> # IDs:                            1
#>    time       pub_date     value id    release  
#>    <date>     <date>       <dbl> <chr> <chr>    
#>  1 1980-01-01 2002-10-01 1239725 US    release_0
#>  2 1980-01-01 2003-01-01 1239725 US    release_1
#>  3 1980-01-01 2003-04-01 1239725 US    release_2
#>  4 1980-01-01 2003-07-01 1239725 US    release_3
#>  5 1980-04-01 2002-10-01 1214450 US    release_0
#>  6 1980-04-01 2003-01-01 1214450 US    release_1
#>  7 1980-04-01 2003-04-01 1214450 US    release_2
#>  8 1980-04-01 2003-07-01 1214450 US    release_3
#>  9 1980-07-01 2002-10-01 1212575 US    release_0
#> 10 1980-07-01 2003-01-01 1212575 US    release_1
#> # ℹ 700 more rows
```
