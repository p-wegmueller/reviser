# Print Method for Publication Date Vintages

Print method for objects of class `tbl_pubdate`. This method delegates
to the tibble print method, which will automatically call
`tbl_sum.tbl_pubdate` to generate the custom header.

## Usage

``` r
# S3 method for class 'tbl_pubdate'
print(x, ...)
```

## Arguments

- x:

  An object of class `tbl_pubdate`.

- ...:

  Additional arguments passed to the next print method.

## Value

The input `x` is returned invisibly.

## See also

Other helpers:
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id == "US")
wide_data <- vintages_wide(df)
print(wide_data$US)
#> # Vintages data (publication date format):
#> # Time periods:                            179
#> # Vintages:                                89
#>    time       `2002-10-01` `2003-01-01` `2003-04-01` `2003-07-01` `2003-10-01`
#>    <date>            <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 1980-01-01      1239725      1239725      1239725      1239725      1305325
#>  2 1980-04-01      1214450      1214450      1214450      1214450      1278975
#>  3 1980-07-01      1212575      1212575      1212575      1212575      1276850
#>  4 1980-10-01      1234150      1234150      1234150      1234150      1300525
#>  5 1981-01-01      1258125      1258125      1258125      1258125      1326875
#>  6 1981-04-01      1249325      1249325      1249325      1249325      1316525
#>  7 1981-07-01      1264200      1264200      1264200      1264200      1332450
#>  8 1981-10-01      1249275      1249275      1249275      1249275      1315850
#>  9 1982-01-01      1228575      1228575      1228575      1228575      1294275
#> 10 1982-04-01      1233875      1233875      1233875      1233875      1301225
#> # ℹ 169 more rows
#> # ℹ 84 more variables: `2004-01-01` <dbl>, `2004-04-01` <dbl>,
#> #   `2004-07-01` <dbl>, `2004-10-01` <dbl>, `2005-01-01` <dbl>,
#> #   `2005-04-01` <dbl>, `2005-07-01` <dbl>, `2005-10-01` <dbl>,
#> #   `2006-01-01` <dbl>, `2006-04-01` <dbl>, `2006-07-01` <dbl>,
#> #   `2006-10-01` <dbl>, `2007-01-01` <dbl>, `2007-04-01` <dbl>,
#> #   `2007-07-01` <dbl>, `2007-10-01` <dbl>, `2008-01-01` <dbl>, …
```
