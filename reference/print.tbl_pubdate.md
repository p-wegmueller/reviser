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
