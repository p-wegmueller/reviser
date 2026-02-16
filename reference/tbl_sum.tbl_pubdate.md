# Tibble Summary for Publication Date Vintages

Provides a custom header for objects of class `tbl_pubdate` when
printed. This method is called automatically by pillar when printing
tibbles.

## Usage

``` r
# S3 method for class 'tbl_pubdate'
tbl_sum(x, ...)
```

## Arguments

- x:

  An object of class `tbl_pubdate`.

- ...:

  Additional arguments (unused).

## Value

A named character vector where names are labels and values are the
corresponding information. The vector is used by pillar to format the
tibble header.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)
