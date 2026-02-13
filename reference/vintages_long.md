# Convert Vintages Data to Long Format

Converts a vintages dataset from wide format to long format, optionally
adding `id` if the input is a list of data frames. The long format
contains one row per combination of `time` and `names_to` (e.g.,
`pub_date` or `release`), with values stored in a single `value` column.

## Usage

``` r
vintages_long(df, names_to = "pub_date", keep_na = FALSE)
```

## Arguments

- df:

  A data frame, tibble, or list of data frames containing vintages data
  in wide format.

- names_to:

  The name of the column to create from the wide-format column names.
  Must be either `"pub_date"` (default) or `"release"`.

- keep_na:

  Logical. If `TRUE`, retains rows with `NA` values in the `value`
  column. Default is `FALSE`.

## Value

A long-format data frame or tibble. If the input is a list of
wide-format data frames, the output will be a single combined
long-format data frame.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`vintages_wide()`](https://p-wegmueller.github.io/reviser/reference/vintages_wide.md)

## Examples

``` r
# Example wide-format data
long_data <- dplyr::filter(reviser::gdp, id == "US")

# Convert to wide format
wide_data <- vintages_wide(long_data)

# Example list of wide-format data frames
wide_list <- list(
  A = wide_data$US,
  B = wide_data$US
)

# Convert list to long format
long_data <- vintages_long(wide_list, names_to = "pub_date")
```
