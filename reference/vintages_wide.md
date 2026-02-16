# Convert Vintages Data to Wide Format

Converts a vintages dataset from long format to wide format, optionally
grouping by `id` if present. The wide format uses one column per unique
value of the `names_from` parameter, with observation dates (`time`) as
rows and values (`value`) as cell contents.

## Usage

``` r
vintages_wide(df, names_from = "pub_date")
```

## Arguments

- df:

  A data frame or tibble containing vintages data in long format.

- names_from:

  The name of the column whose unique values will be used as column
  names in the wide format. Defaults to `"pub_date"`. Other:
  `"release"`.

## Value

If an `id` column is present, the function returns a named list of
wide-format data frames, one for each unique `id`. Otherwise, it returns
a single wide-format data frame.

## See also

Other helpers:
[`print.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_pubdate.md),
[`print.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/print.tbl_release.md),
[`summary.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_pubdate.md),
[`summary.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/summary.tbl_release.md),
[`tbl_sum.tbl_pubdate()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_pubdate.md),
[`tbl_sum.tbl_release()`](https://p-wegmueller.github.io/reviser/reference/tbl_sum.tbl_release.md),
[`vintages_long()`](https://p-wegmueller.github.io/reviser/reference/vintages_long.md)

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
long_data1 <- vintages_long(wide_data, names_to = "pub_date")
long_data2 <- vintages_long(wide_list, names_to = "pub_date")
```
