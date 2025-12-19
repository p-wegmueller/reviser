# Extract the First Data Release (Vintage)

Filters the input dataset to return the earliest release (or vintage)
for each time period.

## Usage

``` r
get_first_release(df, diagonal = FALSE)
```

## Arguments

- df:

  A data frame containing data vintages. The data frame must include the
  columns `pub_date` (publication date of the release) and `time` (the
  corresponding time period for the data).

- diagonal:

  Logical. If `TRUE`, the function only returns real first releases.

## Value

A filtered data frame containing only the first release(s). The
resulting data frame is assigned the class `tbl_release` to indicate its
structure.

## Details

For each time period, the function identifies the release with the
earliest publication date (`pub_date`). A new column `release` is added
and labels all rows in the resulting data frame as `release_0`. If
diagonal is set to `TRUE`, the function only returns the real first
releases. That is historic values for which no vintages exist are not
returned.

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id=="US")

# Get the first release for each time period
first_release <- get_first_release(df)
```
