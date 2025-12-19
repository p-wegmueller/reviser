# Extract the Latest Data Release (Vintage)

Filters the input dataset to return the most recent release (or vintage)
for each time period.

## Usage

``` r
get_latest_release(df)
```

## Arguments

- df:

  A data frame containing data vintages. The data frame must include the
  columns `pub_date` (publication date of the release) and `time` (the
  corresponding time period for the data).

## Value

A filtered data frame containing only the most recent release(s). The
resulting data frame is assigned the class `tbl_release` to indicate its
structure.

## Details

For each time period, the function identifies the release with the
latest publication date (`pub_date`) and adds a column `release` that
labels the release as `release_N`, where `N` is the release index (zero
indexed).

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id=="US")

# Get the latest release for each time period
latest_release <- get_latest_release(df)
```
