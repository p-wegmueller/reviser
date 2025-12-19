# Get Data Releases for a Specific Date

Filters the input dataset to return the releases corresponding to a
specific time period (date).

## Usage

``` r
get_releases_by_date(df, date)
```

## Arguments

- df:

  A data frame containing data vintages. The data frame must include the
  columns `pub_date` (publication date of the release) and `time` (the
  corresponding time period for the data).

- date:

  A Date object specifying the time period (date) for which releases
  should be retrieved.

## Value

A data frame containing the releases for the specified date. The
returned data frame will include the same structure as the input,
filtered to only include rows matching the `date` in the `time` column.

## Details

This function filters the input data based on the specified `date` in
the `time` column. The input dataset must have the `pub_date` and `time`
columns, with `time` being the period to match against the given `date`.
If the dataset is in wide format, it will first be transformed into long
format using the helper function `vintages_long`.

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id=="US")

# Get releases for a specific date
date <- as.Date("2020-04-01")
releases_on_date <- get_releases_by_date(df, date)
```
