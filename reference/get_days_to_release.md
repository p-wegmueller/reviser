# Calculate the Number of Days Between Period End and First Release

Computes the number of days between the publication date (`pub_date`) of
a release and the time period (`time`) end date for each record in the
dataset.

## Usage

``` r
get_days_to_release(df)
```

## Arguments

- df:

  A data frame containing data vintages. The data frame must include the
  columns `pub_date` (publication date of the release) and `time` (the
  corresponding time period for the data).

## Value

A data frame with an additional column `days_to_release` representing
the number of days between the publication date (`pub_date`) and the
time period (`time`) for each release.

## Details

The function calculates the difference between `pub_date` and `time` for
each row in the dataset. The result is expressed as the number of days
between the release publication date and the corresponding time period
end. If the dataset is in wide format, it will first be transformed into
long format using `vintages_long`.

## See also

Other revision utilities:
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id=="US")

# Calculate days to release
df_with_days <- get_days_to_release(df)
```
