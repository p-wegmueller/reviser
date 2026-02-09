# Calculate Revisions in Vintage Data

Computes revisions in vintage data based on specified reference points:
a fixed reference date, the nth release, or a specified interval. This
function allows users to analyze differences between data vintages
across time.

## Usage

``` r
get_revisions(df, interval = NULL, nth_release = NULL, ref_date = NULL)
```

## Arguments

- df:

  A data frame containing vintage data. The data frame must include at
  least the following columns:

  - `pub_date`: The publication date of each vintage.

  - `time`: The reference period (e.g., quarter or month).

  - `value`: The observed value for the given vintage and reference
    period.

- interval:

  A positive integer specifying the lag (in periods) between vintages to
  compute revisions. Defaults to `1` if no other parameter is specified.

- nth_release:

  A positive integer or `"latest"`, specifying the release to use as a
  reference for revisions. If `"latest"`, the most recent vintage is
  used.

- ref_date:

  A date specifying the fixed reference publication date to compare all
  vintages against.

## Value

A data frame (tibble) of class `tbl_revision`, with the following
columns:

- `pub_date`: The publication date of the vintage.

- `time`: The reference period (e.g., quarter or month).

- `value`: The calculated revision, i.e., the difference between the
  observed value and the reference value.

## Details

The function supports three mutually exclusive methods for calculating
revisions:

- **Reference date (`ref_date`)**: Computes revisions relative to a
  fixed publication date.

- **Interval (`interval`)**: Computes revisions relative to vintages
  published `interval` periods earlier.

- **Nth release (`nth_release`)**: Computes revisions relative to the
  nth vintage release for each reference period.

If no method is explicitly specified, `interval = 1` is used by default.

Input validation ensures that only one of `ref_date`, `nth_release`, or
`interval` is specified.

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_fixed_release()`](https://p-wegmueller.github.io/reviser/reference/get_fixed_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md)

## Examples

``` r
# Example data
df <- dplyr::filter(reviser::gdp, id == "US")

# Calculate revisions using an interval of 1
revisions_interval <- get_revisions(df, interval = 1)

# Calculate revisions using a fixed reference date
revisions_date <- get_revisions(df, ref_date = as.Date("2023-02-01"))

# Calculate revisions relative to the nth release (2nd release)
revisions_nth <- get_revisions(df, nth_release = 1)
```
