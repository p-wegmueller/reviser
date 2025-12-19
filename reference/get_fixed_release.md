# Extract Vintage Values from a Data Frame

Some statistical agencies make a final revision of their data after a
certain period of time in a give month in the year. This function
extracts values from a given month or quarter a specified number of
years after the initial release.

## Usage

``` r
get_fixed_release(df, years, month = NULL, quarter = NULL)
```

## Arguments

- df:

  A data frame containing columns `pub_date` (publication date) and
  ` time` (observation date).

- years:

  The integer number of unrestricted years after `pub_date` for which
  the values should be extracted.

- month:

  An optional parameter specifying the target month as a name ("July")
  or an integer (7). Cannot be used with `quarter`.

- quarter:

  An optional parameter specifying the target quarter (1-4). Cannot be
  used with `month`.

## Value

A filtered data frame containing values matching the specified criteria.

## See also

Other revision utilities:
[`get_days_to_release()`](https://p-wegmueller.github.io/reviser/reference/get_days_to_release.md),
[`get_first_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_release.md),
[`get_latest_release()`](https://p-wegmueller.github.io/reviser/reference/get_latest_release.md),
[`get_nth_release()`](https://p-wegmueller.github.io/reviser/reference/get_nth_release.md),
[`get_releases_by_date()`](https://p-wegmueller.github.io/reviser/reference/get_releases_by_date.md),
[`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.md)

## Examples

``` r
df <- dplyr::filter(reviser::gdp, id=="US")
dta <- get_fixed_release(df, month = "July", years = 3)
dta <- get_fixed_release(df, month = 7, years = 3)
dta <- get_fixed_release(df, quarter = 3, years = 3)
```
