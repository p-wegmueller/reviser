# Identify the First Efficient Release in Vintage Data

Identifies the first release in a sequence of vintages that is
"efficient" relative to the final release. A release is deemed efficient
if it satisfies specific conditions of unbiasedness and efficiency,
tested using a Mincer-Zarnowitz type linear regression and hypothesis
testing.

## Usage

``` r
get_first_efficient_release(
  df,
  final_release,
  significance = 0.05,
  test_all = FALSE,
  robust = TRUE
)
```

## Arguments

- df:

  A data frame of class `tbl_release` containing the vintage data. It
  must include the columns:

  - `time`: The reference period (e.g., quarter or month).

  - `value`: The observed value for the given release.

  - `release`: The release number or identifier.

- final_release:

  A data frame containing the final release data. This must include the
  columns:

  - `time`: The reference period.

  - `value`: The observed final value for the given period.

- significance:

  A numeric value specifying the significance level for the hypothesis
  test (default is `0.05`).

- test_all:

  A logical value indicating whether to test all releases, even after
  finding the first efficient release (default is `FALSE`).

- robust:

  A logical value indicating whether to use robust HAC standard errors
  (default is `TRUE`).

## Value

A list of class `lst_efficient` with the following elements:

- `e`: The index of the first efficient release. (0 indexed)

- `data`: A long-format data frame containing the vintage data with the
  final release appended.

- `models`: A list of linear regression models fitted for each release.

- `tests`: A list of hypothesis test results for each release.

## Details

The function performs the following steps:

1.  Validates inputs and ensures both `df` and `final_release` are in
    the correct format.

2.  Iteratively tests each release for efficiency using a linear
    regression model of the form: \$\$final = \beta_0 + \beta_1 \cdot
    release_i + \epsilon\$\$ The null hypothesis for efficiency is:

    - \\\beta_0 = 0\\ (no bias)

    - \\\beta_1 = 1\\ (efficiency) Uses heteroskedasticity and
      autocorrelation consistent (HAC) standard errors for robust
      hypothesis testing.

3.  Stops testing when the first efficient release is found (unless
    `test_all = TRUE`).

If no efficient release is found, a warning is issued.

## References

Aruoba, S. Boragan, "Revisions Are Not Well Behaved", Journal of Money,
Credit and Banking, 40(2-3), 319-340, 2008.

## See also

Other revision analysis:
[`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md),
[`diagnose.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md),
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)

## Examples

``` r
# Example data
df <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
  n = 0:3
)

final_release <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
  n = 10
)

# Identify the first efficient release
result <- get_first_efficient_release(
  df,
  final_release,
  significance = 0.05,
  robust = FALSE
)

result <- get_first_efficient_release(df, final_release, significance = 0.05)

# Access the index of the first efficient release
result$e
#> [1] 0
```
