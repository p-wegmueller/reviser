# Summary of Efficient Release Models

Provides a detailed summary of the regression model and hypothesis test
for the first efficient release identified by the
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md)
function.

## Usage

``` r
# S3 method for class 'lst_efficient'
summary(object, ...)
```

## Arguments

- object:

  An output object from the `get_first_efficient_release` function. The
  object must be of class `list_eff_rel`.

- ...:

  Additional arguments (not used).

## Value

Returns a tibble with the following columns:

- `id`: The identifier of the time series (if present in input data).

- `e`: The index of the first efficient release.

- `alpha`: The intercept coefficient of the regression model.

- `beta`: The coefficient of the slope.

- `p-value`: The p-value for the joint hypothesis (alpha = 0 and beta =
  1).

- `n_tested`: The number of releases tested.

## Details

This function prints the following information:

- The index of the first efficient release.

- A summary of the regression model fitted for the efficient release,
  which includes coefficients, R-squared values, and other relevant
  statistics.

- The hypothesis test results for the efficient release, showing the
  test statistic and p-value for the null hypothesis of unbiasedness and
  efficiency.

The function assumes the object includes:

- `e`: The index of the first efficient release (0-based).

- `models`: A list of linear regression models for each release.

- `tests`: A list of hypothesis test results corresponding to each
  release.

## See also

Other revision analysis:
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md)

## Examples

``` r
# Example usage
df <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp , id=="US")),
  n = 1:4
)

final_release <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp, id=="US")),
  n = 10
)

# Identify the first efficient release
result <- get_first_efficient_release(df, final_release, significance = 0.05)
summary(result)
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.88971 -0.12583  0.02686  0.12286  0.69564 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.0002137  0.0204027    0.01    0.992    
#> release_1   0.9757504  0.0154917   62.98   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.231 on 166 degrees of freedom
#>   (9 observations deleted due to missingness)
#> Multiple R-squared:  0.9598, Adjusted R-squared:  0.9596 
#> F-statistic:  3967 on 1 and 166 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_1 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_1
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    168                 
#> 2    166  2 2.2448 0.1092
```
