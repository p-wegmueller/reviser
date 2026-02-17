# Print Method for Efficient Release Results

Print Method for Efficient Release Results

## Usage

``` r
# S3 method for class 'lst_efficient'
print(x, ...)
```

## Arguments

- x:

  An object of class `lst_efficient`.

- ...:

  Additional arguments (not used).

## Value

The function returns the input `x` invisibly.

## See also

Other revision analysis:
[`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md),
[`diagnose.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md),
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)

## Examples

``` r
df <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
  n = 0:3
)

final_release <- get_nth_release(
  tsbox::ts_pc(dplyr::filter(reviser::gdp, id == "US")),
  n = 10
)

result <- get_first_efficient_release(df, final_release, significance = 0.05)
print(result)
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.89186 -0.12669  0.02046  0.11475  0.97986 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.00299    0.02223   0.134    0.893    
#> release_0    0.97412    0.01692  57.567   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.2518 on 166 degrees of freedom
#>   (10 observations deleted due to missingness)
#> Multiple R-squared:  0.9523, Adjusted R-squared:  0.952 
#> F-statistic:  3314 on 1 and 166 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> 
#> Linear hypothesis test:
#> (Intercept) = 0
#> release_0 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_0
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    168                 
#> 2    166  2 1.9283 0.1486
```
