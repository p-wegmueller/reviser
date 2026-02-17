# Print Method for Revision Summary

Print Method for Revision Summary

## Usage

``` r
# S3 method for class 'revision_summary'
print(x, interpretation = TRUE, digits = 3, ...)
```

## Arguments

- x:

  An object of class `revision_summary`.

- interpretation:

  Logical. If TRUE, provides interpretation of key statistics. Default
  is TRUE.

- digits:

  Integer. Number of digits to display. Default is 3.

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
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)

## Examples

``` r
df <- dplyr::select(
  get_nth_release(
    na.omit(
      tsbox::ts_pc(
        dplyr::filter(reviser::gdp, id == "US")
      )
    ),
    n = 0:3
  ),
  -"pub_date"
)

final_release <- dplyr::select(
  get_nth_release(
    na.omit(
      tsbox::ts_pc(
        dplyr::filter(reviser::gdp, id == "US")
      )
    ),
    n = "latest"
  ),
  -"pub_date"
)

results <- get_revision_analysis(df, final_release, degree = 1)
print(results)
#> 
#> === Revision Analysis Summary ===
#> 
#> # A tibble: 4 × 14
#>   id    release       N `Bias (mean)` `Bias (p-value)` `Bias (robust p-value)`
#>   <chr> <chr>     <dbl>         <dbl>            <dbl>                   <dbl>
#> 1 US    release_0   178         0.023            0.3                     0.255
#> 2 US    release_1   177         0.02             0.347                   0.288
#> 3 US    release_2   176         0.022            0.311                   0.238
#> 4 US    release_3   175         0.032            0.133                   0.053
#> # ℹ 8 more variables: Minimum <dbl>, Maximum <dbl>, `10Q` <dbl>, Median <dbl>,
#> #   `90Q` <dbl>, MAR <dbl>, `Std. Dev.` <dbl>, `Noise/Signal` <dbl>
#> 
#> === Interpretation ===
#> 
#> id=US, release=release_0:
#>   • No significant bias detected (p = 0.255 )
#>   • Moderate revision volatility (Noise/Signal = 0.27 )
#> 
#> id=US, release=release_1:
#>   • No significant bias detected (p = 0.288 )
#>   • Moderate revision volatility (Noise/Signal = 0.257 )
#> 
#> id=US, release=release_2:
#>   • No significant bias detected (p = 0.238 )
#>   • Moderate revision volatility (Noise/Signal = 0.26 )
#> 
#> id=US, release=release_3:
#>   • No significant bias detected (p = 0.053 )
#>   • Moderate revision volatility (Noise/Signal = 0.252 )

# Print without interpretation
print(results, interpretation = FALSE)
#> 
#> === Revision Analysis Summary ===
#> 
#> # A tibble: 4 × 14
#>   id    release       N `Bias (mean)` `Bias (p-value)` `Bias (robust p-value)`
#>   <chr> <chr>     <dbl>         <dbl>            <dbl>                   <dbl>
#> 1 US    release_0   178         0.023            0.3                     0.255
#> 2 US    release_1   177         0.02             0.347                   0.288
#> 3 US    release_2   176         0.022            0.311                   0.238
#> 4 US    release_3   175         0.032            0.133                   0.053
#> # ℹ 8 more variables: Minimum <dbl>, Maximum <dbl>, `10Q` <dbl>, Median <dbl>,
#> #   `90Q` <dbl>, MAR <dbl>, `Std. Dev.` <dbl>, `Noise/Signal` <dbl>
```
