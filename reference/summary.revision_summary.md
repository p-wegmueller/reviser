# Summary Method for Revision Summary

Summary Method for Revision Summary

## Usage

``` r
# S3 method for class 'revision_summary'
summary(object, interpretation = TRUE, ...)
```

## Arguments

- object:

  An object of class `revision_summary`.

- interpretation:

  Logical. If TRUE, provides interpretation of key statistics. Default
  is TRUE.

- ...:

  Additional arguments passed to print.

## Value

The function returns the input `object` invisibly.

## See also

Other revision analysis:
[`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md),
[`diagnose.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md),
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md),
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md)

## Examples

``` r
# Example usage with revision analysis results
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

# Get revision analysis results
results <- get_revision_analysis(df, final_release, degree = 5)

# Summarize revision quality
summary(results)
#> 
#> === Revision Analysis Summary ===
#> 
#> # A tibble: 4 × 39
#>   id    release       N Frequency `Bias (mean)` `Bias (p-value)`
#>   <chr> <chr>     <dbl>     <dbl>         <dbl>            <dbl>
#> 1 US    release_0   178         4         0.023            0.3  
#> 2 US    release_1   177         4         0.02             0.347
#> 3 US    release_2   176         4         0.022            0.311
#> 4 US    release_3   175         4         0.032            0.133
#> # ℹ 33 more variables: `Bias (robust p-value)` <dbl>, Minimum <dbl>,
#> #   Maximum <dbl>, `10Q` <dbl>, Median <dbl>, `90Q` <dbl>, MAR <dbl>,
#> #   `Std. Dev.` <dbl>, `Noise/Signal` <dbl>, Correlation <dbl>,
#> #   `Correlation (p-value)` <dbl>, `Autocorrelation (1st)` <dbl>,
#> #   `Autocorrelation (1st p-value)` <dbl>,
#> #   `Autocorrelation up to 1yr (Ljung-Box p-value)` <dbl>, `Theil's U1` <dbl>,
#> #   `Theil's U2` <dbl>, `Seasonality (Friedman p-value)` <dbl>, …
#> 
#> === Interpretation ===
#> 
#> id=US, release=release_0:
#>   • No significant bias detected (p = 0.255 )
#>   • Moderate revision volatility (Noise/Signal = 0.27 )
#>   • Significant negative correlation between revisions and initial values (ρ = -0.237 , p = 0.001 )
#>   • Revisions contain NEWS (p = 0.002 ): systematic information
#>   • Revisions do NOT contain noise (p = 0.372 )
#>   • Good forecast accuracy (Theil's U1 = 0.115 )
#>   • Excellent sign prediction (94.9% correct)
#> 
#> id=US, release=release_1:
#>   • No significant bias detected (p = 0.288 )
#>   • Moderate revision volatility (Noise/Signal = 0.257 )
#>   • Significant negative correlation between revisions and initial values (ρ = -0.243 , p = 0.001 )
#>   • Revisions contain NEWS (p = 0 ): systematic information
#>   • Revisions do NOT contain noise (p = 0.447 )
#>   • Good forecast accuracy (Theil's U1 = 0.11 )
#>   • Excellent sign prediction (96% correct)
#> 
#> id=US, release=release_2:
#>   • No significant bias detected (p = 0.238 )
#>   • Moderate revision volatility (Noise/Signal = 0.26 )
#>   • Significant negative correlation between revisions and initial values (ρ = -0.237 , p = 0.002 )
#>   • Revisions contain NEWS (p = 0.001 ): systematic information
#>   • Revisions do NOT contain noise (p = 0.342 )
#>   • Good forecast accuracy (Theil's U1 = 0.111 )
#>   • Excellent sign prediction (95.5% correct)
#> 
#> id=US, release=release_3:
#>   • No significant bias detected (p = 0.053 )
#>   • Moderate revision volatility (Noise/Signal = 0.252 )
#>   • Significant negative correlation between revisions and initial values (ρ = -0.251 , p = 0.001 )
#>   • Revisions contain NEWS (p = 0 ): systematic information
#>   • Revisions do NOT contain noise (p = 0.103 )
#>   • Significant autocorrelation in revisions 
#>               (ρ₁ = -0.182 ): revisions are persistent
#>   • Good forecast accuracy (Theil's U1 = 0.108 )
#>   • Excellent sign prediction (95.4% correct)
```
