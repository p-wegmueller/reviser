# Diagnose Revision Quality

Generic function to provide diagnostic summaries for revision analysis
objects.

## Usage

``` r
diagnose(object, ...)
```

## Arguments

- object:

  An object for which diagnostics are desired.

- ...:

  Additional arguments passed to methods.

## Value

Method-specific diagnostic output.

## See also

Other revision analysis:
[`diagnose.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/diagnose.revision_summary.md),
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md),
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)

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

# Diagnose revision quality
diagnose(results)
#> 
#> === Revision Quality Diagnostics ===
#> 
#> US_release_0 :
#> # A tibble: 6 × 4
#>   Metric        Status Value            Assessment                     
#>   <chr>         <chr>  <chr>            <chr>                          
#> 1 Unbiasedness  ✓ PASS p=0.255, μ=0.023 No significant bias            
#> 2 Noise/Signal  ✓ GOOD 0.27             Low revision volatility        
#> 3 News Test     ✗ FAIL p=0.002          Contains systematic information
#> 4 Noise Test    ✓ PASS p=0.372          No noise component             
#> 5 Theil's U1    ✓ GOOD 0.115            Good forecast accuracy         
#> 6 Sign Accuracy ✓ GOOD 94.9%            Excellent sign prediction      
#> 
#> US_release_1 :
#> # A tibble: 6 × 4
#>   Metric        Status Value           Assessment                     
#>   <chr>         <chr>  <chr>           <chr>                          
#> 1 Unbiasedness  ✓ PASS p=0.288, μ=0.02 No significant bias            
#> 2 Noise/Signal  ✓ GOOD 0.257           Low revision volatility        
#> 3 News Test     ✗ FAIL p=0             Contains systematic information
#> 4 Noise Test    ✓ PASS p=0.447         No noise component             
#> 5 Theil's U1    ✓ GOOD 0.11            Good forecast accuracy         
#> 6 Sign Accuracy ✓ GOOD 96%             Excellent sign prediction      
#> 
#> US_release_2 :
#> # A tibble: 6 × 4
#>   Metric        Status Value            Assessment                     
#>   <chr>         <chr>  <chr>            <chr>                          
#> 1 Unbiasedness  ✓ PASS p=0.238, μ=0.022 No significant bias            
#> 2 Noise/Signal  ✓ GOOD 0.26             Low revision volatility        
#> 3 News Test     ✗ FAIL p=0.001          Contains systematic information
#> 4 Noise Test    ✓ PASS p=0.342          No noise component             
#> 5 Theil's U1    ✓ GOOD 0.111            Good forecast accuracy         
#> 6 Sign Accuracy ✓ GOOD 95.5%            Excellent sign prediction      
#> 
#> US_release_3 :
#> # A tibble: 6 × 4
#>   Metric        Status Value            Assessment                     
#>   <chr>         <chr>  <chr>            <chr>                          
#> 1 Unbiasedness  ✓ PASS p=0.053, μ=0.032 No significant bias            
#> 2 Noise/Signal  ✓ GOOD 0.252            Low revision volatility        
#> 3 News Test     ✗ FAIL p=0              Contains systematic information
#> 4 Noise Test    ✓ PASS p=0.103          No noise component             
#> 5 Theil's U1    ✓ GOOD 0.108            Good forecast accuracy         
#> 6 Sign Accuracy ✓ GOOD 95.4%            Excellent sign prediction      
#> 
#> === Overall Assessment ===
#> Passed: 20 of 24 checks ( 83.3 %)
#> Overall: ✓ GOOD - Revisions are of high quality
```
