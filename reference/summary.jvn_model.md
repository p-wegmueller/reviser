# Summary Method for JVN Model

Computes and displays a summary of the results from a Jacobs-Van Norden
(JVN) model fit, including convergence status, information criteria, and
parameter estimates.

## Usage

``` r
# S3 method for class 'jvn_model'
summary(object, ...)
```

## Arguments

- object:

  An object of class `jvn_model`.

- ...:

  Additional arguments passed to or from other methods.

## Value

The function returns the input `object` invisibly.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`print.kk_model()`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)

## Examples

``` r
gdp_growth <- dplyr::filter(
  tsbox::ts_pc(
    reviser::gdp
  ), id %in% c("EA"),
  time >= min(pub_date),
  time <= as.Date("2020-01-01")
)
gdp_growth <- tidyr::drop_na(gdp_growth)
df <- get_nth_release(gdp_growth, n = 0:4)

# Estimate model
result <- jvn_nowcast(
  df = df,
  e = 3,
  ar_order = 2,
  h = 4,
  include_news = TRUE,
  include_noise = TRUE
)
#> Warning: 2 parameter(s) have problematic SEs
summary(result)
#> 
#> === Jacobs-Van Norden Model ===
#> 
#> Convergence: Success 
#> Log-likelihood: 123.32 
#> AIC: -228.65 
#> BIC: -198.52 
#> 
#> Parameter Estimates:
#>     Parameter Estimate Std.Error
#>         rho_1    0.737     0.171
#>         rho_2   -0.117     0.174
#>       sigma_e    0.610        NA
#>    sigma_nu_1    0.101        NA
#>    sigma_nu_2    0.001     0.027
#>    sigma_nu_3    0.003     0.035
#>  sigma_zeta_1    0.070     0.006
#>  sigma_zeta_2    0.001     0.010
#>  sigma_zeta_3    0.053     0.005
#> 
```
