
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reviser <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/p-wegmueller/reviser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/p-wegmueller/reviser/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**reviser** is an R package designed for working with time-series
vintages data. The package provides tools to clean, visualize, and
analyze time-series revisions.

## Installation

You can install the development version of reviser from
[GitHub](https://github.com/) with:

``` r
# Install devtools if not already installed
# install.packages("devtools")

# Install the reviser package
devtools::install_github("p-wegmueller/reviser")
```

## Usage

``` r
library(reviser)
suppressMessages(library(dplyr))

gdp <- gdp %>% tsbox::ts_pc() %>% tsbox::ts_span(start = "1980-01-01") 

gdp_wide <- vintages_wide(gdp)

gdp_long <- vintages_long(gdp_wide, keep_na = FALSE)

plot_vintages(
  gdp_long %>% 
  filter(
    pub_date >= as.Date("2009-01-01") & pub_date < as.Date("2010-01-01"),
    time < as.Date("2010-01-01") & time > as.Date("2008-01-01")
    ),
  type = "line",
  title = "Revisions of GDP during the financial crisis",
  subtitle = "qoq growth rates")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
  
final_release <- get_nth_release(gdp_long, n = 16)

df <- get_nth_release(gdp_long, n = 0:6)

summary <- get_revision_analysis(df, final_release)
print(summary)
#> # A tibble: 7 × 10
#>   release       N `Bias (mean)` `Bias (p-value)` Minimum Maximum `Std. Dev.`
#>   <chr>     <dbl>         <dbl>            <dbl>   <dbl>   <dbl>       <dbl>
#> 1 release_0   163       0.00983            0.762  -1.34     1.60       0.414
#> 2 release_1   163      -0.0204             0.477  -0.981    1.13       0.365
#> 3 release_2   163      -0.0223             0.427  -0.981    1.13       0.357
#> 4 release_3   163      -0.0158             0.532  -0.932    1.09       0.323
#> 5 release_4   163      -0.0121             0.617  -0.932    1.05       0.307
#> 6 release_5   163      -0.0115             0.600  -0.930    1.05       0.279
#> 7 release_6   163      -0.0216             0.279  -0.603    1.05       0.254
#> # ℹ 3 more variables: `Noise/Signal` <dbl>, Correlation <dbl>,
#> #   `Correlation (p-value)` <dbl>
```

``` r

efficient_release <- get_first_efficient_release(df, final_release)
summary(efficient_release)
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.44976 -0.24845  0.06358  0.23515  0.90387 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.04755    0.03551   1.339    0.182    
#> release_0    0.93135    0.02827  32.943   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.4077 on 161 degrees of freedom
#>   (16 observations deleted due to missingness)
#> Multiple R-squared:  0.8708, Adjusted R-squared:   0.87 
#> F-statistic:  1085 on 1 and 161 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> Linear hypothesis test
#> 
#> Hypothesis:
#> (Intercept) = 0
#> release_0 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_0
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    163                    
#> 2    161  2 2.8555 0.06045 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
