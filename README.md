
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

gdp <- gdp %>% 
  filter(id == "US") %>%
  tsbox::ts_pc() %>% 
  tsbox::ts_span(start = "1980-01-01")

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
#> Warning: Both 'release' and 'pub_date' columns are present in 'df. The
#> 'release' column will be used.
```

``` r
print(summary)
#> # A tibble: 7 × 12
#>   id    release       N `Bias (mean)` `Bias (p-value)` Minimum Maximum    MAR
#>   <chr> <chr>     <dbl>         <dbl>            <dbl>   <dbl>   <dbl>  <dbl>
#> 1 US    release_0   154       -0.0296          0.162    -0.844   1.01  0.188 
#> 2 US    release_1   154       -0.0304          0.133    -0.801   0.706 0.186 
#> 3 US    release_2   154       -0.0287          0.161    -0.930   0.706 0.185 
#> 4 US    release_3   154       -0.0259          0.118    -0.930   0.662 0.111 
#> 5 US    release_4   154       -0.0290          0.0654   -0.930   0.662 0.103 
#> 6 US    release_5   154       -0.0363          0.0101   -0.930   0.468 0.0919
#> 7 US    release_6   154       -0.0336          0.00369  -0.574   0.359 0.0735
#> # ℹ 4 more variables: `Std. Dev.` <dbl>, `Noise/Signal` <dbl>,
#> #   Correlation <dbl>, `Correlation (p-value)` <dbl>
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
#> -0.82139 -0.12266  0.04048  0.13457  1.00888 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.005314   0.029173  -0.182    0.856    
#> release_0    0.963024   0.030626  31.445   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2617 on 152 degrees of freedom
#>   (16 observations deleted due to missingness)
#> Multiple R-squared:  0.8668, Adjusted R-squared:  0.8659 
#> F-statistic: 988.8 on 1 and 152 DF,  p-value: < 2.2e-16
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
#> 1    154                    
#> 2    152  2 2.4805 0.08708 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
