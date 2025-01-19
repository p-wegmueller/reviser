
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reviser <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
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
devtools::install_github("p-wegmueller/ReviseR")
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
    pub_date > as.Date("2009-01-01") & pub_date < as.Date("2009-10-01"),
    time < as.Date("2010-01-01") & time > as.Date("2008-01-01")
    ),
  type = "line")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
  
final_release <- get_nth_release(gdp_long, n = 16)

df <- get_nth_release(gdp_long, n = 0:6)

summary <- get_revision_analysis(df, final_release)

efficient_release <- get_first_efficient_release(df, final_release)
summary(efficient_release)
#> Efficient release:  1 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.98452 -0.23844 -0.00011  0.24685  1.12034 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.01395    0.03165   0.441     0.66    
#> release_1    0.94074    0.02481  37.920   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3599 on 161 degrees of freedom
#>   (16 observations deleted due to missingness)
#> Multiple R-squared:  0.8993, Adjusted R-squared:  0.8987 
#> F-statistic:  1438 on 1 and 161 DF,  p-value: < 2.2e-16
#> 
#> 
#> Test summary: 
#> Linear hypothesis test
#> 
#> Hypothesis:
#> (Intercept) = 0
#> release_1 = 1
#> 
#> Model 1: restricted model
#> Model 2: final ~ release_1
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1    163                      
#> 2    161  2 5.1444 0.006828 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
