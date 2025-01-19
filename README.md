
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
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo p-wegmueller/ReviseR@HEAD
#> rlang      (1.1.4     -> 1.1.5    ) [CRAN]
#> colorspace (2.1-0     -> 2.1-1    ) [CRAN]
#> cpp11      (0.5.0     -> 0.5.1    ) [CRAN]
#> withr      (3.0.0     -> 3.0.2    ) [CRAN]
#> gtable     (0.3.5     -> 0.3.6    ) [CRAN]
#> pillar     (1.9.0     -> 1.10.1   ) [CRAN]
#> SparseM    (1.83      -> 1.84-2   ) [CRAN]
#> broom      (1.0.6     -> 1.0.7    ) [CRAN]
#> RcppEigen  (0.3.4.0.0 -> 0.3.4.0.2) [CRAN]
#> Rcpp       (1.0.12    -> 1.0.14   ) [CRAN]
#> nloptr     (2.0.3     -> 2.1.1    ) [CRAN]
#> minqa      (1.2.7     -> 1.2.8    ) [CRAN]
#> lme4       (1.1-35.3  -> 1.1-36   ) [CRAN]
#> quantreg   (5.98      -> 5.99.1   ) [CRAN]
#> pbkrtest   (0.5.2     -> 0.5.3    ) [CRAN]
#> abind      (1.4-5     -> 1.4-8    ) [CRAN]
#> sandwich   (3.1-0     -> 3.1-1    ) [CRAN]
#> car        (3.1-2     -> 3.1-3    ) [CRAN]
#> Installing 18 packages: rlang, colorspace, cpp11, withr, gtable, pillar, SparseM, broom, RcppEigen, Rcpp, nloptr, minqa, lme4, quantreg, pbkrtest, abind, sandwich, car
#> Installing packages into '/private/var/folders/pz/59xk8g7n4t7cr15jnn39_7d40000gn/T/Rtmp7KXs60/temp_libpathdf76d3512c4'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/pz/59xk8g7n4t7cr15jnn39_7d40000gn/T//RtmpX1qNVx/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/pz/59xk8g7n4t7cr15jnn39_7d40000gn/T/RtmpX1qNVx/remotes2ffc17d351d4/p-wegmueller-ReviseR-02fd9aa165b81c8794890d3a1a808cbecb1ba72d/DESCRIPTION’ ... OK
#> * preparing ‘reviser’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘reviser_0.1.0.9000.tar.gz’
#> Installing package into '/private/var/folders/pz/59xk8g7n4t7cr15jnn39_7d40000gn/T/Rtmp7KXs60/temp_libpathdf76d3512c4'
#> (as 'lib' is unspecified)
```

## Usage

``` r
library(reviser)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r

gdp <- gdp %>% tsbox::ts_pc()

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
#> Efficient release:  0 
#> 
#> Model summary: 
#> 
#> Call:
#> stats::lm(formula = formula, data = df_wide)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.45694 -0.21023  0.03547  0.22227  0.93713 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.05930    0.03033   1.955   0.0519 .  
#> release_0    0.93609    0.02380  39.328   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.382 on 200 degrees of freedom
#>   (16 observations deleted due to missingness)
#> Multiple R-squared:  0.8855, Adjusted R-squared:  0.8849 
#> F-statistic:  1547 on 1 and 200 DF,  p-value: < 2.2e-16
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
#>   Res.Df Df      F  Pr(>F)  
#> 1    202                    
#> 2    200  2 3.1605 0.04453 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
