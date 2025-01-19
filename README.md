
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

## Usage

``` r
library(reviser)
library(dplyr)

gdp <- data(gdp) %>% tsbox::ts_pc()

gdp_wide <- vintages_wide(gdp)

gdp_long <- vintages_long(gdp_wide, keep_na = FALSE)

plot_vintages(
  gdp_long %>% 
  filter(
    pub_date > as.Date("2009-01-01") & pub_date < as.Date("2009-10-01"),
    time < as.Date("2010-01-01") & time > as.Date("2008-01-01")
    ),
  type = "line")
  
final_release <- get_nth_release(gdp_long_pc, n = 16)

df <- get_nth_release(gdp_long_pc, n = 0:6)

summary <- get_revision_analysis(df, final_release)

efficient_release <- get_first_efficient_release(df, final_release)
summary(efficient_release)
```
