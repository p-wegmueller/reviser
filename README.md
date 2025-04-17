
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reviser <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/p-wegmueller/reviser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/p-wegmueller/reviser/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/p-wegmueller/reviser/workflows/pkgcheck/badge.svg)](https://github.com/p-wegmueller/reviser/actions?query=workflow%3Apkgcheck)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![reviser status
badge](https://marcburri.r-universe.dev/badges/reviser)](https://marcburri.r-universe.dev/reviser)
[![Codecov test
coverage](https://codecov.io/gh/p-wegmueller/reviser/graph/badge.svg)](https://app.codecov.io/gh/p-wegmueller/reviser)
<!-- badges: end -->

**reviser** is an R package designed for working with time-series
vintages data. The package provides tools to clean, visualize, and
analyze time-series revisions.

## Why reviser?

Economic data often arrives in multiple waves—with initial estimates
updated or revised as more complete information becomes available (See
the vignette [**the role and importance of
revisions**](https://p-wegmueller.github.io/reviser/articles/literature-review.html)).
These revisions, while common, can have major implications for how
economic conditions are perceived and how decisions are made by
policymakers, analysts, and markets. Yet, tools to systematically
analyze, visualize, and communicate these revisions are still limited.
This is where the R package **reviser** comes in.

**reviser** is built to support transparent, reproducible workflows for
tracking and interpreting data revisions. Whether you’re evaluating GDP
estimates, inflation statistics, or high-frequency indicators,
**reviser** helps quantify how vintages evolve, assess their
reliability, and highlight cases where revisions may alter the economic
narrative.

### Real-world relevance

The importance of revisions isn’t just academic. They shape real-world
outcomes:

<figure>
<img src="man/figures/gdp_over_time.jpeg"
alt="Revision paths for Euro Area GDP (Sources: BEA, Eurostat)" />
<figcaption aria-hidden="true">Revision paths for Euro Area GDP
(Sources: BEA, Eurostat)</figcaption>
</figure>

- US GDP 2015-Q1 — Weather or Weakness?: A sharp downward revision from
  +0.1% to -0.2% sparked fears of a slowdown—until a later benchmark
  revision lifted it back to +0.8%, highlighting challenges in seasonal
  adjustment.

- Euro Area GDP in 2012Q1 — A Recession Delayed?: A flash estimate of
  0.0% avoided the recession label—until it was revised to -0.1%,
  confirming back-to-back contractions and altering the policy
  discussion at a critical time.

These examples underscore how even small numerical changes can shift
narratives, delay responses, and affect credibility.

**reviser** is an R package designed to streamline the analysis and
visualization of data revisions—especially in the context of official
statistics and macroeconomic indicators. Built with tidy principles and
seamless integration in mind, reviser offers intuitive tools to compare
data vintages, quantify revision patterns, and produce publication-ready
outputs. Whether you’re tracking GDP estimate updates or evaluating
forecast accuracy over time, **reviser** provides a robust and flexible
framework tailored for economists, data analysts, and statistical
agencies alike.

The **reviser** package provides a comprehensive toolkit for analyzing
data revisions — crucial for anyone working with real-time data. It
allows users to **visualize**, **analyze**, and **evaluate** the impact
of data updates across different release vintages, helping to understand
and analyze revision patterns.

Get started:

- Structure your data according to **reviser conventions**. See the
  [*get
  started*](https://p-wegmueller.github.io/reviser/articles/reviser.html)
  vignette

Key features include:

- **Calculate revisions** across vintages using
  [`get_revisions()`](https://p-wegmueller.github.io/reviser/reference/get_revisions.html).
  See the vignette [*Understanding Data
  Revisions*](https://p-wegmueller.github.io/reviser/articles/understanding-revisions.html)
  to learn how to structure and compute revision tables.

- **Analyze revision patterns** and evaluate revision accuracy and bias
  using
  [`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.html).
  For more, read the vignette [*Revision Patterns and
  Statistics*](https://p-wegmueller.github.io/reviser/articles/revision-analysis.html).

- **Detect the first efficient release**, i.e., the earliest vintage
  that closely matches the final values, with
  [`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.html).
  See the vignette [*Efficient Release
  Identification*](https://p-wegmueller.github.io/reviser/articles/efficient-release.html).

- **Nowcast future data revisions** using
  [`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.html),
  a tool to anticipate upcoming changes to early releases. Explore the
  methodology in the vignette [*Nowcasting
  Revisions*](https://p-wegmueller.github.io/reviser/articles/nowcasting-revisions.html).

## Installation

You can install the development version of reviser from
[GitHub](https://github.com/) with:

``` r
# Install the reviser package
remotes::install_github("p-wegmueller/reviser")
```

## Usage

The following example analyzes GDP data revisions for the United States
by transforming the data into a format suitable for vintage analysis,
visualizing revisions during the financial crisis, and assessing how
early estimates compare to the final release. It then identifies the
point at which the estimates become stable and reliable.

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
print(summary)
#> # A tibble: 7 × 14
#>   id    release       N `Bias (mean)` `Bias (p-value)` `Bias (robust p-value)`
#>   <chr> <chr>     <dbl>         <dbl>            <dbl>                   <dbl>
#> 1 US    release_0   162      -0.0172             0.439                   0.459
#> 2 US    release_1   162      -0.0186             0.377                   0.382
#> 3 US    release_2   162      -0.0157             0.458                   0.459
#> 4 US    release_3   162      -0.00553            0.783                   0.768
#> 5 US    release_4   162      -0.0166             0.326                   0.380
#> 6 US    release_5   162      -0.0231             0.138                   0.181
#> 7 US    release_6   162      -0.0208             0.144                   0.191
#> # ℹ 8 more variables: Minimum <dbl>, Maximum <dbl>, `10Q` <dbl>, Median <dbl>,
#> #   `90Q` <dbl>, MAR <dbl>, `Std. Dev.` <dbl>, `Noise/Signal` <dbl>
```

``` r

efficient_release <- get_first_efficient_release(df, final_release)
summary(efficient_release)
#> No efficient release found!
```

## Comparison to [`rjd3revisions`](https://rjdverse.github.io/rjd3revisions/)

The `reviser` package sets itself apart from `rjd3revisions` not only
through its focus on advanced analysis of efficient releases and
nowcasting performance, but also in its pure R implementation, which
avoids external dependencies. In contrast, `rjd3revisions` relies
heavily on Java via the JDemetra+ platform, which can make setup and
integration more complex. `reviser` offers a lightweight, R-native
solution for revision analysis, combining user-friendly tools for data
wrangling, visualization, and evaluation of release efficiency.

## Citation

Burri M, Wegmueller P (2025). reviser: Tools for Studying Revision
Properties in Real-Time Time Series Vintages. R package version 0.1.0,
<https://p-wegmueller.github.io/reviser/>.
