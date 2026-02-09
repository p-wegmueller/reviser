# Plot JVN Model Results

Plot JVN Model Results

## Usage

``` r
# S3 method for class 'jvn_model'
plot(x, state = "true_lag_0", type = "filtered", ...)
```

## Arguments

- x:

  An object of class 'jvn_model'

- state:

  String. The name of the state to visualize.

- type:

  String. Type of estimate to plot: "filtered" or "smoothed".

- ...:

  Additional arguments passed to theme_reviser.

## Value

A ggplot2 object visualizing the specified state estimates.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md)

## Examples

``` r
gdp <- dplyr::filter(
  tsbox::ts_pc(
    reviser::gdp
  ), id %in% c("EA"),
  time >= min(pub_date),
  time <= as.Date("2020-01-01")
)
gdp <- tidyr::drop_na(gdp)
df <- get_nth_release(gdp, n = 0:4)

# Estimate model
result <- jvn_nowcast(
  df = df,
  e = 3,
  ar_order = 2,
  h = 4,
  include_news = TRUE,
  include_noise = TRUE
)
#> Warning: 2 parameter(s) have problematic standard errors (2 NaN)
plot(result)

```
