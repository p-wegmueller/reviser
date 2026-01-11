# Plot JVN Model Results

Plot JVN Model Results

## Usage

``` r
# S3 method for class 'kk_model'
plot(x, state = NULL, type = "filtered", ...)
```

## Arguments

- x:

  An object of class 'jvn_model'

- state:

  String. The name of the state to visualize (e.g., "state_1").

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
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)
