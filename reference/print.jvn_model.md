# Print Method for JVN Model

Default print method for `jvn_model` objects. Wraps the `summary` method
for a consistent output.

## Usage

``` r
# S3 method for class 'jvn_model'
print(x, ...)
```

## Arguments

- x:

  An object of class `jvn_model`.

- ...:

  Additional arguments passed to `summary.jvn_model`.

## Value

The function returns the input `x` invisibly.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)
