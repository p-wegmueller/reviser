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
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)
