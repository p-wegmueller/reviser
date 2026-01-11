# Summarize the results of a kk_model object.

This function calculates and prints the Mean Squared Error (MSE), Root
Mean Squared Error (RMSE), and Mean Absolute Error (MAE) of the filtered
state variables against both the final release and the true efficient
release.

## Usage

``` r
# S3 method for class 'kk_model'
summary(object, ...)
```

## Arguments

- object:

  A list of class 'kk_model' produced by the
  [`kk_nowcast`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md)
  function.

- ...:

  Additional arguments (not used).

## Value

A list containing two data frames:

- final_release_metrics:

  A data frame with MSE, RMSE, and MAE against the final release.

- true_efficient_release_metrics:

  A data frame with MSE, RMSE, and MAE against the true efficient
  release.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md)

## Examples

``` r
# Assuming 'kk_model_obj' is the result of kk_nowcast(your_data, ...)
# and 'your_data' is the original data frame.
# results <- summary.kk_model(kk_model_obj, your_data)
```
