# Jacobs-Van Norden Model for Data Revisions

Implements the Jacobs & Van Norden (2011) state-space model for
analyzing data revisions, allowing for news and noise components in
measurement errors.

## Usage

``` r
jvn_nowcast(
  df,
  e,
  ar_order = 1,
  h = 0,
  include_news = TRUE,
  include_noise = TRUE,
  include_spillovers = FALSE,
  spillover_news = TRUE,
  spillover_noise = TRUE,
  method = "MLE",
  alpha = 0.05,
  solver_options = list()
)
```

## Arguments

- df:

  A matrix or data frame where each column represents a different
  vintage estimate for the same time period. Rows are time periods.

- e:

  An integer indicating the number of data vintages to include in the
  model. Must be greater than 0.

- ar_order:

  Integer specifying the AR order for true values (default = 2).

- h:

  Integer specifying the forecast horizon (default = 0).

- include_news:

  Logical, whether to include news component in measurement error
  (default = TRUE).

- include_noise:

  Logical, whether to include noise component in measurement error
  (default = TRUE).

- include_spillovers:

  Logical, whether to include spillover effects (default = FALSE).

- spillover_news:

  Logical, whether spillovers apply to news component (default = TRUE).

- spillover_noise:

  Logical, whether spillovers apply to noise component (default = TRUE).

- method:

  A string specifying the estimation method to use. Options are Maximum
  likelihood ("MLE") for now.

- alpha:

  Significance level for confidence intervals (default = 0.05).

- solver_options:

  List of options for the optimizer:

  - trace: Integer controlling output level (default = 0)

  - maxiter: Maximum iterations (default = 1000)

  - startvals: Named vector of starting values (optional)

  - transform_se: T/F whether standard errors should be constrained to
    be positive in optimization. . - method: String specifying
    optimization method (default = "L-BFGS-B").

  - se_method: Method for standard error calculation (default =
    "hessian")

  - n_starts: Number of random starting points for multi-start
    optimization

## Value

A list of class 'jvn_model' with components:

- filtered_true:

  Filtered estimates of true values

- smoothed_true:

  Smoothed estimates of true values

- forecast_true:

  Forecasted true values (if h \> 0)

- forecast_vintages:

  Forecasted vintage values (if h \> 0)

- jvn_model_mat:

  Model matrices (Z, T, R, H, Q)

- params:

  Estimated parameters with standard errors

- fit:

  Optimization results

- loglik:

  Log-likelihood value

- aic:

  Akaike Information Criterion

- bic:

  Bayesian Information Criterion

- data:

  Input data

## References

Jacobs, Jan P.A.M. and Van Norden, Simon, "Modeling Data Revisions:
Measurement Error and Dynamics of 'True' Values", Journal of
Econometrics, 2011.

## See also

Other revision nowcasting:
[`kk_nowcast()`](https://p-wegmueller.github.io/reviser/reference/kk_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
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
```
