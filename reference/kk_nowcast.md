# Generalized Kishor-Koenig Model for Nowcasting

Implements a generalized Kishor-Koenig (KK) model for nowcasting and
forecasting with state-space models, allowing for multiple vintages of
data, efficient estimation, and Kalman filtering and smoothing.

## Usage

``` r
kk_nowcast(
  df,
  e,
  h = 0,
  model = "Kishor-Koenig",
  method = "SUR",
  alpha = 0.05,
  solver_options = list()
)
```

## Arguments

- df:

  A data frame containing the time series data in either "long" or
  "wide" format. It must include columns for the time index and the
  different release vintages.

- e:

  An integer indicating the number of data vintages to include in the
  model. Must be greater than 0.

- h:

  An integer specifying the forecast horizon. Default is 0, which
  implies no forecasts. Must be greater than or equal to 0.

- model:

  A string specifying the type of model to use. Options are:

  - "Kishor-Koenig" or "KK" (default): Full Kishor-Koenig model.

  - "Howrey": Howrey's simplified framework.

  - "Classical": Classical model without vintage effects.

- method:

  A string specifying the estimation method to use. Options are "SUR"
  (default), Maximum likelihood ("MLE") and "OLS".

- alpha:

  Significance level for confidence intervals (default = 0.05).

- solver_options:

  An optional list to control the behaviour of the underlying
  [`systemfit::nlsystemfit()`](https://rdrr.io/pkg/systemfit/man/nlsystemfit.html),
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) and
  [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) solvers:

  - **trace**: An integer controlling the level of output for the
    optimization procedure. Default is 0 (minimal output).

  - **maxiter**: An integer specifying the maximum number of iterations
    for the optimization procedure. Default is 1000.

  - **startvals**: A list of starting values for the optimization
    procedure (must match the number of parameters of the model).

  - **solvtol**: Tolerance for detecting linear dependencies in the
    columns of X in the qr function calls (See
    [`systemfit::nlsystemfit()`](https://rdrr.io/pkg/systemfit/man/nlsystemfit.html)).
    Default is .Machine\$double.eps.

  - **gradtol**: A a positive scalar giving the tolerance at which the
    scaled gradient is considered close enough to zero to terminate the
    algorithm (See [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html)).
    Default is 1e-6.

  - **steptol**: A positive scalar providing the minimum allowable
    relative step length (See
    [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html)). Default is
    1e-6.

  - **transform_se**: T/F whether standard errors should be constrained
    to be positive in optimization.

  - **method**: String specifying optimization method (default =
    "L-BFGS-B").

  - **se_method**: Method for standard error calculation (default =
    "hessian")

  - **n_starts**: Number of random starting points for multi-start
    optimization

## Value

A list with the following components:

- states:

  A tibble containing filtered and smoothed state estimates.

- kk_model_mat:

  A list of KK model matrices, such as transition and observation
  matrices.

- ss_model_mat:

  A list of state-space model matrices derived from the KK model.

- model:

  The KFAS state-space model object.

- params:

  Estimated model parameters with standard errors.

- fit:

  The fitted model object from the estimation procedure.

- loglik:

  Log-likelihood value (MLE only).

- aic:

  Akaike Information Criterion (MLE only).

- bic:

  Bayesian Information Criterion (MLE only).

- convergence:

  Convergence status.

- e:

  The number of the efficient release (0-indexed).

- data:

  The input data in wide format.

## Details

The function supports multiple models, including the full Kishor-Koenig
framework, Howrey's model, and a classical approach. It handles data
preprocessing, estimation of system equations using Seemingly Unrelated
Regressions (SUR), and application of the Kalman filter. This is the
first openly available implementation of the Kishor-Koenig model (See
the vignette `vignette("nowcasting_revisions")` for more details).

## References

Kishor, N. Kundan and Koenig, Evan F., "VAR Estimation and Forecasting
When Data Are Subject to Revision", Journal of Business and Economic
Statistics, 2012.

## See also

Other revision nowcasting:
[`jvn_nowcast()`](https://p-wegmueller.github.io/reviser/reference/jvn_nowcast.md),
[`plot.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/plot.jvn_model.md),
[`plot.kk_model()`](https://p-wegmueller.github.io/reviser/reference/plot.kk_model.md),
[`print.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/print.jvn_model.md),
[`print.kk_model()`](https://p-wegmueller.github.io/reviser/reference/print.kk_model.md),
[`summary.jvn_model()`](https://p-wegmueller.github.io/reviser/reference/summary.jvn_model.md),
[`summary.kk_model()`](https://p-wegmueller.github.io/reviser/reference/summary.kk_model.md)

## Examples

``` r
# Example usage:
df <- get_nth_release(
  tsbox::ts_span(
    tsbox::ts_pc(
      dplyr::filter(reviser::gdp, id == "US")
    ),
    start = "1980-01-01"
  ),
  n = 0:1
)
df <- dplyr::select(df, -c("id", "pub_date"))
df <- na.omit(df)

e <- 1 # Number of efficient release
h <- 2 # Forecast horizon
result <- kk_nowcast(df, e, h = h, model = "Kishor-Koenig")

result$params
#>   Parameter     Estimate    Std.Error
#> 1        F0  0.200853533 0.0735783747
#> 2      G0_0  0.995630065 0.0048794659
#> 3      G0_1 -0.001694615 0.0047626028
#> 4        v0  1.598322193 0.1718526669
#> 5      eps0  0.006664367 0.0007165572
```
