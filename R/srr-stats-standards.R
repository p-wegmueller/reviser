#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G1.5} Software should include all code necessary to reproduce
#' results which form the basis of performance claims made in associated
#' publications.
#' @srrstatsNA {G1.6} Software should include code necessary to compare
#' performance claims with alternative implementations in other R packages.
#' @srrstatsNA {G2.4d} explicit conversion to factor via `as.factor()`
#' @srrstatsNA {G2.4e} explicit conversion from factor via `as...()` functions
#' @srrstatsNA {G2.5} Where inputs are expected to be of `factor` type,
#' secondary documentation should explicitly state whether these should be
#' `ordered` or not, and those inputs should provide appropriate error or other
#' routines to ensure inputs follow these expectations.
#' @srrstatsNA {G2.11} Software should ensure that `data.frame`-like tabular
#' objects which have columns which do not themselves have standard class
#' attributes (typically, `vector`) are appropriately processed, and do not
#' error without reason. This behaviour should be tested. Again, columns
#' created by the [`units` package](https://github.com/r-quantities/units/)
#' provide a good test case.
#' @srrstatsNA {G2.12} Software should ensure that `data.frame`-like tabular
#' objects which have list columns should ensure that those columns are
#' appropriately pre-processed either through being removed, converted to
#' equivalent vector columns where appropriate, or some other appropriate
#' treatment such as an informative error. This behaviour should be tested.
#' @srrstatsNA {G2.14c} replace missing data with appropriately imputed values
#' @srrstatsNA {G2.16} All functions should also provide options to handle
#' undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially
#' ignoring or removing such values.
#' @srrstatsNA {G3.0} Statistical software should never compare floating point
#'  numbers for equality. All numeric equality comparisons should either ensure
#'  that they are made between integers, or use appropriate tolerances for
#'  approximate equality.
#' @srrstatsNA {G3.1} Statistical software which relies on covariance
#' calculations should enable users to choose between different algorithms for
#' calculating covariances, and should not rely solely on covariances from the
#' `stats::cov` function.
#' @srrstatsNA {G3.1a} The ability to use arbitrarily specified covariance
#' methods should be documented (typically in examples or vignettes).
#' @srrstatsNA {G4.0} Statistical Software which enables outputs to be written
#' to local files should parse parameters specifying file names to ensure
#' appropriate file suffices are automatically generated where not provided.
#' @srrstatsNA {G5.0} Where applicable or practicable, tests should use
#' standard data sets with known properties (for example, the
#' [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or
#'  data sets provided by other widely-used R packages).
#' @srrstatsNA {G5.1} Data sets created within, and used to test, a package
#' should be exported (or otherwise made generally available) so that users can
#' confirm tests and run examples.
#' correctness tests should include tests against previous implementations.
#' Such testing may explicitly call those implementations in testing, preferably
#' from fixed-versions of other software, or use stored outputs from those
#' where that is not possible.
#' @srrstatsNA {G5.3} For functions which are expected to return objects
#' containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence
#' of any such values in return objects should be explicitly tested.
#' @srrstatsNA {G5.4b} For new implementations of existing methods,
#' @srrstatsNA {G5.4c} Where applicable, stored values may be drawn from
#' published paper outputs when applicable and where code from original
#' implementations is not available
#' @srrstatsNA {G5.5} Correctness tests should be run with a fixed random seed
#' @srrstatsNA {G5.6} Parameter recovery tests: to test that the
#' implementation produce expected results given data with known properties.
#' For instance, a linear regression algorithm should return expected
#' coefficient values for a simulated data set generated from a linear model.
#' @srrstatsNA {G5.6a} Parameter recovery tests should generally be expected
#' to succeed within a defined tolerance rather than recovering exact values.
#' @srrstatsNA {G5.6b} Parameter recovery tests should be run with multiple
#' random seeds when either data simulation or the algorithm contains a random
#' component. (When long-running, such tests may be part of an extended, rather
#' than regular, test suite; see G5.10-4.12, below).
#' @srrstatsNA {G5.7} Algorithm performance tests: to test that implementation
#' performs as expected as properties of data change. For instance, a test may
#' show that parameters approach correct estimates within tolerance as data size
#' increases, or that convergence times decrease for higher convergence
#' thresholds.
#' @srrstatsNA {G5.9} Noise susceptibility tests: Packages should test for
#' expected stochastic behaviour, such as through the following conditions:
#' @srrstatsNA {G5.9b} Running under different random seeds or initial
#' conditions does not meaningfully change results
#' @srrstatsNA {G5.10} Extended tests should included and run under a common
#' framework with other tests but be switched on by flags such as as a
#' `<MYPKG>_EXTENDED_TESTS="true"` environment variable. - The extended tests
#' can be then run automatically by GitHub Actions for example by adding the
#' following to the `env` section of the workflow:
#' @srrstatsNA {G5.11} Where extended tests require large data sets or other
#' assets, these should be provided for downloading and fetched as part of the
#' testing workflow.
#' @srrstatsNA {G5.11a} When any downloads of additional data necessary for
#' extended tests fail, the tests themselves should not fail, rather be skipped
#' and implicitly succeed with an appropriate diagnostic message.
#' @srrstatsNA {G5.12} Any conditions necessary to run extended tests such as
#' platform requirements, memory, expected runtime, and artefacts produced that
#' may need manual inspection, should be described in developer documentation
#' such as a `CONTRIBUTING.md` or `tests/README.md` file.
#'
#' @srrstatsNA {TS1.7} Accept inputs defined via the
#' [`units` package](https://github.com/r-quantities/units/) for attributing SI
#' units to R vectors.
#' @srrstatsNA {TS2.0} Time Series Software which presumes or requires regular
#'  data should only allow **explicit** missing values, and should issue
#'  appropriate diagnostic messages, potentially including errors, in response
#'  to any **implicit** missing values.
#' @srrstatsNA {TS2.1c} replace missing data with appropriately imputed values
#' @srrstatsNA {TS2.2} Consider stationarity of all relevant moments -
#' typically first (mean) and second (variance) order, or otherwise document
#' why such consideration may be restricted to lower orders only.
#' @srrstatsNA {TS2.3} Explicitly document all assumptions and/or requirements
#'  of stationarity
#' @srrstatsNA {TS2.4} Implement appropriate checks for all relevant forms of
#' stationarity, and either:
#' @srrstatsNA {TS2.4a} issue diagnostic messages or warnings; or
#' @srrstatsNA {TS2.4b} enable or advise on appropriate transformations to
#' @srrstatsNA {TS2.5} Incorporate a system to ensure that both row and column
#'  orders follow the same ordering as the underlying time series data. This
#'  may, for example, be done by including the `index` attribute of the time
#'  series data as an attribute of the auto-covariance matrix.
#' @srrstatsNA {TS2.6} Where applicable, auto-covariance matrices should also
#' include specification of appropriate units.
#' @srrstatsNA {TS3.0} Provide tests to demonstrate at least one case in which
#' errors widen appropriately with forecast horizon.
#' @srrstatsNA {TS3.1} If possible, provide at least one test which violates
#' TS3.0
#' @srrstatsNA {TS3.2} Document the general drivers of forecast errors or
#' horizons, as demonstrated via the particular cases of TS3.0 and TS3.1
#' @srrstatsNA {TS3.3} Either:
#' @srrstatsNA {TS3.3a} Document, preferable via an example, how to trim
#' forecast values based on a specified error margin or equivalent; or
#' @srrstatsNA {TS3.3b} Provide an explicit mechanism to trim forecast values
#' to a specified error margin, either via an explicit post-processing function,
#' or via an input parameter to a primary analytic function.
#' @srrstatsNA {TS4.1} Any units included as attributes of input data should also
#' be included within return values.
#' @srrstatsNA {TS4.4} Document the effect of any such transformations on
#' forecast data, including potential effects on both first- and second-order
#' estimates.
#' @srrstatsNA {TS4.5} In decreasing order of preference, either:
#' @srrstatsNA {TS4.5a} Provide explicit routines or options to back-transform
#'  data commensurate with original, non-stationary input data
#' @srrstatsNA {TS4.5b} Demonstrate how data may be back-transformed to a
#' form commensurate with original, non-stationary input data.
#' @srrstatsNA {TS4.5c} Document associated limitations on forecast values
#' @srrstatsNA {TS4.6a} A distribution object, for example via one of the many
#' packages described in the CRAN Task View on [Probability Distributions]
#' (https://cran.r-project.org/web/views/Distributions.html) (or the new
#' [`distributional` package](https://pkg.mitchelloharawild.com/distributional/)
#' as used in the [`fable` package](https://fable.tidyverts.org) for time-series
#' forecasting).
#' @srrstatsNA {TS4.6c} Some more general indication of error associated with
#' forecast estimates.
#' @srrstatsNA {TS4.7c} Combining model and forecast values into a single
#' return object with an appropriate additional column clearly distinguishing
#' the two kinds of data.
#' @srrstatsNA {TS5.4} For frequency visualization, abscissa spanning
#' $[-\pi, \pi]$ should be avoided in favour of positive units of
#' $[0, 2\pi]$ or $[0, 0.5]$, in all cases with appropriate additional
#' explanation of units.
#' @srrstatsNA {TS5.6} By default indicate distributional limits of forecast
#' on plot
#' @srrstatsNA {TS5.7} By default include model (input) values in plot, as
#' well as forecast (output) values
#' @srrstatsNA {TS5.8} By default provide clear visual distinction between model
#' (input) values and forecast (output) values.
#' @noRd
NULL
