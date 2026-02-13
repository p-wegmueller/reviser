# Diagnose Revision Quality

Provides a quick diagnostic summary of revision quality with color-coded
pass/fail indicators for key metrics.

## Usage

``` r
# S3 method for class 'revision_summary'
diagnose(object, alpha = 0.05, ...)
```

## Arguments

- object:

  An object of class `revision_summary`.

- alpha:

  Significance level for hypothesis tests. Default is 0.05.

- ...:

  Additional arguments (not used).

## Value

A tibble with diagnostic results.

## See also

Other revision analysis:
[`diagnose()`](https://p-wegmueller.github.io/reviser/reference/diagnose.md),
[`get_first_efficient_release()`](https://p-wegmueller.github.io/reviser/reference/get_first_efficient_release.md),
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md),
[`print.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/print.lst_efficient.md),
[`print.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/print.revision_summary.md),
[`summary.lst_efficient()`](https://p-wegmueller.github.io/reviser/reference/summary.lst_efficient.md),
[`summary.revision_summary()`](https://p-wegmueller.github.io/reviser/reference/summary.revision_summary.md)
