# Revision Patterns and Statistics

This vignette provides a detailed explanation of the statistical
measures and hypothesis tests used in
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md).
The function is designed to analyze revisions between preliminary and
final data releases, helping to assess bias, efficiency, correlation,
seasonality, and the relative contributions of news and noise in
revisions. The function returns a tibble with the desired statistics and
hypothesis tests. In the following sections, we provide a detailed
explanation of the statistics and hypothesis tests used in the function
and provide the corresponding column name in brackets to assess it.

Let’s define some notation to facilitate the discussion of the
statistics and hypothesis tests used in the function:

- $y_{t}^{h}$ denote the $h$th released value for time $t$ with $h = 0$
  being the preliminary release
- $y_{t}^{f}$ denote the final revised value for time $t$
- $r_{t}^{f} = y_{t}^{f} - y_{t}^{h}$ be the final revision compared to
  the $h$th release for time $t$
- $N$ is the number of observations

The function takes two mandatory arguments:

1.  A data frame containing the preliminary data releases.
2.  A data frame containing the final release data against which the
    revisions are calculated and preliminary data will be compared.

The function argument `degree` allows users to specify which statistics
and hypothesis tests to include in the output:

- `degree = 1` includes information about revision size (default)
- `degree = 2` includes correlation statistics of revision
- `degree = 3` includes news and noise tests
- `degree = 4` includes sign switches, seasonality analysis and Theil’s
  U
- `degree = 5` includes all the statistics and hypothesis tests

## Summary Statistics

### Revision Size

Revisions provide insights into the reliability of initial data
releases. Various metrics assess their magnitude and distribution:

#### 1. **Mean Revision** (`"Bias (mean)"`)

The mean revision quantifies systematic bias in the revisions:

$$\text{Bias} = \bar{r} = \frac{1}{N}\sum\limits_{t = 1}^{N}r_{t}^{f}$$

If the mean revision is significantly different from zero, it indicates
a tendency for initial releases to be systematically over- or
under-estimated. A **t-test** is conducted to test the null hypothesis
$H_{0}:\bar{r} = 0$.

- `"Bias (p-value)"` reports the standard t-test p-value.  
- `"Bias (robust p-value)"` provides a heteroskedasticity-robust
  alternative.

#### 2. **Mean Absolute Revision** (`"MAR"`)

The mean absolute revision measures the average size of revisions,
regardless of direction:

$$\text{MAR} = \frac{1}{N}\sum\limits_{t = 1}^{N}\left| r_{t}^{f} \right|$$

This metric is useful when evaluating the magnitude of revisions rather
than their direction.

#### 3. **Minimum and Maximum Revisions** (`"Minimum"`, `"Maximum"`)

These statistics capture the most extreme downward and upward revisions
in the dataset.

#### 4. **Percentiles of Revisions** (`"10Q"`, `"Median"`, `"90Q"`)

The 10th, 50th (median), and 90th percentiles provide a distributional
perspective, helping to identify skewness and tail behavior in
revisions.

#### 5. **Standard Deviation of Revisions** (`"Std. Dev."`)

The standard deviation quantifies the dispersion of revisions:

$$\sigma_{r} = \sqrt{\frac{1}{N - 1}\sum\limits_{t = 1}^{N}\left( r_{t}^{f} - \bar{r} \right)^{2}}$$

A higher standard deviation indicates greater variability in the
revision process.

#### 6. **Noise-to-Signal Ratio** (`"Noise/Signal"`)

This metric measures the relative size of revisions compared to the
total variance in the final data:

$$\frac{\sigma_{r}}{\sigma_{y}}$$

where $\sigma_{y}$ is the standard deviation of the final value
$y_{t}^{f}$. A high noise-to-signal ratio suggests that revisions are
large relative to the underlying variability in the final data,
indicating high uncertainty in initial estimates.

``` r
library(reviser)
library(dplyr)
library(tsbox)

gdp <- reviser::gdp %>%
  ts_pc() %>%
  na.omit()

df <- get_nth_release(gdp, 0:1)
final_release <- get_latest_release(gdp)

results <- get_revision_analysis(
  df, 
  final_release,
  degree = 1
  )

head(results)
#> # A tibble: 6 × 14
#>   id    release       N `Bias (mean)` `Bias (p-value)` `Bias (robust p-value)`
#>   <chr> <chr>     <dbl>         <dbl>            <dbl>                   <dbl>
#> 1 CHE   release_0   178       0.110            0.00692                0.000107
#> 2 CHE   release_1   177       0.0960           0.0148                 0.000202
#> 3 EA    release_0   178       0.0553           0.00167                0.000450
#> 4 EA    release_1   177       0.0502           0.00367                0.000673
#> 5 JP    release_0   178       0.00989          0.848                  0.795   
#> 6 JP    release_1   177       0.00920          0.859                  0.790   
#> # ℹ 8 more variables: Minimum <dbl>, Maximum <dbl>, `10Q` <dbl>, Median <dbl>,
#> #   `90Q` <dbl>, MAR <dbl>, `Std. Dev.` <dbl>, `Noise/Signal` <dbl>
```

### Correlation of Revisions

Understanding how revisions relate to initial releases and past
revisions can reveal patterns in the revision process:

#### 1. **Correlation Between Revisions and Initial Releases** (`"Correlation"`)

This measures whether revisions are systematically related to the
initial release $y_{t}^{h}$, which serves as a proxy for available
information at the time of release:

$$\rho = \frac{\sum\left( y_{t}^{h} - {\bar{y}}^{h} \right)\left( Rr_{t}^{f} - \bar{r} \right)}{\sqrt{\sum\left( y_{t}^{h} - {\bar{y}}^{h} \right)^{2}}\sqrt{\sum\left( r_{t}^{f} - \bar{r} \right)^{2}}}$$

A significant correlation suggests that initial estimates contain
information that predicts later revisions. A **t-test**
(`"Correlation (p-value)"`) is used to test whether $\rho$ is
significantly different from zero.

#### 2. **1st order Autocorrelation of Revisions** (`"Autocorrelation (1st)"`)

The **first-order autocorrelation** measures the persistence in the
revision process by examining whether past revisions predict future
revisions:

$$\rho_{1} = \frac{\sum\left( r_{t}^{f} - \bar{r} \right)\left( r_{t - 1}^{f} - \bar{r} \right)}{\sqrt{\sum\left( r_{t}^{f} - \bar{r} \right)^{2}}\sqrt{\sum\left( r_{t - 1}^{f} - \bar{r} \right)^{2}}}$$

If revisions exhibit strong autocorrelation, it may indicate a
systematic pattern in the revision process rather than purely random
adjustments.  
A **t-test** (`"Autocorrelation (1st p-value)"`) is used to assess
whether $\rho_{1}$ is significantly different from zero.

#### 3. **Autocorrelation of Revisions up to 1 year** (`"Autocorrelation up to 1yr (Ljung-Box p-value)"`)

This metric assesses the autocorrelation of revisions up to a lag of 1
year using the Ljung-Box test. The null hypothesis of the Ljung-Box test
is that there is **no autocorrelation** up to the specified lag. In
other words, the revisions are independent of one another. The
alternative hypothesis is that there **is autocorrelation** in the
revisions.

- **For quarterly data (4 observations per year)**, the test checks for
  autocorrelation up to **4 lags**.
- **For monthly data (12 observations per year)**, the test checks for
  autocorrelation up to **12 lags**.
- **For other frequencies**, the test is skipped.

The Ljung-Box test statistic is computed using the following formula:

$$Q = N(N + 2)\sum\limits_{k = 1}^{m}\frac{{\widehat{\rho}}_{k}^{2}}{N - k}$$

- $Q$ is the Ljung-Box statistic.

- $N$ is the number of observations in the time series.

- ${\widehat{\rho}}_{k}$ is the **sample autocorrelation** at lag $k$.
  This measures the correlation between the revisions at time $t$ and
  time $t - k$.

- $m$ is the number of lags considered (which corresponds to the
  frequency of the data: 4 for quarterly data, 12 for monthly data).

  The test statistic $Q$ is used to determine whether there is
  significant autocorrelation. If the autocorrelation at a particular
  lag is large, it suggests that the revisions at that lag are not
  independent of each other. The larger the $Q$-statistic, the stronger
  the evidence that the revisions are autocorrelated.

These metrics collectively help evaluate the reliability,
predictability, and potential biases in data revisions.

``` r
results <- get_revision_analysis(
  df, 
  final_release,
  degree = 2
  )

head(results)
#> # A tibble: 6 × 8
#>   id    release      N Correlation Correlation (p-value…¹ Autocorrelation (1st…²
#>   <chr> <chr>    <dbl>       <dbl>                  <dbl>                  <dbl>
#> 1 CHE   release…   178      -0.209              0.00517                  -0.0887
#> 2 CHE   release…   177      -0.204              0.00656                  -0.114 
#> 3 EA    release…   178      -0.315              0.0000187                -0.0922
#> 4 EA    release…   177      -0.318              0.0000158                -0.157 
#> 5 JP    release…   178      -0.204              0.00626                  -0.278 
#> 6 JP    release…   177      -0.241              0.00126                  -0.292 
#> # ℹ abbreviated names: ¹​`Correlation (p-value)`, ²​`Autocorrelation (1st)`
#> # ℹ 2 more variables: `Autocorrelation (1st p-value)` <dbl>,
#> #   `Autocorrelation up to 1yr (Ljung-Box p-value)` <dbl>
```

### Sign Switches

Sign switches in data revisions can occur when the direction of an
economic indicator changes between its initial release and later
revisions. We define two key metrics to assess the stability of these
directional signals:

#### 1. **Fraction of sign changes**

This metric (`"Fraction of correct sign"`) evaluates how often the sign
of the initially reported value $y_{t}^{h}$ differs from the final
revised value $y_{t}^{f}$. Mathematically, we compute:

$$\text{Fraction of sign consistency} = \frac{\sum\limits_{t = 1}^{T}\mathbb{1}\left( \text{sign}\left( y_{t}^{h} \right) = \text{sign}\left( y_{t}^{f} \right) \right)}{T}$$

where $\mathbb{1}( \cdot )$ is an indicator function that equals 1 if
the signs match and 0 otherwise.

#### 2. **Fraction of sign changes in the growth rate**

This metric (`"Fraction of correct growth rate change"`) assesses
whether the direction of change in the variable remains consistent after
revisions. Specifically, we compare the sign of the period-over-period
differences:

$$\text{Fraction of sign consistency in growth} = \frac{\sum\limits_{t = 2}^{T}\mathbb{1}\left( \text{sign}\left( \Delta y_{t}^{h} \right) = \text{sign}\left( \Delta y_{t}^{f} \right) \right)}{T - 1}$$

where $\Delta y_{t}^{h} = y_{t}^{h} - y_{t - 1}^{h}$ and
$\Delta y_{t}^{f} = y_{t}^{f} - y_{t - 1}^{f}$ represent the first
differences of the initially reported and final values, respectively.

A high fraction of incorrect signs in either metric suggests that early
estimates may be unreliable in capturing the true direction of the
variable.

## Hypothesis Tests

### News and Noise Tests for Data Revisions

The **news and noise tests** analyze the properties of **data
revisions** in relation to the final and preliminary releases of an
economic variable. These tests help to evaluate whether the revisions
are systematic, whether they contain new information, or whether they
are simply noisy adjustments ([Mankiw and Shapiro
1986](#ref-mankiwNewsNoiseAnalysis1986); [Aruoba
2008](#ref-aruobaDataRevisionsAre2008)).

**Final revisions** can be classified into two categories:

1.  **Noise:** The initial announcement is an observation of the final
    series, measured with error. This means that the revision is
    uncorrelated with the final value but correlated with the data
    available when the estimate is made.

2.  **News:** The initial announcement is an efficient forecast that
    reflects all available information, and subsequent estimates reduce
    the forecast error by incorporating new information. The revision is
    correlated with the final value but uncorrelated with the data
    available when the estimate is made, i.e., unpredictable using the
    information set at the time of the initial announcement.

To test these categories, we run **two regression tests:**

#### 1. **The Noise Test**

The **noise test** examines whether the **revision** can be predicted by
the final value:

$$r_{t}^{f} = \alpha + \beta \cdot y_{t}^{f} + \epsilon_{t}$$

If the preliminary value were an **efficient and unbiased estimate** of
the final value (i.e incorporating all relevant information), revisions
should be **uncorrelated** with the final value. The null hypothesis is:

$$H_{0}:\alpha = 0,\quad\beta = 0$$

- The function tests the joint hypothesis
  (`"Noise joint test (p-value)"`) that both $\alpha = 0$ and
  $\beta = 0$ using a **heteroskedasticity and autocorrelation
  consistent (HAC) covariance matrix** to ensure robust inference.
- $\alpha = 0$ tests (`"Noise test Intercept (p-value)"`) whether the
  revisions have a systematic bias.
- $\beta = 0$ tests (`"Noise test Coefficient (p-value)"`) whether the
  revisions are correlated with the final value, indicating
  inefficiency.

#### 2. **The News Test**

The **news test** examines whether the revision is predictable using the
preliminary release:

$$r_{t}^{f} = \alpha + \beta \cdot y_{t}^{h} + \epsilon_{t}$$

Here, we test whether revisions are **systematically related** to the
initial value. The null hypothesis is:

$$H_{0}:\alpha = 0,\quad\beta = 0$$

- The function tests the joint hypothesis
  (`"News joint test (p-value)"`) that both $\alpha = 0$ and $\beta = 0$
  using a **HAC covariance matrix** to ensure robust inference.
- $\alpha = 0$ tests (`"News test Intercept (p-value)"`) whether the
  revisions have a systematic bias.
- $\beta = 0$ tests (`"News test Coefficient (p-value)"`) whether the
  revisions are correlated with the initial value, indicating
  inefficiency.

Note: Instead of regressing $r_{t}^{f}$ on $y_{t}^{f}$ or $y_{t}^{h}$,
one can similarly regress \$\$ \text{Noise: } \quad y_t^h = \alpha +
\beta \cdot y_t^f + \epsilon_t \\ \text{News: } \quad y_t^f = \alpha +
\beta \cdot y_t^h + \epsilon_t \$\$ and test $\alpha = 0$ and
$\beta = 1$ for the presence of noise and news in the data revisions.

``` r
results <- get_revision_analysis(
  df, 
  final_release,
  degree = 3
  )

head(results)
#> # A tibble: 6 × 17
#>   id    release       N `News test Intercept` `News test Intercept (std.err)`
#>   <chr> <chr>     <dbl>                 <dbl>                           <dbl>
#> 1 CHE   release_0   178                0.149                           0.0419
#> 2 CHE   release_1   177                0.134                           0.0440
#> 3 EA    release_0   178                0.0741                          0.0195
#> 4 EA    release_1   177                0.0690                          0.0180
#> 5 JP    release_0   178                0.0585                          0.0438
#> 6 JP    release_1   177                0.0650                          0.0407
#> # ℹ 12 more variables: `News test Intercept (p-value)` <dbl>,
#> #   `News test Coefficient` <dbl>, `News test Coefficient (std.err)` <dbl>,
#> #   `News test Coefficient (p-value)` <dbl>, `News joint test (p-value)` <dbl>,
#> #   `Noise test Intercept` <dbl>, `Noise test Intercept (std.err)` <dbl>,
#> #   `Noise test Intercept (p-value)` <dbl>, `Noise test Coefficient` <dbl>,
#> #   `Noise test Coefficient (std.err)` <dbl>,
#> #   `Noise test Coefficient (p-value)` <dbl>, …
```

### Test of Seasonality in Revisions

To test whether seasonality is present in revisions, we employ a
Friedman test. Note that this test is always performed on
first-differentiated series.

#### **Friedman Test**

The Friedman test (`"Seasonality (Friedman p-value)"`) is a
non-parametric statistical test that examines whether the distribution
of ranked data differs systematically across seasonal periods, such as
months or quarters. It does not require distributional assumptions,
making it a robust tool for detecting seasonality in revision series.
The test is applied by first transforming the revision series into a
matrix where each row represents a year and each column represents a
specific month or quarter. It is constructed as follows. Consider first
the matrix of data $\{ x_{ij}\}_{n \times k}$ with $n$ rows (i.e., the
number of years in the sample) and $k$ columns (i.e., either 12 months
or 4 quarters, depending on the frequency of the data). The data matrix
needs to be replaced by a new matrix $\{ r_{ij}\}_{n \times k}$, where
the entry $r_{ij}$ is the rank of $x_{ij}$ within the row $i$. The test
is executed using the
[`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html)
function. The null hypothesis is that the average ranking across columns
is the same, indicating no seasonality in the revisions.

### Theil’s U Statistics

In the context of revision analysis, **Theil’s inequality coefficient**,
or **Theil’s U**, provides a measure of the accuracy of a initial
estimates ($y_{t}^{h}$) relative to final or more refined values
($y_{t}^{f}$). Various definitions of Theil’s statistics exist, leading
to different interpretations. This package considers two widely used
variants: **U1** and **U2**.

#### 1. **Theil’s U1 Statistic** (`"Theil's U1"`)

The first measure, **U1**, is given by:

$$U_{1} = \frac{\sqrt{\frac{1}{n}\sum\limits_{t = 1}^{n}\left( y_{t}^{f} - y_{t}^{h} \right)^{2}}}{\sqrt{\frac{1}{n}\sum\limits_{t = 1}^{n}{y_{t}^{f}}^{2}} + \sqrt{\frac{1}{n}\sum\limits_{t = 1}^{n}{y_{t}^{h}}^{2}}}$$

The **U1 statistic is bounded between 0 and 1**:

- A value of **0** implies perfect forecasting ($y_{t}^{h} = y_{t}^{f}$
  for all $t$).  
- A value **closer to 1** indicates lower accuracy.

However, U1 suffers from **several limitations**, as highlighted by
Granger and Newbold ([1973](#ref-Granger01031973)). A critical issue
arises when **one of $y_{t}^{h}$ or $y_{t}^{f}$ equals zero in all
periods**. In such cases, the denominator and numerator become equal,
leading to **U1 = 1** even when preliminary estimates are close to the
final values.

#### 2. **Theil’s U2 Statistic** (`"Theil's U2"`)

To address these shortcomings, Theil et al.
([1966](#ref-theil1966applied)) proposed an alternative measure, **U2**,
which is defined as:

$$U_{2} = \frac{\sqrt{\sum\limits_{t = 1}^{n}\left( \frac{y_{t + 1}^{h} - y_{t + 1}^{f}}{Y_{t}^{f}} \right)^{2}}}{\sqrt{\sum\limits_{t = 1}^{n}\left( \frac{y_{t + 1}^{f} - y_{t}^{f}}{Y_{t}^{f}} \right)^{2}}}$$

where:

- The numerator captures the **relative errors** in the preliminary
  estimates.  
- The denominator measures the **magnitude of changes** in the final
  values over time.

Unlike U1, **U2 is not bounded between 0 and 1**. Instead:

- A value of **0** implies perfect accuracy ($y_{t}^{h} = y_{t}^{f}$ for
  all $t$).  
- **$U_{2} = 1$** means the accuracy of the preliminary estimates is
  equal to a **naïve forecast**.  
- **$U_{2} > 1$** suggests the preliminary estimates are **less
  accurate** than the naïve approach.  
- **$U_{2} < 1$** indicates the preliminary estimates are **more
  accurate** than the naïve method.

Whenever possible (**if $y_{t}^{f} \neq 0$ for all $t$**), **U2 is the
preferred metric** for evaluating preliminary estimates.

``` r
results <- get_revision_analysis(
  df, 
  final_release,
  degree = 4
  )

head(results)
#> # A tibble: 6 × 8
#>   id    release     N Fraction of correct …¹ Fraction of correct …² `Theil's U1`
#>   <chr> <chr>   <dbl>                  <dbl>                  <dbl>        <dbl>
#> 1 CHE   releas…   178                  0.803                  0.652       0.262 
#> 2 CHE   releas…   177                  0.802                  0.644       0.250 
#> 3 EA    releas…   178                  0.944                  0.764       0.0818
#> 4 EA    releas…   177                  0.944                  0.763       0.0797
#> 5 JP    releas…   178                  0.820                  0.725       0.266 
#> 6 JP    releas…   177                  0.802                  0.712       0.262 
#> # ℹ abbreviated names: ¹​`Fraction of correct sign`,
#> #   ²​`Fraction of correct growth rate change`
#> # ℹ 2 more variables: `Theil's U2` <dbl>,
#> #   `Seasonality (Friedman p-value)` <dbl>
```

In the above examples, we analyze GDP revisions by comparing the final
release with the first and second releases, which correspond to the
diagonal elements of the real-time data matrix. This approach provides
insight into how initial estimates evolve over time.

Alternatively, we can compare GDP revisions between specific publication
dates. In the example below, we analyze revisions from the GDP releases
from April and July 2024, using the final release from October 2024 as
the benchmark. The grouping of statistics is by default determined by
the `pub_date` or `release` column. Alternatively, it can be specified
via the optional function argument `grouping_var`.

``` r
df <- gdp %>%
  filter(pub_date %in% c("2024-04-01", "2024-07-01"))


results <- get_revision_analysis(
  df, 
  final_release,
  degree = 5
  )

head(results)
#> # A tibble: 6 × 39
#>   id    pub_date       N Frequency `Bias (mean)` `Bias (p-value)`
#>   <chr> <date>     <dbl>     <dbl>         <dbl>            <dbl>
#> 1 CHE   2024-04-01   176         4       0.00226            0.478
#> 2 CHE   2024-07-01   177         4      -0.00123            0.371
#> 3 EA    2024-04-01   176         4       0.00656            0.159
#> 4 EA    2024-07-01   177         4       0.00205            0.520
#> 5 JP    2024-04-01   176         4      -0.00323            0.490
#> 6 JP    2024-07-01   177         4      -0.00276            0.305
#> # ℹ 33 more variables: `Bias (robust p-value)` <dbl>, Minimum <dbl>,
#> #   Maximum <dbl>, `10Q` <dbl>, Median <dbl>, `90Q` <dbl>, MAR <dbl>,
#> #   `Std. Dev.` <dbl>, `Noise/Signal` <dbl>, Correlation <dbl>,
#> #   `Correlation (p-value)` <dbl>, `Autocorrelation (1st)` <dbl>,
#> #   `Autocorrelation (1st p-value)` <dbl>,
#> #   `Autocorrelation up to 1yr (Ljung-Box p-value)` <dbl>, `Theil's U1` <dbl>,
#> #   `Theil's U2` <dbl>, `Seasonality (Friedman p-value)` <dbl>, …
```

The
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md)
function requires a single time series in `final_release` and compares
it to multiple time series in `df` for the same `id`. To always compare
two consecutive publication dates, it is necessary to structure the
analysis sequentially. The code below iterates over consecutive
publication dates in the gdp dataset, comparing revisions between each
pair. For each pair of consecutive dates, it extracts the corresponding
data, runs
[`get_revision_analysis()`](https://p-wegmueller.github.io/reviser/reference/get_revision_analysis.md)
to analyze revisions, and then combines the results into a single
dataframe.

``` r
# Get unique sorted publication dates
pub_dates <- gdp %>%
  distinct(pub_date) %>%
  arrange(pub_date) %>%
  pull(pub_date)

# Run the function for each pair of consecutive publication dates
results <- purrr::map_dfr(seq_along(pub_dates[-length(pub_dates)]), function(i) 
  {
  
  df <- gdp %>%
    filter(pub_date %in% pub_dates[i])
  
  final_release <- gdp %>%
    filter(pub_date %in% pub_dates[i + 1])
  
  get_revision_analysis(df, final_release, degree = 5)
}
)

head(results)
#> # A tibble: 6 × 39
#>   id    pub_date       N Frequency `Bias (mean)` `Bias (p-value)`
#>   <chr> <date>     <dbl>     <dbl>         <dbl>            <dbl>
#> 1 CHE   2002-10-01    90         4      0.000632            0.634
#> 2 EA    2002-10-01    90         4      0.00106             0.512
#> 3 JP    2002-10-01    90         4      0.0231              0.629
#> 4 US    2002-10-01    90         4      0                 NaN    
#> 5 CHE   2003-01-01    91         4     -0.00384             0.563
#> 6 EA    2003-01-01    91         4      0.00192             0.750
#> # ℹ 33 more variables: `Bias (robust p-value)` <dbl>, Minimum <dbl>,
#> #   Maximum <dbl>, `10Q` <dbl>, Median <dbl>, `90Q` <dbl>, MAR <dbl>,
#> #   `Std. Dev.` <dbl>, `Noise/Signal` <dbl>, Correlation <dbl>,
#> #   `Correlation (p-value)` <dbl>, `Autocorrelation (1st)` <dbl>,
#> #   `Autocorrelation (1st p-value)` <dbl>,
#> #   `Autocorrelation up to 1yr (Ljung-Box p-value)` <dbl>, `Theil's U1` <dbl>,
#> #   `Theil's U2` <dbl>, `Seasonality (Friedman p-value)` <dbl>, …
```

## References

Aruoba, S. Borağan. 2008. “Data Revisions Are Not Well Behaved.”
*Journal of Money, Credit and Banking* 40 (2-3): 319–40.
<https://doi.org/10.1111/j.1538-4616.2008.00115.x>.

Granger, C. W. J., and P. Newbold. 1973. “Some Comments on the
Evaluation of Economic Forecasts.” *Applied Economics* 5 (1): 35–47.
<https://doi.org/10.1080/00036847300000003>.

Mankiw, N. Gregory, and Matthew Shapiro. 1986. “News or Noise? An
Analysis of GNP Revisions.” w1939. Cambridge, MA: National Bureau of
Economic Research. <https://doi.org/10.3386/w1939>.

Theil, Henri, GAC Beerens, CG Tilanus, and Christiaan Bernhard De Leeuw.
1966. *Applied Economic Forecasting*. Vol. 4. North-Holland Publishing
Company Amsterdam.
