% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revisions.R
\name{get_revision_analysis}
\alias{get_revision_analysis}
\title{Revision Analysis Summary Statistics}
\usage{
get_revision_analysis(df, final_release, degree = 1, grouping_var = NULL)
}
\arguments{
\item{df}{A data frame containing the initial data releases.
Must include columns:
\itemize{
\item \code{time}: The time variable.
\item \code{value}: The observed values in the initial release.
\item Optionally, \code{release} (release identifier) and \code{id} (grouping variable).
}}

\item{final_release}{A data frame containing the final release data.
Must include columns:
\itemize{
\item \code{time}: The time variable (matching the initial release data).
\item \code{value}: The observed values in the final release.
}}

\item{degree}{An integer between 1 and 5 specifying the level of
detail for the output:
1: Default, includes information about revision size.
2: includes correlation statistics of revision.
3: includes news and noise tests.
4: includes sign switches, seasonality analysis and Theil’s U.
5: Full set of all statistics and tests.}

\item{grouping_var}{A character string specifying the grouping variable in
the data frame. Defaults to \code{"pub_date"} or \code{"release"} if available.}
}
\value{
A data frame with one row per grouping (if applicable) and columns
for summary statistics and test results. The resulting data frame is of
class \code{revision_summary}.
}
\description{
Calculates a comprehensive set of summary statistics and hypothesis tests
for revisions between initial and final data releases.
}
\details{
This function performs a variety of statistical analyses to understand
the nature of revisions between the initial and final data releases.
The function:
\itemize{
\item Checks the input data for consistency and transforms it as necessary.
\item Merges the initial and final release datasets by their time variable and
optional grouping variables (\code{id} or \code{release}).
\item Computes summary statistics such as the mean, standard deviation, and
range of the revisions.
\item Performs hypothesis tests for bias, efficiency, and correlation using
robust methods (e.g., Newey-West standard errors).
\item Includes tests for seasonality, noise, and news effects.
}

Key tests include:
\itemize{
\item \strong{Bias Tests}: Tests for the presence of mean bias and regression bias.
\item \strong{Autocorrelation and Seasonality}: Tests for serial correlation and
seasonal patterns in revisions.
\item \strong{Theil's U Statistics}: Measures predictive accuracy of the initial
releases relative to the final values.
\item \strong{Noise vs. News}: Differentiates between unpredictable errors (noise)
and systematic adjustments (news).
}

The function supports grouped calculations based on the presence
of \code{id} or \code{release} columns in the input.

The following statistics and tests are calculated (See the vignette
\code{vignette("revision-analysis")} for more details):

\itemize{
\item \strong{N}: The number of observations in the group.
\item \strong{Frequency}: The inferred data frequency (e.g., 12 for monthly
or 4 for quarterly data).
\item \strong{Bias (mean)}: The mean revision, testing whether revisions are
systematically biased.
\item \strong{Bias (p-value)}: p-value from a t-test evaluating the
significance of the mean revision.
\item \strong{Bias (robust p-value)}: Newey-West HAC robust p-value for the
mean revision test.
\item \strong{Minimum}: The minimum revision in the group.
\item \strong{Maximum}: The maximum revision in the group.
\item \strong{10Q}: The 10th percentile revision.
\item \strong{Median}: The median revision.
\item \strong{90Q}: The 90th percentile revision.
\item \strong{MAR}: The mean absolute revision.
\item \strong{Std. Dev.}: The standard deviation of revisions, indicating
their variability.
\item \strong{Noise/Signal}: The ratio of the standard deviation of revisions
to the standard deviation of final values.
\item \strong{Correlation}: The Pearson correlation between revisions and
initial values, testing the relationship.
\item \strong{Correlation (p-value)}: p-value for the significance of
the correlation.
\item \strong{Autocorrelation (1st)}: The first-order autocorrelation of
revisions, measuring persistence.
\item \strong{Autocorrelation (1st p-value)}: p-value for the first-order
autocorrelation test.
\item \strong{Autocorrelation up to 1yr (Ljung-Box p-value)}: p-value for the
Ljung-Box test for higher-order autocorrelation.
\item \strong{Theil's U1}: A normalized measure of forecast accuracy,
comparing the root mean squared error (RMSE) of revisions to the RMSE of
final and initial values.
\item \strong{Theil's U2}: Compares forecast changes to actual changes.
\item \strong{Seasonality (Friedman p-value)}: p-value from the Friedman test
for seasonality in revisions.
\item \strong{News joint test (p-value)}: p-value for the joint news test.
\item \strong{News test Intercept}: The estimated intercept from the news
test regression.
\item \strong{News test Intercept (std.err)}: The standard error of the
intercept in the news test regression.
\item \strong{News test Intercept (p-value)}: p-value for the intercept in
the news test regression.
\item \strong{News test Coefficient}: The estimated coefficient for the
\code{value} in the news test regression.
\item \strong{News test Coefficient (std.err)}: The standard error of the
coefficient in the news test regression.
\item \strong{News test Coefficient (p-value)}: p-value for the coefficient
in the news test regression.
\item \strong{Noise joint test (p-value)}: p-value for the joint noise test.
\item \strong{Noise test Intercept}: The estimated intercept from the noise
test regression.
\item \strong{Noise test Intercept (std.err)}: The standard error of the
intercept in the noise test regression.
\item \strong{Noise test Intercept (p-value)}: p-value for the intercept in
the noise test regression.
\item \strong{Noise test Coefficient}: The estimated coefficient for the
\code{final_value} in the noise test regression.
\item \strong{Noise test Coefficient (std.err)}: The standard error of the
coefficient in the noise test regression.
\item \strong{Noise test Coefficient (p-value)}: p-value for the coefficient
in the noise test regression.
\item \strong{Fraction of correct sign}: The fraction of correct sign changes
in revisions.
\item \strong{Fraction of correct growth rate change}: The fraction of correct
sign changes of growth rates in revisions.
}
}
\examples{
# Example usage:
df <- dplyr::select(
  get_nth_release(
    na.omit(
      tsbox::ts_pc(
        reviser::gdp)
            ),
   n = 0:10
 ),
-pub_date
)

final_release <- dplyr::select(
  get_nth_release(
  na.omit(
  tsbox::ts_pc(
    reviser::gdp)
       ),
 n = "latest"
),
-pub_date
)

results <- get_revision_analysis(
  df,
  final_release
 )


}
\seealso{
Other revision analysis: 
\code{\link{get_first_efficient_release}()},
\code{\link{summary.lst_efficient}()}
}
\concept{revision analysis}
