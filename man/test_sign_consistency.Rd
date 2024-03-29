% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/non_directional_tests.R
\name{test_sign_consistency}
\alias{test_sign_consistency}
\title{Tests for Sign Consistency}
\usage{
test_sign_consistency(
  data,
  idv = "id",
  dv = "rt",
  iv = "condition",
  nSplits = 500,
  summary_function = base::mean,
  perm_repetitions = 100,
  null_dist_samples = 10^4,
  max_invalid_reps = 10^3,
  split_type = c("noverlap", "random")
)
}
\arguments{
\item{data}{The dataset to analyze}

\item{idv}{The name of the participant identifier column.}

\item{dv}{The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),}

\item{iv}{Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ .}

\item{nSplits}{The number of splits to use when estimating sign consistency probability.}

\item{summary_function}{The summary function to apply to the dependent variables ('dv') under each level of the independent variable ('iv') for each participant ('idv').
This function should map a matrix maintaining the original dataframe columns to a number: {matrix} -> numeric (e.g. function(mat) {mean(mat)}, which is the default summary function).
The function should return NA if the summary statistic cannot be computed for the input given. In such case another split of the data will be sampled and used.}

\item{perm_repetitions}{The number of label shuffling for each participant.}

\item{null_dist_samples}{The number of samples taken from the null distribution.}

\item{max_invalid_reps}{- The maximal number repetitions in which invalid consistency was computed before returning NA result.}

\item{split_type}{- A string from {"noverlap" / "random"}, indicating if data should be split to non-overlapping halves (the
default option, "noverlap" option), or to two random halves ("random").}
}
\value{
A list including the results of the function
\itemize{
  \item p - The p_value of the estimated sign consistency compared with the distribution of sign consistency probabilities under the bootstrapped null distribution.
  the p-value is adjusted according to (B + 1) / (M + 1), see Phipson & Smyth, 2010.
  \item statistic - The group-level statistic describing the average sign consistency across participants.
  \item null_dist - A numerical vector of samples of sign consistency under the null hypothesis (no consistent difference in the dependent variable ('dv') between the levels of the independent variable ('iv')).
  \item consistency_per_id - Sign consistency estimate for each participant.
}
}
\description{
The function tests for a consistent sign of a difference score for a random split of the data, using bootstrapping and permuting each participants' independent variable labels.
The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
For each participant, the function calculates the estimated probability that for a random splitting of the data, the summary function (summary_function) returns two values with the same sign when applied to the two halves.
Then, the average sign consistency across participants is tested against a bootstrapped null distribution in which sign consistency probabilities are calculated for each participant after shuffling its independent variable labels (see Stelzer, J., Chen, Y., & Turner, R., 2013).
All levels of the independent variable must be included under each identifier.
}
\seealso{
[signcon::get_sign_consistency()] returns the probability of a consistent sign of a difference score for a random split of the data
}
