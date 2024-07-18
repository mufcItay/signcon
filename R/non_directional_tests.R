#' @title Get Sign Consistency
#' @description The function returns the average probability of a consistent difference score sign (comparing two conditions) for a random split of each participant's data.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the estimated probability that for a random splitting of the data, the summary function (summary_function) returns two values with the same sign when applied to the two halves.
#' Then, the average sign consistency probability across participants is calculated and returned.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables ('dv') under each level of the independent variable ('iv') for each participant ('idv').
#' This function should map a matrix maintaining the original dataframe columns to a number: {matrix} -> numeric (e.g. function(mat) {mean(mat)}, which is the default summary function).
#' The function should return NA if the summary statistic cannot be computed for the input given. In such case another split of the data will be sampled and used.
#' @param nSplits The number of splits to use when estimating sign consistency probability.
#' @param max_invalid_reps - The maximal number repetitions in which invalid consistency was computed before returning NA result.
#' @param split_type - A string from {"noverlap" / "random"}, indicating if data should be split to non-overlapping halves (the
#' default option, "noverlap" option), or to two random halves ("random").

#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average sign consistency across all participants.
#'   \item consistency_per_id - Sign consistency estimate for each participant.
#' }
#' @seealso [signcon::test_sign_consistency()] which uses this function to test the significance of the group-level sign consistency.
#' @export
get_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition", nSplits = 500,
                                 summary_function = base::mean, max_invalid_reps = 10^3,
                                 split_type = c("noverlap", "random")) {
  split_type <- match.arg(split_type)
  params <- create_sign_consistency_params(nSplits, summary_function, max_invalid_reps, split_type)
  validate_data(data, idv, dv,iv)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = calculate_sign_consistency)
  valid_res <- res[!is.nan(res$score),]
  participants_scores <- unlist(valid_res$score)
  if(nrow(valid_res) == 0) {
    stop('could not compute sign-consistency scores for any participant')
  }
  if(any(is.nan(res$score))) {
    n_valid <- nrow(res[!is.nan(res$score),])
    nan_particiapnts <- res[is.nan(res$score), idv]
    some_invalid_msg <- paste('calculating group-level sign consistency for', n_valid,
                            'participants.', "Invalid participant identifiers:",
                            paste(nan_particiapnts, sep = ','))
    warning(some_invalid_msg)
  }
  obs_stat <- base::mean(participants_scores, na.rm = TRUE)
  ret <- list(statistic = obs_stat, consistency_per_id = valid_res)

  return(ret)
}


#' @title Tests for Sign Consistency
#' @description The function tests for a consistent sign of a difference score for a random split of the data, using bootstrapping and permuting each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the estimated probability that for a random splitting of the data, the summary function (summary_function) returns two values with the same sign when applied to the two halves.
#' Then, the average sign consistency across participants is tested against a bootstrapped null distribution in which sign consistency probabilities are calculated for each participant after shuffling its independent variable labels (see Stelzer, J., Chen, Y., & Turner, R., 2013).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ .
#' @param summary_function The summary function to apply to the dependent variables ('dv') under each level of the independent variable ('iv') for each participant ('idv').
#' This function should map a matrix maintaining the original dataframe columns to a number: {matrix} -> numeric (e.g. function(mat) {mean(mat)}, which is the default summary function).
#' The function should return NA if the summary statistic cannot be computed for the input given. In such case another split of the data will be sampled and used.
#' @param nSplits The number of splits to use when estimating sign consistency probability.
#' @param perm_repetitions The number of label shuffling for each participant.
#' @param max_invalid_reps - The maximal number repetitions in which invalid consistency was computed before returning NA result.
#' @param split_type - A string from {"noverlap" / "random"}, indicating if data should be split to non-overlapping halves (the
#' default option, "noverlap" option), or to two random halves ("random").
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the estimated sign consistency compared with the distribution of sign consistency probabilities under the bootstrapped null distribution.
#'   the p-value is adjusted according to (B + 1) / (M + 1), see Phipson & Smyth, 2010.
#'   \item statistic - The group-level statistic describing the average sign consistency across participants.
#'   \item null_dist - A numerical vector of samples of sign consistency under the null hypothesis (no consistent difference in the dependent variable ('dv') between the levels of the independent variable ('iv')).
#'   \item consistency_per_id - Sign consistency estimate for each participant.
#' }
#' @seealso [signcon::get_sign_consistency()] returns the probability of a consistent sign of a difference score for a random split of the data
#' @export
test_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition",
                                  nSplits = 500, summary_function = base::mean,
                                  perm_repetitions = 100, null_dist_samples = 10^4,
                                  max_invalid_reps = 10^3, split_type = c("noverlap", "random")) {
  split_type <- match.arg(split_type)
  res <- get_sign_consistency(data, idv, dv, iv, nSplits, summary_function, max_invalid_reps, split_type)
  valid_participants <- res$consistency_per_id |> dplyr::pull(!!dplyr::sym(idv))
  data <- data |> dplyr::filter(!!dplyr::sym(idv) %in% valid_participants)
  params <- create_sign_consistency_params(nSplits, summary_function, max_invalid_reps, split_type)
  null_dist <-
    get_null_distribution_perm(data, idv, dv, iv, params = params,
                               f = calculate_sign_consistency,
                               null_dist_samples = null_dist_samples,
                               perm_repetitions = perm_repetitions)
  # adjust p-value (according to (B + 1) / (M + 1), see Phipson & Smyth, 2010)
  p_val <- get_corrected_pval(res$statistic, null_dist)

  if(any(is.na(null_dist))) {
    prop_na <- sum(is.na(null_dist)) / length(null_dist)
    warning(paste('the null distribution includes invalid (NA) samples which were removed
                  before calculating the p-value (', prop_na * 100, '(%) of samples were removed) '))
  }
  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist,
              consistency_per_id = res$consistency_per_id)
  return(ret)
}

#' @title Get Absolute Effect Size
#' @description The function returns the average absolute effect size across participants.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables ('dv') as a function of the independent variable ('iv') for each participant ('idv').

#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average absolute effect size across all participants.
#'   \item absolute_es_per_id - Absolute effect size for each participant.
#' }
#' @seealso [signcon::test_absolute_es()] which uses this function to test the significance of the group-level absolute effect size.
#' @export
get_absolute_es <- function(data, idv = "id", dv = "rt", iv = "condition",
                                 summary_function = lsr::cohensD) {

  params <- list()
  params$summary_function <- summary_function

  validate_data(data, idv, dv,iv)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = calculate_absolute_es)
  valid_res <- res[!is.nan(res$score),]
  participants_scores <- unlist(valid_res$score)
  if(nrow(valid_res) == 0) {
    stop('could not compute absolute effect sizes for any participant')
  }
  if(any(is.nan(res$score))) {
    n_valid <- nrow(res[!is.nan(res$score),])
    nan_particiapnts <- res[is.nan(res$score), idv]
    some_invalid_msg <- paste('calculating group-level absolute effect size for', n_valid,
                              'participants.', "Invalid participant identifiers:",
                              paste(nan_particiapnts, sep = ','))
    warning(some_invalid_msg)
  }
  obs_stat <- base::mean(participants_scores, na.rm = TRUE)
  ret <- list(statistic = obs_stat, absolute_es_per_id = valid_res)

  return(ret)
}


#' @title Tests for Absolute Effect Size
#' @description The function tests whether the absolute effect size is larger than expected by chance, using bootstrapping and permuting each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the absolute effect size.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ .
#' @param summary_function The summary function to apply to the dependent variables ('dv') as a function of the independent variable ('iv') for each participant ('idv').
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the absolute effect size against a bootstrapped null distribution.
#'   the p-value is adjusted according to (B + 1) / (M + 1), see Phipson & Smyth, 2010.
#'   \item statistic - The group-level statistic describing the average absolute effect size
#'   \item null_dist - A numerical vector of samples of absolute effect sizes under the null hypothesis (no consistent difference in the dependent variable ('dv') between the levels of the independent variable ('iv')).
#'   \item absolute_es_per_id - Absolute effect sizefor each participant.
#' }
#' @seealso [signcon::get_absolute_es()]
#' @export
test_absolute_es <- function(data, idv = "id", dv = "rt", iv = "condition",
                                  summary_function = lsr::cohensD,
                                  perm_repetitions = 100, null_dist_samples = 10^4) {
  res <- get_absolute_es(data, idv, dv, iv, summary_function)
  valid_participants <- res$absolute_es_per_id |> dplyr::pull(!!dplyr::sym(idv))
  data <- data |> dplyr::filter(!!dplyr::sym(idv) %in% valid_participants)
  params <- list()
  params$summary_function <- summary_function
  null_dist <-
    get_null_distribution_perm(data, idv, dv, iv, params = params,
                               f = calculate_absolute_es,
                               null_dist_samples = null_dist_samples,
                               perm_repetitions = perm_repetitions)
  # adjust p-value (according to (B + 1) / (M + 1), see Phipson & Smyth, 2010)
  p_val <- get_corrected_pval(res$statistic, null_dist)

  if(any(is.na(null_dist))) {
    prop_na <- sum(is.na(null_dist)) / length(null_dist)
    warning(paste('the null distribution includes invalid (NA) samples which were removed
                  before calculating the p-value (', prop_na * 100, '(%) of samples were removed) '))
  }
  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist,
              absolute_es_per_id = res$absolute_es_per_id)
  return(ret)
}
