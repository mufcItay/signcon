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
#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average sign consistency across all participants.
#'   \item consistency_per_id - Sign consistency estimate for each participant.
#' }
#' @seealso [weaknull::test_sign_consistency()] which uses this function to test the significance of the group-level sign consistency.
#' @export
get_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition", nSplits = 500,
                                 summary_function = base::mean, max_invalid_reps = 10^4) {
  params <- create_sign_consistency_params(nSplits, summary_function, max_invalid_reps)
  validate_data(data, idv, dv,iv)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = calculate_sign_consistency)
  participants_scores <- unlist(res$score)
  obs_stat <- base::mean(participants_scores)
  ret <- list(statistic = obs_stat, consistency_per_id = res)

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
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the estimated sign consistency compared with the distribution of sign consistency probabilities under the bootstrapped null distribution.
#'   \item statistic - The group-level statistic describing the average sign consistency across participants.
#'   \item null_dist - A numerical vector of samples of sign consistency under the null hypothesis (no consistent difference in the dependent variable ('dv') between the levels of the independent variable ('iv')).
#'   \item consistency_per_id - Sign consistency estimate for each participant.
#' }
#' @seealso [weaknull::get_sign_consistency()] returns the probability of a consistent sign of a difference score for a random split of the data
#' @export
test_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition",
                                  nSplits = 500, summary_function = base::mean,
                                  perm_repetitions = 25, null_dist_samples = 10000,
                                  max_invalid_reps = 10^4) {
  res <- get_sign_consistency(data, idv, dv, iv, nSplits, summary_function, max_invalid_reps)
  params <- create_sign_consistency_params(nSplits, summary_function, max_invalid_reps)
  null_dist <- get_null_distribution_perm(data, idv, dv, iv, params = params, f = calculate_sign_consistency, null_dist_samples = null_dist_samples, perm_repetitions = perm_repetitions)
  p_val <- mean(res$statistic <= null_dist,na.rm = TRUE)
  if(any(is.na(null_dist))) {
    prop_na <- sum(is.na(null_dist)) / length(null_dist)
    warning(paste('the null distribution includes invalid (NA) samples which were removed
                  before calculating the p-value (', prop_na * 100, '(%) of samples were removed) '))
  }
  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist,
              consistency_per_id = res$consistency_per_id)
  return(ret)
}


#' @title Get Classification Accuracy
#' @description The function returns the average classification accuracy of condition labels according to a dependent variable of each participant's data.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the k-fold cross-validated accuracy of a SVM classifier with a linear kernel, trained to predict the labels of levels under the independent variable ('iv') based on the dependent variable ('dv').
#' Then, it returns the average accuracy of individual participants' classifiers.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variables columns to classify conditions according to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')).
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variables ('dv') are expected to differ.
#' @param K - the number of folds to use when calculating the performance of the classifier. If K is set to 'NA' (it's default value), the function will set it to the number of observations of the minority class.
#' @param handleImbalance - A Boolean indicating whether to adjust class imbalance (using different weight for each label)
#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average classification accuracy across participants and folds.
#'   \item accuracy_per_id - The classification accuracy across folds, for each participant.
#' }
#' @seealso [weaknull::test_condition_classification()] which uses this function to test for the significance of condition classification accuracy.
#' @seealso [e1071::svm()] the SVM classifier used in this function
#' @export
get_condition_classification <- function(data, idv = "id", dv = "rt", iv = "condition", K = NA, handleImbalance = NA) {
  validate_data(data, idv, dv,iv)
  params <- create_classification_params(K, handleImbalance)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = classify_conditions)
  obs_stat <- mean(unlist(res$score))

  ret <- list(statistic = obs_stat, accuracy_per_id = res)
  return(ret)
}


#' @title Tests for Classification Accuracy
#' @description The function tests for a significant average classification accuracy of condition labels according to a dependent variable (or depedent variables, see the documentation of the 'dv' argument) across participants (without assuming a directional effect), using bootstrapping and permuting each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the k-fold cross-validated accuracy of a SVM classifier with a linear kernel trained to predict the labels of levels under the independent variable ('iv') based on the dependent variable ('dv').
#' Then, the average classification accuracy across participants is tested against a bootstrapped null distribution in which classification accuracy is calculated for each participant after shuffling its independent variable labels (see Stelzer, J., Chen, Y., & Turner, R., 2013).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variables columns to classify conditions according to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')).
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variables ('dv') are expected to differ.
#' @param K - the number of folds to use when calculating the performance of the classifier. If K is set to 'NA', the function resets it to the number of observations of the minority class.
#' @param handleImbalance - A Boolean indicating whether to adjust class imbalance (using different weight for each label)
#' @param perm_repetitions The number of label shuffling for each participant.
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the average classification accuracy across participants, under the bootstrapped null distribution.
#'   \item statistic - The group-level statistic describing the average classification accuracy across participants.
#'   \item null_dist - A numerical vector of samples of the classifier accuracies under the bootstrapped null distribution.
#' }
#' @seealso [weaknull::get_condition_classification()] returns the condition classification accuracy of each participant in the dataset.
#' @seealso [e1071::svm()] the SVM classifier used in this function
#' @export
test_condition_classification <- function(data, idv = "id", dv = "rt", iv = "condition", K = NA, handleImbalance = NA,perm_repetitions = 25, null_dist_samples = 10000) {
  params <- create_classification_params(K, handleImbalance)
  res <- get_condition_classification(data, idv, dv, iv, K, handleImbalance)
  null_dist <- get_null_distribution_perm(data, idv, dv, iv, params = params, f = classify_conditions, null_dist_samples = null_dist_samples, perm_repetitions = perm_repetitions)
  p_val <- mean(res$statistic <= null_dist,na.rm = TRUE)
  if(any(is.na(null_dist))) {
    prop_na <- sum(is.na(null_dist)) / length(null_dist)
    warning(paste('the null distribution includes invalid (NA) samples which were removed
                  before calculating the p-value (', prop_na * 100, '(%) of samples were removed) '))
  }
  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist)
  return(ret)
}
