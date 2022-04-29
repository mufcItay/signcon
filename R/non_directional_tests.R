#' @title Get Sign Consistency
#' @description The function returns the mean probability of a consistent difference score sign for a random split of each participant's data.
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the estimated probability that for a random splitting of the data, the summary function (summary_function) returns two values with the same sign when applied to the two halves.
#' Then, it returns the mean sign consistency probability across participants.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param summary_function The summary function applied to the dependent variable (dv) under each level of the independent variable (iv) for each participant (id).
#' @param nSplits The number of splits to use when estimating sign consistency probability.
#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The mean sign consistency across all participants.
#'   \item consistency_per_id - Sign consistency estimate for each participant.
#' }
#' @seealso [weaknull::test_sign_consistency()] which uses this function to test the significance of the group-level sign consistency.
#' @export
get_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition", nSplits = 500, summary_function = base::mean) {
  params <- create_sign_consistency_params(nSplits, summary_function)
  res <- get_scores_per_subject(data, idv, dv, iv, params = params, f = calculate_sign_consistency)
  obs_stat <- mean(unlist(res))

  ret <- list(consistency_per_id = res, statistic = obs_stat)
  return(ret)
}


#' @title Tests for Sign Consistency
#' @description The function tests for a consistent sign of a difference score for a random split of the data, using bootstrapping and permutating each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the estimated probability that for a random splitting of the data, the summary function (summary_function) returns two values with the same sign when applied to the two halves.
#' Then, the mean sign consistency across participants is tested against a bootstrapped null distribution in which sign consistency probabilities are calculated for each participant after shuffling its independent variable labels (see Stelzer, J., Chen, Y., & Turner, R. (2013)).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param summary_function The summary function applied  to the dependent variable (dv) under each level of the independent variable (iv) for each identifier (id).
#' @param nSplits The number of splits to use when estimating sign consistency probability.
#' @param perm_repetitions The number of label shuffling for each participant.
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the estimated sign consistency under the distribution of sign consistency probabilities under the bootstrappednull distribution.
#'   \item statistic - The group-level statstic describing the mean sign consistency across participants.
#'   \item null_dist - A numerical vector of samples of sign consistency under the assumption that there is no consistent difference in the dependent variable (dv) between the levels of the independent variable (iv).
#' }
#' @seealso [weaknull::get_sign_consistency()] which returns the probability of a consistent sign of a difference score for a random split of the data
#' @export
test_sign_consistency <- function(data, idv = "id", dv = "rt", iv = "condition", nSplits = 500, summary_function = base::mean, perm_repetitions = 25, null_dist_samples = 10000) {
  params <- create_sign_consistency_params(nSplits, summary_function)
  res <- get_sign_consistency(data, idv, dv, iv, nSplits, summary_function)
  null_dist <- get_null_distribution(data, idv, dv, iv, params = params, f = calculate_sign_consistency, null_dist_samples = null_dist_samples)
  nullN <- length(null_dist)
  p_val <- sum(res$statistic <= null_dist) / nullN

  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist)
  return(ret)
}


#' @title Get Classification Accuracy
#' @description The function returns the mean classification accuracy of condition labels according to a dependent variable of each participant's data.
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the k-fold cross-validated accuracy of a classifier trained to predict the labels of levels under the independent variable (iv) based on the dependent variable (dv).
#' Then, it returns the mean accuracy of individual participants classifiers.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param trainingControl The training control regime for testing for classification. The default value (NA), will result in using the maximal number of folds possible (LOOCV). See 'caret' package's trainControl function for more details.
#' @param classifier The classifier name. The default value (NA) will result in creating a SVM classifier with a linear kernel. See 'caret' package's method for more details.
#' @param handleImbalance The strategy to handle class imbalance. The default value (NA), will result in using weights to account for the imbalance. See 'caret' package's trainControl method for more details.
#' @return A list including the results of the function
#' \itemize{
#'   \item accuracy - The mean classification accuracy across participants and folds.
#'   \item accuracy_per_id - The classification accuracy across folds, for each participant.
#'   \item prediction_results - A table including a 'k' column, indicating the fold index, a 'test' column, indicating the label to predict, and a 'prediction' column indicating the prediction accuracy for the test.
#' }
#' @seealso [weaknull::test_condition_classification()] which uses this function to test for the statistical significance of sign consistency.
#' @export
get_condition_classification <- function(data, idv = "id", dv = "rt", iv = "condition", classifier = NA, K = NA, handleImbalance = NA) {
  params <- create_classification_params(classifier, K, handleImbalance)
  res <- get_scores_per_subject(data, idv, dv, iv, params = params, f = classify_conditions)
  obs_stat <- mean(unlist(res))

  ret <- list(accuracy_per_id = res, statistic = obs_stat)
  return(ret)
}


#' @title Tests for Classification Accuracy
#' @description The function tests for a significant mean classification accuracy of condition labels according to a dependent variable across participants (without assuming a directional effect), using bootstrapping and permutating each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the k-fold cross-validated accuracy of a classifier trained to predict the labels of levels under the independent variable (iv) based on the dependent variable (dv).
#' Then, the mean classification accuracy across participants is tested against a bootstrapped null distribution in which classification accuracy is calculated for each participant after shuffling its independent variable labels(see Stelzer, J., Chen, Y., & Turner, R. (2013)).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param trainingControl The training control regime for testing for classification. The default value (NA), will result in using the maximal number of folds possible (LOOCV). See 'caret' package's trainControl function for more details.
#' @param classifier The classifier name. The default value (NA) will result in creating a SVM classifier with a linear kernel. See 'caret' package's method for more details.
#' @param handleImbalance The strategy to handle class imbalance. The default value (NA), will result in using weights to account for the imbalance. See 'caret' package's trainControl method for more details.
#' @param perm_repetitions The number of label shuffling for each participant.
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the mean classification accuracy across participants, under the bootstrappednull distribution.
#'   \item statistic - The group-level statstic describing the mean classification accuracy across participants.
#'   \item null_dist - A numerical vector of samples of the classifier accuracies under the bootstrappednull distribution.
#' }
#' @seealso [weaknull::get_condition_classification()] which returns the probability of a consistent sign of a difference score for a random split of the data
#' @seealso [caret::trainControl()] for the definition of the 'trainingControl' parameter
#' @export
test_condition_classification <- function(data, idv = "id", dv = "rt", iv = "condition", K = NA, classifier = NA, handleImbalance = NA,perm_repetitions = 25, null_dist_samples = 10000) {
  params <- create_classification_params(classifier, K, handleImbalance)
  res <- get_condition_classification(data, idv, dv, iv, classifier, K)
  null_dist <- get_null_distribution(data, idv, dv, iv, params = params, f = classify_conditions, null_dist_samples = null_dist_samples)
  nullN <- length(null_dist)
  p_val <- sum(res$statistic <= null_dist) / nullN

  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist)
  return(ret)
}
