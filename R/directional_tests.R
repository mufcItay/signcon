#' @title Get Directional Effect
#' @description The function returns the directional effect (comparing two conditions, under the independent variable, iv) according to the summary function (summary_function).
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the difference score between two conditions.
#' The difference score is calculated by subtracting the result of applying the summary function (summary_function) to the 2nd level if the independent variable, iv, from the 1st level.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables (dv) under each level of the independent variable (iv) for each participant (id).
#' This function should map a data frame to a number: {data.frame} -> numeric (e.g. function(df) {mean(as.matrix(df[,dv]))}, which is the default summary function).
#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average effect across all participants.
#'   \item effect_per_id - An effect score for each participant.
#' }
#' @seealso [weaknull::test_directional_effect()] which uses this function to test the significance of the group-level effect.
#' @export
get_directional_effect <- function(data, idv = "id", dv = "rt", iv = "condition", summary_function = NULL) {
  summary_function <- ifelse(is.null(summary_function), function (df) base::mean(as.matrix(df[,dv])), summary_function)
  params <- create_directional_effect_params(summary_function)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = calculate_directional_effect)
  obs_stat <- base::mean(unlist(res$score))

  ret <- list(statistic = obs_stat, effect_per_id = res)
  return(ret)
}


#' @title Tests for a Directional Effect
#'
#' @description The function tests for a group-level directional of a difference score (comparing two conditions, under the independent variable, iv) according to the summary function (summary_function)for a random split of the data, using bootstrapping and permuting each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier (id), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the difference score between two conditions.
#' The difference score is calculated by subtracting the result of applying the summary function (summary_function) to the 2nd level if the independent variable, iv, from the 1st level.
#' Then, the average effect across participants is tested against a bootstrapped null distribution in which effects are calculated for each participant with a random sign (see Stelzer, J., Chen, Y., & Turner, R., 2013).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables (dv) under each level of the independent variable (iv) for each participant (id).
#' This function should map a data frame to a number: {data.frame} -> numeric (e.g. function(df) {mean(as.matrix(df[,dv]))}, which is the default summary function).
#' @param perm_repetitions The number of label shuffling for each participant.
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The p_value of the estimated effect compared with the distribution of effects under the bootstrapped null distribution.
#'   \item statistic - The group-level statistic describing the average effect across participants.
#'   \item null_dist - A numerical vector of samples of effects under the null hypothesis (where the effect of each participant is assigned a random sign).
#' }
#' @seealso [weaknull::get_directional effect()] returns the directional effect of each participant.
#' @export
test_directional_effect <- function(data, idv = "id", dv = "rt", iv = "condition", summary_function = NULL, perm_repetitions = 25, null_dist_samples = 10000) {
  res <- get_directional_effect(data, idv, dv, iv, summary_function)
  summary_function <- ifelse(is.null(summary_function), function (df) base::mean(as.matrix(df[,dv])), summary_function)
  params <- create_directional_effect_params(summary_function)
  null_dist <- get_null_distribution(data, idv, dv, iv, params = params, f = calculate_directional_effect, null_dist_samples = null_dist_samples)
  nullN <- length(null_dist)
  p_val <- sum(res$statistic <= null_dist) / nullN

  ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist)
  return(ret)
}
