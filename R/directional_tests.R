#' @title Get Directional Effect
#' @description The function returns the directional effect (comparing two conditions, under the independent variable, iv) according to the summary function (summary_function).
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the difference score between two conditions.
#' The difference score is calculated by subtracting the result of applying the summary function (summary_function) to the 2nd level if the independent variable, iv, from the 1st level.
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables ('dv') under each level of the independent variable ('iv') for each participant ('idv').
#' This function should map a matrix maintaining the original dataframe columns to a number: {matrix} -> numeric (e.g. function(mat) {mean(mat)}, which is the default summary function).
#' @param ci_level - The confidence level (in percents, e.g. setting the argument to 50 generates a 50% CI)
#' to use when computing the bootstrapped confidence interval on the group-level
#' statistic (see the return value 'ci', and the argument 'ci_reps').
#' The default value of this argument is 95, that would lead to computing the 95% confidence interval for
#' the group-level statistic.
#' @param ci_reps - The number repetitions to use when computing the bootstrapped confidence interval
#' around the group-level statistic.
#' The default value of this argument is zero, which would lead to not computing the confidence interval at all.
#' @return A list including the results of the function
#' \itemize{
#'   \item statistic - The average effect across all participants.
#'   \item effect_per_id - An effect score for each participant.
#'   \item ci_low (optional) - The lower bound of a confidence interval around the statistic (returned only if
#'    the 'ci_reps' argument was set to a value different than 0).
#'   \item ci_high (optional) - The higher bound of a confidence interval around the statistic (returned only if
#'    the 'ci_reps' argument was set to a value different than 0).
#' }
#' @seealso [signcon::test_directional_effect()] which uses this function to test the significance of the group-level effect.
#' @export
get_directional_effect <- function(data, idv = "id", dv = "rt", iv = "condition",
                                   summary_function = base::mean, ci_level = 95, ci_reps = 0) {
  validate_data(data, idv, dv,iv)
  params <- create_directional_effect_params(summary_function)
  res <- get_scores_per_participant(data, idv, dv, iv, params = params, f = calculate_directional_effect)
  participants_scores <- unlist(res$score)
  obs_stat <- base::mean(participants_scores)
  ci <- get_boot_ci(participants_scores, ci_level, ci_reps)
  if (ci_reps == 0) {
    ret <- list(statistic = obs_stat, effect_per_id = res)
  } else {ret <- list(statistic = obs_stat, effect_per_id = res, ci_low = ci[1], ci_high = ci[2])}

  return(ret)
}


#' @title Tests for a Directional Effect
#'
#' @description The function tests for a group-level directional of a difference score (comparing two conditions, under the independent variable, iv) according to the summary function (summary_function)for a random split of the data, using bootstrapping and permuting each participants' independent variable labels.
#' The function accepts a dataset in long format with specific columns: identifier ('idv'), independent and dependent variables (iv and dv, respectively).
#' For each participant, the function calculates the difference score between two conditions.
#' The difference score is calculated by subtracting the result of applying the summary function (summary_function) to the 2nd level if the independent variable, iv, from the 1st level.
#' Then, the average effect across participants is tested against a bootstrapped null distribution in which effects are calculated for each participant with a random sign (see Stelzer, J., Chen, Y., & Turner, R., 2013).
#' All levels of the independent variable must be included under each identifier.
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ.
#' @param summary_function The summary function to apply to the dependent variables ('dv') under each level of the independent variable ('iv') for each participant ('idv').
#' This function should map a matrix maintaining the original dataframe columns to a number: {matrix} -> numeric (e.g. function(mat) {mean(mat)}, which is the default summary function).
#' @param null_dist_samples The number of samples taken from the null distribution.
#' @param ci_level - The confidence level (in percents, e.g. setting the argument to 50 generates a 50% CI)
#' to use when computing the bootstrapped confidence interval on the group-level
#' statistic (see the return value 'ci', and the argument 'ci_reps').
#' The default value of this argument is 95, that would lead to computing the 95% confidence interval for
#' the group-level statistic.
#' @param ci_reps - The number repetitions to use when computing the bootstrapped confidence interval
#' around the group-level statistic.
#' The default value of this argument is zero, which would lead to not computing the confidence interval at all.
#' @return A list including the results of the function
#' \itemize{
#'   \item p - The two-sided p_value of the estimated effect compared with the distribution of effects under the bootstrapped null distribution.
#'   \item statistic - The group-level statistic describing the average effect across participants.
#'   \item null_dist - A numerical vector of samples of effects under the null hypothesis (where the effect of each participant is assigned a random sign).
#'   \item effect_per_id - An effect score for each participant.
#'   \item ci_low (optional) - The lower bound of a confidence interval around the statistic (returned only if
#'    the 'ci_reps' argument was set to a value different than 0).
#'   \item ci_high (optional) - The higher bound of a confidence interval around the statistic (returned only if
#'    the 'ci_reps' argument was set to a value different than 0).
#' }
#' @seealso [signcon::get_directional effect()] returns the directional effect of each participant.
#' @export
test_directional_effect <- function(data, idv = "id", dv = "rt", iv = "condition",
                                    summary_function = base::mean, null_dist_samples = 10000,
                                    ci_level = 95, ci_reps = 0) {
  res <- get_directional_effect(data, idv, dv, iv, summary_function,
                                ci_level = ci_level, ci_reps = ci_reps)
  params <- create_directional_effect_params(summary_function)
  null_dist <- get_null_distribution_sign_flip(data, idv, dv, iv, params = params, f = calculate_directional_effect, null_dist_samples = null_dist_samples)
  p_val <- mean(res$statistic <= null_dist,na.rm = TRUE)
  p_val <- 2*min(p_val, 1-p_val)
  if(any(is.na(null_dist))) {
    prop_na <- sum(is.na(null_dist)) / length(null_dist)
    warning(paste('the null distribution includes invalid (NA) samples which were removed
                  before calculating the p-value (', prop_na * 100, '(%) of samples were removed) '))
  }
  if (ci_reps == 0) {
    ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist,
                effect_per_id = res$effect_per_id)
  } else {
    ret <- list(p = p_val, statistic = res$statistic, null_dist = null_dist,
                      effect_per_id = res$effect_per_id, ci_low = res$ci_low, ci_high = res$ci_high)}

  return(ret)
}
