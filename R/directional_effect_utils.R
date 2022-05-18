#' @title Calculate Directional Effect
#' @description The function calculates the directional effect analysis to the dataset of a specific participant.
#' Called from 'get_true_score'
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to calculate the effect. Includes:
#' \itemize{
#'   \item summary_function - The summary function applied to the dependent variable(s), 'dv', under each split of the data.
#' }
#'
#' @return the function returns the mean consistency of signs for the given data
calculate_directional_effect <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,iv)
  # binarization of labels => True for the 1st label, False for the 2nd label
  first_label <- dplyr::first(label)
  # we  calculate the direction of the effect as summary_function(1st label)-summary_function(2nd label)
  statistic <- params$summary_function
  # calculate the effect
  dir_effect <- statistic(data[label == first_label, dv]) - statistic(data[label != first_label, dv])
  # the avoid shuffling parameter signals that we are generating the null distribution for a directional effect.
  # this means we should randonly sample the sign of the effect
  if(params$avoid_shuffling) {
    sign = sample(c(1,-1), 1)
    dir_effect <- sign * dir_effect
  }
  retVal <- dir_effect
  return (retVal)
}

#' @title Create Parameters for Directional Effect
#' @description The function creates a list of parameters to be later passed to the directional effect analysis function.
#'
#' @param summary_function The summary function applied to the dependent variable(s), 'dv' under each split of the data.
#' @param is_null_dist Indicating whether params should include an avoid shuffling entry.
#' This parameter is used when testing for a significant directional effect.
#' In this test we do not shuffle labels of the independent variable, we simply randomly assign a sign for the effect.
#'
#' @return a list of parameters that includes both arguments.
create_directional_effect_params <- function(summary_function, is_null_dist = FALSE) {
  params <- list()
  params$summary_function <- summary_function
  params$avoid_shuffling <- is_null_dist

  return (params)
}
