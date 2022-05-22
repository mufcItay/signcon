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
  retVal <- dir_effect
  return (retVal)
}

#' @title Create Parameters for Directional Effect
#' @description The function creates a list of parameters to be later passed to the directional effect analysis function.
#'
#' @param summary_function The summary function applied to the dependent variable(s), 'dv' under each split of the data.
#' This parameter is used when testing for a significant directional effect.
#' In this test we do not shuffle labels of the independent variable, we simply randomly assign a sign for the effect.
#'
#' @return a list of parameters that includes both arguments.
create_directional_effect_params <- function(summary_function) {
  params <- list()
  params$summary_function <- summary_function
  params$nullDistFunc <- get_sign_flipped_score

  return (params)
}

#' @title Get Sign Flipped Score
#' @description The function calculated the directional effect according to the indepdedent variable ('iv') and the function 'f',
#' samples a sign randomly and assigns the sign to the directional effect
#'
#' @param idx the index of the iteration we are currently running (this function is being called iteratively)
#' @param data the data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv the names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param preprocessFs a vector of functions to apply to the data for preprocessing
#' @param preprocessArgs a vector of arguments for the preprocessing functions
#' @param params configuration for the function to apply to the data of each participant ('f')
#' @param f the function to apply to the data of each participant, returning the value of interest for the analysis.
#'
#' @return the function returns the score calculated by applying the function 'f' to the data after suffling the labels of the indepdent variable 'iv'.
get_sign_flipped_score <- function(idx, data, idv, dv, iv, preprocessFs, preprocessArgs, params, f) {
  # get the score of the participant
  res <- get_scores_per_participant(data, idv, dv, iv, preprocessFs, preprocessArgs, params, f)
  # randomly sample the sign of the effect
  sampled_sign = sample(c(1,-1), 1)
  rand_sign_effect <- sampled_sign * res$score

  # return the potentially sign flipped score
  return(rand_sign_effect)
}

