#' @title Calculate Directional Effect
#' @description The function calculates the directional effect analysis to the dataset of a specific participant.
#' The function assumed that the dataset ('data') was sorted according to the independent variable prior to the invocation of this function.
#' Called from 'get_true_score'
#'
#' @param data The dataset to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable ('dv') is expected to differ.
#' @param params A list of parameters used by the function to calculate the effect. Includes:
#' \itemize{
#'   \item summary_function - The summary function applied to the dependent variable(s), 'dv', under each split of the data.
#'   \item null_dist_f - A function that calculates the effect score for the individual under the null hypothesis
#' }
#'
#' @return the function returns the mean consistency of signs for the given data
calculate_directional_effect <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,!!dplyr::sym(iv))
  # get the last condition label ( we assume that the dataset was sorted according to the independent variable)
  last_label <- dplyr::last(label)
  y <- data |> dplyr::select(tidyr::all_of(dv))

  # we  calculate the direction of the effect as summary_function(last label)-summary_function(first label)
  statistic <- params$summary_function
  # calculate the effect
  dir_effect <- statistic(y[label == last_label,]) - statistic(y[label != last_label,])
  retVal <- dir_effect
  return (retVal)
}

#' @title Get Null Distribution Sign Flip
#' @description The function returns the null distribution according to randomly sign flipped observed effects.
#'
#' @param data the data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params Configuration for the function to apply to the data of each participant ('f')
#' @param f The function to apply to the data of each participant, returning the value of interest for the analysis.
#' @param null_dist_samples The number of samples that comprise the output null distribution
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis.
#' The default value of the argument (empty list), will result in shuffling the labels of the independent variable, iv.
#' The function accepts a 'data' argument (a data frame including the data of an individual),
#' and an 'args' argument which includes all additional arguments needed to run the preprcessing function.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs' (in order, meaning preprocessFs[i](preprocessArgs[i]) will be invoked for each i).
#' For example, if the preprocessing function accepts arguments 'a','b' as inputs, preprocessArgs should be set to list(a,b)
#'
#' @return A distribution of mean score values computed according to a 'null' effect condition
get_null_distribution_sign_flip <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, null_dist_samples = 10000, preprocessFs = c(), preprocessArgs = c()) {
  de_res <- get_directional_effect(data, idv, dv, iv, params$summary_function)
  observed_scores <- de_res$effect_per_id
  print('Generating null distribution')
  pb <- utils::txtProgressBar(0,null_dist_samples,0)
  # define a function that computes a sample of the null distribution from the data of all participants,
  # randomly sample a specific permutation for each participant, and return the mean score of the group
  get_null_sample <- function(idx, pb, observed_scores) {
    utils::setTxtProgressBar(pb, idx)
    # randomly sample one permutation per participant
    rndSigns <- sample(c(1,-1), nrow(observed_scores), replace = TRUE)
    # get the sampled permutation scores
    sampled <- rndSigns * observed_scores$score
    # return the mean score of all sampled permutations
    return(base::mean(sampled))
  }
  # use the 'get_null_sample' to compute the group level mean score in each sample to build the distribution
  null_dist <- sapply(1:null_dist_samples, get_null_sample, observed_scores = observed_scores, pb = pb)
  close(pb)
  return(null_dist)
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

  return (params)
}
