non_overlapping_halves_split <- function(trial_selection) {
  # sample a random permutation of the data
  order_permutation = sample(trial_selection)
  # define groups according to the permutation:
  # group0 = {all data points <= midpoint}, group1 {all data points > midpoint}
  split <- list(grp0 = !order_permutation, grp1 = order_permutation)
  return(split)
}

random_halves_split <- function(trial_selection) {
  group0 <- sample(trial_selection)
  group1 <- sample(trial_selection)
  split <- list(grp0 = group0, grp1 = group1)
  return(split)
}


#' @title Calculate Sign Consistency
#' @description The function applies the sign consistency analysis to the dataset of a specific participant.
#' Called from 'get_true_score'
#'
#' @param data The dataset of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to calculate sign consistency. Includes:
#' \itemize{
#'   \item n_splits - The number of random splits to analyze for the estimation of sign consistency.
#'   \item summary_function - The summary function applied to the dependent variable(s), 'dv', under each split of the data.
#'   \item null_dist_f - A function that calculates sign consistency score for the individual under the null hypothesis.
#'   \item max_invalid_reps - The maximal number repetitions in which invalid consistency was computed before returning NA result.
#' }
#'
#' @return the function returns the mean consistency of signs for the given data
calculate_sign_consistency <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the dependent variable column
  y <- data |> dplyr::select(all_of(dv))
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,!!dplyr::sym(iv))
  # binarization of labels => True for the 1st label, False for the 2nd label
  label <- label == dplyr::first(label)
  # get the parameters for the calculation of sign consistency
   # get the parameters for the calculation of sign consistency
  n_splits <- params$n_splits
  statistic_f <- params$summary_function
  get_group_split <- params$split_f
  # check for errors
  if (nrow(y) != length(label)) {
    stop('inconsistent lengths for vectors the dependent and independent variables')
  }
  # set the number of trials and midpoint, to compute random splits of the data
  nTrials <- nrow(y)
  midpoint <- round(nTrials/2)
  trial_selection <- rep(c(TRUE,FALSE), each = midpoint)
  # define an inner function to compute sign consistency in each repetition.
  # the function returns true if difference score signs are consistency across splits,
  # and false otherwise.
  inner_calculate_sign_consistency <- function(iteration) {
    # we run this function until we get a valid result
    is_valid_result = FALSE
    invalid_counter <- 0
    while(invalid_counter < params$max_invalid_reps && !is_valid_result) {
      split <- get_group_split(trial_selection)
      # compute the sign of difference scores between groups, using the specified statistic
      group0_sign <- sign(statistic_f(y[split$grp0 & label, dv]) -
                            statistic_f(y[split$grp0 & !label, dv]))
      group1_sign <- sign(statistic_f(y[split$grp1 & label, dv]) -
                            statistic_f(y[split$grp1 & !label, dv]))

      # return the consistency of sign across groups
      is_consistent <- group0_sign == group1_sign
      # check if we got a tie where the effect was exactly zero for both groups
      is_sign_diff <- group0_sign | group1_sign
      # if we could not compute the result for one of the groups
      # invalid results may reflect missing values under a group and or label
      is_valid_result <- ! is.na(is_consistent) & is_sign_diff
      invalid_counter <- invalid_counter + 1
    }
    return (ifelse(is_valid_result, is_consistent, NA))
  }
  # apply the function above for each split
  consistency <- vapply(1:n_splits, inner_calculate_sign_consistency, logical(1))
  if(any(is.na(consistency))) {
    prop_na <- sum(is.na(consistency)) / length(consistency)
    warning(paste('could not compute consistency scores on', prop_na * 100,
                  '(%) of the splits\n check if there are enough observations in each cell) '))
  }
  # calculate the mean consistency across splits
  retVal <- base::mean(consistency, na.rm=TRUE)
  return (retVal)
}

#' @title Create Parameters For Sign Consistency
#' @description The function creates a list of parameters to be later passed to the sign consistency function.
#'
#' @param n_splits - The number of random splits to analyze for the estimation of sign consistency.
#' @param summary_function The summary function applied to the dependent variable(s), 'dv' under each split of the data.
#' @param max_invalid_reps - The maximal number repetitions in which invalid consistency was computed before returning NA result.
#' @param split_type - A string from {"noverlap" / "random"}, indicating if data should be split to non-overlapping halves (the
#' default option, "noverlap" option), or to two random halves ("random").
#'
#' @return a list of parameters that includes both arguments.
create_sign_consistency_params <- function(n_splits, summary_function, max_invalid_reps, split_type) {
  params <- list()
  params$n_splits <- n_splits
  params$summary_function <- summary_function
  params$max_invalid_reps = max_invalid_reps
  params$split_f <- ifelse(split_type == 'random', random_halves_split,
                           non_overlapping_halves_split)
  return (params)
}
