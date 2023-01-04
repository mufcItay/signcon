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
  y <- as.matrix(data[, dv])
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,iv)
  # binarization of labels => True for the 1st label, False for the 2nd label
  label <- label == dplyr::first(label)
  # get the parameters for the calculation of sign consistency
  n_splits <- params$n_splits
  statistic <- params$summary_function
  # check for errors
  if (nrow(y) != length(label)) {
    stop('inconsistent lengths for vectors the dependent and independent variables')
  }
  # set the number of trials and midpoint, to compute random splits of the data
  nTrials <- nrow(y)
  midpoint <- round(nTrials/2)
  # define an innner function to compute sign consistency in each repetition.
  # the function returns true if difference score signs are consistency across splits,
  # and false otherwise.
  inner_calculate_sign_consistency <- function(iteration) {
    # we run this function until we get a valdi result
    is_valid_result = FALSE
    invalid_counter <- 0
    while(invalid_counter < params$max_invalid_reps && !is_valid_result) {
      # sample a random permutation of the data
      order_permuatation = sample(nTrials)
      # define groups according to the permutation:
      # group0 = {all data points <= midpoint}, group1 {all data points > midpoint}
      group <- order_permuatation > midpoint
      # compute the sign of difference scores between groups, using the specified statistic
      group0_sign <- sign(statistic(y[!group & label, dv]) - statistic(y[!group & !label, dv]))
      group1_sign <- sign(statistic(y[group & label, dv]) - statistic(y[group & !label, dv]))
      # return the consistency of sign across groups
      is_consistent <- group0_sign == group1_sign
      # if we could not compute one of the groups the result is invalid
      # NAs may result from no values under one or more group and label combination (and NA == NA also returns NA)
      is_valid_result <- ! is.na(is_consistent)
      invalid_counter <- invalid_counter + 1
    }
    if (invalid_counter == max_resampling) {
      warning(paste0("Reached max resampling (=",max_resampling,"). Check if your data includes enough trials under all levels of the independent variable for each subject"))
    }
    return (ifelse(is_valid_result, is_consistent, NA))
  }
  # apply the function above for each split
  consistency <- sapply(1:n_splits, inner_calculate_sign_consistency)
  if(any(is.na(consistency))) {
    prop_na <- sum(is.na(consistency)) / length(consistency)
    warning(paste('could not compute consistency scores on', prop_na * 100,
                  '(%) of the splits\n check if there are enough observations in each cell) '))
  }
  # calculate the mean consistency across splits
  retVal <- mean(consistency, na.rm=TRUE)
  return (retVal)
}

#' @title Create Parameters For Sign Consistency
#' @description The function creates a list of parameters to be later passed to the sign consistency function.
#'
#' @param n_splits - The number of random splits to analyze for the estimation of sign consistency.
#' @param summary_function The summary function applied to the dependent variable(s), 'dv' under each split of the data.
#' @param max_invalid_reps - The maximal number repetitions in which invalid consistency was computed before returning NA result.
#'
#' @return a list of parameters that includes both arguments.
create_sign_consistency_params <- function(n_splits, summary_function, max_invalid_reps) {
  params <- list()
  params$n_splits <- n_splits
  params$summary_function <- summary_function
  params$max_invalid_reps = max_invalid_reps
  return (params)
}
