#' @title Calculate Sign Consistency
#' @description The function applies the sign consistency analysis to the dataset of a specific individual
#' Called from 'get_true_score'
#'
#' @param data The dataset of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to calculate sign consistency. Includes:
#' \itemize{
#'   \item nSplits - The number of random splits to analyze for the estimation of sign consistency.
#'   \item summary_function - The summary function applied to the dependent variable (dv) under each split of the data.
#' }
#'
#' @return
calculate_sign_consistency <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the dependent variable column
  y <- dplyr::pull(data, dv)
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,iv)
  # binarization of labels => True for the 1st label, False for the 2nd label
  label <- label == dplyr::first(label)
  # get the parameters for the calculation of sign consistency
  nSplits <- params$nSplits
  statistic <- params$summary_function
  # check for errors
  if (length(y) != length(label)) {
    stop('inconsistent lengths for vectors the dependent and independent variables')
  }
  # set the number of trials and midpoint, to compute random splits of the data
  nTrials <- length(y)
  midpoint <- round(nTrials/2)
  # define an innner function to compute sign consistency in each repetition.
  # the function returns true if difference score signs are consistency across splits,
  # and false otherwise.
  inner_calculate_sign_consistency <- function(iteration) {
    # sample a random permutation of the data
    order_permuatation = sample(nTrials)
    # define groups according to the permutation:
    # group0 = {all data points <= midpoint}, group1 {all data points > midpoint}
    group <- order_permuatation > midpoint
    # compute the sign of difference scores between groups, using the specified statistic
    group0_sign <- sign(statistic(y[!group & label]) - statistic(y[!group & !label]))
    group1_sign <- sign(statistic(y[group & label]) - statistic(y[group & !label]))
    # return the consistency of sign across groups
    return (group0_sign == group1_sign)
  }
  # apply the function above for each split
  consistency <- sapply(1:nSplits, inner_calculate_sign_consistency)
  # calculate the mean consistency across splits
  retVal <- mean(consistency)
  return (retVal)
}

#' @title Create Parameters For Sign Consistency
#' @description The function creates a list of parameters to be later passed to the sign consistency function.
#'
#' @param nSplits - The number of random splits to analyze for the estimation of sign consistency.
#' @param summary_function The summary function applied to the dependent variable (dv) under each split of the data.
#'
#' @return a list of parameters that includes both arguments.
create_sign_consistency_params <- function(nSplits, summary_function) {
  params <- list()
  params$nSplits <- nSplits
  params$summary_function <- summary_function

  return (params)
}
