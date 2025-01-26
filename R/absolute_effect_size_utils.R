
#' @title Calculate Absolute Effect Size
#' @description The function gets the absolute effect size for a specific participant.
#' Called from 'test_absolute_es'
#'
#' @param data The dataset of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.  For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to calculate absolute effect size. Includes:
#' \itemize{
#'   \item summary_function - The summary function applied to the dependent variable(s), 'dv', under each split of the data.
#' }
#'
#' @return the function returns the absolute effect size for the given data
calculate_absolute_es <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the dependent variable column
  y <- data |> dplyr::select(tidyr::all_of(dv))
  # get the independent variable column (items are labels describing the experimental conditions)
  label <- dplyr::pull(data,!!dplyr::sym(iv))
  # binarization of labels => True for the 1st label, False for the 2nd label
  label <- label == dplyr::first(label)
  # get the parameters for the calculation of sign consistency
   # get the parameters for the calculation of sign consistency
  statistic_f <- params$summary_function
  # check for errors
  if (nrow(y) != length(label)) {
    stop('inconsistent lengths for vectors the dependent and independent variables')
  }
  # calculate the absolute effect size
  absVal <- abs(statistic_f(y[label, dv],y[!label, dv]))
  return (absVal)
}


