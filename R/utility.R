#' Get Subject Data
#' Returns processed data for  a specific subject.
#'
#' @importFrom magrittr %>%
#' @param data the data of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param preprocessFs vector of functions to apply to the data for preprocessing
#' @param preprocessArgs vector of arguments for the preprocessing functions
#'
#' @return processed subject data, prepared to be analyzed
prepare_subject_data <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs, preprocessArgs) {
  for (fInd in 1:length(preprocessFs)) {
    data <- base::do.call(preprocessFs[[fInd]], base::list(data, preprocessArgs[[fInd]]))
  }
  return(data)
}


#' Get Null Distribution
#' The function returns the null distribution according to the 'statistic' and 'configuration' arguments.
#'
#' @param data the data of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params
#' @param f
#' @param perm_repetitions
#' @param null_dist_samples
#' @param preprocessFs vector of functions to apply to the data for preprocessing
#' @param preprocessArgs vector of arguments for the preprocessing functions
#'
#' @return
get_null_distribution <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, perm_repetitions = 25, null_dist_samples = 10000, preprocessFs = c(), preprocessArgs = c()) {
  inner_shuffle <- function(idx) {
    preprocessFs <- c(preprocessFs, function(d,col) {
      d[col] <- base::sample(dplyr::pull(d,col))
      return(d)
    })
    preprocessArgs <- c(preprocessArgs, iv)

    get_scores_per_subject(data, idv, dv, iv, params, f, preprocessFs, preprocessArgs)
  }
  shuffled_scores <- base::sapply(1:perm_repetitions, inner_shuffle)
  get_null_sample <- function(iteration) {
    rndShuff <- base::sample(perm_repetitions,size = base::nrow(shuffled_scores), replace = TRUE)
    sampled <- base::unlist(shuffled_scores[rndShuff * nrow(shuffled_scores) + 1:base::nrow(shuffled_scores)])
    return(base::mean(sampled))
  }
  null_dist <- base::sapply(1:null_dist_samples, get_null_sample)
  return(null_dist)
}

#' Get True Scores
#' The function computes and returns the true score of the analysis to conduct for each individual,
#'as specified by the 'f' and 'params' arguments.
#'
#' @importFrom magrittr %>%
#' @param data the dataset of a all individuals to analyze.
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params Configuration for the function to apply to the data of each individual ('f')
#' @param f The function to apply to the data to compute the score of interest for each individual.
#' @param preprocessFs
#' @param preprocessArgs
#'
#' @return The function returns a list of the computed scores of each individual
get_scores_per_subject <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, preprocessFs, preprocessArgs) {
  preprocessFs <- c(dplyr::arrange, preprocessFs)
  preprocessArgs = c(iv, preprocessArgs)
  return (data %>%
            dplyr::group_by(!!dplyr::sym(idv)) %>%
            dplyr::group_map(~f(prepare_subject_data(.x,idv, dv, iv, preprocessFs, preprocessArgs), idv, dv, iv, params)))
}
