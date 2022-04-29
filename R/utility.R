#' @title Get Subject Data
#' @description Returns processed data for  a specific subject.
#'
#' @param data The data of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param preprocessFs A vector of functions to apply to the data for preprocessing
#' @param preprocessArgs A vector of arguments for the preprocessing functions
#'
#' @return processed subject data, prepared to be analyzed
prepare_subject_data <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs, preprocessArgs) {
  # iterate over the preprocessing functions and apply them to the data, with the 'preprocessArgs' as arguments
  for (fInd in 1:length(preprocessFs)) {
    # extract the current function and arguments
    perprocess_f <- preprocessFs[[fInd]]
    perprocess_args <- preprocessArgs[[fInd]]
    # apply the preprocessing function given its arguments, and update 'data'
    data <- do.call(perprocess_f, list(data, perprocess_args))
  }
  return(data)
}

#' @title Get the Null Distribution
#' @description The function returns the null distribution according to the 'statistic' and 'configuration' arguments.
#'
#' @param data the data of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params Configuration for the function to apply to the data of each individual ('f')
#' @param f The function to apply to the data of each individual ('f')
#' @param perm_repetitions The number of permutation reperitions for each individual
#' @param null_dist_samples The number of samples that comprise the output null distribution
#' @param preprocessFs vector of functions to apply to the data for preprocessing
#' @param preprocessArgs vector of arguments for the preprocessing functions
#'
#' @return A distribution of mean score values computed according to a 'null' effect condition
get_null_distribution <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, perm_repetitions = 25, null_dist_samples = 10000, preprocessFs = c(), preprocessArgs = c()) {
  # define a preprocessing function that shuffled the independent variable column for each individual,
  # and then gets the scores of each subject for shuffled data
  inner_shuffle <- function(idx) {
    preprocessFs <- c(preprocessFs, function(d,col) {
      # randomly shuffle the independe variable values
      d[col] <- sample(dplyr::pull(d,col))
      # returning the shuffled data
      return(d)
    })
    preprocessArgs <- c(preprocessArgs, iv)
    # get the scores per individual for the suffled data
    get_scores_per_subject(data, idv, dv, iv, preprocessFs, preprocessArgs, params, f)
  }
  # use the inner_shuffle function to create #'perm_repetitions' per individual
  shuffled_scores <- sapply(1:perm_repetitions, inner_shuffle)
  # define a function that computes a sample of the null distribution from the shuffled data of all individuals,
  # randomly sample a specific permutation for each individual, and retunr the mean score of the group
  get_null_sample <- function(iteration) {
    # randomly sample one permutation per individual
    rndShuff <- sample(perm_repetitions,size = nrow(shuffled_scores), replace = TRUE)
    # get the sampled permutation scores
    sampled <- unlist(shuffled_scores[rndShuff + (seq(0,length(rndShuff) - 1)) * perm_repetitions])
    # return the mean score of all sampled permutations
    return(mean(sampled))
  }
  # use the 'get_null_sample' to compute the group level mean score in each sample to build the distribution
  null_dist <- sapply(1:null_dist_samples, get_null_sample)
  return(null_dist)
}

#' @title Get True Scores
#' @description The function computes and returns the true score of the analysis to conduct for each individual,
#'as specified by the 'f' and 'params' arguments.
#'
#' @param data the dataset of a all individuals to analyze.
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs'.
#' @param params Configuration for the function to apply to the data of each individual ('f').
#' @param f The function to apply to the data to compute the score of interest for each individual.
#'
#' @return The function returns a list of the computed scores of each individual
get_scores_per_subject <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs = c(), preprocessArgs = c(), params, f) {
  # define a preprocessing function that arranges data uniformly according to the values of the independent variable
  preprocessFs <- c(dplyr::arrange, preprocessFs)
  preprocessArgs = c(iv, preprocessArgs)
  # return the score of each individual by running 'f' on its 'prepared' data
  return (data %>%
            dplyr::group_by(!!dplyr::sym(idv)) %>%
            dplyr::group_map(~f(prepare_subject_data(.x,idv, dv, iv, preprocessFs, preprocessArgs), idv, dv, iv, params)))
}
