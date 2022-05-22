#' @title Prepare Participant Data
#' @description Returns preprocessed data for a specific participant.
#'
#' @param data The data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable(s), 'dv', is expected to differ.
#' @param preprocessFs A vector of functions to apply to the data for preprocessing
#' @param preprocessArgs A vector of arguments for the preprocessing functions
#'
#' @return processed participant data, ready to be analyzed
prepare_participant_data <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs, preprocessArgs) {
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

#' @title Get Null Distribution
#' @description The function returns the null distribution according to the 'params' argument, and permutation analysis configuration.
#'
#' @param data the data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params Configuration for the function to apply to the data of each participant ('f')
#' @param f The function to apply to the data of each participant, returning the value of interest for the analysis.
#' @param perm_repetitions The number of permutation repetitions for each participant
#' @param null_dist_samples The number of samples that comprise the output null distribution
#' @param preprocessFs vector of functions to apply to the data for preprocessing. The default value of the argument (empty list), will result in shuffling the labels of the independent variable, iv.
#' @param preprocessArgs vector of arguments for the preprocessing functions
#'
#' @return A distribution of mean score values computed according to a 'null' effect condition
get_null_distribution <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, perm_repetitions = 25, null_dist_samples = 10000, preprocessFs = c(), preprocessArgs = c()) {
  shuffled_scores <- sapply(1:perm_repetitions, params$nullDistFunc, data = data, idv = idv, dv = dv, iv = iv,
                            preprocessFs = preprocessFs, preprocessArgs = preprocessArgs, params = params, f = f)
  # define a function that computes a sample of the null distribution from the shuffled data of all participants,
  # randomly sample a specific permutation for each participant, and return the mean score of the group
  get_null_sample <- function(iteration) {
    # randomly sample one permutation per participant
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

#' @title Get Scores Per Participant
#' @description The function computes and returns the true score of the analysis to conduct for each participant,
#' as specified by the 'f' and 'params' arguments.
#'
#' @param data the dataset of a all participants to analyze.
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variable(s) to apply the summary function (summary_function) to.
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable(s), 'dv', is expected to differ. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis. The default value of the argument (empty list) will result in arrabging the dataset according to the independent variable, iv.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs' (in order, meaning preprocessFs[i](preprocessArgs[i]) will be invoked for each i).
#' @param params Configuration for the function to apply to the data of each participant ('f').
#' @param f The function to apply to the data to compute the score of interest for each participant.
#'
#' @return The function returns a data frame, mapping the 'idv' column with a 'score' columns of the scores of each participant.
get_scores_per_participant <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs = c(), preprocessArgs = c(), params, f) {
  # define a preprocessing function that arranges data uniformly according to the values of the independent variable
  preprocessFs <- c(dplyr::arrange, preprocessFs)
  preprocessArgs = c(iv, preprocessArgs)
  # return the score of each participant by running 'f' on its 'prepared' data
  return (data |>
            dplyr::group_by(!!dplyr::sym(idv)) |>
            dplyr::group_modify(~data.frame(score = f(prepare_participant_data(.x,idv, dv, iv, preprocessFs, preprocessArgs), idv, dv, iv, params))))
}


#' @title Get Shuffled Score
#' @description The function shuffles the labels the independent variable (according to the value of 'iv'),
#' and returns result of the function f on the shuffled dataset.
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
get_shuffled_score <- function(idx, data, idv, dv, iv, preprocessFs, preprocessArgs, params, f) {
  # define a preprocessing function that shuffles the independent variable column for each participant
  preprocessFs <- c(preprocessFs, function(data,col) {
    # randomly shuffle the labels under 'col' column
    data[col] <- sample(dplyr::pull(data,col))
    return(data)
  })
  # sepcify that we shuffle the indepdent variable column ('iv')
  preprocessArgs <- c(preprocessArgs, iv)

  # get the scores per participant for the shuffled data
  res <- get_scores_per_participant(data, idv, dv, iv, preprocessFs, preprocessArgs, params, f)

  # return the score calculated for the shuffled data
  return(res$score)
}
