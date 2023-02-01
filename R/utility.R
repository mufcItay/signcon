#' @title Prepare Participant Data
#' @description Returns preprocessed data for a specific participant.
#'
#' @param data The data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv The names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv Labels of an independent variable, indicating the different levels under which the dependent variable(s), 'dv', is expected to differ.
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis.
#' The function accepts a 'data' argument (a data frame including the data of an individual),
#' and an 'args' argument which includes all additional arguments needed to run the preprcessing function.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs' (in order, meaning preprocessFs[i](preprocessArgs[i]) will be invoked for each i).
#' For example, if the preprocessing function accepts arguments 'a','b' as inputs, preprocessArgs should be set to list(a,b)
#'
#' @return processed participant data, ready to be analyzed
prepare_participant_data <- function(data, idv = "id", dv = "rt", iv = "condition", preprocessFs, preprocessArgs) {
  # iterate over the preprocessing functions and apply them to the data, with the 'preprocessArgs' as arguments
  for (fInd in 1:length(preprocessFs)) {
    # extract the current function and arguments
    perprocess_f <- preprocessFs[[fInd]]
    perprocess_args <- preprocessArgs[[fInd]]
    # apply the preprocessing function given its arguments, and update 'data'
    data <- do.call(perprocess_f, list(data = data, args = perprocess_args))
  }
  return(data)
}

#' @title Get Null Distribution Perm
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
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis.
#' The default value of the argument (empty list), will result in shuffling the labels of the independent variable, iv.
#' The function accepts a 'data' argument (a data frame including the data of an individual),
#' and an 'args' argument which includes all additional arguments needed to run the preprcessing function.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs' (in order, meaning preprocessFs[i](preprocessArgs[i]) will be invoked for each i).
#' For example, if the preprocessing function accepts arguments 'a','b' as inputs, preprocessArgs should be set to list(a,b)
#'
#' @return A distribution of mean score values computed according to a 'null' effect condition
get_null_distribution_perm <- function(data, idv = "id", dv = "rt", iv = "condition", params, f, perm_repetitions = 25, null_dist_samples = 10000, preprocessFs = c(), preprocessArgs = c()) {
  print('Generating null distribution')
  pb <- utils::txtProgressBar(0,perm_repetitions,0)
  # get scores from the null distribution for each permutation sampled within each subject.
  # we calcualte the scores based on the 'null_dist_f' argument in 'params'.
  null_scores <- sapply(1:perm_repetitions, function(idx, data, idv, dv, iv, preprocessFs, preprocessArgs, params, f, pb) {
    utils::setTxtProgressBar(pb, idx)
    get_shuffled_score(data = data, idv = idv, dv = dv, iv = iv,
                       preprocessFs = preprocessFs, preprocessArgs = preprocessArgs, params = params, f = f)
    }, data = data, idv = idv, dv = dv, iv = iv,
    preprocessFs = preprocessFs, preprocessArgs = preprocessArgs, params = params, f = f, pb = pb)
  close(pb)
  # define a function that computes a sample of the null distribution from the data of all participants,
  # randomly sample a specific permutation for each participant, and return the mean score of the group
  get_null_sample <- function(iteration, pb) {
    # randomly sample one permutation per participant
    rndShuff <- sample(perm_repetitions,size = nrow(null_scores), replace = TRUE)
    # get the sampled permutation scores
    sampled <- unlist(null_scores[rndShuff + (seq(0,length(rndShuff) - 1)) * perm_repetitions])
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
  preprocessFs <- append(preprocessFs, function(data,args) {
    # randomly shuffle the labels under 'args' column
    data <- data |> dplyr::arrange(!!dplyr::sym(args))
    return(data)
  })
  preprocessArgs = append(preprocessArgs, list(iv))
  # return the score of each participant by running 'f' on its 'prepared' data
  return (data |>
            dplyr::group_by(!!dplyr::sym(idv)) |>
            dplyr::group_modify(~data.frame(score = f(prepare_participant_data(.x,idv, dv, iv, preprocessFs, preprocessArgs), idv, dv, iv, params))))
}


#' @title Get Shuffled Score
#' @description The function shuffles the labels the independent variable (according to the value of 'iv'),
#' and returns result of the function f on the shuffled dataset.
#'
#' @param data the data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv the names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param preprocessFs An ordered list of functions to apply to the dataset before starting with the analysis.
#' The default value of the argument (empty list), will result in shuffling the labels of the independent variable, iv.
#' The function accepts a 'data' argument (a data frame including the data of an individual),
#' and an 'args' argument which includes all additional arguments needed to run the preprcessing function.
#' @param preprocessArgs An ordered list of function arguments, to be used when invoking the 'preprocessFs' (in order, meaning preprocessFs[i](preprocessArgs[i]) will be invoked for each i).
#' For example, if the preprocessing function accepts arguments 'a','b' as inputs, preprocessArgs should be set to list(a,b)
#' @param params configuration for the function to apply to the data of each participant ('f')
#' @param f the function to apply to the data of each participant, returning the value of interest for the analysis.
#'
#' @return the function returns the score calculated by applying the function 'f' to the data after shuffling the labels of the indepdent variable 'iv'.
get_shuffled_score <- function(data, idv, dv, iv, preprocessFs, preprocessArgs, params, f) {
  # define a preprocessing function that shuffles the independent variable column for each participant
  preprocessFs <- append(preprocessFs, function(data,args) {
    # randomly shuffle the labels under 'args' column
    data[args] <- sample(dplyr::pull(data,args))
    return(data)
  })
  # specify that we shuffle the independent variable column ('iv')
  preprocessArgs <- append(preprocessArgs, iv)

  # get the scores per participant for the shuffled data
  res <- get_scores_per_participant(data, idv, dv, iv, preprocessFs, preprocessArgs, params, f)


  # return the score calculated for the shuffled data
  return(res$score)
}


#' Get Boot CI
#' The function gets participant-level scores, and calculates a bootstrapped confidence
#' interval (CI) on the group-level statistic
#' @param scores participant-level scores
#' @param ci_level - The confidence level (in percents, e.g. setting the argument to 50 generates a 50% CI)
#' to use when computing the bootstrapped confidence interval on the group-level
#' statistic (see the return value 'ci', and the argument 'ci_reps').
#' The default value of this argument is 95, that would lead to computing the 95% confidence interval for
#' the group-level statistic.
#' @param ci_reps - The number repetitions to use when computing the bootstrapped confidence interval
#' around the group-level statistic.
#' The default value of this argument is zero, which would lead to not computing the confidence interval at all.
#'
#' @return a vector with the ci values (lower bound, upper bound) according to the given
#' confidence level ('ci_level'). If 'ci_reps' is set to zero, the function returns NA.
get_boot_ci <- function(scores, ci_level = 95, ci_reps = 0) {
  if(ci_reps == 0) {
    return (NA)
  }
  print('Calcualting a bootstrapped CI')
  dist <- sapply(1:ci_reps,
                 function(iter, data) mean(sample(data, length(data), replace = TRUE)),
                 data = scores)
  # convert from confidence level to alpa (and percent to ratio)
  alpha <- 1-(ci_level / 100)
  ci = stats::quantile(dist,probs = c(alpha/2, 1-alpha/2))
  return(ci)
}

#' Validate Data
#' @description The function checks if the dataset is valid by performing different checks on it.
#'
#' @param data the data of a specific participant, arranged according to the independent variable ('iv')
#' @param idv The name of the participant identifier column.
#' @param dv the names of the dependent variable(s) to apply the summary function (summary_function) to. For multiple dependent variables use a string list with the names of each dependent variable (e.g., c('dv1','dv2')),
#' @param iv labels of an independent variable, indicating the different levels under which the dependent variable (dv) is expected to differ.
validate_data <- function(data, idv, dv, iv) {
  # checks if any cell in the dataset contains an invalid value (NA / NaN).
  if(any(sapply(as.matrix(data), function(x) (is.na(x) | is.nan(x))))) {
    stop("Execution terminated.\nThe data contains an invalid value (NA / NaN)")
  }
  # checks if all column names exist
  cols <- c(idv,dv,iv)
  if(length(cols) > length(colnames(data))) {
    stop(paste0('Missing columns, there are only: ', length(colnames(data)),
                ' columns in the dataset'))
  }
  missing_cols_str <- sapply(cols, function(c) ifelse(!(c %in% names(data)), c, ''))
  missing_cols <- missing_cols_str[missing_cols_str != '']
  if(length(missing_cols != 0)) {
    stop(paste0('Could not find columns: ', paste(missing_cols, sep = ','),
                '\nMake sure to set the relevant column names as they appear in the data object'))
  }
}
