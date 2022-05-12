#' @title Classify Conditions
#' @description The function fits the data of an individual with a linear SVM classifier and than uses of a specific individual.
#' Called from 'get_true_score'
#'
#' @param data The dataset of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The name of the dependent variable column to classify conditions according to.
#' @param iv The name of the independent variable column - the condition to classify,
#' indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to perform the classification task. Includes:
#' \itemize{
#'   \item K - the number of folds to use when calculating the performance of the classifier.
#'   \item handleImbalance - A Boolean indicating whether to adjust class imbalance (using different weight for each label)
#' }
#' @return The classification accuracy of the classifier in classifying the 'iv' parameter based on the 'dv' parameter.
classify_conditions <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the classifier parameters to use
  K <- params$K
  imbalance <- params$handleImbalance
  # train the classifier according to the configuration defined by the 'params' argument,
  # an get its cross validated accuracy
  res <- get_classifier_accuracy(data, idv, dv, iv, K, imbalance)
  return (res)
}

#' @title Create Parameters For Classification
#' @description The function creates a list of parameters to be later passed to the classification function.
#'
#' @param K - the number of folds to use when calculating the performance of the classifier. The default value is set to the number of observations of the minority class.
#' @param handleImbalance - A Boolean indicating whether to adjust class imbalance (using different weight for each label).
#'
#' @return a list of parameters that includes all arguments after applying default values.
create_classification_params <- function(K = NA, handleImbalance = NA) {
  params <- list()
  # the default value is set to NA, to be set to the number of observations of the minority class.
  if(is.na(K)) { K <- NA}
  params$K <- K
  # the default value is 'TRUE', which results in assigning different weights to each class,
  # aiming at balancing the sample
  if(is.na(handleImbalance)) { handleImbalance <- TRUE }
  params$handleImbalance <- handleImbalance

  return (params)
}

#' Get Classifier Accuracy
#' @description The function calculates the cross-validated classification accuracy for the condition labels, by the dependent variable. for a specific participant's data
#' Called from 'classify_conditions'

#' @param data The dataset of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The name of the dependent variable column to classify conditions according to.
#' @param iv The name of the independent variable column - the condition to classify,
#' indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param K - the number of folds to use when calculating the performance of the classifier.
#' If K is set to 'NA', the function set it to the number of observations of the minority class.
#' @param handleImbalance - A Boolean indicating whether to adjust class imbalance (using different weight for each label)
#'
#' @return the function returns the trained classifier accuracy rate
get_classifier_accuracy <- function(data, idv = "id", dv = "y", iv = "condition", K, handleImbalance) {
  # adds all variables needed for classification (copy variables to 'x' and 'y' columns for simplicity)
  data <- data |> dplyr::mutate(x = dplyr::pull(data,dv), y = as.factor(dplyr::pull(data,iv)))
  # if K is set to 'NA', reset it to the minority class,
  # this should keep a minimum of 1 sample per label in the validation set of each fold.
  K = ifelse(is.na(K), min(table(data$y)), K)
  # create folds for cross validation procedure
  folds <- caret::createFolds(data$y, k = K, list = TRUE, returnTrain = TRUE)

  # handle the 'weights' imbalance handling technique by assigning different weights to each class,
  # to balance the sample of labels.
  if (handleImbalance) {
    # calculate the weight of each class in the labels column
    weights <- min(table(data$y)) / table(data$y)
  }  else	{
    # default weights => each label is assigned a weight of 1
    weights <- table(data$y) / table(data$y)
  }
  # train the model, and get its accuracy
  res <- sapply(folds, function(f, data, weights) {
    # split to train and test based on the current fold
    train <- data[f, ]
    test <- data[-f, ]
    # create the model and train it
    model <- e1071::svm(x = train$x, y= train$y, kernel = "linear", class.weights = weights)
    # predict condition labels using the trained model
    pred <- stats::predict(model, test$x)
    # calculate accuracy
    accuracy <- mean(test$y == pred)
    return (accuracy)
  }, data = data, weights = weights)

  # calculate the average accuracy across folds
  retVal <- mean(unlist(res))
  return(retVal)
}
