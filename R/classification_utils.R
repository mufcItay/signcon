#' @title Classify Conditions
#' @description The function fits the data of an individual with the given classifier and than uses of a specific individual.
#' Called from 'get_true_score'
#'
#' @param data The dataset of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to classify conditions according to.
#' @param iv Labels of an independent variable - the condition to classify, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param params A list of parameters used by the function to calculate sign consistency. Includes:
#' \itemize{
#'   \item model - the name of the classification model to use.
#'   \item K - the number of folds to use when calculating the performance of the classifier.
#'   \item handleImbalance - a string indicating whether and which class imbalance adjustment to use.
#' }
#'
#' @return The classification accuracy of the classifier in classifying the 'iv' parameter based on the 'dv' parameter.
classify_conditions <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the classifier parameters to use
  model <- params$model
  K <- params$K
  imbalance <- params$handleImbalance
  # get the condition names to classify, and cast them as a factor variable
  labels <- as.factor(dplyr::pull(data,iv))
  data[,iv] <- as.factor(labels)
  # set the classification formula
  pred_formula <- stats::as.formula(paste(iv, dv, sep = '~'))
  # train the classifier according to the configuration defined by the 'params' argument
  res <- train_classifier(pred_formula, data, idv, dv, iv, model, K, imbalance)
  return (res)
}

#' @title Create Parameters For Classification
#' @description The function creates a list of parameters to be later passed to the classification
#' @param model - the name of the classification model to use. Currently supported value: 'linear' (a SVM classifier with a linear kernel from the 'e1071' package).
#' @param K - the number of folds to use when calculating the performance of the classifier. The default value is set to the number of observations of the minority class.
#' @param handleImbalance - a string indicating whether and which class imbalance adjustment to use.
#' Currently supported value: 'weights' - handles imbalance by assigning different weights for each class that should balance the sample.
#'
#' @return a list of parameters that includes all arguments after applying default values.
create_classification_params <- function(model = NA, K = NA, handleImbalance = NA) {
  params <- list()
  # the default value is a linear SVM classifier and Leave one out training control.
  if(is.na(model)) { model <- 'SVMLinear' }
  params$model <- model
  # the default value is set to NA, to be handles later in the training function and set to the number of observations of the minority class.
  if(is.na(K)) { K <- NA}
  params$K <- K
  # the default value is set to the only currently available value of 'weights' which handles imbalance by assigning different weights for each class that should balance the sample.
  if(is.na(handleImbalance)) { handleImbalance <- 'weights' }
  params$handleImbalance <- handleImbalance

  return (params)
}

#' Train Classifier
#'
#' @param formula the formula that defines the classification task (e.g., iv ~ dv, in the general case).
#' @param data The dataset of a specific individual, arranged according to the independent variable ('iv')
#' @param idv The name of the subject identifier column.
#' @param dv The dependent variable to classify conditions according to.
#' @param iv Labels of an independent variable - the condition to classify, indicating the different levels under which the dependent variable (dv) is expected to differ.
#' @param model - the name of the classification model to use.
#' @param K - the number of folds to use when calculating the performance of the classifier. If K is set to 'NA', the function resets it to the number of observations of the minority class.
#' @param handleImbalance - a string indicating whether and which class imbalance adjustment to use.
#'
#' @return the function returns the trained classifier accuracy rate
train_classifier <- function(formula, data, idv = "id", dv = "y", iv = "condition", model, K, handleImbalance) {
  # gets all variables needed for classification
  labels <- as.factor(dplyr::pull(data,iv))
  ulabels <- unique(labels)
  x <- dplyr::pull(data,dv)
  y <- as.factor(dplyr::pull(data,iv))
  data$x <- x
  data$y <- y
  # if K is set to 'NA', reset it to the minority class (maintaining a minimum of 1 sample per label in the validation set of each fold).
  K = ifelse(is.na(K), min(table(labels)), K)
  folds <- caret::createFolds(y, k = K, list = TRUE, returnTrain = TRUE)

  # handle the 'weights' imbalance handling technique by assigning different weights to each class,
  # to balance the sample of labels.
  if (handleImbalance == 'weights') {
    # if the model was set to a SVM classifier with a linear kernel, use e1071's classifier.
    if(model == 'SVMLinear') {
      # calculate the weight of each class in the labels column,
      weights <- min(table(labels)) / table(labels)

      # train the model, and get its accuracy
      res <- sapply(folds, function(f, data) {
        train <- data[f, ]
        test <- data[-f, ]
        model <- e1071::svm(x = train$x, y= train$y, kernel = "linear", class.weights = weights)
        pred <- stats::predict(model, test$x)
        accuracy <- mean(test$y == pred)
        return (accuracy)
      }, data = data)
      retVal <- mean(unlist(res))
    } else { # in case the user chose an alternative model, use the 'caret' package (TODO: NOT CHECKED)

      # set training control - a configuration for the training regime
      trainControl <- caret::trainControl(K)
      # set weights to each sample
      weights <- min(table(labels)) / table(labels)
      # train the model and return accuracy rate
      res <- caret::train(formula, data = data, method = model, trControl = trainControl, weights = weights)
      retVal <- res$results$Accuracy
    }
  }
  # if the use chose an alternative class imbalance handling technique
  else	{
    # if the model was set to a SVM classifier with a linear kernel, use e1071's classifier.
    if(model == 'SVMLinear') {
      # train the model without using any handle imbalance technique (TODO: NOT SUPPORTING ALTERNATIVE CLASS IMBALANCE HANDLING TECHNIQUES)
      res <- sapply(folds, function(f, data) {
        train <- data[f, ]
        test <- data[-f, ]
        model <- e1071::svm(x = x, y= y, cross = K, kernel = "linear")
        pred <- stats::predict(model, test$x)
        accuracy <- mean(test$y == pred)
        return (accuracy)
      }, data = data)
      retVal <- mean(unlist(res))
    } else { # in case the user chose an alternative model, use the 'caret' package (TODO: NOT CHECKED)
      # set training control - a configuration for the training regime
      trainControl <- caret::trainControl(K)
      # set the class imbalance parameter accordign to user's preference
      tc$sampling <- handleImbalance
      # train the model and return accuracy rate
      res <- caret::train(formula, data = data, method = model, trControl = trainControl)
      retVal <- res$results$Accuracy
    }
  }
  return(retVal)
}
