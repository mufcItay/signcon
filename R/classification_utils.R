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
#'   \item model -
#' }
#'
#' @return The classification accuracy of the classifier in classifying the 'iv' parameter based on the 'dv' parameter.
classify_conditions <- function(data, idv = "id", dv = "y", iv = "condition", params) {
  # get the parameters for the calculation of sign consistency
  model <- params$model
  K <- params$K
  imbalance <- params$handleImbalance
  labels <- as.factor(dplyr::pull(data,iv))
  data[,iv] <- as.factor(labels)
  # train the classifier according to the training control regime defined by the 'tc' parameter
  pred_formula <- as.formula(paste(iv, dv, sep = '~'))
  res <- train_classifier(pred_formula, data, idv, dv, iv, model, K, imbalance)
  return (res)
}

#' @title Create Parameters For Sign Consistency
#' @description The function creates a list of paramteres to be later passed to the sign consistency function.
#'
#' @param model -
#'
#' @return a list of parameters that includes both arguments.
create_classification_params <- function(model = NA, K = NA, handleImbalance = NA) {
  params <- list()
  # the default value of model and training control are (linear) SVM and Leave one out training control.
  if(is.na(model)) { model <- 'svmLinear' }
  params$model <- model
  if(is.na(K)) { K <- NA}
  params$K <- K
  if(is.na(handleImbalance)) { handleImbalance <- 'weights' }
  params$handleImbalance <- handleImbalance

  return (params)
}

#' Title
#'
#' @param formula
#' @param data
#' @param labels
#' @param model
#' @param trainControl
#' @param handleImbalance
#'
#' @return
train_classifier <- function(formula, data, idv = "id", dv = "y", iv = "condition", model, K, handleImbalance) {
  if (handleImbalance == 'weights') {
    labels <- as.factor(dplyr::pull(data,iv))
    ulabels <- unique(labels)
    x <- dplyr::pull(data,dv)
    y <- as.factor(dplyr::pull(data,iv))
    K = ifelse(is.na(K), min(table(labels)), K)

    if(model == 'svmLinear') {
      weightPerClass <- unlist(lapply(ulabels, function (l) (1 - length(labels[labels == l])/length(labels))))
      weights <- list()
      weights <- weightPerClass
      names(weights) <- as.character(ulabels)
      model <- ksvm(x = x, y= y, cross = K,
                class.weights=weights, kernel = "vanilladot")
      retVal <- 1 - model@error
    } else {
      trainControl <- trainControl(K)
      data$weight <- unlist(lapply(labels, function (l) (1/length(ulabels))/length(labels[labels == l])))
      res <- train(formula, data = data, method = model, trControl = trainControl, weights = data$weight)
      retVal <- res$results$Accuracy
    }
  }
  else	{
    if(model == 'svmLinear') {
      model <- ksvm(x = x, y= y, cross = K, kernel = "vanilladot")
      retVal <- 1 - model@error
    } else {
      trainControl <- trainControl(K)
      tc$sampling <- handleImbalance
      res <- train(formula, data = data, method = model, trControl = trainControl)
      retVal <- res$results$Accuracy
    }
  }
  return(retVal)
}
