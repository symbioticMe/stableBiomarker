#' Iteration function to extract model performance and feature list for embedded methods
#'
#' @param response.column name of the column with responses
#' @param resampleIndex list of the integer indices of samples to be included in
#'  the model
#' @param inTrain indices to be included in the model
#' @param outTrain indices of the hold-out samples
#' @param method machine learning method to be used to build the model
#' @param fs.method feature selection method to be used to select features
#' @param fs.config configurations of the feature selection model
#' @param data data frame or data table used to build the model
#'
#' @return A list is returned  containing: \item{method
#' }{the object of class \code{train}.see \code{\link[caret]{train}}}
#' \item{importance}{list of variables (predictors) with their corresponding
#' importance values or coefficients, see \link[caret]{varImp}}
#' \item{results }{a data frame the training error rate and values of the
#' tuning parameters.} \item{bestTune }{a data frame with the final
#' parameters.}
#' @export
#'@import dplyr
#'@importFrom caret train varImp postResample trainControl
#'
#'@author Jelena Chuklina 
#'
#' the guts of \code{embeddedIter}
#' is a wrapper aroung the functions of \code{\link[caret]{train}})
#'
embeddedIter <- function(data, inTrain, outTrain,
                         method, method.config, fs.method, fs.config,
                         response.column = '.outcome',
                         metric = ifelse(is.factor(data[[response.column]]), 
                                         "Accuracy", "RMSE"),
                         optim.config = NULL,
                         verbose = F, ...){
  
  if (verbose){print("Next iteration...")}
  #for each resampling:
  training = data[inTrain, ]
  test = data[outTrain, ]
  
  var_training = var(training[[response.column]])
  var_test = var(test[[response.column]])
  
  if (is.null(optim.config)){
    method_name = method$label
    
    model.train = if (method_name == 'Random Forest') {
      #otherwise importance is not exported
      #ToDo: add if(!is.null(method.config)) {param.List = list(x = training %>%, y = training[[]], method  = method, metric = metric); model.train = do.call(train, c(method.config, paramList))}
      
      model.train = train(x = training %>% select(-one_of(response.column)),
                          y = training[[response.column]],
                          method = method, metric = metric, 
                          importance = T, ...)
    } else {
      model.train = train(x = training %>% select(-one_of(response.column)),
                          y = training[[response.column]],
                          method = method, metric = metric, ...)
    }
    
    
  } else {
    optim.config.methods = tolower(c("boot", "CV", "LOOCV", "LGOCV", "repeatedcv", "test"))
    if (!(optim.config$resampMethod %in% optim.config.methods)){
      stop(paste(optim.config$resampMethod, "method is not defined in caret built-in resampling methods"))
    }
    
    trControl = turnOptimConfigToTrainControl(optim.config)
    method_name = method$label
    if (method_name == 'Random Forest'){
      model.train = train(x = training %>% select(-one_of(response.column)),
                          y = training[[response.column]],
                          method = method,
                          trControl = trControl,
                          metric = optim.config$metric,
                          importance = T,
                          ...)
    } else {
      model.train = train(x = training %>% select(-one_of(response.column)),
                          y = training[[response.column]],
                          method = method,
                          trControl = trControl,
                          metric = optim.config$metric,  ...)
    }
  }
  
  
  if(verbose){
    print("Training of a model complete, extracting importance...")
    flush.console()}
  importance = varImp(model.train)$importance
  
  selected_features = rownames(importance)[importance[,1] != 0]
  
  if(verbose){
    print("fitting the resulting model to train & test data")
    flush.console()}
  
  # Fit the newly developed model ----------------------------------------------
  fitted.model = model.train$finalModel
  model = model.train$modelInfo
  train.fit.result = model$predict(fitted.model,
                                   newdata = training %>% select(-one_of(response.column)))
  test.fit.result = model$predict(fitted.model,
                                  newdata = test %>% select(-one_of(response.column)))
  
  #calculate the performance (in-Train and hold-out set) -----------------------
  perf.training = postResample(train.fit.result, training[[response.column]])
  perf.test = postResample(test.fit.result, test[[response.column]])
  
  
  return(list(model = fitted.model,
              importance = importance,
              selected_features = selected_features,
              perf.training = perf.training,
              perf.test = perf.test,
              method = method,
              fs.config = fs.config,
              fs.method = fs.method,
              var_test = var_test,
              var_training = var_training))
}