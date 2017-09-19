#' Main function to train the model and get a (ranked) list of features
#'
#' @param data an object where samples are in rows and features are in columns
#' and one column is the response. This can be a data.frame or data.table.
#' Other types not tested
#' @param response a string specifying the name of the column with the response
#' @param method a string specifying which model to use
#' @param fs.method a string specifying which feature selection method to use
#' @param fs.config a list specifying configurations of the feature selection method
#' @param times number of repetitions for resampling
#' @param resamplingConfig configuration of resampling
#'
#' @return list containing:
#' \item{result}{TO FIX}
#' \item{next item}{TO FIX}
#'
#' @export
#' @importFrom parallel mclapply detectCores
train_main <- function(data, response = '.outcome',
                       method = 'rf', fs.method = 'TopN',
                       fs.config = ifelse(fs.method == 'TopN', 
                                          list(top.n = 100), NULL),
                       repeats = 100,
                       resamplingConfig = list(resampMethod = 'LGOCV', 
                                               resampConfig = list(p = .9), 
                                               repetitions = 10),
                       metric = ifelse(is.factor(data[[response.column]]), 
                                       "Accuracy", "RMSE"),
                       optim.config = NULL,
                       parallel = F, verbose = F, ...){
  
  startTime <- proc.time()
  #create resampling index
  set.seed(1)
  resampleIndex <- create_resampling(response = data[[response]],
                                     resamplingConfig,
                                     times = repeats)
  
  if (verbose) {print('Resampling created'); flush.console()}
  
  nr = nrow(data)
  outTrain <- lapply(resampleIndex,
                     function(inTrain, total) total[-unique(inTrain)],
                     total = seq(nr))
  if (!parallel){
    result = list()
    for (k in 1:repeats){
      #result <- lapply(1:repeats, function(k)
      print(k)
      print(resampleIndex[[k]])
      print(outTrain[[k]])
      if(is(tryCatch(train_with_feature_extraction(data, inTrain = resampleIndex[[k]],
                                                   outTrain = outTrain[[k]],
                                                   method = method,
                                                   fs.method = fs.method,
                                                   fs.config = fs.config,
                                                   response.column = response,
                                                   metric = metric,
                                                   optim.config = optim.config,
                                                   verbose = verbose, ...), 
                     warning = function(w) w), 'warning')){
        result[[k]] <- train_with_feature_extraction(data, inTrain = resampleIndex[[k]],
                                      outTrain = outTrain[[k]],
                                      method = method,
                                      fs.method = fs.method,
                                      fs.config = fs.config,
                                      response.column = response,
                                      metric = metric,
                                      optim.config = optim.config,
                                      verbose = verbose, ...)
        print(result[[k]]$model.train$resamples)
      } else {
        result[[k]] <- train_with_feature_extraction(data, inTrain = resampleIndex[[k]],
                                                     outTrain = outTrain[[k]],
                                                     method = method,
                                                     fs.method = fs.method,
                                                     fs.config = fs.config,
                                                     response.column = response,
                                                     metric = metric,
                                                     optim.config = optim.config,
                                                     verbose = verbose, ...)
      }
      
      #)
    }
    
  } else {
    mc.cores = detectCores()
    if(verbose){print(sprintf('Going parallel, using %s cores...', mc.cores))}
    result <- mclapply(1:repeats, function(k)
      train_with_feature_extraction(data, inTrain = resampleIndex[[k]],
                                    outTrain = outTrain[[k]],
                                    method = method,
                                    fs.method = fs.method,
                                    fs.config = fs.config,
                                    response.column = response,
                                    metric = metric,
                                    optim.config = optim.config,
                                    verbose = verbose, ...),
      mc.cores = mc.cores)
  }
  
  if(verbose){print("Training complete, preparing results for output...")}
  importance = lapply(result, function(res) res$importance)
  performance.train = lapply(result, function(res) res$perf.training)
  performance.test = lapply(result, function(res) res$perf.test)
  selected.features = lapply(result, function(res) res$selected_features)
  
  res_names = paste("Rep", gsub(' ','0', format(1:repeats)), sep ='')
  names(importance) = res_names
  names(performance.train) =  res_names
  names(performance.test) = res_names
  
  endTime <- proc.time()
  
  return(list(importance = importance,
              selected.features = selected.features,
              performance.test = performance.test,
              performance.train = performance.train,
              repeats = repeats,
              fs.config = fs.config,
              resamplingConfig = resamplingConfig,
              method = method,
              resamples = result,
              fs.method = fs.method,
              optim.config = optim.config,
              times = endTime - startTime,
              dots = list(...)))
}

#' Workhorse function to extract model performance and feature list
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
#'@author Jelena Chuklina (the guts of \code{train.with.feature.extraction}
#' is a wrapper aroung the functions of \code{\link[caret]{train}})
#'
#'

train_with_feature_extraction <- function(data, inTrain, outTrain,
                                          method, fs.method, fs.config,
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
    #fitControl = trainControl(method = 'none', summaryFunction = summaryFunction)
    model.train = if (method_name == 'Random Forest') {
      
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
    #TODO configure trainControl for cases, other than CV
    optim.config.methods = tolower(c("boot", "CV", "LOOCV", "LGOCV", "repeatedcv", "test"))
    if (!(optim.config$resampMethod %in% optim.config.methods)){
      stop(paste(optim.config$resampMethod, "method is not defined in caret built-in resampling methods"))
    }
    
    trControl = switch(tolower(optim.config$resampMethod),
                       boot =       trainControl(method = optim.config$resampMethod,
                                                 number = ifelse(is.numeric(optim.config$resampConfig$number),
                                                                 optim.config$resampConfig$number,
                                                                 10)),
                       cv =         trainControl(method = optim.config$resampMethod,
                                                 number = ifelse(is.numeric(optim.config$resampConfig$k),
                                                                 optim.config$resampConfig$k,
                                                                 10),
                                                 repeats = optim.config$internal_repetitions),
                       loocv =      trainControl(method = optim.config$resampMethod),
                       lgocv =      trainControl(method = optim.config$resampMethod,
                                                 number = ifelse(is.numeric(optim.config$resampConfig$number),
                                                                 optim.config$resampConfig$number,
                                                                 10),
                                                 p = optim.config$resampConfig$p),
                       repeatedcv = trainControl(method = optim.config$resampMethod,
                                                 number = ifelse(is.numeric(optim.config$resampConfig$k),
                                                                 optim.config$resampConfig$k,
                                                                 10),
                                                 repeats = optim.config$resampConfig$number),
                       test =       trainControl(method = optim.config$resampMethod,
                                                 p = optim.config$resampConfig$p))
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
  
  #TODO insert the section with feature selection here
  selected_features = select_features(importance, fs.method, fs.config)
  
  if(verbose){
    print("fitting the resulting model to train & test data")
    flush.console()}
  
  fs.type = 'embedded'
  final.model = switch(fs.type,
                       fitted.model = model.train$finalModel,
                       wrapper = wrapper.finalModel,
                       filter = filter.finalModel)
  fitted.model = model.train$finalModel
  model = model.train$modelInfo
  train.fit.result = model$predict(fitted.model,
                                   newdata = training %>% select(-one_of(response.column)))
  test.fit.result = model$predict(fitted.model,
                                  newdata = test %>% select(-one_of(response.column)))
  
  #calculate the performance (in-Train and hold-out set)
  perf.training = postResample(train.fit.result, training[[response.column]])
  perf.test = postResample(test.fit.result, test[[response.column]])
  
  
    return(list(train = model.train,
              importance = importance,
              selected_features = selected_features,
              perf.training = perf.training,
              perf.test = perf.test,
              fs.config = fs.config,
              fs.method = fs.method,
              var_test = var_test,
              var_training = var_training))
}


