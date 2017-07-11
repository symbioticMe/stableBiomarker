#' Title
#'
#' @param data 
#' @param response 
#'
#' @return A list is returned  containing: \item{method
#' }{the object of class \code{train}.see \code{\link[caret]{train}}} 
#' \item{importance}{list of variables (predictors) with their corresponding 
#' importance values or coefficients, see \link[caret]{varImp}}
#' \item{results }{a data frame the training error rate and values of the
#' tuning parameters.} \item{bestTune }{a data frame with the final
#' parameters.}
#' @export
#'
#'@author Jelena Chuklina (the guts of \code{train.with.feature.extraction}
#' is a wrapper aroung the functions of \code[caret]{train})
#'#'
#' @examples
train.with.feature.extraction <- function(data, response.column, resampleIndex, 
                                          inTrain, outTrain, 
                                          method, fs.method, fs.config){

  
  #for each resampling:
  training = data[inTrain, ]
  test = data[outTrain, ]
  if (method == 'rf'){
    model.train = train(training[,-response.column], training[[response.column]], 
                        method = method, 
                        trControl = trainControl(method = 'none'), importance = T, ...)
  }
  else {
    model.train =train(training[,-response], training[[response]], method = method, 
                       trControl = trainControl(method = 'none'),  ...)
  }
  fitted.model = model.train$finalModel
  model = model.train$models
  train.fit.result = model$predict(fitted.model, newdata = training[,-response.column])
  test.fit.result = model$predict(fitted.model, newdata = test[,-response.column])
  
  #calculate the performance (in-Train and hold-out set)
  perf.training = postResample(train.fit.result, training[[response.column]])
  perf.test = postResample(test.fit.result, test[[response.column]])
  
  importance = varImp(model.train)
  return(list(train = model.train, 
              importance = importance, 
              perf.training, 
              perf.test,
              fs.config,
              fs.method))
}

#' Title
#'
#' @param data 
#' @param response 
#' @param method 
#' @param fs.method 
#' @param fs.config 
#' @param times 
#' @param resamplingConfig 
#'
#' @return
#' @export

#'
#' @examples
train.main <- function(data, response = 'outcome', 
                  method = 'rf', fs.method = 'TopN', 
                  fs.config = ifelse(fs.method = 'TopN', top.n = 100, NULL),
                  repeats = 100, 
                  resamplingConfig = list(method = 'Fraction', p = .9)){

  #check the resampling configuration
  resampleIndex <- create.resampling(data[['response']], resamplingConfig)

  nr = nrow(data)
  outTrain <- lapply(resampleIndex, 
                     function(inTrain, total) total[-unique(inTrain)],
                     total = seq(nr))
  if (!parallel){
    result <- lapply(1:repeats, function(k)
      train.with.feature.extraction(data, response, resampleIndex[[k]], outTrain[[k]], 
                      method, fs.method, fs.config))
  } else {
    result <- mclapply(1:repeats, function(k)
      train.with.feature.extraction(data, response, resampleIndex[[k]], outTrain[[k]], 
                      method, fs.method, fs.config))
  }
  
  importance.list = lapply(result, function(res) res$importance)
  performance.list = lapply(result, function(res) res$perf.training)
  performance.test.list = lapply(result, function(res) res$perf.test)
    
  return(result = result,
         repeats, fs.config, resamplingConfig, method, fs.method)
}