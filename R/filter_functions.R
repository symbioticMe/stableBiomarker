#' Title
#'
#' @param data 
#' @param inTrain 
#' @param outTrain 
#' @param method 
#' @param method.config 
#' @param fs.method 
#' @param fs.config 
#' @param response.column 
#' @param metric 
#' @param optim.config 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#' @import dplyr
#'
#' @examples
filterIter <- function(data, inTrain,
                       outTrain,
                       method,
                       method.config,
                       fs.method,
                       fs.config,
                       response.column,
                       metric,
                       optim.config,
                       verbose, ...){
  
  if (verbose){print("Next iteration...")}
  #for each resampling:
  training = data[inTrain, ]
  test = data[outTrain, ]
  
  
  var_training = var(training[[response.column]])
  var_test = var(test[[response.column]])
  
  x = training %>% select(-one_of(response.column))
  y = training[[response.column]]
  testX = test %>% select(-one_of(response.column))
  testY = test[[response.column]]

  if(fs.config$multivariate) {
    scores <- fs.config$scoring.function(x, y)
    if(length(scores) != ncol(x))
      stop(paste("when control$multivariate == TRUE, 'scores'",
                 "should return a vector with", ncol(x), "numeric values"))
  } else  {
    scores <-score_features(x = x, y = y, 
                            scoring.function = fs.config$scoring$scoring.function,
                            params = fs.config$scoring$scoring.params,
                            metric = metric)
  }

  retained <- filter_features(scores, x, y, fs.method, fs.config$threshold)
  ## deal with zero length results
  if (is.logical(retained)) {
    testX <- testX[, which(retained), drop = FALSE]
    x_selected = x[, which(retained), drop = FALSE]
  } else {
    testX <- testX[, names(retained), drop = FALSE]
    x_selected = x[, names(retained), drop = FALSE]
  }
  
  default.params = method$grid(x_selected, y, 1)
  fitObject <- caret::createModel(x = x_selected, y = y, wts = NULL,
                                  method = method, tuneValue = default.params,
                                  obsLevels = NA, pp = NULL, last = TRUE,
                                  classProbs = FALSE, sampling = NULL, ...)
  #fitObject <- method$fit(x_selected, y, ...)
    
  pred.test = predict_outcome(method, fitObject$fit, x = testX, y = testY)
  pred.train = predict_outcome(method, fitObject$fit, x = x_selected, y = y)
  
  perf.training = postResample(pred.train$pred, pred.train$obs)
  perf.test = postResample(pred.test$pred, pred.test$obs)
    
  return(list(model = fitObject,
              importance = scores,
              selected_features = retained,
              perf.training = perf.training,
              perf.test = perf.test,
              method = method,
              fs.config = fs.config,
              fs.method = fs.method,
              var_test = var_test,
              var_training = var_training))
}

predict_outcome <- function(method, fitObject, x, y){
  modelPred <- method$predict(modelFit = fitObject, 
                           newdata = x)
  if(is.data.frame(modelPred) | is.matrix(modelPred))
  {
    if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred)
    modelPred$obs <- y
  } else modelPred <- data.frame(pred = modelPred, obs = y)
  return(modelPred)
}

looFilterIter <- function(){
  
}

nominalFilterIter <- function(){
  
}

###         Auxiliary functions            #####################################

score_features <- function(x, y, scoring.function, params = NULL, 
                           metric = ifelse(is.factor(y), "Accuracy", "RMSE")){
  ## To get of warnings "some row.names duplicated: " when resampling with replacement
  if(is.data.frame(x) | is.matrix(x)) 
    rownames(x) <- make.names(rownames(x), unique = TRUE)
  if (is.list(scoring.function) ){
    score = score_by_importance(x, y, scoring.function, params, metric)
  } else {
    score = scoring.function(x = x, y = y)
  }
  return(score)
}

#' Title
#'
#' @param x 
#' @param y 
#' @param scoring.function 
#' @param params 
#' @param metric 
#'
#' @return
#' @export
#'
#' @examples
score_by_importance <- function(x, y, scoring.function, params = NULL, 
                                metric = ifelse(is.factor(y), "Accuracy", "RMSE")){
  minNames <- c("library", "type", "parameters", "grid",
                "fit", "predict", "prob")
  nameCheck <- minNames %in% names(scoring.function)
  if(!all(nameCheck)) stop(paste("some required components are missing:",
                                 paste(minNames[!nameCheck], collapse = ", ")), 
                           call. = FALSE)
  if (!is.null(params)){
    if (is.data.frame(params)){
      tuneGrid = params
    } else {
      tuneGrid = data.frame(t(params))
      tuneNames <- as.character(scoring.function$parameters$parameter)
      if (length(setdiff(tuneNames, names(tuneGrid)))> 0 & 
          length(setdiff(names(tuneGrid), tuneNames)) == 0){
        missing_params = setdiff(tuneNames, names(tuneGrid))
        defaultGrid = scoring.function$grid(x, y, 1)[,missing_params, drop = F]
        defaultGrid = defaultGrid[rep(1, nrow(tuneGrid)), , drop = F]
        tuneGrid = merge(tuneGrid, defaultGrid)
      }
    }
  }
  if (nrow(tuneGrid) == 1){
    trControl = trainControl(method = 'none')
  } else {
    trControl = trainControl(method = 'repeatedcv')
  }
  fit <- train(x, y, method = scoring.function, 
               trControl = trControl, tuneGrid = tuneGrid)
  importance = varImp(fit)$importance
  score = importance[,1]
  #importance = importance %>% 
  #mutate(features = rownames(.)) %>% 
  #arrange_(-'Overall')
  names(score) = rownames(importance)
  return(score)
}

filter_features <- function(scores, x, y, fs.method, threshold){
  retained = switch(tolower(fs.method),
                    topn = topN(scores, threshold),
                    significance = significance(scores, threshold))
  return(retained)
}


###            Filter functions            #####################################

#' Select top N best features
#'
#' @param importance
#' @param fs.config
#'
#' @return
#'@import dplyr
#'
topN <- function(scores, threshold){
  selected_features = sort(scores, decreasing = T)[1:threshold]
  return(selected_features)
}

significance <- function(scores, threshold){
  selected_features = scores[scores <= threshold]
  return(selected_features)
}

#ToDo: add mutualInformation, pointWiseMutualInformation, gamScores, anovaScores

###    Scoring (ranking) functions             #################################



#' Correlation filter
#'
#' @param x
#' @param y
#' @param fs.config
#'
#' @return
#' @export
#'
#' @importFrom Hmisc rcorr
correlation <- function(x, y, fs.config) {
  corr_significance = rcorr(rbind(y, x))$P[1,2:(1+ncol(x))]
  selected_features = names(x)[corr_significance < fs.config$p_value]
  return(selected_features)
}

#ToDo: from caret import significance score or re-write the function