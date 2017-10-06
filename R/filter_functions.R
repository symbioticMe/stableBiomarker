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
  #function, similar to sbfIter
}

looFilterIter <- function(){
  
}

nominalFilterIter <- function(){
  
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
topN <- function(x, y, fs.config, method.config){
  if (is.null(method.config)){
    model.train = train(x = x,
                        y = y,
                        method = method, metric = metric, 
                        importance = T, ...)
  } else {
    param.List = list(x = x,
                      y = y,
                      method = method, metric = metric, 
                      importance = T)
    #so that ntree could be added to ...
    param.List = c(param.List, method.config)
    model.train = do.call(train, param.List)
  }
  importance = varImp(model.train)$importance
  topN = fs.config$top_n
  importance = importance %>% 
    mutate(features = rownames(.)) %>% 
    arrange_(-'Overall')
  selected_features = importance$features[1:topN]
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