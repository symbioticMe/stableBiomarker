#' Title
#'
#' @param x
#' @param y
#' @param fs.config
#'
#' @return
#' @export
#'
correlation_significance <- function(x, y) {
  corr_significance = Hmisc::rcorr(cbind(x, y))$P[ncol(x) + 1,1:(ncol(x))]
  return(corr_significance)
}

correlation_value <- function(x, y){
  corr_value = abs(Hmisc::rcorr(cbind(x, y))$r[ncol(x) + 1,1:(ncol(x))])
  return(corr_value)
}

score_RF_for_topN <- function(x, y, ntree = 500){
  loadNamespace("randomForest")
  model = randomForest::randomForest(x, y, ntree = ntree, importance = T)
  importance = importance(model)
  if (is.factor(y)) {return(importance[,'MeanDecreaseAccuracy'])}
  else {return(importance[,'%IncMSE'])}
    
}


topN_rf_SBF <- list(summary = caret::defaultSummary,
                 fit = function(x, y, ...)
                 {
                   if(ncol(x) > 0)
                   {
                     loadNamespace("randomForest")
                     randomForest::randomForest(x, y, ...)
                   } else nullModel(y = y)
                 },
                 pred = function(object, x)
                 {
                   if(class(object) != "nullModel")
                   {
                     tmp <- predict(object, x)
                     if(object$modelType == "Classification" &
                        !is.null(object$modelInfo$prob))
                     {
                       out <- cbind(data.frame(pred = tmp),
                                    as.data.frame(predict(object, x, type = "prob")))
                     } else out <- tmp
                   } else {
                     tmp <- predict(object, x)
                     if(!is.null(object$levels))
                     {
                       out <- cbind(data.frame(pred = tmp),
                                    as.data.frame(predict(object, x, type = "prob")))
                     } else out <- tmp
                   }
                   out
                 },
                 score = function(x, y, ...)
                 {
                   ## should return a named logical vector
                   score_RF_for_topN(x, y, ...)
                 },
                 filter = function(score, x, y, n, decreasing = T) {
                   if (decreasing) {
                     score = -score
                     }
                   rank(score) <= n
                 }
)