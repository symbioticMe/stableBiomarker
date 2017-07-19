#' select features with the feature selection method
#'
#' @param importance
#' @param fs.method
#' @param fs.config
#' @param model
#' @param data
#'
#' @return
#' @export
#'
#' @examples
select_features <- function(importance, fs.method, fs.config, model = NULL, data = NULL){
  #TODO: check how to throw an error for a method not available within the switch
  selected_features = switch(tolower(fs.method),
                             #rfe = rfe(importance, model, data, fs.config),
                             #ga = ga(importance, model, data, fs.config),
                             #filter = filter_fs(data, model, fs.config),
                             topn = topN(importance, fs.config),
                             none = rownames(importance)[importance[,1] != 0])
  return(selected_features)
}

#' Select top N best features
#'
#' @param importance
#' @param fs.config
#'
#' @return
#'@import dplyr
#'
topN <- function(importance, fs.config){
  topN = fs.config$top_n
  importance = importance %>% mutate(features = rownames(.)) %>% arrange(-Overall)
  selected_features = importance$features[1:topN]
  return(selected_features)
}
