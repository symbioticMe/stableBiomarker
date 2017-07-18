#' Summarize the performance across the resampling iterations
#'
#' @param data
#' @param response.column
#' @param performance
#' @param metric
#'
#' @return list containing of
#' \item{perf_mean}{mean performance}
#' \item{perf_sd}{standard deviation of performance}
#' @export
#'
#' @examples
summarize_perf <- function(data, response.column, performance, metric = NULL){
  if (class(data[[response.column]]) == 'numeric') {metric = 'Rsquared'}
  else if (class(data[[response.column]]) == 'character' |
           class(data[[response.column]]) == 'character') {metric = 'Accuracy'}
  performance = sapply(performance, '[[', metric)
  return(list(perf_mean = mean(performance),
         perf_sd = sd(performance)))
  }
