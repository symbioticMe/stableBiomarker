#' Create resampling according to configurations specified by user
#'
#' @param resamplingConfig list of configurations for the resampling of class
#' \code{resamplingConfig}, with the following accepted resampling methods:
#' The resampling method: \code{"bootstrap"}, \code{"Fraction"},
#' \code{"CV"}, the latter as number of folds \code{k} accepts value \code{"LOO"},
#' @param times number of repetitions for the resampling
#' @param data data matrix
#'
#' @return \code{resampleIndex} list of size \code{times} with indices for each resample
#'
#' @importFrom caret createDataPartition createFolds createResample createMultiFolds
#' @export
#'
create_resampling <- function(resamplingConfig, times, response){
  if (!is.list(resamplingConfig)){
    stop('Resampling configuration is stored in a list!')
  }

  resamplingMethod = resamplingConfig$resampMethod
  resamplingConfig = resamplingConfig$resampConfig

  caret_resamp_methods = c("boot", "cv", "LOOCV", "repeatedcv")

  resampleIndex = switch(tolower(resamplingMethod),
                         fraction = createDataPartition(response,
                                                        p = resamplingConfig$p,
                                                        times = times),
                         repeatedcv = createMultiFolds(response, k = k, times = times),
                         cv = createFolds(response, k = k),
                         loocv = createFolds(response, length(response), returnTrain = TRUE),
                         boot = createResample(response, times = times))
  return(resampleIndex)
}