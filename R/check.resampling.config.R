#' Title
#'
#' @param resamplingConfig list of configurations for the resampling
#' @param times number of repetitions for the resampling
#' @param data data matrix
#'
#' @return \code{resampleIndex} list of size \code{times} with indices for each resample
#' @export
#'
#' @examples
create.resampling <- function(resamplingConfig, times, data){
  if (!is.list(resamplingConfig)){
    stop('Resampling configuration is stored in a list!')
  }
  
  resamplingMethod = resamplingConfig$method
  
  if (resamplingMethod == 'Fraction'){
    if (!('p' %in% names(resamplingConfig))){
      stop('Method = Fraction requires fraction to be specified!')
    } else {
      resampleIndex = createDataPartition(data, p = resamplingConfig$p, times = times)
      if('k' %in% names(resamplingConfig)){
        warning('number of folds k is not applicaple to cross-validation')
      }
    }
    
  } else {
    if (resamplingMethod == 'CV'){
      if (!('k' %in% names(resamplingConfig))){
        stop('for cross-validation, please specify the number of folds!')
      }
      if (is.numeric(k)){
        resampleIndex = createMultiFolds(data, k = k, times = times)
      } else {
        if (k == 'LOO'){
          resampleIndex = createFolds(trainGroup, 
                                      length(trainGroup), 
                                      returnTrain = TRUE)
        }
      }
      
      if('p' %in% names(resamplingConfig)){
        warning('fraction p is not applicaple to cross-validation')
      }
    } else {
      if (resamplingMethod == 'bootstrap'){
        resampleIndex = rlply(k, sample(nrow(data), replace = T))
      } else {
        stop('Please specify one of the acceptable resampling methods')
      }
    }
  }
  return(resampleIndex)
}