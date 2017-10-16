#' Main function to train the model and get a (ranked) list of features
#'
#' @param data an object where samples are in rows and features are in columns
#' and one column is the response. This can be a data.frame or data.table.
#' Other types not tested
#' @param response a string specifying the name of the column with the response
#' @param method a string specifying which model to use
#' @param method.config list of method parameters to pass to the train function
#' @param fs.method a string specifying which feature selection method to use
#' @param fs.config a list specifying configurations of the feature selection method
#' @param resamplingConfig configuration of resampling
#' @param repeats number of resamplings
#' @param metric metric to optimise the feature selection for or evaluate the fit
#' @param optim.config configuration of algorithm parameter optimisation
#' @param allowParallel should the resampling be parallelized if multiple cores are available
#' @param verbose 
#' @param ... 
#'
#' @return list containing:
#' \item{result}{TO FIX}
#' \item{next item}{TO FIX}
#'
#' @export
#' @importFrom dplyr slice select one_of
#' @importFrom parallel mclapply detectCores
train_main <- function(data, response = '.outcome',
                       method = 'rf', method.config = NULL,
                       fs.method = 'TopN',
                       fs.config = ifelse(fs.method == 'TopN', 
                                          list(top.n = 100), NULL),
                       repeats = 100,
                       resamplingConfig = list(resampMethod = 'LGOCV', 
                                               resampConfig = list(p = .9), 
                                               repetitions = 10),
                       metric = ifelse(is.factor(data[[response.column]]), 
                                       "Accuracy", "RMSE"),
                       optim.config = NULL,
                       allowParallel = F, verbose = F, ...){
  
  startTime <- proc.time()
  
  if(is.null(colnames(data))) stop("x must have column names")
  
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
  
  #Test Summary Function
  #Set seeds
  
  fs.type = check_fs_type(method$label, method.config, 
                          fs.method, fs.config)$fs_type
  iterFunction = switch(fs.type,
                        embedded = embeddedIter,
                        filter = filterIter,
                        wrapper = wrapperIter)
  errorContainer = list()
  if (!allowParallel){
    result = list()
    for (k in 1:repeats){
      #result <- lapply(1:repeats, function(k) - restore after debugging?
      print(k)
      print(resampleIndex[[k]])
      print(outTrain[[k]])
      #remove after the debugging with real data
      if(is(tryCatch(iterFunction(data, inTrain = resampleIndex[[k]],
                                                   outTrain = outTrain[[k]],
                                                   method = method,
                                                   method.config = method.config,
                                                   fs.method = fs.method,
                                                   fs.config = fs.config,
                                                   response.column = response,
                                                   metric = metric,
                                                   optim.config = optim.config,
                                                   verbose = F, ...), 
                     warning = function(w) w), 'warning')){
        result[[k]] <- iterFunction(data, inTrain = resampleIndex[[k]],
                                      outTrain = outTrain[[k]],
                                      method = method,
                                      fs.method = fs.method,
                                      fs.config = fs.config,
                                      response.column = response,
                                      metric = metric,
                                      optim.config = optim.config,
                                      verbose = verbose, ...)
        errorContainer[[k]] = result[[k]]$train$resample
        #print(result[[k]]$train$resample)
      } else {
        errorContainer[[k]] = 'nothing'
        result[[k]] <- iterFunction(data, inTrain = resampleIndex[[k]],
                                                     outTrain = outTrain[[k]],
                                                     method = method,
                                                     fs.method = fs.method,
                                                     fs.config = fs.config,
                                                     response.column = response,
                                                     metric = metric,
                                                     optim.config = optim.config,
                                                     verbose = verbose, ...)
        
        errorContainer[[k]] = result[[k]]$train$resample #delete after debugging
        #print(result[[k]]$train$resample) #delete after debugging
      }
      
      #)
    }
    
  } else {
    #ToDo: rewrite in foreach paradigm
    mc.cores = detectCores()
    if(verbose){print(sprintf('Going parallel, using %s cores...', mc.cores))}
    result <- mclapply(1:repeats, function(k)
      iterFunction(data, inTrain = resampleIndex[[k]],
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
  performance.train = data.frame(t(sapply(result, function(res) res$perf.training)))
  performance.test = data.frame(t(sapply(result, function(res) res$perf.test)))
  
  selected.features = lapply(result, function(res) res$selected_features)
  models = lapply(result, function(res) res$train)
  

  res_names = names(resampleIndex)
  rownames(performance.train) =  res_names
  rownames(performance.test) = res_names
  names(selected.features) = res_names
  names(models) = res_names
  endTime <- proc.time()
  
  return(list(selected.features = selected.features,
              performance.test = performance.test,
              performance.train = performance.train,
              models = models,
              repeats = repeats,
              fs.config = fs.config,
              resamplingConfig = resamplingConfig,
              method = method,
              method.config = method.config,
              resamples = result,
              fs.method = fs.method,
              optim.config = optim.config,
              times = endTime - startTime,
              errorContainer = errorContainer, #delete after the debugging
              resampleIndex = resampleIndex, #consider deleting after the debugging
              dots = list(...)))
}