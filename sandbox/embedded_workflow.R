#' @export
embeddedIter <- function(x, y,
                         testX, testY,
                         embeddedControl = embeddedControl(), ...)
{
  if(is.null(colnames(x))) stop("x must have column names")
  
  if(is.null(testX) | is.null(testY)) stop("a test set must be specified")
  
  trControl = 
    params = list(x = x, y = y, metric = metric)
    names(params) = 
    model.train = train(x = x, y = y, method = method, metric = )
  } else {
    
  }
  
  retained <- embeddedControl$functions$filter(scores, x, y)
  ## deal with zero length results
  
  testX <- testX[, which(retained), drop = FALSE]
  
  fitObject <- embeddedControl$functions$fit(x[, which(retained), drop = FALSE],
                                        y,
                                        ...)
  
  modelPred <- embeddedControl$functions$pred(fitObject, testX)
  if(is.data.frame(modelPred) | is.matrix(modelPred))
  {
    if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred)
    modelPred$obs <- testY
  } else modelPred <- data.frame(pred = modelPred, obs = testY)
  
  
  list(variables = names(retained)[which(retained)],
       pred = modelPred)
  
}


"embedded.default" <-
  function(x, y,
           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
           maximize = ifelse(metric == 'RMSE', FALSE, TRUE),
           embeddedControl = embeddedControl(), ...)
  {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    
    numFeat <- ncol(x)
    classLevels <- levels(y)
    
    if(embeddedControl$method == "oob") stop("out-of-bag resampling cannot be used with this function")
    
    if(is.null(sbfControl$index)) sbfControl$index <- switch(
      tolower(sbfControl$method),
      cv = createFolds(y, sbfControl$number, returnTrain = TRUE),
      repeatedcv = createMultiFolds(y, sbfControl$number, sbfControl$repeats),
      loocv = createFolds(y, length(y), returnTrain = TRUE),
      boot =, boot632 = createResample(y, sbfControl$number),
      test = createDataPartition(y, 1, sbfControl$p),
      lgocv = createDataPartition(y, sbfControl$number, sbfControl$p))
    
    if(is.null(names(sbfControl$index))) names(sbfControl$index) <- prettySeq(sbfControl$index)
    if(is.null(sbfControl$indexOut)){
      sbfControl$indexOut <- lapply(sbfControl$index,
                                    function(training, allSamples) allSamples[-unique(training)],
                                    allSamples = seq(along = y))
      names(sbfControl$indexOut) <- prettySeq(sbfControl$indexOut)
    }
    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))
    
    if(is.factor(y))
    {
      for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    }
    
    test <- sbfControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)
    
    ## Set or check the seeds when needed
    if(is.null(sbfControl$seeds))
    {
      sbfControl$seeds <- sample.int(n = 1000000, size = length(sbfControl$index) + 1)
    } else {
      if(!(length(sbfControl$seeds) == 1 && is.na(sbfControl$seeds)))
      {
        if(length(sbfControl$seeds) != length(sbfControl$index) + 1)
          stop(paste("Bad seeds: the seed object should be an integer vector of length",
                     length(sbfControl$index) + 1))
      }
    }
    
    
    
    #########################################################################
    
    
    if(sbfControl$method == "LOOCV")
    {
      tmp <- looSbfWorkflow(x = x, y = y, ppOpts = preProcess,
                            ctrl = sbfControl, lev = classLevels, ...)
      resamples <- do.call("rbind", tmp$everything[names(tmp$everything) == "pred"])
      rownames(resamples) <- 1:nrow(resamples)
      selectedVars <- tmp$everything[names(tmp$everything) == "variables"]
      performance <- tmp$performance
    } else {
      tmp <- nominalSbfWorkflow(x = x, y = y, ppOpts = preProcess,
                                ctrl = sbfControl, lev = classLevels, ...)
      resamples <- do.call("rbind", tmp$everything[names(tmp$everything) == "resamples"])
      rownames(resamples) <- 1:nrow(resamples)
      selectedVars <- tmp$everything[names(tmp$everything) == "selectedVars"]
      performance <- tmp$performance
    }
    
    #########################################################################
    
    varList <- unique(unlist(selectedVars))
    if(sbfControl$multivariate) {
      scores <- sbfControl$functions$score(x, y)
      if(length(scores) != ncol(x))
        stop(paste("when control$multivariate == TRUE, 'scores'",
                   "should return a vector with", ncol(x), "numeric values"))
    } else  {
      scores <- apply(x, 2, sbfControl$functions$score, y = y)
    }
    retained <- sbfControl$functions$filter(scores, x, y)
    
    finalTime <- system.time(
      fit <- sbfControl$functions$fit(x[, retained, drop = FALSE],
                                      y,
                                      ...))
    
    
    
    performance <- data.frame(t(performance))
    performance <- performance[,!grepl("\\.cell|Resample", colnames(performance))]
    
    if(is.factor(y) & any(names(resamples) == ".cell1"))
    {
      keepers <- c("Resample", grep("\\.cell", names(resamples), value = TRUE))
      resampledCM <- resamples[,keepers]
      resamples <- resamples[, -grep("\\.cell", names(resamples))]
    } else resampledCM <- NULL
    
    resamples <- switch(sbfControl$returnResamp,
                        none = NULL,
                        all =, final = resamples)
    
    endTime <- proc.time()
    times <- list(everything = endTime - startTime,
                  final = finalTime)
    
    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model
    
    out <- structure(
      list(
        pred = if(sbfControl$saveDetails) tmp else NULL,
        variables = selectedVars,
        results = performance,
        fit = fit,
        optVariables = names(retained)[retained],
        call = funcCall,
        control = sbfControl,
        resample = resamples,
        metrics = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...)),
      class = "sbf")
    if(sbfControl$timingSamps > 0)
    {
      out$times$prediction <- system.time(predict(out, x[1:min(nrow(x), sbfControl$timingSamps),,drop = FALSE]))
    } else  out$times$prediction <- rep(NA, 3)
    out
  }



######################################################################
######################################################################

#' Control Object for Selection, Embedded in Training Model
#'
#' @param functions 
#' @param method 
#' @param saveDetails 
#' @param number 
#' @param repeats 
#' @param verbose 
#' @param returnResamp 
#' @param p 
#' @param index 
#' @param indexOut 
#' @param timingSamps 
#' @param seeds 
#' @param allowParallel 
#'
#' @return
#' @import caret
#' @export
#'
#' @examples
embeddedControl <- function(functions = NULL,
                       method = "repeatedcv",
                       saveDetails = FALSE,
                       number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
                       repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
                       verbose = FALSE,
                       returnResamp = "final",
                       p = .75,
                       index = NULL,
                       indexOut = NULL,
                       timingSamps = 0,
                       seeds = NA,
                       allowParallel = TRUE)
{
  list(
    fit = if(is.null(functions)) defaultEmbed else functions,
    method = method,
    saveDetails = saveDetails,
    number = number,
    repeats = repeats,
    returnResamp = returnResamp,
    verbose = verbose,
    p = p,
    index = index,
    indexOut = indexOut,
    timingSamps = timingSamps,
    seeds = seeds,
    allowParallel = allowParallel)
}

defaultEmbed <- list(summary = caret::defaultSummary,
                   fit = function(x, y, optim.config = NULL, 
                                  metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                                  maximize = ifelse(metric == 'RMSE', FALSE, TRUE),
                                  ...)
                     {
                     if (ncol(x) > 0){
                       if (is.null(optim.config)){
                         train(x, y, ...)
                       } else {
                         trControl = turnOptimConfigToTrainControl(optim.config)
                         train(x, y, trControl = trControl, metric = optim.config$metric)
                       }
                       
                   } else {caret::nullModel(y = y)}
                     },
                   pred = function(object, x)
                   {
                     if(class(object) != "nullModel"){
                       tmp <- predict(object, x)
                       if (object$modelType == "Classification" &
                           !is.null(object$modelInfo$prob)){
                         out <- cbind(data.frame(pred = tmp), 
                                      as.data.frame(predict(object, x, type = "prob")))
                       } else out <- tmp
                     } else {
                       tmp <- predict(object, x)
                       if (!is.null(object$levels)){
                         out <- cbind(data.frame(pred = tmp),
                                      as.data.frame(predict(object, x, type = "prob")))
                       } else out <- tmp
                     }
                     out
                   },
                   selectFeatures = function(importance){
                     importance
                   }
                   )

  
lassoEmbed <- list(summary = caret::defaultSummary,
                   fit = function(x, y ){
                     
                   } else caret::nullModel(y = y))

embeddedWorkflow <- function(){
  
}