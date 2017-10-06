wrapperIter <- function(data, inTrain, outTrain, method, method.config,
                        fs.method, fs.config, response.column, metric,
                        optim.config, verbose, ...){
  #for each resample, create the feature set
  #TODO: make a wrapper that would configure the rfeControl and runRF
  res <- switch(tolower(fs.method),
                rfe = rfeWorkflow(),
                ga = gaWorkflow(),
                sa = saWorkflow())
  return(res)
}



gaWorkflow <- function(){
  #ToDo: configure ga_control
  #ToDO: run GA
  #ToDo: wrap the GA results into the shape, that is roughly the same as for SBF
  return(NULL)
}

rfeWorkflow <- function(){
  #ToDo: configure rfeControl
  #ToDo: run RFE
  #ToDo: wrap the RFE results into the shape, similar to that of SBF
  return(NULL)
}

saWorkflow <- function(){
  #ToDo: configure SA_control
  #ToDo: run SA
  #ToDo: wrap the SA results into the shape, similar to that of SA
  return(NULL)
}