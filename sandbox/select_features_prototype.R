selFeatures.default <- function(x, y, model.method, model.config,
                                resampling.method, resampling.config,
                                fs.method, fs.config, optim.config = NULL, 
                                parallel = F, verbose = F, ...){
  #TODO: check how to throw an error for a method not available within the switch
  fs.type = check_fs_type(model.method, model.config, fs.method, fs.config)
  result = switch(tolower(fs.type),
                             #rfe = rfe(importance, model, data, fs.config),
                             #ga = ga(importance, model, data, fs.config),
                             #filter = filter_fs(data, model, fs.config),
                             filter = filter_fs(x, y, model.method, model.config,
                                                resampling.method, resampling.config,
                                                fs.method, fs.config, optim.config = optim.config,
                                                parallel = parallel, verbose = verbose),
                             embedded = embedded_fs(x, y, model.method, model.config,
                                                    resampling.method, resampling.config,
                                                    fs.method, fs.config, optim.config = optim.config,
                                                    parallel = parallel, verbose = verbose),
                             wrapper = wrapper_fs(x, y, model.method, model.config,
                                                  resampling.method, resampling.config,
                                                  fs.method, fs.config, optim.config = optim.config,
                                                  parallel = parallel, verbose = verbose),
                             topn = topN(importance, fs.config),
                             none = rownames(importance)[importance[,1] != 0])
  return(result)
}

filter_fs <- function(x, y, model.method, model.config,
                      resampling.method, resampling.config,
                      fs.method, fs.config, optim.config = NULL,
                      parallel = T, verbose = F){
  #TODO: wrapper around the SBF
}

embedded_fs <- function(x, y, model.method, model.config,
                        resampling.method, resampling.config,
                        fs.method, fs.config, optim.config = optim.config,
                        parallel = parallel, verbose = verbose){
  
}

wrapper_fs <- function(x, y, model.method, model.config,
                       resampling.method, resampling.config,
                       fs.method, fs.config, optim.config = optim.config,
                       parallel = parallel, verbose = verbose){
  
}