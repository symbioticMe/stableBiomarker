#' configure the training
#'
#' @param configFile name of the file with configuration
#' @param data data.frame with features and response
#'
#' @return list containing:
#' \item{fs_config}{feature selection configuration list}
#' \item{resamp_config}{resampling configuration list}
#' \item{optimization_config}{optimization configuration list}
#' @export
configurePipeline <- function(configFile, data, response.column){
  config = read_config(configFile)

  #TODO: write the check for the availability of machine learning methods
  #(see train.default of caret)
  method = config$method

  if (is.null(config$fs_method)){
    fsMethod = fs_config_res$fs_method
  }
  fs_config = check_fs_config(fsMethod = config$fs_method,
                                  config = config, data)

  #resamp_config = check_resamp(config)
  resamp_config = check_resamp_config(config = config)
  optim_config = check_optimization_config(config = config,
                                           data = data,
                                           response.column = response.column)
  return(list(fs_config = fs_config,
              optim_config = optim_config,
              resamp_config = resamp_config,
              method = method))
}

#' Read and parse configuration file
#'
#' @param fileName path to the file with configuration options
#'@importFrom utils flush.console
#' @return list with configuration parameters
read_config <- function(fileName){
  fc = file(fileName, encoding = "UTF-8")
  config <- strsplit(readLines(fc), '\n')
  #remove empty lines
  config = config[sapply(config, length) > 0]
  #remove comments
  config <- config[!sapply(config, function(x) startsWith(x[1],'#'))]
  config <- lapply(config, function(string) unlist(strsplit(string, ' = ')))
  if (!all(lengths(config) == 2)) {
    print(head(config[lengths(config) != 2]))
    flush.console()
    stop('Check the config file, some values of the parameters are invalid')
  }

  names(config) <- sapply(config, function(entry) tolower(entry[1]))
  config <- lapply(config, function(entry) entry[2])
  close(fc)
  return(config)
}

#' Check configuration of feature selection
#'
#' @param fsMethod feature selection method as specified in config file
#' @param config configuration list
#' @param data data frame to determine the number of features and samples
#'
#' @return list containing:
#' \item{fs_method}{feature selection method, see \link{link to feature selection}}
#' \item{fs_config}{configuration of feature selection}
check_fs_config <- function(fsMethod, config, data){
  #if fsMethod
  fs_config = list()
  if (is.null(fsMethod)){
    fs_method = 'TopN'
    fs_config = list(topN = min(100, round(0.1 * ncol(data))))
  }
  else if (fsMethod == 'TopN'){
    if (!('top_n' %in% tolower(names(config)))){
      fs_config$top_n = min(100, round(0.1 * ncol(data)))
    } else {
      if (as.numeric(config$top_n) > .5*ncol(data)){
        warning('number of top features is more than a half of total number of features,
                are you sure?')
      }
      fs_config$top_n = as.numeric(config$top_n)
    }
  }
    return(list(fs_method = fsMethod,
                fs_config = fs_config))
}

#' Check configuration of resampling
#'
#' @param config list of configuration parameters
#'
#' @return list of resampling configuration parameters containing:
#' \item{resampMethod}{method of resampling, such as \code{'CV'}, \code{'bootstrap'}}
check_resamp_config <- function(config){
  resampConfig = list()
  resampMethod = config$resampling
  repeats = as.numeric(config$repetitions)
  #TODO add check if resampling is in the list and if not, add default
  resampConfig <- switch(resampMethod,
                         CV = list(k = as.numeric(config$CV_folds)),
                         Fraction  = list(p = as.numeric(config$fraction)),
                         boot = NULL,
                         LOOCV = NULL)

  return(list(resampMethod = resampMethod,
              resampConfig = resampConfig,
              repeats = repeats))
}

#' check optimization configuration
#'
#' For some methods, such as \code{glmnet} optimization is always required,
#' for other such as \code{rf} parameters can be used once and for all,
#' this is why for some methods we need to list methods that require optimization
#' vs those that do not
#'
#' @param config list of configuration parameters
#' @param data data.frame with data
#' @param response.column string with the name of \code{response.column} to
#' determine the type of response and correspondingly the method valid for
#' optimization of parameters
#'
#' @return \code{NULL} if no optimization is anticipated and list otherwise,
#' containing:
#' \item{grid_resolution}{default value is \code{3}, unless specified in config}
#' \item{optimization_criterion}{\code{"performance"}, other accepted values are
#' \code{stability}, \code{RPT} as in \link{RPT paper}}
#' \item{metric}{for \code{performance}, depending on type of response, for regression it is
#' \code{"Rsquared_spearman"}, for classification it is \code{"AUC"}}
#' \item{internal_resampling}{default is "CV", can be any, see the \code{resamplingConfig}}
#' \item{internal_resamp_config}{configuration of internal resampling,
#' the same way as for external resampling}
check_optimization_config <- function(config, data, response.column = '.outcome'){
  if (!('optimize' %in% names(config))){
    warning(paste('optimization is required for this method'), config$method)
    #TODO: check which methods require optimization
    }
  else {
    if (is.na(as.logical(config$optimize))) {
    warning('optimize value is not logical, defaulting to the one required for the method')
    #TODO: check which methods require optimization
    optim_config = NULL
    }
    else {
      if (as.logical(config$optimize)){
      #check if required items are provided (see how it's done in getModelInfo of caret)
      minNames <- c("grid_resolution", "optimization_criterion", "metric",
                    "internal_resampling", "internal_repetitions")
      assign_default <- function(name){
        value <- switch(name,
               grid_resolution = 3,
               optimization_criterion = 'performance',
               metric = 'Rsquared_spearman',
               internal_resampling = 'CV',
               internal_repetitions = 10
        )
        return(value)
      }
      numeric_configurations = c('grid_resolution', 'internal_repetition')
      optim = list()
      for (name in minNames){
        if (name %in% names(config)){
          optim[[name]] = config[[name]]
          if (name %in% numeric_configurations &
              is(tryCatch(as.numeric(config[[name]], warning = function(w) w)), 'warning')){
            warning(paste(name, 'is not numeric, going to the default'))
            optim[[name]] = assign_default(name)
          } else {
            #TODO check the string values of internal configuration so that they correspond to the values desired
            next
          }
        } else {
          optim[[name]] = assign_default(name)
          }
        }
      optim_config = optim
      }
      else {
        optim_config = NULL
        }
      }
    }
  return(optim_config)
}
