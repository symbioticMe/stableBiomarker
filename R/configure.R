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
configurePipeline <- function(data, response.column, configFile = NULL, config = NULL){
  if (!is.null(configFile) & !is.null(config)){
    stop('either the configuration file or configuration object alone should be identified!')
  }
  else if(!is.null(configFile)) {
    config = read_config(configFile)
  }
  

  config_params = names(config)
  #TODO: write the check for the availability of machine learning methods
  #(see train.default of caret)
  if (is.list(config$method) && 'method_name' %in% names(config)){
    method = config$method
    method_config = check_method_config(config, method = method)
  } else {
    method_name = config$method
    method_config = check_method_config(config,method_name)
  }
  
  method_name = method_config$method_name
  method = method_config$method
  method_config = method_config$method_config

  #TODO: make config_param method substraction one by one
  #why do we need config params at all?
  config_params = setdiff(config_params, method)

  fs_config = check_fs_config(fsMethod = config$fs_method,
                                  config = config, data)

  #resamp_config = check_resamp(config)
  resamp_config = check_resamp_config(config = config)
  optim_config = check_optimization_config(config = config,
                                           data = data,
                                           response.column = response.column)
  performance_metric = check_performance_metric(config, data, response.column)
  stability_metric = check_stability_metric(config)
  aggregation_method = check_aggregation_method(config)
  return(list(fs_config = fs_config,
              optim_config = optim_config,
              resamp_config = resamp_config,
              method = method,
              method_config = method_config,
              method_name = method_name,
              performance_metric = performance_metric,
              stability_metric = stability_metric,
              aggregation_method = aggregation_method
                ))
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
  config <- lapply(config, function(entry) trimws(entry[2]))
  close(fc)
  return(config)
}

#' Read method parameters
#'
#' @param method string with the method name
#' @param config list of configuration parameters
#'
#' @return list with:
#' \item{method}{method, turned into \code{list}}
#' \item{method_config}{\code{list} of method parameters}
#' @importFrom caret getModelInfo
check_method_config <- function(config, method_name = NULL, method = NULL){
  if (is.null(method) || !is.list(method)) {
    method = try(getModelInfo(method_name, regex = T), silent = T)
  } else {
    if(class(method) == 'try-error'){
      method = try(get(method_name), silent = T)
      if (class(method) == 'try-error' | !is.list(method)){
        stop("Method should be defined in caret package or as a list")
      }
    } else if (!is.null(method_name)){
      method = getModelInfo(method_name, regex = F)[[1]]
    } else {
      method = getModelInfo('rf', regex = F)[[1]]
      method_name = 'rf'
    }
  }

  parameters = as.character(method$parameters$parameter)
  method_config = config[names(config) %in% parameters]
  if (length(method_config) == 0) {method_config = NULL}
  if ('method_config_file' %in% tolower(config)){
    if (!is.null(method_config)){
      warning('parameters will be overriden by method configuration file')
      }
    method_config = read.csv(config$method_config_file, header = T)
    extra_params = setdiff(names(method_config), parameters)
    if (length(setdiff(names(method_config), parameters)) > 0){
      warning(paste('Extra parameters:', paste(extra_params, collapse = ', ')))
    }
    method_config = method_config[names(method_config) %in% parameters]
  }
  #check if all parameters of machine learning methods in caret are numeric
  if(!is.null(method_config)) method_config = lapply(method_config, as.numeric)

  return(list(method        = method,
              method_name   = method_name,
              method_config = method_config))
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
  #make uniform fsMethod vs fs_method
  fs_config = list()
  if (is.null(fsMethod)){
    warning('feature selection method missing, opting for the default')
    fsMethod = 'TopN'
    fs_config = list(topN = min(100, round(0.1 * ncol(data))))
  } else if (fsMethod == 'none') {
    fs_config = NULL
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
  resampMethod = if (!is.null(config$resampling)){
    tolower(config$resampling)
  } else   {NULL}

  resampling.methods = tolower(c("boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "test"))
  resamp.parameters = c('cv_folds', 'repetitions', 'fraction')
  if (!is.null(resampMethod) && !(resampMethod %in% resampling.methods)){
    stop(paste(resampMethod, "method is not defined in caret built-in resampling methods"))
  }
  else if (is.null(resampMethod) && (is.null(names(config)) || sum(tolower(names(config)) %in% resamp.parameters) == 0)){
    warning('resampling method or resampling parameters not specified, using default (LGOCV)')
    resampMethod = 'lgocv'
    config$fraction = .7
    config$repetitions = 50
  } else {
    if ('cv_folds' %in% names(config) && 'fraction' %in% names(config)){
      warning('both CV and fraction partitioning is not possible, using default for the method...')
      if (grepl('cv', resampMethod) && resampMethod != 'lgocv'){config$fraction = NULL}
      else {config$cv_folds = NULL}
    }
    if (resampMethod == 'cv' && 'repetitions' %in% names(config)){
      resampMethod = 'repeatedcv'}

    resampConfig = list()
    
    for (name in names(config)){
      if (name %in% resamp.parameters) {
        if(is(tryCatch(as.numeric(config[[name]],
                                  warning = function(w) w)),
              'warning')){
          warning(paste(name, 'is not numeric, going to the default'))
          config[[name]] = assign_default(name)
        } else {
          config[[name]] = as.numeric(config[[name]])
        }
      }
    }
  }
  repetitions = as.numeric(config$repetitions)
  
  resampConfig <- switch(tolower(resampMethod),
                         cv         = list(k = as.numeric(config$cv_folds)),
                        repeatedcv = list(k = as.numeric(config$cv_folds),
                                          number = repetitions),
                        test       = list(p = as.numeric(config$fraction)),
                        lgocv      = list(p = as.numeric(config$fraction),
                                          number = repetitions),
                        boot       = list(number = repetitions),
                        loocv      = NULL)

  return(list(resampMethod = resampMethod,
              resampConfig = resampConfig,
              repetitions = repetitions))
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
    warning(paste('assuming optimization is not required for this method'), config$method_name)
    optim_config = NULL
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

        check_grid_resolution <- function(config){
          grid_resolution = config$grid_resolution
          if (is.null(grid_resolution) || 
              is(tryCatch(as.numeric(grid_resolution, warning = function(w) w)), 
                 'warning')) {
              warning(paste(grid_resolution, 
                            'is not numeric or missing, going to the default'))
              grid_resolution = assign_default('grid_resolution', data)
          } else {
              grid_resolution = as.numeric(grid_resolution)
            }
          return(grid_resolution)
        }
        
        check_opt_criterion <- function(config){
          opt_criteria = c('performance', 'stability', 'RPT')
          opt_criterion = config$optimization_criterion
          if (is.null(opt_criterion)) {opt_criterion = 'performance'}
          else if (!(opt_criterion %in% opt_criteria)) {
            warning(paste(opt_criterion, 'is invalid optimization criterion', opt_criterion))
          }
          return(opt_criterion)
        }
        
        check_opt_metric <- function(config, data, response.column){
          metric = switch(config$optimization_criterion,
                          performance = check_performance_metric(list(performance_metric = config$metric), 
                                                                 data = data, 
                                                                 response.column = response.column),
                          stability = check_stability_metric(list(stability_metric = config$metric)),
                          RPT = 'RPT')
          return(metric)
        }
        
        optim = list()
        if (sum(grepl('^internal_(.)', names(config))) != 0){
          optim_resampling_config = config[grepl('^internal_(.)', names(config))]
          names(optim_resampling_config) = gsub('^internal_(.)', '\\1', names(optim_resampling_config))
          optim_resampling = check_resamp_config(optim_resampling_config)
          optim = optim_resampling
        } else {
          warning('none of the internal resampling parameters specified, using default')
          optim = check_resamp_config(config = NULL)
          }
        for (name in setdiff(minNames, names(config)[grepl('^internal_(.)', names(config))])){
          if (name %in% names(config)){
            optim[[name]] = switch(name, 
                                 grid_resolution = check_grid_resolution(config),
                                 optimization_criterion = check_opt_criterion(config),
                                 metric = check_opt_metric(config, data = data, 
                                                           response.column = response.column))

          } else {
            optim[[name]] = assign_default(name, data, response.column)
          }
        }
        optim_config = optim
      } else {
          optim_config = NULL
      }
    }
  }
  return(optim_config)
}

check_stability_metric <- function(config){
  stability_metric <- config$stability_metric
  stability_metrics = c('ALL','jaccard', 'kuncheva', 'sorensen', 'ochiai', 'POG', 'POGR',
                        'multi',
                        'correlation_pearson',
                        'correlation_spearman')
  if (is.null(stability_metric) || !(stability_metric %in% stability_metrics)){
    warning('stability metric not specified or missing in the list of pre-defined metric, using the default')
    stability_metric = assign_default('stability_metric')
  }
  return(stability_metric)
}

#' Title
#'
#' @param config 
#' @param data 
#' @param response.column 
#'
#' @return
#' @export
#'
#' @examples
check_performance_metric <- function(config, data, response.column){
  performance_metric = config$performance_metric
  classification_metrics =  c('Accuracy', 'Kappa')
  regression_metrics = c('Rsquared', 'Rsquared_spearman', 
                         'Rsquared_pearson','pseudo_Rsquared','RMSE')
  recognized_perf_metrics = c(classification_metrics, regression_metrics)
  if (is.null(performance_metric)){
    warning('performance metric missing, opting for default')
      performance_metric = assign_default('performance_metric', data, response.column)
  } else if (performance_metric != 'ALL'){
    if (!(performance_metric %in% recognized_perf_metrics)){
      warning(paste(performance_metric, 'performance metric is not recognized, using the default'))
      performance_metric = assign_default('performance_metric', data, response.column)
    } else {
      task_type = check_task_type(data, response.column)
      compatible_metrics = switch(task_type,
                                  classification = classification_metrics,
                                  regression     = regression_metrics)
      if (!(performance_metric %in% compatible_metrics)){
        stop(sprintf(' %s is not compatible with %s task', performance_metric, task_type))
      }
    }
  }
  return(performance_metric)
}

check_aggregation_method <- function(config){
  aggregation_method = config$aggregation_method
  aggregation_methods = c('CAL')
  if (is.null(aggregation_method) || !(aggregation_method %in% aggregation_methods)){
    warning(paste(aggregation_method, ' not in pre-defined aggregation methods, using default'))
    aggregation_method = assign_default('aggregation_method')
  }
  return(aggregation_method)
}

assign_default <- function(name, data = NULL, response.column = NULL){
  value <- switch(name,
                  grid_resolution = 3,
                  optimization_criterion = 'performance',
                  metric = ifelse(is.factor(data[[response.column]]),
                                  'Accuracy','Rsquared_spearman'),
                  internal_resampling = 'repeatedcv',
                  internal_repetitions = 10,
                  repetitions = 50,
                  resampling = 'LGOCV',
                  fraction = .8,
                  performance_metric = 'ALL',
                  stability_metric = 'jaccard',
                  aggregation_method = 'CAL')
  return(value)
}
