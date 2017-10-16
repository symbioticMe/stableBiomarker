check_filtered_fs <- function(fs.method, fs.config, config, data, response.column){
  defined.fs.methods = tolower(c('topN', 'significance'))
  if (!(tolower(fs.method) %in% defined.fs.methods)){
    warning('fs.method configured is not defined, opting for default')
    fs.method = 'TopN'
  }
  fs.config$scoring = check_filter_scoring(fs.config, config, data, response.column)
  #check thresholds
  fs.method = tolower(fs.method)
  fs.config = switch(fs.method,
                     topn = check_topN(fs.config, config, data),
                     significance = check_significance(fs.config, config))
  multivariate_fs_methods = c()
  fs.config$multivariate = ifelse(fs.method %in% multivariate_fs_methods, T, F)
  return(fs.config)
}

check_filter_scoring <- function(fs.config, config, data, response.column){
  if (!('scoring' %in% tolower(names(fs.config))) &
      !('scoring' %in% tolower(names(config)))){
    warning('filter scoring function not defined, opting for default')
    scoring.function = 'customRF'
  } else if (('scoring' %in% tolower(names(config))) &
             ('scoring' %in% tolower(names(fs.config)))){
    warning('filter scoring function defined twice, picking fs_threshold')
    scoring.function = fs.config$scoring
    #ToDo: check_filter_scoring(fs.config$scoring, config)
  } else{
    scoring.function = ifelse('scoring' %in% tolower(names(config)),
                     config$scoring, fs.config$scoring)}
  scoring.functions = c('correlation', 'correlation_spearman','correlation_pearson',
                        'GAM','ANOVA', 'customRF')
  if (!scoring.function %in% scoring.functions){
    warning(paste('Scoring function provided(',scoring.function,'is not defined,',
            'opting for default'))
    scoring.function = 'customRF'
  }
  #check if classification/regression, so that to opt for GAM/ANOVA
  if (scoring.function == 'correlation'){
    scoring.function = 'correlation_pearson'
  }
  #ToDo: add checking for parameters for the custom functions
  #Call function; function;parameters = scoring.params = as.numeric?
  scoring.params = get_scoring_params(scoring.function, fs.config)
  scoring.function = scoring.params$scoring.function
  scoring.params = scoring.params$scoring.params
  
  return(list(scoring.function = scoring.function,
              scoring.params = scoring.params))
}

get_scoring_params <- function(scoring.function, fs.config){
  scoring.function = try(get(scoring.function))
  #ToDo: update the scoring.function "get" behaviour
  scoring.params = NULL
  if (sum(grepl('^scoring_', names(fs.config))) > 0){
    names(fs.config) = gsub('^scoring_', '',  names(fs.config))
    for (i in 1:nrow(scoring.function$parameters)){
      param_name = as.character(scoring.function$parameters[i, 'parameter'])
      if (param_name %in% names(fs.config)){
        if (scoring.function$parameters[i, 'class'] == 'numeric'){
          scoring.params[[param_name]] = as.numeric(fs.config[[param_name]])
        }
      }
    }
  }
  return(list(scoring.function = scoring.function,
              scoring.params = scoring.params))
}
#' Title
#'
#' @param fs.config 
#' @param config 
#'
#' @return
#' @export
check_significance <- function(fs.config, config){
  if (!('threshold' %in% tolower(names(config))) &
      !('threshold' %in% tolower(names(fs.config)))){
    warning('threshold not defined, opting for default')
    threshold = .05
  } else {
    if (('threshold' %in% tolower(names(config))) &
        ('threshold' %in% tolower(names(fs.config)))){
      warning('filter threshold defined twice, picking fs_threshold')
      threshold = as.numeric(fs.config$threshold)
    } else{
      threshold = ifelse('threshold' %in% tolower(names(config)),
                         as.numeric(fs.config$threshold),
                         as.numeric(config$threshold))
    }
  }
  if (threshold >= 1 & threshold <= 0){
    warning('significance threshold as probability should be between 0 and 1, opting for default')
    threshold = .05
  }
  fs.config$threshold = threshold
  return(fs.config = fs.config)
}

#' Title
#'
#' @param fs.config 
#' @param config 
#'
#' @return
check_topN <- function(fs.config, config, data){
  if (!('threshold' %in% tolower(names(config))) &
      !('threshold' %in% tolower(names(fs.config)))){
    warning('threshold not defined, opting for default')
    threshold = min(100, round(0.1 * ncol(data)))
  } else {
    if (('threshold' %in% tolower(names(config))) &
        ('threshold' %in% tolower(names(fs.config)))){
      warning('filter threshold defined twice, picking fs_threshold')
      threshold = as.numeric(fs.config$threshold)
    } else{
      threshold = ifelse('threshold' %in% tolower(names(config)),
                         as.numeric(config$threshold),
                         as.numeric(fs.config$threshold))
    }
    if (threshold > .5*ncol(data)){
      warning('number of top features is more than a half of total number of features,
                are you sure?')
    }
    fs.config$threshold = threshold
  }
  return(fs.config)
}