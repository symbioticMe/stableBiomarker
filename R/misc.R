#' Check type of the learning problem: Classification vs Regression
#'
#' @param data 
#' @param response.column 
#'
#' @return
#'
#' @examples
check_task_type <- function(data, response.column){
  response = data[[response.column]]
  task_type = if(is.factor(response) | is.character(response) | is.logical(response)){
    task_type = 'classification'
  } else if(is.numeric(response)){ 
    task_type = 'regression'
  } else {
      stop(sprintf("%s response class is not valid for the supervised machine learning task", class(response)))
    }
  return(task_type)
}

#' Attribute Feature Selection method to Embedded, Wrapper of Filter method family
#'
#' @param model.method 
#' @param model.config 
#' @param fs.method 
#' @param fs.config 
#'
#' @return
#' @export
#'
#' @examples
check_fs_type <- function(model.method, model.config, fs.method, fs.config){
  embedded_modelling_methods = c('elastic net')
  filter_methods = c('topn','correlation', 'correlation_spearman','correlation_pearson',
                     'GAM','ANOVA')
  wrapper_methods = c('RFE', 'GA', 'SA')
  fs.methods = union(filter_methods, wrapper_methods)
  if (model.method %in% c('glmnet')){
    if ('alpha' %in% names(model.config)){
      model_type = "elastic net"
    }
    else {model_type = 'ridge regression'}
  }
  if (model_type %in% embedded_modelling_methods) {
    fs_type = 'embedded'
    return(fs_type)
  } else if (is.null(fs.method)){
    stop(paste("For this method (",model.method,") feature selection method needs to be defined"))
  } else if (!(fs.method %in% fs.methods)){
    stop(paste(fs.method, "is not defined with this package"))
  }
  if (fs.method %in% filter_methods) {return("filter")}
  if (fs.method %in% wrapper_methods) {return("wrapper")}
  fs_type = c('embedded', 'filter', 'wrapper')
  
  return(fs_type)
}

#' Turn optimization configuration list into trainControl object
#'
#' @param optim.config 
#'
#' @return trControl
#' @importFrom caret trainControl
#'
#' @examples
turnOptimConfigToTrainControl <- function(optim.config){
  trControl = switch(tolower(optim.config$resampMethod),
                     boot =       trainControl(method = optim.config$resampMethod,
                                               number = ifelse(is.numeric(optim.config$resampConfig$number),
                                                               optim.config$resampConfig$number,
                                                               10)),
                     cv =         trainControl(method = optim.config$resampMethod,
                                               number = ifelse(is.numeric(optim.config$resampConfig$k),
                                                               optim.config$resampConfig$k,
                                                               10),
                                               repeats = optim.config$internal_repetitions),
                     loocv =      trainControl(method = optim.config$resampMethod),
                     lgocv =      trainControl(method = optim.config$resampMethod,
                                               number = ifelse(is.numeric(optim.config$resampConfig$number),
                                                               optim.config$resampConfig$number,
                                                               10),
                                               p = optim.config$resampConfig$p),
                     repeatedcv = trainControl(method = optim.config$resampMethod,
                                               number = ifelse(is.numeric(optim.config$resampConfig$k),
                                                               optim.config$resampConfig$k,
                                                               10),
                                               repeats = optim.config$resampConfig$number),
                     test =       trainControl(method = optim.config$resampMethod,
                                               p = optim.config$resampConfig$p))
  trControl
}

#' Title
#'
#' @param method_config 
#' @param optim_config 
#' @param method 
#' @param method_name 
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' data("Prostate")
#' response.column = 'lpsa'
#' config = configurePipeline(Prostate, response.column = response.column, 
#' 'config_sandbox/prostate_lasso.config')
#' method = config$method
#' method_name = config$method_name
#' method_config = config$method_config
#' optim_config = config$optim_config
#' tuneGrid = defineGrid(method_config, optim_config, method, method_name, 
#' Prostate %>% select(-one_of(response.column)), Prostate[[response.column]])
#' }
defineGrid <- function(method_config, optim_config, method, method_name, x, y) {
  method_parameters = method$parameters$parameter
  if (!all(names(method_config) %in% method_parameters)) {
    bad_params = setdiff(names(method_config), method_parameters)
    stop(sprintf("Parameter(s) %s not defined for the %s method", 
                 paste(bad_params, collapse = ', '), method_name))
  }
  grid_resolution  = optim_config$grid_resolution
  if (length(method_config) == length(method_parameters)){
    out = data.frame(method_config)
  } else {
    out = method$grid(x, y, len = grid_resolution)
    if (length(method_config) != 0){
      #some of the parameters are specified, the others are not
      for (name in names(method_config)){
        out[[name]] = method_config[[name]]
      }
    }
    out = unique(out)
  }
  return(out)
}