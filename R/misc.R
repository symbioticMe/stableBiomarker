#' Title
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

check_fs_type <- function(fs_config, scoring_algorithm){
  fs_type = c('embedded', 'filter', 'wrapper')
  return(fs_type)
}