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
  task_type = if(is.factor(response) | is.character(response)){
    task_type = 'classification'
  } else if(is.numeric(responce)){ 
    task_type = 'regression'
  } else {
      stop(sprintf("%s response class is not valid for the supervised machine learning task", class(response)))
    }
  return(task_type)
}