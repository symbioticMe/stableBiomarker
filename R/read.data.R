#' Title
#'
#' @param data.file the name of the file with the data matrix. It may or may not contain the response
#' @param response.column the name of response column
#' @param response.file if not NULL, the response table must be in this file
#' @param match.column the column (columns?)
#'
#' @return the data.table with data and the colum that gives the information about the response
#' @export
#'
#' @examples
read.data <- function(data.file, response.column, response.file = NULL, match.column = NULL){
  if (!is.null(response.file)){
    x = fread(data.file, header = T)
    y = fread(response.file, header = T)
    data = merge(x, y, by = match_column) %>% select_(-one_of(match_column))
  } else {
    if (is.null(match.column)){
      stop('Column(s) to match predictor file and response file not provided')
    } else {
      data = fread(data.file, header = T)
    }
  }
  return(list(data = data, response.column = reponse.column))
}