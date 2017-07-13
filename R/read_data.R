#' Read the data from two files with feature table and responses
#'
#' @param data.file the name of the file with the data matrix. It may or may not contain the response
#' @param response.column the name of response column
#' @param response.file if not NULL, the response table must be in this file
#' @param match.column the column (columns?)
#'
#' @return the list containing:
#' \item{data}{the data frame with features + response column named \code{.outcome}}
#' \item{response.column}{the name of the response columnn}
#' @export
#'
#' @importFrom data.table fread
read_data <- function(data.file, response.column,
                      response.file = NULL,
                      match.column = NULL){
  if (!is.null(response.file)){
    x = fread(data.file, header = T)
    y = fread(response.file, header = T)

    #here will be if about how to call response.column
    data = merge(x, y, by = match.column) %>%
      select(-one_of(setdiff(names(y), response.column))) %>%
      rename_('.outcome' = response.column)
  } else {
    if (is.null(match.column)){
      stop('Column(s) to match predictor file and response file not provided')
    } else {
      data = fread(data.file, header = T)
    }
  }
  return(list(data = data,
              response.column = response.column))
}
