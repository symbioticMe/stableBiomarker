#' stableBiomarker
#'
#' @name stableBiomarker
#' @docType package
NULL

#' Friedman benchmark dataset
#'
#' A dataset containing the 100 \eqn{y} values calculated by formula:
#' \deqn{y = 10sin(pi x_1 x_2) + 20(x_3 -0.5)^2 + 10x_4+5x_5+N(0, sigma^2)}
#'
#' Thus, of the 50 predictors, there are 45 pure noise variables: 5 are uniform on
#' \eqn{0, 1} and 40 are random univariate standard normals. The predictors are \
#' centered and scaled
#' 
#' @docType data
#' @keywords datasets
#' @name mlbenchFriedman1
#' @usage data(diamonds)
#' @format A data frame with 53940 rows and 10 variables