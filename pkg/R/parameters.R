#' Retrieves parameters from estimator
#' 
#' @param x the estimator
#' @param ... further arguments, none implemented
#' @export
#' @return List

parameters <- function(x, ...) {
  UseMethod("parameters", x)
}

#' Retrieves parameters of an object of class RhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @method parameters RhrHREstimator
#' @return List

parameters.RhrHREstimator <- function(x, ...) {
  return(x$parameters)
}
