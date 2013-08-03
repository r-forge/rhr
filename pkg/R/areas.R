#' Get areas from rhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return numeric vector

areas <- function(x, ...) {
  UseMethod("areas", x)
}

#' Get areas from rhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return data.frame
#' @method areas RhrHREstimator

areas.RhrHREstimator <- function(x, ...) {
    return(data.frame(isopleths(x)))
}
