#' Retrives the isopleths of an rhrHREstimator object
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return SpatialPolygonsDataFrame

isopleths <- function(x, ...) {
  UseMethod("isopleths", x)
}


#' get the isopleths from rhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @method isopleths RhrHREstimator

isopleths.RhrHREstimator <- function(x, ...) {
    return(x$results$isopleths)
}
