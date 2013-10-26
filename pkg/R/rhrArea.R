#' Get areas from object of class RhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return \code{data.frame}

rhrArea <- function(x, ...) {
  UseMethod("rhrArea", x)
}

#' @export
#' @rdname rhrArea
#' @method rhrArea RhrHREstimator

rhrArea.RhrHREstimator <- function(x, ...) {
    return(data.frame(isopleths(x)))
}
