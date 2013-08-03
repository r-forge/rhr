#' get data from RhrHREstimator
#' 
#' generic print for RhrHREstimator
#' @usage dat(x, ...)
#' @param x RhrHREstimator object
#' @param ... ignored
#' @return data.frame
#' @export

dat <- function(x, ...) {
  return(x$dat)
}
