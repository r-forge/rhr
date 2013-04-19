#' Function to calculate characteristics of an estimator (i.e. area)
#' 
#' @param est an object of class RhrEstimator*
#' @param characteristic the name of the characteristic to be calculated
#' @param ... parameters passed to the characteristic function
#' @export
#' @return object of class RhrEstimatorCharacteristic

rhrEstimatorCharacteristic <- function(est, characteristic, ...) {

  if (tolower(characteristic) == "area") {
    return(rhrArea(est, ...))
  }

  if (tolower(characteristic) == "corearea") {
    return(rhrCoreArea(est, ...))
  }
  
}
