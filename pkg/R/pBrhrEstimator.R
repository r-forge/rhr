#' Function to do a parallel bootstrap of a RhrEstimator*characteristic
#' 
#' @param xy a data.frame or matrix
#' @param estimator string specifiying the estimator (e.g. mcp)
#' @param characteristic string specifiying characteristic to bootstrap (e.g. area)
#' @param nsamples how many points should be drawn each time
#' @param niter specifiyies the number of bootstrap iteration
#' @param args.estimator list of further arguments passed to the estimator function
#' @param args.characteristic list of further arguments passed to characteristic function
#' @export
#' @return list with values of characteristic of length

pBrhrEstimator <- function(xy, estimator, characteristic, nsamples, niter, args.estimator=NULL, args.characteristic=NULL) {
  if (tolower(estimator) == "mcp") {
    # Figure out the scenarios I need to do

    # 1. data.frame with all scenarios
    # 
    scn <- expand.grid(estimator=rep(estimator, each=niter), characteristic=characteristic, n=nsamples, percent=args.estimator$percent )

    # 2. calculate home range for each scenario
    res <- apply(scn, 1, function(x) {
                 est <- x[1]
                 char <- x[2]
                 n <- x[3]
                 percent <- x[4] # find why p is not working
                 pts <- xy[sample(1:nrow(xy), n, TRUE),]

                 # Calculate homerange
                 #                  hr <- do.call("rhrEstimator", list(pts, est, percent=10)) # no further args are required
                 hr <- rhrEstimator(pts, est)
                 # 3. calculate area
                 hr.a <- do.call("rhrEstimatorCharacteristic", c(list(hr, "area")))
                 return(list(area=hr.a, n=n, percent=percent))
          })
  return(res)
  }
}
