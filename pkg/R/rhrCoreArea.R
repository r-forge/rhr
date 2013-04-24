#' Generic Function to calculate area of a RhrEstimator*
#' 
#' @param est a RhrEstimator* object
#' @param ... further parameters passed to the area function
#' @export
#' @return The area

rhrCoreArea <- function(est, ...) {
  UseMethod("rhrCoreArea", est)
}

#' Core Area for Minimum Convex Polygon
#' 
#' @param est a RhrEstimatorMCP object
#' @param ... further parameters passed to the area function
#' @export
#' @return The area

rhrCoreArea.RhrEstimatorMCP <- function(est, ...) {
  
 stop("Does not makes sense")

}

#' Core Area for Kernel Density Estimation
#' 
#' @param est Kernel density estimation
#' @export
#' @return The area

rhrCoreArea.RhrEstimatorKDE <- function(est, ...) {

  nr <- ceiling(diff(range(est$xrange)) / est$resolution)
  nc <- ceiling(diff(range(est$yrange)) / est$resolution)
  r <- raster(nrow=nr, ncol=nc, xmn=est$xrange[1], xmx=est$xrange[2], ymn=est$yrange[1], ymx=est$yrange[2])
  r[] <- est$data$fhat[nr:1,]


  res <- rhrCoreAreaBase(r)

}

rhrCoreAreaBase <- function(r) {

  obs <- getValues(r)
  obs <- obs[!is.na(obs)]
  ranks <- order(obs)
  obs <- obs[order(obs)]
  prob <- obs / sum(obs)

  pctprob <- prob / max(prob)

  # pctrange <- sapply(prob, function(x) sum(prob >= x) /length(prob))

 # to move to src
  pd <- function(prob) {
    .Call("pdcpp", prob, PACKAGE="rhr")
  }

#cppFunction('
#  NumericVector pd(NumericVector prob) {
#    int n = prob.size();
#    NumericVector out(n);
#
#    for(int i = 0; i < n; ++i) {
#      int sum = 0;
#      for (int j = 0; j < n; ++j) {
#        if ( prob[j] >= prob[i]) {
#          sum += 1;
#        }
#      }
#      out[i] = (double)sum/n;
#    }
#    return out;
#  }
#')



pctrange <- pd(prob)

  dd <- sapply(1:length(prob), function(i) distancePointLine(pctrange[i], pctprob[i], 0, 1, 1, 0))



  data.frame(obs=obs, ranks=ranks, prob=prob, pctprob=pctprob, pctrange=pctrange, mindist=dd)
}


distancePointLine <- function(x, y, x1, y1, x2, y2) {

 # code from: http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
 normalLength = ((x2 - x1)^2 + (y2 - y1)^2)
 abs((x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)) / normalLength

}

