#' Generic Function to calculate area of a RhrEstimator*
#' 
#' @param x a RhrEstimator* object
#' @param ... further parameters passed to the area function
#' @export
#' @return The area

rhrCoreArea <- function(x, ...) {
  UseMethod("rhrCoreArea", x)
}


#' Core area for home range estimator
#' 
#' @param x object of class RhrHREstimator
#' @param ... further parameters passed to the area function
#' @method rhrCoreArea RhrHREstimator
#' @export
#' @return object of class RhrHRCoreArea

rhrCoreArea.RhrHREstimator <- function(x, ...) {

  if (!hasUD(x)) {
    stop("UD is required to calculate core area")
  }

  ## retrieves the ud
  r <- ud(x)

  ## Standardize the ud to 1, not necessary anymore
  uds <- r[] / sum(r[], na.rm=TRUE)

  ## fraction of max uds
  fuds <- uds / max(uds, na.rm=TRUE)

  ## Create a rastre with fraction of maximum ud
  pctp.r <- setValues(r, fuds)

  ## oder fraction of maximum ud 
  fuds.o <- order(fuds, decreasing=T)

  uds.fudso <- uds[fuds.o]

  pctp <- fuds[fuds.o]

  pctr <- rep(NA, length(uds))

  for (i in seq_along(uds)) {
    pctr[i] <- sum(uds.fudso >= uds.fudso[i], na.rm=TRUE) / length(uds.fudso)
  }
  dd <- sapply(1:length(pctr), function(i) distancePointLine(pctr[i], pctp[i], 0, 1, 1, 0))
  out <- list(pctprob=pctp, pctrange=pctr, rast=pctp.r >= pctp[which.max(dd)], dist=dd, method="powell")
  class(out) <- "RhrHRCoreArea"
  return(out)
}


distancePointLine <- function(x, y, x1, y1, x2, y2) {
 # code from: http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
 normalLength = ((x2 - x1)^2 + (y2 - y1)^2)
 abs((x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)) / normalLength
}


#' plot for RhrHRCoreArea
#' 
#' generic plot for RhrHREstimator
#' @param x RhrHRCoreArea object
#' @param ... none implemented
#' @method plot RhrHRCoreArea
#' @export

plot.RhrHRCoreArea <- function(x, ...) {
  ## Plot curve
  p <- ggplot(data.frame(x=x$pctrange, y=x$pctprob), aes(x=x, y=y)) +
    geom_point(alpha=0.04, position=position_jitter(height=0.01, width=0.01)) +
      xlim(c(0,1)) + ylim(c(0,1)) +
        geom_abline(intercept=1, slope=-1) + 
          geom_point(aes(x=x,y=y), colour="red", size=4,
                     data=data.frame(x=x$pctrange[which.max(x$dist)], y=x$pctprob[which.max(x$dist)])) +
                       geom_hline(aes(yintercept=y), colour="red", 
                                  data=data.frame(y=x$pctprob[which.max(x$dist)])) +
                                    theme_bw() + labs(title="Corea Area Estimation", x="Fraction of Home Range", y="Fraction of maximum Relative Frequency")
  return(p)
}

