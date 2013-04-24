#' Kernel Density Estimation (KDE)
#'
#' @param xy data.frame with two columns. The first column contains x coordinaes and the second column contains y coordinates
#' @param h bandwidth, either a string ("href", "lscv") or an actual value
#' @param xrange the xrange for the out grid
#' @param yrange the yrange for the out grid 
#' @param resolution the resolution for the out grid 
#' @param lscv.n number of iterations for lscv
#' @param tol the tolerace for lscv 
#' @param return.raster returns the hat matrix with kernel densities as an object of class raster
#' @return object of class \code{RhrEstimatorKDE}
#' @export
#' @author Johannes Signer 
#' @examples
#' cat("hello world")


rhrKDE <- function(xy, h="href", xrange=NA, yrange=NA, resolution=100, lscv.n=100, tol=1, return.raster=FALSE) {
  # load libraries
  require(KernSmooth)

  # List to save results of h
  hres <- list()

  ## helper function for bandwidth estimation
  href <- function(x) {
    sigma <- sqrt(0.5 * (var(xy[,1]) + var(xy[,2])))       
    sigma * nrow(xy)^(-1/6)
  }

  ## Least Square Cross Validation
  lscv <- function(h, xy, tol=tol) {
    # h: vector of length 2 with min and max bandwidth
    # xy: data.frame with 2 col, first for x and second for y
    # tol: the tolerance, see ?optimize 	  
    
    n <- nrow(xy)	
	  d <- (as.vector(as.matrix(dist(xy, diag=T, upper=T))) )
	
	  hlp <- function(h, xy, d) {
		  d <- d^2 / h^2
      # Formula as fund on p. 20 of HRT Manual, possibly also by Worton et al 1995
		  sum((1/(4 * pi) * exp(-d/4) - 1/pi * exp(-d/2)) / (n^2 * h^2)) + 1 /(pi * n * h^2)
	  }
	
	  return(optimize(hlp, range(h), tol=tol, xy=xy, d=d)$minimum)

  }

  ##
  xy <- apply(xy, 2, as.numeric)

  if (any(is.na(xrange)) | length(xrange) != 2) {
    xrange <- c(min(xy[,1]), max(xy[,1]))
    warning("retrieved xrange from data")
  } 
  if (any(is.na(yrange)) | length(yrange) != 2) {
    yrange <- c(min(xy[,2]), max(xy[,2]))
    warning("retrieved yrange from data")
  }

  ## determine gridsize
  ncolumns <- ceiling(diff(xrange) / resolution)
  nrows <- ceiling(diff(yrange) / resolution)

  gridsize <- c(ncolumns, nrows)

  if (tolower(h) == "href") {
    ## Formula from ?adehabitatHR::kernelUD
    h <- href(xy)
    h <- c(h, h)
    hres$method <- "href"
  } else if (tolower(h) == "hpi") {
    ## Do for each coordiante seperately
    hx <- dpik(xy[,1], gridsize=ncolumns)
    hy <- dpik(xy[,2], gridsize=nrows)
    h <- c(hx, hy)
    hres$method <- "hpi"
  } else if (tolower(h) == "lscv") {
    # tolerance for optimizer
    tol <- 1
    # atm range of possible h values is hard coded
    hrange <- href(xy)
    hrange <- c(0.1 * hrange, 2 * hrange)

    # try to find bandwidth
    h <- lscv(hrange, xy, tol=tol)

    # check wether ornot LSCV converged or not
    hasConverged <- TRUE
    if (h <= hrange[1] - tol | h >= hrange[2] + tol) {
      warning("LSCV did not converge")
      hasConverged <- FALSE
    }
    h <- c(h, h)
    hres$method <- "lscv"
    hres$hasConverged <- hasConverged
  } else {
    hres$method <- "user specified"
  }

  # add the acualt h to the result list of h
  hres$h <- h

  kde <- bkde2D(xy, bandwidth=h, range.x=list(xrange, yrange), gridsize=gridsize)


  res <- list()
 
  res$estimator <- "kde"
  res$h <- hres
  res$xrange <- xrange
  res$yrange <- yrange
  res$resolution <- resolution
  res$data <- kde

  if (return.raster) {
    r <- kde$fhat
    res$data$rast <- raster(t(r)[ncol(r):1,], xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2])
  }

  class(res) <- "RhrEstimatorKDE"

  return(res)

}


