#' Kernel Density Estimation (KDE)
#'
#' @param xy data.frame with two columns. The first column contains x coordinaes and the second column contains y coordinates
#' @param levels a vector with the percentage of closest points to the centroid that are used to calculated MCP
#' @param ud a logical value, whether or not a utilitisation distribution should be calculated
#' @param cud a logical value, whether or not a cumulative utilitisation distribution should be calculated
#' @param xrange vector of length 2, with xmin and xmax for the UD
#' @param yrange vector of length 2, with ymin and ymax for the UD
#' @param res the resolution for the ud. 
#' @param h bandwidth, either a string ("href", "lscv") or an actual value
#' @param lscv.n number of iterations for lscv
#' @param tol the tolerace for lscv 
#' @return object of class \code{RhrHREstimator}
#' @export
#' @author Johannes Signer 
#' @examples
#' data(datSH)
#' \dontrun{ 
#' # Kernel with href bandwidth estimation
#' k1 <- rhrKDE(datSH[, 2:3], h="href", resolution=100)
#' 
#' # what is the actually estimated bandwidth?
#' k1$h$h
#' 
#' # Return results as an object of class raster
#' k1 <- rhrKDE(datSH[, 2:3], h="href", resolution=100, return.raster=TRUE)
#' k1$data$rast
#' plot(k1$data$rast)
#' 
#' # Kernel with href bandwidth estimation
#' k2 <- rhrKDE(datSH[, 2:3], h="lscv", resolution=100)
#' 
#' # what is the actually estimated bandwidth?
#' k2$h$h
#' 
#' # Return results as an object of class raster
#' k2 <- rhrKDE(datSH[, 2:3], h="lscv", resolution=100, return.raster=TRUE)
#' k1$data$rast
#' plot(k1$data$rast)
#'
#' # Compare
#' par(mfrow=c(2,1))
#' plot(k1)
#' plot(k2)}

rhrKDE <- function(xy, xrange=NA, yrange=NA, res=100, ud=TRUE, cud=TRUE, levels=95, h="href",lscv.n=100, tol=1) {
  # load libraries
  require(KernSmooth)

  argsIn <- as.list(environment())[-1]
  projString <- CRS(NA)  # contains the projection information

  ## Input checks
  ## Coordinates
  if(!is(xy, "data.frame")) {
    if(inherits(xy, "SpatialPoints")) {
      projString <- proj4string(xy)
      xy <- data.frame(coordinates(xy))
    } else {
      stop(paste0("xy should be of class data.frame or SpatialPoints. The provided xy is of class ", class(xy)))
    }
  }

  ## Levels
  if (!is(levels, "numeric")) {
    stop(paste0("levels should be of class numeric. The provided level is of class ", class(xy)))
  }

  ## Levels
  if (any(levels > 100) | any(levels < 1)) {
    stop(paste0("levels should be between 1 and 100. The current range is ", paste0(range(levels), collapse=" - ")))
  }

  ## UD
  if (!is(ud, "logical")) {
    stop(paste0("ud should be logical. the provided object is ", class(ud)))
  }

  
  # ---------------------------------------------------------------------------- #
  ## Calculate bandwidth

  # List to save results of h
  hres <- list()


  ## Reference bandwidth
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

  # Create out raster
  r1 <- rasterFromXYVect(xy, xrange=xrange, yrange=yrange, res=res)
  ## determine gridsize
  ncolumns <- ncol(r1)
  nrows <- nrow(r1)
  xrange <- c(xmin(r1), xmax(r1))
  yrange <- c(ymin(r1), ymax(r1))
  gridsize <- c(ncolumns, nrows)

  ## Calculate bandwidth
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

    # check wether or not LSCV converged or not
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

  ## ---------------------------------------------------------------------------- #
  ## Prepare output
  
  ## call constructor for output
  out <- rhrHREstimator(xy, call=match.call(),
                        params=list(name="kde", method=hres$method, levels=levels,
                          h=hres$h, 
                          ud=ud,
                          cud=cud,
                          args=argsIn,
                          proj4string=projString),
                        ud=ud,
                        cud=cud)

  # ---------------------------------------------------------------------------- #
  ## Estimate kernels
  
  ## Create Raster
  kde <- bkde2D(xy, bandwidth=h, range.x=list(xrange, yrange), gridsize=gridsize)

  # ---------------------------------------------------------------------------- #
  ## Finish output
  
  r <- kde$fhat
  r1 <- raster(t(r)[ncol(r):1,], xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2])
  

  # standardize
  v <- r1[]
  v <- v / sum(v)
  udFromDat <- setValues(r1, v)

  v <- cumsum(v[order(-v)])[order(order(-v))]
  cudFromDat <- setValues(r1, v) 

  if (ud) {
    out <- rhrSetUD(out, udFromDat)
  }

  if (cud) {
    out <- rhrSetCUD(out, cudFromDat)
  }

  con <- rasterToContour(cudFromDat * 100, levels=levels)

  b <- coordinates(con)
  
  ## make sure there are at least 2 points
  b <- lapply(b, function(x) Filter(function(x) nrow(x) > 2, x))

  ## Make spatial polyon
  ## Complete ring and create each Polygon
  con <- lapply(b, function(x) {
    if (length(x) == 1) {
      lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, 1:2], hole=FALSE))

    } else { 
      bb <- SpatialPolygons(lapply(seq(length(x)), function(i) Polygons(list(Polygon(rbind(x[[i]], x[[i]][1,])[, 1:2])), i)))
      if (any((tm <- gIntersects(bb, byid=T))[upper.tri(tm)])) {

        ## some polys intersect find out which and set as wholes
        pos <- expand.grid(b=1:length(bb), s=1:length(bb))
        holes <- rep(FALSE, length(bb))

        for (i in 1:nrow(pos)) {
          if (gContainsProperly(bb[pos[i,1]], bb[pos[i,2]])) {

            ## seconds poly is contained by the first
            holes[pos[i,2]] <- TRUE
          }
        }

        lapply(seq_along(x), function(i) Polygon(rbind(x[[i]], x[[i]][1,])[, 1:2], hole=holes[i]))


      } else {
        lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, 1:2], hole=FALSE))

      }
    }
  })

  ## Check holes, if more than 1 poly, make sp polygons, then check wholes

  ## create a list of Polygons for each level
  con <- lapply(seq_along(con), function(i) Polygons(con[[i]], i))
  con <- SpatialPolygons(con)

  proj4string(con) <- projString  # set projection

  df <- data.frame(area=gArea(con, byid=TRUE), level=levels)
  row.names(df) <- 1:length(levels)
  con <- SpatialPolygonsDataFrame(con, df)

  out <- rhrSetIso(out, con)
  return(out)


}


