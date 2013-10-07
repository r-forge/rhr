#' Kernel Density Estimation (KDE)
#'
#' @param xy data.frame with two columns. The first column contains x coordinaes and the second column contains y coordinates
#' @param levels a vector with the percentage of closest points to the centroid that are used to calculated MCP
#' @param ud a logical value, whether or not a utilitisation distribution should be calculated
#' @param cud a logical value, whether or not a cumulative utilitisation distribution should be calculated
#' @param xrange vector of length 2, with xmin and xmax for the UD
#' @param yrange vector of length 2, with ymin and ymax for the UD
#' @param res the resolution for the ud. 
#' @param h bandwidth, either a string ("href", "lscv", or "hpi") or an actual value
#' @param extent an optional extent, by which the x and y range are exteded. A value of 0 means no extent and a value of 1 means that the extent is doubled.
#' @return object of class \code{RhrHREstimator}
#' @export
#' @author Johannes Signer 
#' @examples
#' data(datSH)
#' \dontrun{ 
#' # Kernel with href bandwidth estimation
#' k1 <- rhrKDE(datSH[, 2:3], h="href", res=100)
#' plot(k1)
#' 
#' # what is the actually estimated bandwidth?
#' k1$parameters$h
#' 
#' # Kernel with href bandwidth estimation
#' k2 <- rhrKDE(datSH[, 2:3], h="lscv", res=100)
#' plot(k2)
#' 
#' # what is the actually estimated bandwidth?
#' k2$parameters$h
#' }

rhrKDE <- function(xy, xrange=NA, yrange=NA, res=100, ud=TRUE, cud=TRUE, levels=95, h="href", extent=0) {
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
    hres$method <- "lscv"

    ## Calculate home range with adehabitatHR::kernelUD and extract the reference value
    htmp <- kernelUD(SpatialPoints(xy), h="LSCV", grid=grid)
    h <- htmp@h
    hres$h <- h
    hres$converged <- htmp@h$convergence
  } else {
    hres$method <- "user specified"
  }

  # add the acualt h to the result list of h
  hres$h <- h

  ## ---------------------------------------------------------------------------- #
  ## Estimate kernels
  
  ## Create Raster
  kde <- bkde2D(xy, bandwidth=h, range.x=list(xrange, yrange), gridsize=gridsize)

  ## Did h converged, only relevant for LSCV, hence default is NA
  hres$converged <- NA

  if (h[1] == "LSCV") {
    hres$h <- kde@h$h
    hres$converged <- kde@h$convergence
  }

  ## ---------------------------------------------------------------------------- #
  ## Prepare output
  
  ## call constructor for output
  out <- rhrHREstimator(xy, call=match.call(),
                        params=list(name="kde", method=hres$method, levels=levels,
                          h=hres$h, 
                          converged=hres$convergence,
                          ud=ud,
                          cud=cud,
                          args=argsIn,
                          proj4string=projString),
                        ud=ud,
                        cud=cud)

  # ---------------------------------------------------------------------------- #
  ## Finish output
  r1 <- raster(t(kde$fhat)[ncol(r):1,], xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2])

  

  # standardize
  v <- r1[]
  v <- v / sum(v, na.rm=TRUE)
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

            ## second poly is contained by the first
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


