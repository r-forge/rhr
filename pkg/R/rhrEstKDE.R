#' Kernel Density Estimation (KDE)
#'
#' A function to estimate home ranges with kernel density estimation. 
#'
#' @param xy \code{data.frame} with two columns: x and y coordinates.
#' @param levels numeric vector with the isopleth levels.
#' @param h character ("href" or "hlscv") specifying the method to estimate the bandwidth or numeric value specifying the bandwidth.
#' @param xrange numeric vector specifying min and max x for the output grid.
#' @param yrange numeric vector specifying min and max y for the output grid.
#' @param increaseExtend numeric value by which the x and y range are extended, see also \code{?extendrange}.
#' @param trast a \code{RasterLayer} used as an template for the output grid.
#' @param buffer numeric value to buffer the bounding box of points in map units from which the extent of the output grid will be retrieved.
#' @param res numeric value specifying the resolution for the output grid. 
#' @param gridsize a vector of length 2, specifying the number of rows and the number of columns for the ouput grid.
#' @param ud logical value, indicating whether or not a utilization distribution should be calculated.
#' @param cud logical value, indicating whether or not a cumulative utilization distribution should be calculated.
#' @param rescale character value specifying if the data should be rescaled before calculating bandwidth. Possible values are: \code{unitvar} to rescale to unit variance, \code{unitx} to rescale data to variance of x and \code{none}.
#' @param lscvSearch numeric vector of length 2, specifying lower and uper bound for candidate bandwidth (as portion of reference bandwidth) for estimating bandwidth with least squre cross validation.
#' @param lscvWhichMin character value, specifying how candidate bandwidths are chosen with least squre cross validation. Possible values are: \code{global} or \code{local} minimum.

#' @details The size and resolution of the resulting utilization distribution (UD) grid is influenced by \code{traster, xrange, yrange, increaseExtent, buffer, res, gridsize}. The size of the grid can be set either through a template raster (\code{traster}), \code{xrange} and \code{yrange} or \code{increaseExtend}. \code{traster} takes precedence over \code{xrange} and \code{yrange}, \code{buffer} and \code{grid}. If none of the previous arguments are provided, \code{xrange} and \code{yrange} are taken from the data.
#'
#' The resolution of the resulting UD grid can be set through either \code{res} or \code{gridsize}. \code{res} takes precedence over \code{gridsize}. If none of the previous arguments is provided the grid is set by default to a 100 by 100 grid.

#' The bandwidth can be provided by the user or estimated through the reference bandwidth (this method is often refered to as the ad hoc method), plug in the euqtion method or the least square cross validation method. Reference bandwidth estimation is implemented as suggested by Silverman 1986. Plugin the equation method is wrapped from \code{KernSmooth::dpki} and a simple binned version Silverman's suggestion for least square cross validation is implemented.

#' Kernels densities are estimated with \code{KernSmooth::bkde2d}. This is a binned approximation of 2D kernel density estimates (see \code{?KernSmooth::bkde2d} for more details. 
#'

#' @seealso \code{KernSmooth::bkde2d}, \code{KernSmooth::dpik}, \code{rhr::rhrHref}, \code{rhr::rhrHlscv}, \code{rhr::rhrHpi}


#' @return object of class \code{RhrHREstimator}
#' @export
#' 
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

rhrKDE <- function(xy,
                   levels=95,
                   h="href",
                   xrange=NULL,
                   yrange=NULL,
                   increaseExtent=NULL,
                   trast=NULL,
                   buffer=NULL,
                   ud=TRUE,
                   cud=TRUE,
                   res=NULL,
                   rescale="unitvar",
                   lscvSearch=NULL,
                   lscvWhichMin="global",
                   gridsize=NULL) {

  argsIn <- as.list(environment())[-1]
  projString <- CRS(NA)  # contains the projection information

  ## Input checks
  if (ncol(xy) > 2) {
    xy <- xy[, 1:2]
    warning("xy has more than 2 columns, only the first two are used")
  }

  
  ## Coordinates
  if(!is(xy, "data.frame")) {
    if(inherits(xy, "SpatialPoints")) {
      projString <- proj4string(xy)
      xy <- data.frame(coordinates(xy))
    } else {
      stop(paste0("xy should be of class data.frame or SpatialPoints. The provided xy is of class ", class(xy)))
    }
  }

  names(xy) <- c("x", "y")

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

  ## Is the bandwidth estimation valid
  if (!is(h, "numeric")) {
    if (!tolower(h) %in% c("hlscv", "href", "hpi")) {
      stop("rhrKDE: bandwidth: unknown method requested")
    }
  }

  ## Create out raster
  if (!is.null(trast)) {
    ## use template, thats it
    r1 <- trast
    rnrow <- nrow(r1)
    rncol <- ncol(r1)
    xrange <- c(xmin(r1), xmax(r1))
    yrange <- c(ymin(r1), ymax(r1))

  } else { ## !template raster provided
    if (!is.null(xrange) & !is.null(yrange)) {
      ## obtain raster from xrange
      xrange <- xrange
      yrange <- yrange

    } else if (!is.null(increaseExtent)) {
      ## figure out range from extent, i.e. extent input range by factor
      xrange <- extendrange(xy[ , "x"], increaseExtend)
      yrange <- extendrange(xy[ , "y"], increaseEextend)

    } else if (!is.null(buffer)) {
      ## figure out range from buffer, i.e. input range + buffer
      xrange <- range(xy[, "x"]) + c(-buffer, buffer)
      yrange <- range(xy[, "y"]) + c(-buffer, buffer)

    } else {
      ## take range from data
      xrange <- range(xy[, "x"])
      yrange <- range(xy[, "y"])
    }

    ## Figure out resolution
    if (!is.null(res)) {
      ## take resolution from input
      ## if only one value is provided, double it
      if (length(res) == 1L) {
        res <- rep(res, 2)
      }
      resx <- res[1]
      resy <- res[2]

      rncol <- ceiling(diff(xrange) / resx)
      rnrow <- ceiling(diff(yrange) / resy)

    } else if (!is.null(gridsize)) {
      ## take resolution from grid size
      if (length(gridsize) == 1L) {
        gridsize <- rep(gridsize, 2)
      }
       
      rncol <- gridsize[1]
      rnrow <- gridsize[2]

    } else {
      ## take default resolution of 100 * 100
      rncol <- 100
      rnrow <- 100
    }

  }
  r1 <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2],
               nrows=rnrow, ncols=rncol)
  
  
  ## ---------------------------------------------------------------------------- #
  ## Calculate bandwidth

  ## List to save results of h
  hres <- list()

  ## Calculate bandwidth
  if (is(h, "numeric")) {
    hres$method <- "user specified"
    if (length(h) == 1) {
      hres$h <- c(h, h)
    } else {
      hres$h <- h[1:2]
    }
  } else if (tolower(h) == "href") {
    hres$h <- rhrHref(xy, rescale=rescale)
    hres$method <- "href"

  } else if (tolower(h) == "hpi") {
    ## Do for each coordiante seperately
    hres$h <- rhrHpi(xy, rescale=rescale)
    hres$method <- "hpi"

  } else if (tolower(h) == "hlscv") {
    ## Bin data
    rr <- rasterize(xy, r1, fun="count")
    aa <- rasterToPoints(rr)
    hres$method <- "lscv"

    ## get candidate h's
    h <- rhrHlscv(data.frame(aa), range=lscvSearch, whichMin=lscvWhichMin)
    hres$h <- h$h
    hres$converged <- h$converged
    hres$vals <- h$res
  }

  ## ---------------------------------------------------------------------------- #
  ## Estimate kernels
  
  ## Create Raster
  kde <- bkde2D(xy, bandwidth=hres$h, range.x=list(xrange, yrange), gridsize=c(rncol, rnrow))

  ## ---------------------------------------------------------------------------- #
  ## Prepare output
  
  ## call constructor for output
  out <- rhrHREstimator(xy, call=match.call(),
                        params=list(name="kde", method=hres$method, levels=levels,
                          h=hres$h, 
                          converged=hres$convergenced,
                          ud=ud,
                          cud=cud,
                          args=argsIn,
                          proj4string=projString),
                        ud=ud,
                        cud=cud)

  # ---------------------------------------------------------------------------- #
  ## Finish output
  r1 <- raster(t(kde$fhat)[nrow(r1):1,], xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2])

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



