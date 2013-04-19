#' Generic Function to calculate area of a RhrEstimator*
#' 
#' @param est a RhrEstimator* object
#' @param ... further parameters passed to the area function
#' @export
#' @return The area

rhrArea <- function(est, ...) {
  UseMethod("rhrArea", est)
}

#' Area for Minimum Convex Polygon
#' 
#' @param est a RhrEstimatorMCP object
#' @param ... further parameters passed to the area function
#' @export
#' @return The area

rhrArea.RhrEstimatorMCP <- function(est, ...) {
  warning("this function is depricated")
  
  poly.area <- function(xy) {
    # function to calculate the area of a non overlapping regular polygone
    n <- nrow(xy)
    x <- as.numeric(xy[,1])
    y <- as.numeric(xy[,2])
    return(abs(0.5 * sum(x[-n] * y[-1] - x[-1] * y[-n])))
  }

  poly.area(est$estimatorData)
}

#' Area for Kernel Density Estimation
#' 
#' @param xy An object of class \code{SpatialPointsDataFrame}
#' @param level The level of the UD at which area is calculated
#' @param ud returns only the utility distribution as raster
#' @param raster returns a rasterstack with a raster for each level
#' @export
#' @return The area

rhrArea.RhrEstimatorKDE <- function(est, level=95, ud=FALSE, raster=FALSE, matrix=FALSE, contour=FALSE) {

  # list for results
  res <- list()

  if (!is.numeric(level)) {
    stop("level must be numeric")
  }

  level <- level/100

  # standardize
  v <- est$data$fhat
  v <- v / sum(v)

  
  # get values and multiply by cell size
  #v <- v * est$resolution^2
  #index <- 1:length(v)
  #vord<-v[order(v, decreasing=TRUE)]
  #indord<-index[order(v, decreasing=TRUE)]
  #vsu <-cumsum(vord)
  #v <- vsu[order(indord)]

  v <- cumsum(v[order(-v)])[order(order(-v))]
  n <- sapply(level, function(x) sum(v <= x))
  dim(v) <- dim(est$data$fhat)
  if (ud) {
    # returns ud

    r <- raster(t(v)[ncol(v):1,], xmn=est$xrange[1], xmx=est$xrange[2], ymn=est$yrange[1], ymx=est$yrange[2])
    res$ud <- r
  }
  if (matrix) {
    # returns a list of matrices
    m <- lapply(level, function(x) v <= x)
    for (i in 1:length(m)) dim(m[[i]]) <- dim(est$data$fhat)
    res$matrix <- m

  }

  if (raster) {
    # returns a raster stack
    m <- lapply(level, function(x) v <= x)
    for (i in 1:length(m)) dim(m[[i]]) <- dim(est$data$fhat)
    r <- do.call("stack", lapply(m, function(x) {
      r <- raster(t(x)[ncol(x):1,],
                  xmn=est$xrange[1],
                  xmx=est$xrange[2],
                  ymn=est$yrange[1],
                  ymx=est$yrange[2])
    }))
    if (raster) {
      res$raster <- r
    }
  }
  if (contour) {

    con <- contourLines(x=est$data$x1, y=est$data$x2, z=v, levels=level) # add levels

    con <- data.frame(do.call("rbind", lapply(seq_along(con),   
                                              function(x) do.call("cbind", c(con[[x]], list(comp=x))))))

    # split by contour level
    con <- split(con, con$level)
    
    # split by polygon
    con <- lapply(con, function(x) split(x, x$comp))

    # make sure there are at least 2 points
    con <- lapply(con, function(x) Filter(function(x) nrow(x) > 2, x))

    # Make spatial polyon
    # Complete ring and create each Polygon
    con <- lapply(con, function(x) {
      if (length(x) == 1) {
        lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, c("x", "y")], hole=FALSE))

      } else { 
        bb <- SpatialPolygons(lapply(seq(length(x)), function(i) Polygons(list(Polygon(rbind(x[[i]], x[[i]][1,])[, c("x", "y")])), i)))
        if (any((tm <- gIntersects(bb, byid=T))[upper.tri(tm)])) {
          # some polys intersect find out which and set as wholes
          pos <- expand.grid(b=1:length(bb), s=1:length(bb))
          holes <- rep(FALSE, length(bb))

          for (i in 1:nrow(pos)) {
            if (gContainsProperly(bb[pos[i,1]], bb[pos[i,2]])) {
              # seconds poly is contained by the first
              holes[pos[i,2]] <- TRUE
            }
          }

          lapply(seq_along(x), function(i) Polygon(rbind(x[[i]], x[[i]][1,])[, c("x", "y")], hole=holes[i]))


        } else {
          lapply(x, function(xx) Polygon(rbind(xx, xx[1,])[, c("x", "y")], hole=FALSE))

        }
      }
    })

    ## Check holes, if more than 1 poly, make sp polygons, then check wholes

    # create a list of Polygons for each level
    con <- lapply(seq_along(con), function(i) Polygons(con[[i]], i))
    con <- SpatialPolygons(con)

    df <- data.frame(area=gArea(con, byid=TRUE), level=level)
    row.names(df) <- 1:length(level)
    con <- SpatialPolygonsDataFrame(con, df)
    res$contour <- con
    
  } 
    A <- cbind(level=level * 100, area=n*est$resolution^2)
  res$A <- A

  return(res)
}
