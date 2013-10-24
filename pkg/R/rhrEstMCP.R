#' Minimum Convex Polygon (MCP)
#'
#' @param xy data.frame with two columns. The first column contains x coordinates and the second column contains y coordinates
#' @param levels vector with the percentage of closest points to the centroid that are used to calculated MCP
#' @param ud logical value, indicating whether or not a utilization distribution should be calculated.
#' @param cud a logical value, indicating whether or not a cumulative utilization distribution should be calculated
#' @param xrange vector of length two, with xmin and xmax for the UD
#' @param yrange vector of length two, with ymin and ymax for the UD
#' @param res the resolution for the ud. 
#' @return object of class \code{RhrHREstimator}
#' @export
#' @author Johannes Signer inspired from \code{adehabitatHR::mcp}
#' @seealso \code{adehabitatHR::mcp}, \code{rgeos::gConvexHull}
#' @note Computes the minimum convex polygon of a subset of points. First the centroid of the home range is found with \code{rgeos::gCentroid} and then the `100 - levels` points are used to calculate a minimum convex polygon. 
#' @examples
#' data(datSH)
#' ## Calculate mcp at one level
#' mcp1 <- rhrMCP(datSH[, 2:3], levels=95)
#' ## Calculate mcp at several levels
#' mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))
#'
#' ## Area at each isopleth level
#' areas(mcp2)
#'
#' ## SptialPolygonsDataFrame of isopleth
#' isopleths(mcp2)

rhrMCP <- function(xy, levels=95, ud=FALSE, cud=FALSE, xrange=NA, yrange=NA, res=100) {

  ## local variables
  argsIn <- as.list(environment())[-1]
  projString <- CRS(as.character(NA))  # stores the projection information
  
  ## Input checks
  ## Coordinates
  if(is(xy, "data.frame")) {
    xy <- SpatialPoints(xy)
  } else if (inherits(xy, "SpatialPoints")) {
    projString <- proj4string(xy)
    stop(paste0("rhr: rhrMCP: xy should be of class data.frame or SpatialPoints. The provided xy is of class ", class(xy)))
  }

  ## Levels
  if (!is(levels, "numeric")) {
    stop(paste0("rhr: rhrMCP: levels should be of class numeric. The provided level is of class ", class(xy)))
  }

  ## Levels
  if (any(levels > 100) | any(levels < 1)) {
    stop(paste0("rhr: rhrMCP: levels should be between 1 and 100. The current range is ", paste0(range(levels), collapse=" - ")))
  }

  ## UD
  if (!is(ud, "logical")) {
    stop(paste0("rhr: rhrMCP: ud should be logical. the provided object is ", class(ud)))
  }

  ## call constructor for output
  out <- rhrHREstimator(data.frame(coordinates(xy)), call=match.call(),
                        params=list(name="mcp", levels=levels, ud=ud, proj4string=projString, cud=cud,
                          args=argsIn), 
                        ud=ud, cud=cud)


  ## Distances from the relocations to the centroid: we keep
  dists <- data.frame(id=1:length(xy), dist=as.vector(gDistance(gCentroid(xy), xy, byid=TRUE)))
  

  ## Levels
  if (ud) {
    levelsUd <- c(levels, 1:100)
    levelsUd <- levelsUd[order(levelsUd)]
    levelsUd <- unique(levelsUd)
  } else {
    levelsUd <- levels
  }

  ## calculate mcps
  mcps <- lapply(levelsUd, function(l) gConvexHull(xy[dists[dists$dist <= quantile(dists$dist, l/100), "id"], ], id=l))

  ## Project
  bb <- do.call(rbind, mcps)
  bb <- SpatialPolygonsDataFrame(bb, data.frame(level=names(bb), area=gArea(bb, byid=TRUE)))
  proj4string(bb) <- projString

  out <- rhrSetIso(out, bb)

  if (ud) {
    r1 <- rasterFromXYVect(coordinates(xy), xrange=xrange, yrange=yrange, res=res)
    cud <- rasterize(bb, r1, field="level", fun="min") / 100
    ud <- (1 - cud) / sum(1 - cud[], na.rm=T)

    ## mcp at levls
    mcp <- bb[bb$level %in% levels,]

    out <- rhrSetIso(out, mcp)
    out <- rhrSetUD(out, ud)
    out <- rhrSetCUD(out, cud)
  }

  return(out)

}


