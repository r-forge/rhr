#' Minimum Convex Polygon (MCP)
#'
#' @param xy data.frame with two columns. The first column contains x coordinaes and the second column contains y coordinates
#' @param levels a vector with the percentage of closest points to the centroid that are used to calculated MCP
#' @param ud a logical value, whether or not a utilitisation distribution should be calculated
#' @param cud a logical value, whether or not a cumulative utilitisation distribution should be calculated
#' @param xrange vector of length 2, with xmin and xmax for the UD
#' @param yrange vector of length 2, with ymin and ymax for the UD
#' @param res the resolution for the ud. 
#' @return object of class \code{SpatialPolygonsDataFrame}
#' @export
#' @author Johannes Signer modified and adapted code from \code{adehabitatHR::mcp}
#' @examples
#' data(datSH)
#' mcp1 <- rhrMCP(datSH[, 2:3], levels=95)
#' mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))



rhrMCP <- function(xy, levels=95, ud=FALSE, cud=FALSE, xrange=NA, yrange=NA, res=100) {

  ## local variables
  argsIn <- as.list(environment())[-1]
  projString <- CRS(as.character(NA))  # stores the projection information
  
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

  ## call constructor for output
  out <- rhrHREstimator(xy, call=match.call(),
                        params=list(name="mcp", levels=levels, ud=ud, proj4string=projString, cud=cud,
                          args=argsIn), #as.list(expand.call())[-(1:2)]), # we do not want the first two args (name of the function and data)
                        ud=ud, cud=cud)

  ## Computes the centroid of the relocations for each animal    
  centroid <- colMeans(xy)

  ## Distances from the relocations to the centroid: we keep
  ## the "percent" closest
  distances <- sqrt((xy[,1] - centroid[1])^2 + (xy[,2] - centroid[2])^2)
  key <- 1:length(distances)

  ## Levels
  if (ud) {
    levels.ud <- c(levels, 1:100)
    levels.ud <- levels.ud[order(levels.ud)]
    levels.ud <- unique(levels.ud)
  } else {
    levels.ud <- levels
  }


  # calculate mcps
  which.pts <- lapply(levels.ud, function(x) key[distances <= quantile(distances, x/100)])
  xy.t <- lapply(which.pts, function(x) xy[x, ])

  ## Coordinates of the MCP
  xy.bord <- lapply(xy.t, function(coords) {
    coords.t <- chull(coords[,1], coords[,2])
    xy.bord <- coords[coords.t, ]
    xy.bord <- rbind(xy.bord[nrow(xy.bord),], xy.bord)
    return(xy.bord)
  })
  
  bb <- SpatialPolygons(lapply(seq(length(levels.ud)), function(x) Polygons(list(Polygon(xy.bord[[x]])), x)))
  bb <- SpatialPolygonsDataFrame(bb, data=data.frame(level=levels.ud, area=gArea(bb, byid=TRUE)), match.ID=FALSE)

  ## Project
  proj4string(bb) <- projString

  out <- rhrSetIso(out, bb)

  if (ud) {
    r1 <- rasterFromXYVect(xy, xrange=xrange, yrange=yrange, res=res)
    cud <- rasterize(bb, r1, field="level", fun=min) / 100
    ud <- (1 - cud) / sum(1 - cud[], na.rm=T)

    ## mcp at levls
    mcp <- bb[bb$level %in% levels,]

    out <- rhrSetIso(out, mcp)
    out <- rhrSetUD(out, ud)
    out <- rhrSetCUD(out, cud)
  }

  return(out)

}


