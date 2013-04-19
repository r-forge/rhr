#' Minimum Convex Polygon (MCP)
#'
#' @param xy: SpatialPointsDataFrame, holding fixes for the animal and time frame that the MCP should be calculated
#' @param levels: a vector with the percentage of closest points to the centroid that are used to calculated MCP
#' @return object of class \code{SpatialPolygonsDataFrame}
#' @export
#' @author Johannes Signer modified code from \code{adehabitatHR::mcp}
#' @examples
#' cat("hello world")


rhrMCP <- function(xy, levels=95) {

  # Original function adehabitatHR:::mcp this is a for speed improved and stripped down version

  ## Computes the centroid of the relocations for each animal    

  centroid <- colMeans(xy)


  ## Distances from the relocations to the centroid: we keep
  ## the "percent" closest
  di <- sqrt((xy[,1] - centroid[1])^2 + (xy[,2] - centroid[2])^2)
  key <- 1:length(di)


  which.pts <- lapply(levels, function(x) key[di<=quantile(di, x/100)])
  xy.t <- lapply(which.pts, function(x) xy[x,])

  ## Coordinates of the MCP
  xy.bord <- lapply(xy.t, function(coords) {
    coords.t <- chull(coords[,1], coords[,2])
    xy.bord <- coords[coords.t, ]
    xy.bord <- rbind(xy.bord[nrow(xy.bord),], xy.bord)
    return(xy.bord)
  })
  
  bb <- SpatialPolygons(lapply(seq(length(levels)), function(x) Polygons(list(Polygon(xy.bord[[x]])), x)))
  bb <- SpatialPolygonsDataFrame(bb, data=data.frame(level=levels, area=gArea(bb, byid=TRUE)), match.ID=FALSE)

  return(bb)

}


