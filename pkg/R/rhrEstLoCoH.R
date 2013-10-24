#' Local Convex Hull (LoCoH)
#'
#' This function estimates an animals home range using Local Convex Hulls (LoCoH).
#'
#' @param xy data.frame with two columns. The first column contains x coordinates and the second column contains y coordinates.
#' @param levels a vector with isopleth levels at which home range should be calculated.
#' @param ud a logical value, indicating whether or not a utilization distribution should be calculated.
#' @param cud a logical value, indicating whether or not a cumulative utilization distribution should be calculated.
#' @param xrange vector of length 2, with xmin and xmax for the UD
#' @param yrange vector of length 2, with ymin and ymax for the UD
#' @param res numeric value, the resolution of the ud. 
#' @param type character value, one of k, r, a. Three methods to select which neighbours are used for local hulls are availabe: i) k-nearest neighbours; ii) neighbours within a radius r; iii) neighbours within a cummulative dististance a.
#' @param n numeric value, if type is k it is the number of neaerst neibhers, if type is r it is the radius that is searched and if type is a it is the cummulative distance to be used.
#' @param minPts numeric value, the minimum number of neighbours required.
#' @details Three different types available for determining the number of neighbors:
#' \itemize{
#'  \item{"k"}{uses the k nearest neighbours}
#'  \item{"r"}{uses all neighbours within a radius r}
#'  \item{"a"}{uses all neighbours that can be reached within a distance a. The distance to all points is calculated and then the cummulatively summed starting from the smallest until \code{a} is reached.}}
#' @return object of class \code{RhrHREstimator}
#' @author Johannes Signer 
#' @references Getz, W. M., & Wilmers, C. C. (2004). A local nearest‐neighbor convex‐hull construction of home ranges and utilization distributions. _Ecography_, 27(4), 489-505.
#' @references Getz, W. M., Fortmann-Roe, S., Cross, P. C., Lyons, A. J., Ryan, S. J., & Wilmers, C. C. (2007). LoCoH: nonparameteric kernel methods for constructing home ranges and utilization distributions. _PloS one_, 2(2), e207.
#' @export
#' @examples
#' \dontrun{
#' data(datSH)
#' locoh <- rhrLoCoH(datSH[, 2:3], type="k", n=10, level=c(50, 90))
#'
#' ## area at isopleths
#' areas(locoh)
#'
#' ## get isopleths
#' iso <- isopleths(locoh)
#'
#' ## Which parameter were used?
#' parameters(locoh)
#' }

rhrLoCoH <- function(xy, type="k", n=10, levels=95, minPts=3, ud=FALSE, cud=FALSE, xrange=NA, yrange=NA, res=100) {

  # input checking
  # type:
  if (!type %in% c("a", "k", "r")) {
    stop("rhrLocoh: incorrect type")
  }

  n <- as.numeric(n)
  if (is.na(n)) {
    stop(paste("rhrLocoh: n should be numeric, not ", n))
  }
  
  ## Are levels between 1 and 100, remove duplicated, order and add 0
  levels <- levels[order(levels)]

  if (max(levels) > 99 | min(levels) < 1 | length(levels) != length(unique(levels))) {
    levels <- levels[levels < 100 & levels > 0]
    levels <- unique(levels)
    warning(paste0("adjusted level(s) to: ", levels))
  }

  
  levels.o <- levels
  levels <- c(0, levels, 100)

  if (ud) {
    levels <- seq(0, 100, 1)
  }

  
  ## local variables
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

  ## UD
  if (!is(ud, "logical")) {
    stop(paste0("ud should be logical. the provided object is ", class(ud)))
  }

  ind <- 1:nrow(xy)
  if (type == "k") {

    if (n > nrow(xy)) {
      n <- nrow(xy)
      warning(paste0("Locoh, type k, n > number of points, set n to number of points (", n, ")"))
    }

    # 1. calc dist
    # 2. order by dist
    # 3. take n nearest
    n <- n - 1 # to be consistent with adehabitatHR -> check Getz paper

    abc1 <- lapply(ind, function(i) ind[order(sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2))][1:n])
  } else if (type == "r") {
    # 1. calc dist
    # 2. take all pts with dist <= n
    abc1 <- lapply(ind, function(i) ind[sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2) <= n])
  } else if (type == "a") {
    # 1. calc dist
    # 2. order by dist
    # 3. take cum dist
    # 4. take points where cumist <= n
    abc1 <- lapply(ind, function(i) {
                   di <- sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2)
                   ind[order(di)][cumsum(di[order(di)]) <= n]

            })
  }

  ## call constructor for output
  out <- rhrHREstimator(xy, call=match.call(), params=list(name="LoCoH", levels=levels.o, ud=ud, cud=cud, type=type, n=n,
                                                 proj4string=projString), ud=ud, cud=cud)



  ## remove the ones with less than minPts pts
  abc1 <- abc1[sapply(abc1, length) >= minPts]
  ind <- 1:length(abc1)


  ## identification of the coordinates of the MCP
  pol <- lapply(ind, function(i) {
                            ff <- xy[abc1[[i]],]
                            pol <- ff[chull(ff[,1], ff[,2]),]
                            pol <- rbind(pol, pol[1,]) #  close the poly
                            return(pol)
                        })

  ### Call rhr:::poly.area
  poly.area <- function(xy) {
    # function to calculate the area of a non overlapping regular polygone
    n <- nrow(xy)
    x <- as.numeric(xy[,1])
    y <- as.numeric(xy[,2])
    return(abs(0.5 * sum(x[-n] * y[-1] - x[-1] * y[-n])))
  }

  ar1 <- sapply(pol, poly.area)

  ## add size and order by size
  ## first col is the size
  ar1.order <- order(ar1)

  d <- do.call("rbind", lapply(ar1.order, function(i) cbind(id=ind[i], area=ar1[i], pts=abc1[[i]])))
  d <- data.frame(d)

  ## calc isopleths
  p <- cumsum(ifelse(ave(d$id==d$id, d$pts, FUN=cumsum)==1, 1, 0))
  fac <- rep(1:length(unique(d$id)), rle(d$id)$lengths)
  fac.id <- do.call('c', lapply(rle(d$id)$lengths, function(x) 1:x))

  p <- tapply(p, fac, max)
  a <- data.frame(id=d$id[fac.id == 1], per=p/length(p) * 100)

  ## Get groups of isopleths
  a$isopleth <- cut(a$per, breaks=levels)
  a$isopleth.v <- as.numeric(a$isopleth) 

  ### 1. Make a spatial polygon for each isopleth
  ## I have a vector with the ids of polygons for each isopleth, 
  ## For each isopleth merge all polys that fall within this isopleth and the ones already passe (i.e. the ones that are larger)

  bb <- lapply(split(a[, c("id", "isopleth.v")], a$isopleth.v), function(x) Polygons(lapply(pol[x$id], Polygon), unique(x$isopleth.v)))
  bb <- lapply(bb, function(x) SpatialPolygons(list(x)))
  bb <- lapply(bb, function(x) gUnaryUnion(x, row.names(x)))


  if (length(bb) > 1) {
    for (i in 2:length(bb)) {
      bb[[i]] <- gUnion(bb[[i]], bb[[i-1]], id=row.names(bb[[i]]))
    }
    b <- do.call("rbind", bb)
  } else {
    b <- bb[[1]]
  }

  
  attribute.data <- data.frame(level=levels[as.numeric(row.names(b))+1], area=sapply(slot(b, "polygons"), function(x) slot(x, "area")))
  row.names(attribute.data) <- row.names(b)

  
  
  b <- SpatialPolygonsDataFrame(b, data=attribute.data)
 # slot(b, "polygons") <- lapply(slot(b, "polygons"), checkPolygonsHoles)
 # b <- unionSpatialPolygons(b, as.character(b$level))


  proj4string(b) <- projString

  if (!ud & !cud) {
    out <- rhrSetIso(out, b[b$level %in% levels.o,])
    return(out)
  }

  r1 <- rasterFromXYVect(xy, xrange=xrange, yrange=yrange, res=res)
  cud <- rasterize(b, r1, field="level", fun="min") / 100
  ud <- (1 - cud) / sum(1 - cud[], na.rm=T)

  # locoh at levls
  if (!any(b$level %in% levels.o)) {
    levels.o <- b$level[which.min(abs(b$level - levels.o))]
  }

  locoh <- b[b$level %in% levels.o,]

  out <- rhrSetIso(out, locoh)
  out <- rhrSetCUD(out, cud)
  out <- rhrSetUD(out, ud)

  return(out)
}

