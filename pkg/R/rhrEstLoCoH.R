#' Local Convex Hull (LoCoH)
#'
#' @param xy a data frame with 2 columns. The first column contains x-coordiantes, the second one y-coordinates.
#' @param type three types are availabe: i) k-nearest neighbours; ii) neighbours within radius r; iii) a within cum dist
#' @param n if type is k, number of neaerst neibhers, if type is r radius and if type is a cum dist to be used up
#' @param level at which levels should the home range be calculated. Numbers between 1 and 100.
#' @param min.pts min.pts
#' @return object of class \code{SpatialPolygonsDataFrame}
#' @export
#' @author Johannes Signer modified code from \code{adehabitatHR::mcp}
#' @examples
#' cat("hello world")

rhrLoCoH <- function(xy, type="k", n=10, level=95, min.pts=3, ts=NULL) {

  # input checking
  # type:
  if (!type %in% c("a", "k", "r", "t", "tf")) {
    stop("rhrLocoh: incorrect type")
  }

  n <- as.numeric(n)
  if (is.na(n)) {
    stop(paste("rhrLocoh: n should be numeric, not ", n))
  }

  
  # Are levels between 1 and 100, remove duplicated, order and add 0
  level <- level[order(level)]

  if (max(level) > 99 | min(level) < 1 | length(level) != length(unique(level))) {
    level <- level[level < 100 & level > 0]
    level <- unique(level)
    warning(paste0("adjusted level(s) to: ", level))
  }
  level.o <- level
  level <- c(0, level, 100)

  ind <- 1:nrow(xy)
  if (type == "k") {
    # 1. calc dist
    # 2. order by dist
    # 3. take n nearest
    n <- n - 1 # to be consistent with adehabitatHR
    abc1 <- lapply(ind, function(i) ind[order(sqrt((xy[,1] - xy[i,1])^2 + (xy[,2] - xy[i,2])^2))][1:n])
  } else if (type == "t") {
    # 1. time dist
    # 2. order by dist
    # 3. take n nearest
    n <- n - 1 # to be consistent with adehabitatHR
    abc1 <- lapply(ind, function(i) ind[order(sqrt((as.numeric(ts) - as.numeric(ts[i]))^2))][1:n])
  } else if (type == "tf") {
    # 1. time dist
    # 2. order by dist
    # 3. take n nearest
    n <- n - 1 # to be consistent with adehabitatHR
    abc1 <- lapply(ind, function(i) {
      tmp <- as.numeric(ts) - as.numeric(ts[i])
      
      ind[][1:n]})
  
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

  # remove the ones with less than min.pts pts
  abc1 <- abc1[sapply(abc1, length) >= min.pts]
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
  a$isopleth <- cut(a$per, breaks=level)
  a$isopleth.v <- as.numeric(a$isopleth) 

  ### 1. Make a spatial polygon for each isopleth
  ## I have a vector with the ids of polygons for each isopleth, 
  ## For each isopleth merge all polys that fall within this isopleth and the ones already passe (i.e. the ones that are larger)
  #   bb <- lapply(split(a[, c("id", "isopleth.v")], a$isopleth.v), function(x) Polygons(lapply(pol[x$id], Polygon), unique(x$isopleth.v)))
  #   bb <- lapply(bb, function(x) SpatialPolygons(list(x)))
  #   bb <- lapply(bb, function(x) gUnaryUnion(x, row.names(x)))

  bb <- lapply(split(a[, c("id", "isopleth.v")], a$isopleth.v), function(x) Polygons(lapply(pol[x$id], Polygon), unique(x$isopleth.v)))
  bb <- lapply(bb, function(x) SpatialPolygons(list(x)))
  bb <- lapply(bb, function(x) gUnaryUnion(x, row.names(x)))

  for (i in 2:length(bb)) {
    bb[[i]] <- gUnion(bb[[i]], bb[[i-1]], id=row.names(bb[[i]]))
  }

  b <- do.call("rbind", bb)
  attribute.data <- data.frame(level=level[as.numeric(row.names(b))+1], area=sapply(slot(b, "polygons"), function(x) slot(x, "area")))
  row.names(attribute.data) <- row.names(b)
  
   b <- SpatialPolygonsDataFrame(b, data=attribute.data)
   return(b[b$level %in% level.o,])
}

