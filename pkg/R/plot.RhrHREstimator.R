#' plot for RhrHREstimator
#' 
#' generic plot for RhrHREstimator
#' @param x RhrHREstimator object
#' @param what indicates what should be printed. This can be either \code{ud} or \code{iso}.
#' @param draw indicates whether the plot should be drawn or not. If this is  \code{FALSE} a grob is returned.
#' @param useGE indicates wether or not google earth satellite images should be use as background
#' @param ... none implemented
#' @method plot RhrHREstimator
#' @export

plot.RhrHREstimator <- function(x, what=c("ud", "iso"), draw=TRUE, useGE=FALSE, ...) {
  ## plot only ud

  what <- tolower(what)

  doIso <- FALSE
  doUD <- FALSE
  
  ## What do I have to do?
  if ("ud" %in% what) {
    if (hasUD(x)) {
      doUD <- TRUE
    } else {
      warning("Requested to plot UD, but provided estimator does not have an UD")
    }
  }

  if ("iso" %in% what | "isopleths" %in% what) {
    if (hasIsopleths(x)) {
      doIso <- TRUE
    } else {
      warning("Requested to plot isopleths, but provided estimator does not have isopleths")
    }
  }

  ## Should I use GE
  if (useGE) {
    if(!ifelse(is(try(getURL("www.google.com")), "try-error"), FALSE, TRUE)) {
      ## GE requested, but not connectivity to the internet
      warning("Requested to use GE background tiles, but no connection to the internet is available. Request will be ignored")
      useGE <- FALSE
    }
    if (is.na(x$parameters$proj4string)) {
      ## GE requestied but no SRS
      stop("proj4string is NA, but SRS is necessary for using GE. Currently GE only works, if a the input data for rhrEstimator inherits from SpatialPoints with an set SRS.")
      useGE <- FALSE
    }
  }

  ## none requested
  if (!doIso & !doUD) {
    stop("Neither isopleths nor UD provided, there is nothing to plot")
  }
  
  if (doUD) {
    p <- rasterToPoints(ud(x))
    df <- data.frame(p)
    names(df) <- c("x", "y", "ud")

    if (useGE) {
      warning("GE for UD is not yet implemented")
      pUD <- ggplot(data=df) +
        geom_tile(aes(x=x, y=y, fill=ud)) +
          coord_equal() + scale_x_continuous(expand=c(0,0)) +
            scale_fill_gradient(low="darkgreen", high="white") + 
              scale_y_continuous(expand=c(0,0)) + labs(x=NULL, y=NULL) + theme_bw() +
                coord_fixed()
    } else {
      pUD <- ggplot(data=df) +
        geom_tile(aes(x=x, y=y, fill=ud)) +
          coord_equal() + scale_x_continuous(expand=c(0,0)) +
            scale_fill_gradient(low="darkgreen", high="white") + 
              scale_y_continuous(expand=c(0,0)) + labs(x=NULL, y=NULL) + theme_bw() +
                coord_fixed()
    }
  }


  ## plot only isopleths
  if (doIso) {
    if (useGE) {
      tempol <- isopleths(x)
      tempol <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
      tempol@data$id <- rownames(tempol@data)
      tempolPoints <- fortify(tempol, region="id")
      tempolDF <- plyr::join(tempolPoints, tempol@data, by="id")


      bb <- bbox(tempol)
      lon <- bb[1,]
      lat <- bb[2,]

      lon2x <- function(lon) lon * pi/180 * 6378137
      lat2y <- function(lat) log(tan(lat * (pi/180)/2 + pi/4)) * 6378137 

      x <- lon2x(extendrange(lon, f=0.03))
      y <- lat2y(extendrange(lat, f=0.03))


      d <- sqrt(diff(x)^2 + diff(y)^2)
      levels <- 1:21
      ## http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
      zooms <- 40075000/256 * cos(0) / 2^levels * 256 
      l <- which(abs(zooms - d) == min(abs(zooms - d))) + 1

      pIso <- ggmap(get_googlemap(center=c(mean(lon), mean(lat)), zoom=l, maptype="hybrid")) + 
        geom_path(size=3, alpha=0.4, data=tempolDF, aes(x=long, y=lat, group=group, color=factor(level))) +
          labs(colour="Level", x="lon", y="lat") +
            geom_path(size=0.2, colour="black", data=tempolDF, aes(x=long, y=lat, group=group)) +
              scale_color_manual(values=terrain.colors(10)) +
                theme_bw() + coord_fixed()

    } else {
      ## fortify poly
      tempol <- isopleths(x)
      tempol@data$id <- rownames(tempol@data)
      tempolPoints <- try(fortify(tempol, region="id"))
      tempolDF <- plyr::join(tempolPoints, tempol@data, by="id")

      pIso <- ggplot(tempolDF, aes(x=long, y=lat, group=group, color=factor(level))) + 
        geom_point(data=x$dat, aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) +
          geom_path(size=3, alpha=0.4) + 
            labs(colour=paste0("Levels of ", x$parameters$estimator), x="lon", y="lat") +
              geom_path(size=0.2, colour="black") +
                scale_color_manual(values=terrain.colors(10)) + theme_bw() +
                  coord_fixed()
    }
  }

  ## Return
  if (draw) {
    if (doUD & doIso) {

### arrangeGrob, still opens plot window, shouldnt do that
      p <- arrangeGrob(pIso, pUD)
      return(print(p))

    } else if (doUD) {
      return(print(pUD))
    } else {
      return(print(pIso))
    }
  } else {
    if (doUD & doIso) {

      p <- arrangeGrob(pISO, pUD)
      dev.off()
      return(p)

    } else if (doUD) {
      return(pUD)
    } else {
      return(pIso)
    }
  } 
}
