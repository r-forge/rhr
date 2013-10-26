#' printSpRestriction
#' 
#' helper function for the gui
#' @param datrm original dataset
#' @param datsub subsetted dataset
#' @param latmin min y
#' @param latmax max y
#' @param lonmin min x
#' @param lonmax max x 
#' @export

printSpRestriction <- function(datrm, datsub, latmin, latmax, lonmin, lonmax) {
  cat('<table class="table table-striped">
  <tr>
  <th>Data set</th>
  <th>min longitude</th>
  <th>max longitude</th>
  <th>min latitude</th>
  <th>max latitude</th>
  <th>n</th>
  </tr>')


  # all ids
  cat(c('<tr class="info">', '<td>Reference</td>', paste0('<td>', c(min(datrm$lon), max(datrm$lon), min(datrm$lat), max(datrm$lat)), '</td>'), '<td>', nrow(datrm) ,'</td>', '</tr>'))
  cat(c('<tr class="success">', '<td>Current</td>', paste0('<td><input class="input-small" id="', c('lonmin', 'lonmax', 'latmin', 'latmax'), '" value="', c(lonmin, lonmax, latmin, latmax), '"></td>'), '<td>', nrow(datsub) ,'</td>', '</tr>'))
   

  cat('</table>')
}

########################################################
## Temporal subset

#' printTemporalRestriction
#' 
#' helper function for the gui
#' @param datrm original dataset
#' @param datsub subsetted dataset
#' @param tmin min time
#' @param tmax max time
#' @export

printTemporalRestriction <- function(datrm, datsub, tmin, tmax) {

        cat('<table class="table table-striped">
        <tr>
        <th>Data set</th>
        <th>start start</th>
        <th>end timestamp</th>
	<th>n</th>
        </tr>')


  cat(c('<tr class="info">', '<td>Reference</td>', paste0('<td>', c(min(datrm$timestamp), max(datrm$timestamp)), '</td>'), '<td>', nrow(datrm) ,'</td>', '</tr>'))

  cat(c('<tr class="success">', '<td>Current</td>', paste0('<td><input class="input-medium" id="', c('timemin', 'timemax'), '" value="', c(tmin, tmax), '" ></td>'), '<td>', nrow(datsub) ,'</td>', '</tr>'))
  
cat('</table>')
}

# ============================================================================ #
# Functions to report
# ============================================================================ #

# ---------------------------------------------------------------------------- # 
# export to html

#' h1
#' 
#' Wraps a string as a h1 heading
#' @param x a txt string
#' @author Johannes Signer
#' @export
#' @examples
#' h1("Title 1")

h1 <- function(x) {
  cat(paste0("<h1>", x, "</h1>"))
}

#' h2
#' 
#' Wraps a string as a h2 heading
#' @param x a string
#' @export
#' @examples
#' h2("Title 2")

h2 <- function(x) {
  cat(paste0("<h2>", x, "</h2>"))
}

#' h3
#' 
#' Wraps a string as a h3 heading
#' @param x a txt string
#' @export
#' @examples
#' h3("Title 3")

h3 <- function(x) {
  cat(paste0("<h3>", x, "</h3>"))
}

#' h4
#' 
#' Wraps a string as a h4 heading
#' @param x a txt string
#' @export
#' @examples
#' h4("Title 4")

h4 <- function(x) {
  cat(paste0("<h4>", x, "</h4>"))
}

#' img
#' 
#' exports an img with caption
#' @param address path to the image
#' @param cap caption of the image
#' @export

img <- function(address, cap="") {
  cat(paste0("<img src='", address, "'>"))
}

# imgs <- function(address, cap="", cat=TRUE) {
#   out <- ""
#   out <- paste0(out, paste0("<table border='0' align='left', width='100%''>"), collapse="\n")
#   out <- paste0(out, paste0("<tr><td><img src='", address, "'></td><td>", cap, "</td></tr>"), collapse="\n")
#   out <- paste0(out, paste0("</table><br>"), collapse="\n")
#   if (cat) {
#     return(cat(out))
#   } else {
#     return(out)
#   }
# }

#' p
#'
#' Wraps a string as a paragraph
#' @param x a string
#' @param ... additional arguments, none implemented
#' @export

p <- function(x, ...) {
  cat(paste0("<p>", x, "</p>"))
}

#' code
#'
#' Wraps a string as inline code
#' @param x a string
#' @param ... additional arguments, none implemented
#' @export
#' @examples
#' code("cat('foo')")

code <- function(x) {
  paste0("<pre>", x, "</pre>")
}

#' toHTML
#'
#' converts an R object to a data.frame

#' @title toHTML: convert R objects as html
#' @param x a R object
#' @param cap caption
#' @param cat warp output with cat
#' @param ... additional arguments
#' @rdname toHTML
#' @export toHTML

toHTML <- function(x, ...) {
  UseMethod("toHTML", x)
}


#' @return \code{NULL}
#'
#' @rdname toHTML
#' @method toHTML data.frame
#' @S3method toHTML data.frame

toHTML.data.frame <- function(x, cat=TRUE, ...) {
  out <- "<table class='table table-striped'><tr>"
  h <- paste0("<th>", names(x), "</th>", collapse="")
  out <- paste0(out, h, "</tr>")
  # body
  rows <- apply(x, 1, function(dat) paste0("<td>", dat, "</td>", collapse=""))
  rows <- paste0("<tr>", rows, "</tr>", collapse="\n")
  out <- paste0(out, rows, "</table>")

  if (cat) {
    return(cat(out))
  } else {
    return(out)
  }

}


#' @return \code{NULL}
#'
#' @rdname toHTML
#' @method toHTML summaryDefault
#' @S3method toHTML summaryDefault

toHTML.summaryDefault <- function(x, cap="", ...) {
  out <- paste0("<table class='table table-striped'><caption align='bottom'>", cap, "</caption><tr><th>Min</th><th>1st Qu</th><th>Median</th><th>Mean</th><th>3rd Qu</th><th>Max</th></tr><tr>")
  cat(paste0(out, paste0("<td>", x, "</td>", collapse=""), "</tr>", "</table>"))
}

#' @return \code{NULL}
#'
#' @rdname toHTML
#' @method toHTML htest
#' @S3method toHTML htest

toHTML.htest <- function(x, cap=NULL, ...) {
  if (is.null(cap)) {
    cap <- paste0("Table: ", x$method, ": ", x$data.name) 
  }
  v <- c(x$statistic, x$parameter, x$p.value, x$alternative)
  out <- paste0("<table class='table table-striped'><caption align='bottom'>", cap, "</caption><tr><th>Test statistic</th><th>df</th><th>p value</th><th>Alternative hypothesis</th></tr><tr>")
  cat(paste0(out, paste0("<td>", v, "</td>", collapse=""), "</tr>", "</table>"))
}



# ============================================================================ #
#' strShorten
#' 
#' Shortens a string
#' @param str a string
#' @param max.nchar maximum number of chars in the return string
#' @param cont ending of the shortend string
#' @export

strShorten <- function(str, max.nchar=20, cont="...") {
  if (nchar(str) > max.nchar) {
    n.cont <- nchar(cont)
    a <- substr(str, 1, max.nchar - n.cont)
    return(paste0(a, cont, collapse=""))
  } else {
    return(str)
  }
}


# ============================================================================ #
# create gTree for summary

#' summaryGrob
#' 
#' Create a grid representation for an object of class summary
#' @param x a summary
#' @export

summaryGrob <- function(x) {
  h <- textGrob(c("min", "1st quant.", "meadian", "mean", "3rd quant", "max"), x=c(0.03, .19, .35, .51, .67, .83), y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), gp=gpar(fontface="bold"))
  l <- linesGrob(x=c(0.03, 0.97), y=unit(1, "npc") - unit(1.25, "lines"))
  v <- textGrob(x[1:6], x=c(0.03, .19, .35, .51, .67, .83), y=unit(1, "npc") - unit(2.5, "lines"), just=c("left", "bottom"))
  return(gTree(children=gList(h,l,v)))
}

#' ttestGrob
#' 
#' Create a grid representation for an object of class htest
#' @param x a htest
#' @export
#' @author Johannes Signer
#' @examples
#' grid.newpage()
#' pushViewport(viewport())
#' grid.draw(ttestGrob(t.test(1:10, mu=5)))
#' popViewport()

ttestGrob <- function(x) {
  h <- textGrob(c("test statistic", "df", "p-value", "Alternative"), x=c(0.03, .28, .53, .78), y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), gp=gpar(fontface="bold"))
  l <- linesGrob(x=c(0.03, 0.97), y=unit(1, "npc") - unit(1.25, "lines"))
  v <- textGrob(c(round(x$statistic, 3), x$parameter, ifelse(round(x$p.value, 3) == 0, " < 0.001", round(x$p.value, 3)), x$alternative), x=c(0.03, .28, .53, .78), y=unit(1, "npc") - unit(2.5, "lines"), just=c("left", "bottom"))
  return(gTree(children=gList(h,l,v)))
}

#' dfGrob
#' 
#' Create a grid representation for an object of class data.frame
#' @param x a data.frame
#' @param start where to start, default is 0.03
#' @param stop where to stop, default is 0.97
#' @param digits to how many digists numbers are rounded
#' @param ... additional arguments passed to gp
#' @export
#' @author Johannes Signer
#' @examples
#' grid.newpage()
#' pushViewport(viewport())
#' grid.draw(dfGrob(data.frame(id=1:10, class=letters[1:10], stringsAsFactors=FALSE)))
#' popViewport()

dfGrob <- function(x, start=0.03, stop=0.97, digits=2, bodyFont="") {
  xs <- seq(start, stop, length.out=(ncol(x)+1))
  xs <- xs[-length(xs)]
  l <- list()
  l[[length(l) + 1]] <- textGrob(names(x), x=xs, y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), gp=gpar(fontface="bold"))
  l[[length(l) + 1]] <- linesGrob(x=c(start, stop), y=unit(1, "npc") - unit(1.25, "lines"))

  for (i in 1:ncol(x)) if (is.numeric(x[1,i])) x[,i] <- round(x[,i], digits)

  for (i in 1:nrow(x))
    l[[length(l) + 1]] <- textGrob(label=as.character(x[i,]), x=xs, y=unit(1, "npc") - unit(i + 1.5, "lines"), just=c("left", "bottom"),
                                   gp=gpar(fontfamily=bodyFont))

  return(gTree(children=do.call("gList", l)))
}

#' rasterFromXYVect
#' 
#' Creates empty raster
#' @param xy a data.frame or matrix. The first column are x coordinates and the second column are y coordinates
#' @param xrange range of x
#' @param yrange range of y
#' @param res resolution

rasterFromXYVect <- function(xy, xrange=NA, yrange=NA, res=100) {

  if (any(is.na(xrange)) | length(xrange) != 2) {
    xrange <- c(min(xy[,1]), max(xy[,1]))
    warning("retrieved x-range from data")
  } 
  if (any(is.na(yrange)) | length(yrange) != 2) {
    yrange <- c(min(xy[,2]), max(xy[,2]))
    warning("retrieved y-range from data")
  }

  ## determine gridsize
  ncolumns <- ceiling(diff(xrange) / res)
  nrows <- ceiling(diff(yrange) / res)

  return(raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2],
                nrows=nrows, ncols=ncolumns))
}

#' rhrHREstimator
#' 
#' Constructor for RhrHREstimator
#' @param dat used data
#' @param call how the fun was called
#' @param params parameters that where passed
#' @param ud whether or not an UD is desired
#' @param cud whether or not a CUD is desired

rhrHREstimator <- function(dat, call, params, ud, cud) {

  dat <- dat[, 1:2]
  names(dat)[1:2] <- c("lon", "lat")

  l <- list(call=call, dat=dat, parameters=params, results=list(ud=ifelse(ud, 0, NA), cud=ifelse(cud, 0, NA)))
  class(l) <- "RhrHREstimator"
  return(l)
}


#' print for RhrHREstimator
#' 
#' generic print for RhrHREstimator
#' @param x RhrHREstimator object
#' @param ... ignored
#' @method print RhrHREstimator
#' @export

print.RhrHREstimator <- function(x, ...) {

  cat(paste0("class       : ", class(x)),
      paste0("estimator   : ", x$parameters$name),
      paste0("call        : ", deparse(x$call)),
      paste0("n points    : ", nrow(x$dat)),
      paste0("ud          : ", x$parameters$ud),
      paste0("cud         : ", x$parameters$cud),
      sep="\n")
}


#' Generic Function to set UD
#' 
#' @param x an object of class rhrHREstimator
#' @param ud the ud, object of class raster
#' @param ... further arguments, none implemented
#' @export
#' @return an object of class rhrHREstimator with ud

rhrSetUD <- function(x, ud, ...) {
  UseMethod("rhrSetUD", x)
}

#' Set the UD
#' 
#' @param x an object of class rhrHREstimator
#' @param ud the ud, object of class raster
#' @param ... further arguments, none implemented
#' @export
#' @method rhrSetUD RhrHREstimator

rhrSetUD.RhrHREstimator <- function(x, ud, ...) {

  if (!is(ud, "RasterLayer")) {
    stop("ud is no raster")
  }

  x$results$ud <- ud
  return(x)

}

#' Generic Function to set CUD
#' 
#' @param x an object of class rhrHREstimator
#' @param cud the ud, object of class raster
#' @param ... further arguments, none implemented
#' @export
#' @return an object of class rhrHREstimator with a cud

rhrSetCUD <- function(x, cud, ...) {
  UseMethod("rhrSetCUD", x)
}


#' Set the CUD
#' 
#' @param x an object of class rhrHREstimator
#' @param cud the cud, object of class raster
#' @param ... further arguments, none implemented
#' @export
#' @method rhrSetCUD RhrHREstimator

rhrSetCUD.RhrHREstimator <- function(x, cud, ...) {
  if (!is(cud, "RasterLayer")) {
    stop("cud is no raster")
  }
  x$results$cud <- cud
  return(x)
}


#' Set the Set isopleth
#' 
#' @param x an object of class rhrHREstimator
#' @param iso the isoplethes
#' @param ... further arguments, none implemented
#' @export

rhrSetIso <- function(x, iso, ...) {
  UseMethod("rhrSetIso", x)
}


#' Set isopleth
#' 
#' @param x an object of class rhrHREstimator
#' @param iso the isoplethes
#' @param ... further arguments, none implemented
#' @method rhrSetIso RhrHREstimator

rhrSetIso.RhrHREstimator <- function(x, iso, ...) {
  if (!inherits(iso, "SpatialPolygons")) {
    stop("iso is no object of class SpatialPolygons")
  }
  x$results$isopleths <- iso
  return(x)
}


#### has* methods

#' Checks if an rhrEstimator posses an ud
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @return TRUE/FALSE

hasUD <- function(x) {
  UseMethod("hasUD", x)
}


#' Checks if an rhrEstimator posses a ud
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @method hasUD RhrHREstimator

hasUD.RhrHREstimator <- function(x) {
  if (is(x$results$ud, "RasterLayer")) {
    return(TRUE)
  }
  return(FALSE)
}


#' Checks if an rhrEstimator posses a cud
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @return Returns \code{TRUE} if the \code{rhrHREstimator} has a utilisation distribution, else it returns false.

hasCUD <- function(x) {
  UseMethod("hasUD", x)
}


#' Checks if an rhrEstimator posses a cud
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @method hasCUD RhrHREstimator
#' @return Returns \code{TRUE} if the \code{rhrHREstimator} has a utilisation distribution, else it returns false.

hasCUD.RhrHREstimator <- function(x) {

  if (is(x$results$cud, "RasterLayer")) {
    return(TRUE)
  }
  return(FALSE)

}


#' Checks if an rhrEstimator posses isopleths
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @return TRUE/FALSE

hasIsopleths <- function(x) {
  UseMethod("hasIsopleths", x)
}


#' Check if isopleths are available
#' 
#' @param x an object of class rhrHREstimator
#' @export
#' @method hasIsopleths RhrHREstimator
#' @return Returns \code{TRUE} if the \code{rhrHREstimator} has one or more isopleths, else it returns false.

hasIsopleths.RhrHREstimator <- function(x) {
  if (inherits(x$results$isopleths, "SpatialPolygons")) {
    return(TRUE)
  }
  return(FALSE)

}


### Get methods

#' Retrives the UD of an rhrHREstimator object
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return the UD raster

ud <- function(x, ...) {
  UseMethod("ud", x)
}


#' Get the UD
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @method ud RhrHREstimator

ud.RhrHREstimator <- function(x, ...) {

  if (hasUD(x)) {
    return(x$results$ud)
  }
  return(NA)
}


#' Retrives the CUD of an rhrHREstimator object
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return the UD raster

cud <- function(x, ...) {
  UseMethod("cud", x)
}


#' Retrives the CUD of an rhrHREstimator object
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @method cud RhrHREstimator

cud.RhrHREstimator <- function(x, ...) {

  if (hasCUD(x)) {
    return(x$results$cud)
  }
  return(NA)
}



#' Retrives the isopleths of an rhrHREstimator object
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @return SpatialPolygonsDataFrame

isopleths <- function(x, ...) {
  UseMethod("isopleths", x)
}


#' get the isopleths from rhrHREstimator
#' 
#' @param x an object of class rhrHREstimator
#' @param ... further arguments, none implemented
#' @export
#' @method isopleths RhrHREstimator

isopleths.RhrHREstimator <- function(x, ...) {
    return(x$results$isopleths)
}

### Shamelessly copied from:
### http://stackoverflow.com/questions/3478923/displaying-the-actual-parameter-list-of-the-function-during-execution
### It is also available in the amer package, which has however been removed from CRAN
# expand.call <- function(definition=NULL,
#         call=sys.call(sys.parent(1)),
#         expand.dots = TRUE,
#         doEval=TRUE)
#{
#
#    safeDeparse <- function(expr){
#        #rm line breaks, whitespace             
#        ret <- paste(deparse(expr), collapse="")
#        return(gsub("[[:space:]][[:space:]]+", " ", ret))
#    }
#
#    call <- .Internal(match.call(definition, call, expand.dots))
#
#    #supplied args:
#    ans <- as.list(call)
#    if(doEval) ans[-1] <- lapply(ans[-1], eval)
#
#    #possible args:
#    frmls <- formals(safeDeparse(ans[[1]]))
#    #remove formal args with no presets:
#    frmls <- frmls[!sapply(frmls, is.symbol)]
#
#    add <- which(!(names(frmls) %in% names(ans)))
#    return(as.call(c(ans, frmls[add])))
#}
