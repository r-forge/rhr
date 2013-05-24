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
#' @export

h1 <- function(x) {
  cat(paste0("<h1>", x, "</h1>"))
}

#' h2
#' 
#' Wraps a string as a h2 heading
#' @param x a txt string
#' @export
h2 <- function(x) {
  cat(paste0("<h2>", x, "</h2>"))
}

#' h3
#' 
#' Wraps a string as a h3 heading
#' @param x a txt string
#' @export

h3 <- function(x) {
  cat(paste0("<h3>", x, "</h3>"))
}

#' h4
#' 
#' Wraps a string as a h4 heading
#' @param x a txt string
#' @export

h4 <- function(x) {
  paste0("<h4>", x, "</h4>")
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


# chtml <- function(header="From R", bd="Test", template=NULL, outfile="") {
#   if (is.null(template) | !file.exists(template)) {
#     stop("no template found")
#   }
# 
#   bdtmp <- readChar(template, file.info(template)$size)
#   bdtmp <- gsub("<!--header-->", x=bdtmp, replacement=header)
#   bdtmp <- sub("<!--body-->", x=bdtmp, replacement=bd)
#   cat(bdtmp, file=outfile)
# }

# accordion <- function(el, id="accor01") {
#   # el - is a list - each intery is an other list which is 1 accordion
#   #  each entry need to have a heading and a body
#   out <- paste0('<div class="accordion" id="', id, '">')
# 
#   tmp <- lapply(el, function(x) paste0('<div class="accordion-group">
#   <div class="accordion-heading">
#   <a class="accordion-toggle" data-toggle="collapse" data-parent="#', id, '" href="#', gsub(" ", "", x[[1]]), '">', x[[1]], 
#   '</a> </div>
#   <div id="', gsub(" ", "", x[[1]]), '" class="accordion-body collapse">
#   <div class="accordion-inner">', x[[2]], ' </div> </div> </div>'))
# 
#   tmp <- do.call("c", tmp)
#   out <- paste0(out, tmp, collapse="\n")
# 
#   out <- paste0(out, "</div>", collapse="\n")
#   cat(out)
# }

# issue alert
#' alert
#'
#' wraps string as bootstrap alert
#' @param x a string
#' @param cat whether it should be printed to sto or not
#' @export
alert <- function(x, cat=TRUE) {
  out <- paste0("<div class='alert alert-info'>", x, "</div>")
  if (cat) {
    return(cat(out))
  } else {
    return(out)
  }
}

#' alertWarning
#'
#' wraps string as bootstrap alert-warning
#' @param x a string
#' @param cat whether it should be printed to sto or not
#' @export
alertWarning <- function(x, cat=TRUE) {
  out <- paste0("<div class='alert alert-warning'>", x, "</div>")
  if (cat) {
    return(cat(out))
  } else {
    return(out)
  }
}

#' alertSuccess
#'
#' wraps string as bootstrap alert-success
#' @param x a string
#' @param cat whether it should be printed to sto or not
#' @export
alertWarning <- function(x, cat=TRUE) {
  out <- paste0("<div class='alert alert-success'>", x, "</div>")
  if (cat) {
    return(cat(out))
  } else {
    return(out)
  }
}

#' alertError
#'
#' wraps string as bootstrap alert-error
#' @param x a string
#' @param cat whether it should be printed to sto or not
#' @export
alertError <- function(x, cat=TRUE) {
  out <- paste0("<div class='alert alert-error'>", x, "</div>")
  if (cat) {
    return(cat(out))
  } else {
    return(out)
  }
}

#' plot ttsi
#'
#' plots results of rhrSchoener
#' @param res from ttsi
#' @param path where the plot should be safed
#' @param toFirst2 only plot until critical value of 2 is first reached
#' @export

plotTTSI <- function(res, path=NULL, toFirst2=FALSE) {


  if (toFirst2 && max(res[, 'V'], na.rm=TRUE) >= 2) {
    res <- res[1:which(res[, 'V'] >= 2)[1],]
  }

  v <- res[, 'V']
  m <- res[, 'm']
  n <- res[1, 'n']  # the actual number of points

  if (!is.null(path)) png(path)

  # init plot
  pushViewport(viewport(x=0.5, y=0.5, width=0.9, height=0.9))

  # header
  pushViewport(viewport(x=0.0, y=0.9, width=1, height=0.1, just=c("left", "bottom")))
  grid.text("Time to statistical independence")
  popViewport()

  # first graph
  pushViewport(viewport(x=0.0, y=0.3, width=1, height=0.6, just=c("left", "bottom")))
  pushViewport(plotViewport(c(0.5,3,1,1)))
  pushViewport(dataViewport(c(1, length(m)), range(v, na.rm=TRUE))) # plotting region
  grid.yaxis(gp=gpar(cex=0.8))
  grid.text("Schoeners V",x=unit(-3,"lines"),rot=90)
  grid.lines(1:length(m), v, default.units="native")
  if (max(v, na.rm=T) >= 1.9) grid.lines(c(1,length(m)), c(2,2), default.units="native", gp=gpar(col="red", lty=2))
  popViewport(3)

  # second graph
  pushViewport(viewport(x=0.0, y=0.0, width=1, height=0.3, just=c("left", "bottom")))
  pushViewport(plotViewport(c(3,3,0.5,1)))
  pushViewport(dataViewport(c(1, length(m)), range(c(m, n, na.rm=TRUE), na.rm=TRUE))) 
  grid.yaxis(gp=gpar(cex=0.8))
  grid.xaxis(gp=gpar(cex=0.8))
  grid.text("Time interval [seconds]",y=unit(-3,"lines"))
  grid.text("m",x=unit(-3,"lines"),rot=90)
  for (i in seq(m)) grid.lines(rep(i, 2), c(0, m[i]), default.units="native")
  # grid.lines(c(1,length(m)), c(n,n), default.units="native", gp=gpar(col="red", lty=2))

  popViewport(3)

  if (!is.null(path)) dev.off()

}

# ============================================================================ #
# Function to validate and correct levels:
# Arguments:
#  str -  a string of the form ",,12,,2,2,3,1,,2,22,,,,123,,2"
#  low - lower bound, usually 1
#  upper - lower bound, usually 100

#' rhrCorrectLevels
#' 
#' correct Levels at which a home range should be calculated
#' @param str A string with that contains the level, seperated by comas. If there are no suitable levels, then the default.level is used.
#' @param low lower bound for levels
#' @param upper upperbound for levels
#' @param default.level is the default level
#' @export

rhrCorrectLevels <- function(str, low=1, upper=100, default.level=95) {

  # are there any comas?
  if (grepl(",", str)) {
    # Split the string
    s <- strsplit(str, ",")[[1]]

    # remove empty ones
    s <- s[nchar(s) > 0]
    s <- as.numeric(s)

    # ensure range
    s <- s[s >= low & s <= upper]

    if (length(s) == 0) {
      s <- default.level
    }

    # order and remove duplicates
    s <- s[order(s)]
    s <- unique(s)
  } else { 
    str <- as.numeric(str)
    if (str >= low & str <= upper) {
      s <- str # retrun str, if its between acceptable levels
    } else {
      s <- default.level # return default level
    }  
  }
  return(s)
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
#' @export

dfGrob <- function(x, start=0.03, stop=0.97, digits=2) {
  xs <- seq(start, stop, length.out=(ncol(x)+1))
  xs <- xs[-length(xs)]
  l <- list()
  l[[length(l) + 1]] <- textGrob(names(x), x=xs, y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), gp=gpar(fontface="bold"))
  l[[length(l) + 1]] <- linesGrob(x=c(start, stop), y=unit(1, "npc") - unit(1.25, "lines"))

  for (i in 1:ncol(x)) if (is.numeric(x[1,i])) x[,i] <- round(x[,i], digits)

  for (i in 1:nrow(x))
    l[[length(l) + 1]] <- textGrob(x[i,], x=xs, y=unit(1, "npc") - unit(i + 1.5, "lines"), just=c("left", "bottom"))
  return(gTree(children=do.call("gList", l)))
}

#' rhrConvertUnit
#' 
#' Converts areal lunits
#' @param x a vector
#' @param inUnit input unit
#' @param outUnit output unit
#' @export

rhrConvertUnit <- function(x, inUnit="m", outUnit="ha") {
  if (inUnit == "sqinput") {
    return(x)
  } else if (inUnit == "m") {
    if (outUnit == "sqm") {
      return(x / 10e4)
    } else if (outUnit == "ha") {
      return(x / 10e4)
    } else if (outUnit == "sqkm") {
      return(x / 10e6)
    } else if (outUnit == "acres") {
      return(x / 4046.85642)
    } else if (outUnit == "sqft") {
      return(x / 10.76391041671)
    } else if (outUnit == "sqyd") {
      return(x / 1.1959900463011)
    } else if (outUnit == "sqmi") {
      return(x / 0.00000038610215854781)
    }
  } else if (inUnit == "km") {
    if (outUnit == "sqm") {
      return(x / 0.000001)
    } else if (outUnit == "ha") {
      return(x /0.01)
    } else if (outUnit == "sqkm") {
      return(x)
    } else if (outUnit == "acres") {
      return(x / 0.0040468564224)
    } else if (outUnit == "sqft") {
      return(x /0.00000009290304)
    } else if (outUnit == "sqyd") {
      return(x /0.00000083612736)
    } else if (outUnit == "sqmi") {
      return(x /2.5899881103)
    }
  } else if (inUnit == "ft") {
    if (outUnit == "sqm") {
      return(x / 10.76391041671)
    } else if (outUnit == "ha") {
      return(x /107639.1041671 )
    } else if (outUnit == "sqkm") {
      return(x / 10763910.41671 )
    } else if (outUnit == "acres") {
      return(x /43560 )
    } else if (outUnit == "sqft") {
      return(x /1)
    } else if (outUnit == "sqyd") {
      return(x /9)
    } else if (outUnit == "sqmi") {
      return(x / 27878399.999612)
    }
  } else if (inUnit == "yd") {
    if (outUnit == "sqm") {
      return(x /1.1959900463011 )
    } else if (outUnit == "ha") {
      return(x / 11959.900463011)
    } else if (outUnit == "sqkm") {
      return(x /1195990.0463011  )
    } else if (outUnit == "acres") {
      return(x /4840 )
    } else if (outUnit == "sqft") {
      return(x /0.11111111111111 )
    } else if (outUnit == "sqyd") {
      return(x / 1)
    } else if (outUnit == "sqmi") {
      return(x / 3097599.9999569)
    }
  } else if (inUnit == "mi") {
    if (outUnit == "sqm") {
      return(x /0.00000038610215854781)
    } else if (outUnit == "ha") {
      return(x /0.0038610215854781)
    } else if (outUnit == "sqkm") {
      return(x / 0.38610215854781)
    } else if (outUnit == "acres") {
      return(x / 0.0015625000000217)
    } else if (outUnit == "sqft") {
      return(x /0.000000035870064279654)
    } else if (outUnit == "sqyd") {
      return(x / 0.00000032283057851688)
    } else if (outUnit == "sqmi") {
      return(x /1)
    }
  }
}
