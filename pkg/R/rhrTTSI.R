#' rhrTTSI
#'
#' This a wrapper around \code{rhrSchoener} to calculate time to statistical indpendence from a series of intervals.
#'
#' @param dat data.frame, with three columns: x and y coordinates and a timestamp in seconds.
#' @param interval numeric value, the initial interval considered
#' @param ntimes numeric value, the number of times the critical value needs to be exceeded in order to reach independence.
#' @param ... further arguments passed to \code{rhrSchoener}.
#' @return \code{vector} vector of length three. V is Schoeners v, n the number of points and m the number of point pairs considered to calculated t2.
#' @export
#' @author Johannes Signer 
#' @references Swihart, R. and Slade N. 1985, Testing for indpendence of observations in animal movement, Ecology, 66(4), 1176 - 1184
#' @examples
#' \dontrun{
#' library(rhr)
#' data(datSH)
#' dat <- data.frame(datSH[, 2:3], timestamp=as.numeric(ymd(datSH$day) + hms(datSH$time)))
#' ttsi <- rhrTTSI(dat, interval=5*60*50)
#' plot(ttsi)
#' }

rhrTTSI <- function(dat, interval, ntimes=3, ...) {

  names(dat)[1:3] <- c("lon", "lat", "timestamp")

  ## get difference between first and last relocation
  totalDiff <- diff(range(as.numeric(dat$timestamp)))  

  ## create seq of from 0 total Diff
  ints <- seq(interval, totalDiff, interval)  

  if (any(!complete.cases(dat))) {
    dat <- dat[complete.cases(dat), ]
    warning("In rhrSchoener: removed NA")
  }

  if (any(duplicated(dat[,3]))) {
    dat <- dat[!duplicated(dat[,3]), ]
    warning("In rhrSchoener: removed duplicates")
  }

  nTimesAboveCriticalValue <- 0  # keeps track of the number of times the critical value was reached
  cvReached                <- FALSE  # keeps track if the critical value was reached
  cvReachedAt              <- NA
  enoughM                  <- TRUE  # keeps track if enough pairs are available
  resDiff                  <- list() # list to store temporal results

      for (i in seq_along(ints)) {
        resDiff[[i]] <- rhrSchoener(dat[, c('lon', 'lat', 'timestamp')], interval=ints[i], ...)

        ## V is in NA, this means that the number of pairs, M, is fallen below the critical threshold
        ## minM in rhrSchoener
        
        if (is.na(resDiff[[i]]['V'])) {
          break
        }

        ## Check if V is above critical value
        if (resDiff[[i]]['V'] >= resDiff[[i]]['cv']) {
          nTimesAboveCriticalValue <- nTimesAboveCriticalValue + 1
        } else {
          nTimesAboveCriticalValue <- 0
        }

        if (nTimesAboveCriticalValue >= ntimes | is.na(resDiff[[i]]['V'])) {
          cvReached <- TRUE
          cvReachedAt <- ints[i]
          break
        }

     }

 
  a <- data.frame(do.call("rbind", resDiff))
  a <- a[complete.cases(a), ]
  a <- list(dat=a, interval=ints, cvReached=cvReached, cvReachedAt=cvReachedAt)
  class(a) <- "RhrTTSI"
  return(a)

}

#' plot for RhrTTSI
#' 
#' generic plot for RhrTTSI
#' @param x RhrHREstimator object
#' @param ... none implemented
#' @method plot RhrTTSI
#' @export

plot.RhrTTSI <- function(x, ...) {

  v        <- x$dat[, 'V']
  m        <- x$dat[, 'm']
  n        <- x$dat[1, 'n']  # the actual number of points
  cv       <- x$dat[, 'cv']  # confint
  interval <- x$dat[, 'interval'] # time interval
  

  ## init plot
  grid.newpage()
  pushViewport(viewport(x=0.5, y=0.5, width=0.9, height=0.9))

  ## header
  pushViewport(viewport(x=0.0, y=0.9, width=1, height=0.1, just=c("left", "bottom")))
  grid.text("Time to statistical independence")
  popViewport()

  ## first graph
  pushViewport(viewport(x=0.0, y=0.3, width=1, height=0.6, just=c("left", "bottom")))
  pushViewport(plotViewport(c(0.5,3,1,1)))
  pushViewport(dataViewport(c(1, length(m)), c(range(v, na.rm=TRUE), 2))) # plotting region
  grid.yaxis(gp=gpar(cex=0.8))
  grid.text("Schoeners V",x=unit(-3,"lines"), rot=90)
  grid.lines(1:length(m), v, default.units="native")

  ## critical values
  grid.lines(1:length(m), cv, default.units="native", gp=gpar(col="grey", lwd=2))
  if (x$cvReached) {
    grid.lines(c(1,length(m)), c(2,2), default.units="native", gp=gpar(col="red", lty=2))

    ## line where cv passed
    ## At which interval was the cv passed
    cvReachedAtInt <- which(interval == x$cvReachedAt)
    grid.points(cvReachedAtInt, 2, default.units="native", gp=gpar(col="red", pch=2))
  }
  popViewport(3)

  # second graph
  pushViewport(viewport(x=0.0, y=0.0, width=1, height=0.3, just=c("left", "bottom")))
  pushViewport(plotViewport(c(3,3,0.5,1)))
  pushViewport(dataViewport(c(1, length(m)), range(c(m, n, na.rm=TRUE), na.rm=TRUE))) 
  grid.yaxis(gp=gpar(cex=0.8))
  prettyAt <- pretty(1:length(m), n=7)[-c(1, length(pretty(1:length(m), n=7)))]
  grid.xaxis(gp=gpar(cex=0.8), at=prettyAt, label=interval[prettyAt])
  grid.text("Time interval [seconds]",y=unit(-3,"lines"))
  grid.text("m",x=unit(-3,"lines"),rot=90)
  for (i in seq(m)) grid.lines(rep(i, 2), c(0, m[i]), default.units="native")
  # grid.lines(c(1,length(m)), c(n,n), default.units="native", gp=gpar(col="red", lty=2))

  popViewport(3)

}

#' print object of rhrTTSI
#' 
#' @param x RhrTTSI
#' @param ... none implemented
#' @method print RhrTTSI
#' @export

print.RhrTTSI <- function(x, ...) {

  cat(paste0("class           : ", class(x)),
      paste0("TTSI reached    : ", paste0(x$cvReached, collapse=",")),
      paste0("TTSI reached at : ", paste0(x$cvReachedAt, collapse=",")),
      sep="\n")

}
