#' Schoener's ratio
#'
#' @param dat a data.frame with 3 columns. The first column contains x-coordinates, the second column contains y coordinates and the third column contains a timestamp as \code{POSIXct}. 
#' @param interval The interval in seconds
#' @param alpha The alpha value used to calculate the critical value
#' @param minM The minimum number of pairs required, if m is smaller than this argument it will return NA.
#' @param consec indicates whether or not the observations are consecutive
#' @note This implementation uses the normal distribution as a sampling distribution. 
#' @return \code{vector} vector of length three. V is Schoeners v, n the number of points and m the number of point pairs considered to calculated t2.
#' @useDynLib rhr
#' @export
#' @author Johannes Signer 
#' @references Swihart, R. and Slade N. 1985, Testing for indpendence of observations in animal movement, Ecology, 66(4), 1176 - 1184
#' @examples
#' set.seed(123)


rhrSchoener <- function(dat, interval, alpha=0.25, minM=10, consec=TRUE) {


  ## User my not provide fixes in the right order, reorder them
  dat <- dat[order(dat[, 3]), ]

  if (alpha < 0 | alpha > 1) {
    stop("alpha needs to be between 0 and 1")
  }

  if (any(!complete.cases(dat))) {
    dat <- dat[complete.cases(dat), ]
    warning("In rhrSchoener: removed NA")
  }

  if (any(duplicated(dat[,3]))) {
    dat <- dat[!duplicated(dat[,3]), ]
    warning("In rhrSchoener: removed duplicates")
  }

  which <- tsub(dat[,1], dat[,2], as.numeric(dat[,3]), interval)

  dat <- dat[as.logical(which),]
  m <- nrow(dat) - 1

  if (m < minM) {
    warning(paste0("m smaller than ", minM))
    return(c(V=NA, m=m, r2=NA, t2=NA, cv=NA, interval=NA))
  }

  t2 <- 1/m * (sum((dat[1:(nrow(dat) - 1), 1] - dat[2:nrow(dat), 1])^2) +
               sum((dat[1:(nrow(dat) - 1), 2] - dat[2:nrow(dat), 2])^2))
  r2 <- msd(dat[,1], dat[,2])
  V <- t2/r2
  
  ## Eccentricity (as defined in Swihart and Slade 1985, p. 1177)
  ecc <- sqrt(Reduce("/", eigen(cov(dat[, 1:2]))$values))
  
  ## Obtain critical value
  s <- exp(-0.0502 + 0.173 * ecc - 0.0164 * ecc^2 - 0.433 * log(m))
  cv <- 2 - (qnorm(1 - alpha) * s)

  return(c(V=V, m=m, r2=r2, t2=t2, cv=cv, interval=interval))
}


## function to calculate temporal subset
tsub <- function(x, y, t, interval) {
  .Call("t2cpp2", x, y, t, as.integer(interval), PACKAGE="rhr")
}
