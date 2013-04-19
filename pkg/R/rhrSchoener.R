#' Schoener's ratio
#'
#' @param dat a data.frame with 3 columns. The first column contains x-coordinates, the second column contains y coordinates and the third column contains a timestamp as \code{POSIXct}. 
#' @param interval The interval in seconds
#' @param tolerace The tolerance of the interval, i.e. interval +- tolerance
#' @param type either all (for all reachable points within time interval), strict (for stricly consecutive points) or cons (for relaxed consecutive points).
#' @return \code{vector} vector of length three. V is Schoeners v, n the number of points and m the number of point pairs considered to calculated t2.
#' @useDynLib rhr
#' @export
#' @author Johannes Signer 
#' @examples
#' set.seed(123)
#' rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, 2, "all")
#' rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, 2, "strict")
#' rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, type="con")


rhrSchoener <- function(dat, interval, tolerance=0, type="con") {

  type <- tolower(type)

  if (!type %in% c("all", "strict", "con")) {
    stop("incorrect type")
  }

  if (any(!complete.cases(dat))) {
    dat <- dat[complete.cases(dat), ]
    warning("In rhrSchoener: removed NA")
  }

  if (any(duplicated(dat[,3]))) {
    dat <- dat[!duplicated(dat[,3]), ]
    warning("In rhrSchoener: removed duplicates")
  }

  # correct type
  if (type == "all") {
    type <- 1
  }
  if (type == "strict") {
    type <- 2
  }
  if (type == "con") {
    type <- 3
  }

   t2cpp <- function(x, y, t, interval, keep, type) {
    .Call("t2cpp", x, y, t, list(interval=interval, keep=keep, type=type), PACKAGE="rhr")
  }

  r2 <- msd(dat[,1], dat[,2])
  t2 <- t2cpp(dat[,1], dat[,2], as.numeric(dat[,3]), interval, tolerance, type)

  return(c(V=t2[1]/r2, n=t2[2], m=t2[3], r2=r2, t2=t2[1]))
}

