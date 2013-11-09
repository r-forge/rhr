#' Site fidelity
#'
#' 
#'
#' @param dat a data.frame with at least 2 columns. The first column contains the x-coordinates, the second column contains the y-coordinates 
#' @param n the number of simulated trajectories.
#' @useDynLib rhr
#' @export
#' @return object of class \code{RhrFidelity}, which is a list of length 4. \code{msd.dat} and \code{li.dat} is the mean square distance and linearity for the real date. \code{msd.sim} and \code{li.sim} are the mean square distances and linearities for the simulated trajectories. 
#' @examples
#' # simulated data
#' set.seed(123)
#' dat <- data.frame(x=runif(1000, 0, 100), y=runif(1000, 0, 100))
#' rhrFidelity(dat, n=500)
#'
#' # Example Data
#' data(datSH)
#' res <- rhrFidelity(datSH[, 2:3])

rhrFidelity <- function(dat, n=100) {

  ## --------------------------------------------------------------------------- #
  ## Some argument checking
  ## Check how many arguments where provided

  ## is dat correct?
  if (nrow(dat) < 2) {
    stop("dat needs to have 2 columns: x, y")
  }

  ## Coordinates
  if(!is(dat, "data.frame")) {
    if(inherits(dat, "SpatialPoints")) {
      dat <- data.frame(coordinates(dat))
    } else {
      stop(paste0("xy should be of class data.frame or SpatialPoints. The provided xy is of class ", class(xy)))
    }
  }

  res <- fidelityBase(dat[,1], dat[,2], n)

  attr(res, "class") <- "RhrFidelity"
  return(res)
}

#' Base function to calculate msd and li
#'
#' @param x x-coordinates
#' @param y y-coordinates
#' @param n the number of coordinates
#' @return a list
#' @useDynLib rhr
#' @author Johannes Signer


fidelityBase <- function(x, y, n) {
  res <- list()

  ## Calculates cumulative distances
  ## calculate the the successive distances between each fix
  d <- cumdist(x, y)

  ## resample
  d <- sample(d, length(d))

  ## simulate n random walks
  a <- replicate(n, random_walk(x[1], y[1], d, runif(length(d), 0, 360)), simplify=FALSE)

  ## msd 
  msd.dat <- msd(x, y)
  msd.sim <- sapply(a, function(x) msd(x[,1], x[,2]))

  ## li
  li.dat <- li(x, y)
  li.sim <- sapply(a, function(x) li(x[,1], x[,2]))

  ## return
  return(list(msd.dat=msd.dat, li.dat=li.dat, msd.sim=msd.sim,
              li.sim=li.sim ))
}

 
## ---------------------------------------------------------------------------- #
## cum distances

cumdist <- function(x,y) {
  return(sqrt((x[-1] - x[-length(x)])^2 + (y[-1] - y[-length(y)])^2))
}

## ---------------------------------------------------------------------------- #
## Everything in Cpp

random_walk <- function(sx, sy, d, rA) {
  n   <- length(d)
  rA  <- rA # runif(n, 0, 360)
  rx  <- rep(-1, n)
  ry  <- rep(-1, n)
  sinrA <- sin(rA * pi/180)
  cosrA <- cos(rA * pi/180)

  res <- .Call("randomWalkcpp", sx, sy, sinrA, cosrA, d, rx, ry, PACKAGE="rhr")

  return(cbind(res[["rx"]], res[["ry"]]))
}


## ---------------------------------------------------------------------------- #
## calculate msd

msd <- function(x, y) {
  mx  <- mean(x)
  my  <- mean(y)
  .Call("msdcpp", x, y, mx, my, PACKAGE="rhr")
}

## ---------------------------------------------------------------------------- #
## calculate li

li <- function(x, y) {
  d               <- cumdist(x,y)
  line.distance   <- sqrt((x[1] - x[length(x)])^2 + (y[1] - y[length(y)])^2)
  walked.distance <- sum(d)
  return(line.distance / walked.distance)
}

