#' Site fidelity
#'
#' @param dat a data.frame with at least 2 columns. The first column contains the x-coordinates, the second column contains the y-coordinates 
#' @param n the number of bootstrap iterations \cite{rodgers2007a}.
#' @return object of class \code{RhrFidelity}, which is a list of length 4. \code{msd.dat} and \code{li.dat} is the mean square distance and linearity for the real date. \code{msd.sim} and \code{li.sim} are the mean square distances and linearities for the simulated trajectories. 
#' @useDynLib rhr
#' @export
#' @author Johannes Signer
#' @references
#' \itemize{
#'  \item{Munger, J. (1984). Home ranges of horned lizards (Phrynosoma): circumscribed and exclusive? Oecologia, 62(3), 351–360.}
#'  \item{Spencer, S., Cameron, G., & Swihart, R. (1990). Operationally defining home range: temporal dependence exhibited by hispid cotton rats. Ecology, 71(5), 1817 – 1822.}
#'  \item{White, G. C., & Garrott, R. A. (1990). Analysis of Wildlife Radio-Tracking Data (p. 383). Academic Press.}
#' }
#' @examples
#' dat <- data.frame(x=runif(1000, 0, 100), y=runif(1000, 0, 100))
#' rhrFidelity(dat, n=500)

rhrFidelity <- function(dat, n=100) {

  # --------------------------------------------------------------------------- #
  # Some argument checking
  # Check how many arguments where provided

  # is dat correct?
  if (nrow(dat) < 2) {
    stop("dat needs to have 2 columns: x, y")
  }
  res <- fidelityBase(dat[,1], dat[,2], n)

  attr(res, "class") <- "RhrFidelity"
  return(res)
}

#' Site fidelity multiple animals
#'
#' @param dat a data.frame with 3 or 4 columns. The first column contains x-coordinates, the second column contains y coordinates and the third column contains a timestamp as \code{POSIXct}. If data is for more than one animal, the fourth column should provide ids. In case that there are only three 
#' @param n the number of montecarlo simulation
#' @return object of class \code{RhrFidelity}
#' @useDynLib rhr
#' @export
#' @author Johannes Signer
#' @examples
#' cat("hello world")


fidelityBase <- function(x, y, n) {
  res <- list()

  # Calculates cumulative distances
  # calculate the the successive distances between each fix
  d <- cumdist(x, y)

  # simulate n random walks
  a <- replicate(n, random_walk(x[1], y[1], d, runif(length(d), 0, 360)), simplify=FALSE)

  # msd 
  msd.dat <- msd(x, y)
  msd.sim <- sapply(a, function(x) msd(x[,1], x[,2]))

  # li
  li.dat <- li(x, y)
  li.sim <- sapply(a, function(x) li(x[,1], x[,2]))

  # return
  return(list(msd.dat=msd.dat, li.dat=li.dat, msd.sim=msd.sim,
              li.sim=li.sim ))
}

 
# ---------------------------------------------------------------------------- #
# cum distances

cumdist <- function(x,y) {
  return(sqrt((x[-1] - x[-length(x)])^2 + (y[-1] - y[-length(y)])^2))
}

# ---------------------------------------------------------------------------- #
# Everything in C

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


# ---------------------------------------------------------------------------- #
# calculate msd

msd <- function(x, y) {
  mx  <- mean(x)
  my  <- mean(y)
  .Call("msdcpp", x, y, mx, my, PACKAGE="rhr")
}

# ---------------------------------------------------------------------------- #
# calculate li

li <- function(x, y) {
  d               <- cumdist(x,y)
  line.distance   <- sqrt((x[1] - x[length(x)])^2 + (y[1] - y[length(y)])^2)
  walked.distance <- sum(d)
  return(line.distance / walked.distance)
}

