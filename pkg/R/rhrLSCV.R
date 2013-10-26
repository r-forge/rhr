#' Calcualte least square cross validation bandwidth 
#'
#' Function to estimate 2 dimensional least square cross validation for kernel density estimation
#'
#' @param xyz data.frame with three columns: x and y coordinates and of each grid cell and the number of points that are located within this cell.
#' @param range numeric vector prortion of the reference bandwidth that are used to calculate the candidate values for least square cross validation. The default value of \code{c(0.1, 2)} will create a sequence from 0.1*href to 2*href of length \code{seqLength}.
#' @param seqLength numeric value, the length of the sequence with candidate bandwidths.
#' @param whichMin character, indicating if the \code{global} or \code{local} minimum should be searched
#' @param rescale character, indicating if and if how data are rescaled. \code{unitvar} rescales x and y coordinates to unit variance, \code{xvar} rescales x and y coordinate to variance of x and \code{none} uses the raw data.

#' @details Function to calcualte least square cross validation bandwidth. This implementation is based on Seaman and Powell (1996), but uses a binned algorithm. If \code{whichMin} is \code{"global"} the global minimum is returned, else the local minimum with the largest candidate bandwidth is returned.

#' @return \code{vector} of length two
#' @export
#' @references Seaman, D. E., & Powell, R. A. (1996). An evaluation of the accuracy of kernel density estimators for home range analysis. _Ecology, 77(7)_, 2075-2085.
#' @references Carr and Rodges
#' 
#' @author Johannes Signer 

rhrHlscv <- function(xyz, range=c(0.1, 2), whichMin="global", rescale="unitvar", seqLength=100) {

  ## Some input validation
  if (!is(xyz, "data.frame")) {
    stop("rhrHlscv: xyz: is not an object of class data.frame")
  }

  if (ncol(xyz) < 3) {
    stop("rhrHlscv: xyz: should have at three two columns")
  }

  if (ncol(xyz) > 3) {
    warning("rhrHlscv: xyz: using only the first three columns")
  }

  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHlscv: scale: not one of unit, sd or none")
  }


  xs <- xyz[, 1]
  ys <- xyz[, 2]

  if (rescale == "unitvar") {
    ## standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    ## standardize x and y by 
    ys <- (ys / sd(ys)) * sd(xs)
  } 
 

  ## reference bandwidth
  href <- mean(rhrHref(data.frame(xs, ys)))
  range <- href * c(0.1, 2)
  range <- seq(range[1], range[2], length.out=100)

  
  converged <- TRUE

  ## bin points
  res <- .Call("binnedCV", as.double(xs),
               as.double(ys),
               as.double(xyz[, 3]), as.double(range), PACKAGE="rhr")


  if (whichMin == "global") {
    h <- range[which.min(res)]
  } else {
    h <- range[max(localMinima(res))]
  }

  ## Did h converge?
  if (range[1] == h | range[length(range)] == h) {
    converged <- FALSE
    warning("rhrHlscv: lscv did not converge.")
  }

  ## prepare return
  if (rescale == "unitvar") {
    h <- h * c(sd(xyz[, 1]), sd(xyz[, 2]))
  } else {
    h <- c(h, h)
  }
  list(h=h, converged=converged, res=res)
}

## Helper function from: http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMinima <- function(x) {
  ## Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
} 
