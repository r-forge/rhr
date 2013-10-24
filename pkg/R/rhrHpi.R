#' Estimate bandwidth with plug in the equation
#'
#' Estimate bandwidth with plug in the equation
#'
#' @param xy data.frame with two columns: x and y coordinates
#' @param rescale character, indicating if and if how data are rescaled. \code{unitvar} rescales x and y coordinates to unit variance, \code{xvar} rescales x and y coordinate to variance of x and \code{none} uses the raw data.
#' @param ... additional arguments passed to \code{KernSmooth::dpik}.

#' @details Function to calcualte reference bandwidth. This function basically wraps \code{KernSmooth::dpik}

#' @return \code{vector} of length two
#' @export
#' @references Gitzen 2006
#' 
#' @author Johannes Signer 
#' @examples
#' data(datSH)
#' rhrHpi(datSH[, 2:3])

rhrHpi <- function(xy, rescale="none", ...) {

  ## Some input validation
  if (!is(xy, "data.frame")) {
    stop("rhrHpi: xy: is not an object of class data.frame")
  }

  if (ncol(xy) < 2) {
    stop("rhrHpi: xy: should have at least two columns")
  }

  if (ncol(xy) > 2) {
    warning("rhrHpi: xy: using only the first two columns")
  }

  
  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHpi: scale: not one of unitvar, xvar or none")
  }

  xs <- xy[, 1]
  ys <- xy[, 2]

  if (rescale == "unitvar") {
    ## standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    ## standardize x and y by 
    ys <- (ys / sd(ys)) * sd(xs)
  } 

  hx <- KernSmooth::dpik(xs, ...)
  hy <- KernSmooth::dpik(ys, ...)

  h <- c(hx, hy)

  if (rescale == "unitvar") {
    h <- dpik(xs, ...)
    h <- h * c(sd(xy[, 1]), sd(xy[, 2]))
  }   

  ## Gitzen et al. 2006 suggested that if bandwidth is estimated for each coordinate seperately, to correct it x^(5/6)
  return(h^(5/6))
}
