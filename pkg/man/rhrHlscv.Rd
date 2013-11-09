\name{rhrHlscv}
\alias{rhrHlscv}
\title{Calcualte least square cross validation bandwidth}
\usage{
  rhrHlscv(xyz, range = c(0.1, 2), whichMin = "global",
    rescale = "unitvar", seqLength = 100)
}
\arguments{
  \item{xyz}{data.frame with three columns: x and y
  coordinates and of each grid cell and the number of
  points that are located within this cell.}

  \item{range}{numeric vector prortion of the reference
  bandwidth that are used to calculate the candidate values
  for least square cross validation. The default value of
  \code{c(0.1, 2)} will create a sequence from 0.1*href to
  2*href of length \code{seqLength}.}

  \item{seqLength}{numeric value, the length of the
  sequence with candidate bandwidths.}

  \item{whichMin}{character, indicating if the
  \code{global} or \code{local} minimum should be searched}

  \item{rescale}{character, indicating if and if how data
  are rescaled. \code{unitvar} rescales x and y coordinates
  to unit variance, \code{xvar} rescales x and y coordinate
  to variance of x and \code{none} uses the raw data.}
}
\value{
  \code{vector} of length two
}
\description{
  Function to estimate 2 dimensional least square cross
  validation for kernel density estimation
}
\details{
  Function to calcualte least square cross validation
  bandwidth. This implementation is based on Seaman and
  Powell (1996), but uses a binned algorithm. If
  \code{whichMin} is \code{"global"} the global minimum is
  returned, else the local minimum with the largest
  candidate bandwidth is returned.
}
\examples{
\dontrun{
}
}
\author{
  Johannes Signer
}
\references{
  Seaman, D. E., & Powell, R. A. (1996). An evaluation of
  the accuracy of kernel density estimators for home range
  analysis. _Ecology, 77(7)_, 2075-2085.

  Carr and Rodges
}

