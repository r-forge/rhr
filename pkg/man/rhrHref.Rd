\name{rhrHref}
\alias{rhrHref}
\title{Calcualte reference bandwidth}
\usage{
  rhrHref(xy, rescale = "none")
}
\arguments{
  \item{xy}{data.frame with two columns: x and y
  coordinates}

  \item{rescale}{character, indicating if and if how data
  are rescaled. \code{unitvar} rescales x and y coordinates
  to unit variance, \code{xvar} rescales x and y coordinate
  to variance of x and \code{none} uses the raw data.}
}
\value{
  \code{vector} of length two
}
\description{
  Function to estimate 2 dimensional reference bandwidth
  for kernel density estimation
}
\details{
  Function to calcualte reference bandwidth. This
  implementation is based on Worton (1989). If variances
  differ greatly, it is advaisable to rescale the data
  using \code{rescale="unitvar"}. If the data is suspected
  to multimodal other bandwidth estimation methods may be
  more suitable. Seaman and Powell (1996) modified the
  method slighty by calculating the reference bandwidth for
  each dimension independently. The same result is obtained
  if \code{rescale=TRUE}. If rescaling is performed with
  unit variance both methods will lead to the same result.
}
\examples{
data(datSH)
rhrHref(datSH[, 2:3])
}
\author{
  Johannes Signer
}
\references{
  Seaman, D. E., & Powell, R. A. (1996). An evaluation of
  the accuracy of kernel density estimators for home range
  analysis. _Ecology, 77(7)_, 2075-2085.

  Worton, B. J. (1989). Kernel methods for estimating the
  utilization distribution in home-range studies. _Ecology,
  70(1)_, 164-168.
}

