\name{rhrHpi}
\alias{rhrHpi}
\title{Estimate bandwidth with plug in the equation}
\usage{
  rhrHpi(xy, rescale = "none", ...)
}
\arguments{
  \item{xy}{data.frame with two columns: x and y
  coordinates}

  \item{rescale}{character, indicating if and if how data
  are rescaled. \code{unitvar} rescales x and y coordinates
  to unit variance, \code{xvar} rescales x and y coordinate
  to variance of x and \code{none} uses the raw data.}

  \item{...}{additional arguments passed to
  \code{KernSmooth::dpik}.}
}
\value{
  \code{vector} of length two
}
\description{
  Estimate bandwidth with plug in the equation
}
\details{
  Function to calcualte reference bandwidth. This function
  basically wraps \code{KernSmooth::dpik}
}
\examples{
data(datSH)
rhrHpi(datSH[, 2:3])
}
\author{
  Johannes Signer
}
\references{
  Gitzen 2006
}

