\name{rhrKDE}
\alias{rhrKDE}
\title{Kernel Density Estimation (KDE)}
\usage{
  rhrKDE(xy, h = "href", xrange = NA, yrange = NA,
    resolution = 100, lscv.n = 100, tol = 1,
    return.raster = FALSE)
}
\arguments{
  \item{xy}{SpatialPointsDataFrame, holding fixes for the
  animal and time frame that the MCP should be calculated}

  \item{percent}{percentage of closest points to the
  centroid that are used to calculated MCP}

  \item{return.rast}{returns the hat matrix with kernel
  densities as an object of class raster}
}
\value{
  object of class \code{RhrEstimatorKDE}
}
\description{
  Kernel Density Estimation (KDE)
}
\examples{
cat("hello world")
}
\author{
  Johannes Signer
}

