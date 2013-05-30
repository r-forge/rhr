\name{rhrMCP}
\alias{rhrMCP}
\title{Minimum Convex Polygon (MCP)}
\usage{
  rhrMCP(xy, levels = 95)
}
\arguments{
  \item{xy}{SpatialPointsDataFrame, holding fixes for the
  animal and time frame that the MCP should be calculated}

  \item{levels}{a vector with the percentage of closest
  points to the centroid that are used to calculated MCP}
}
\value{
  object of class \code{SpatialPolygonsDataFrame}
}
\description{
  Minimum Convex Polygon (MCP)
}
\examples{
data(datSH)
mcp1 <- rhrMCP(datSH[, 2:3], levels=95)
mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))
}
\author{
  Johannes Signer modified and adapted code from
  \code{adehabitatHR::mcp}
}

