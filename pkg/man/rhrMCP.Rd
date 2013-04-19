\name{rhrMCP}
\alias{rhrMCP}
\title{Minimum Convex Polygon (MCP)}
\usage{
  rhrMCP(xy, levels = 95)
}
\arguments{
  \item{xy:}{SpatialPointsDataFrame, holding fixes for the
  animal and time frame that the MCP should be calculated}

  \item{levels:}{a vector with the percentage of closest
  points to the centroid that are used to calculated MCP}
}
\value{
  object of class \code{SpatialPolygonsDataFrame}
}
\description{
  Minimum Convex Polygon (MCP)
}
\examples{
cat("hello world")
}
\author{
  Johannes Signer modified code from
  \code{adehabitatHR::mcp}
}

