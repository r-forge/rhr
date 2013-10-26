\name{rhrMCP}
\alias{rhrMCP}
\title{Minimum Convex Polygon (MCP)}
\usage{
  rhrMCP(xy, levels = 95, ud = FALSE, cud = FALSE,
    xrange = NA, yrange = NA, res = 100)
}
\arguments{
  \item{xy}{data.frame with two columns. The first column
  contains x coordinates and the second column contains y
  coordinates}

  \item{levels}{vector with the percentage of closest
  points to the centroid that are used to calculated MCP}

  \item{ud}{logical value, indicating whether or not a
  utilization distribution should be calculated.}

  \item{cud}{a logical value, indicating whether or not a
  cumulative utilization distribution should be calculated}

  \item{xrange}{vector of length two, with xmin and xmax
  for the UD}

  \item{yrange}{vector of length two, with ymin and ymax
  for the UD}

  \item{res}{the resolution for the ud.}
}
\value{
  object of class \code{RhrHREstimator}
}
\description{
  Minimum Convex Polygon (MCP)
}
\note{
  Computes the minimum convex polygon of a subset of
  points. First the centroid of the home range is found
  with \code{rgeos::gCentroid} and then the `100 - levels`
  points are used to calculate a minimum convex polygon.
}
\examples{
data(datSH)
## Calculate mcp at one level
mcp1 <- rhrMCP(datSH[, 2:3], levels=95)
## Calculate mcp at several levels
mcp2 <- rhrMCP(datSH[, 2:3], levels=c(50, 90, 95))

## Area at each isopleth level
rhrArea(mcp2)

## SptialPolygonsDataFrame of isopleth
isopleths(mcp2)
}
\author{
  Johannes Signer inspired from \code{adehabitatHR::mcp}
}
\seealso{
  \code{adehabitatHR::mcp}, \code{rgeos::gConvexHull}
}

