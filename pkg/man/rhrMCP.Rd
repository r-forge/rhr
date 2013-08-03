\name{rhrMCP}
\alias{rhrMCP}
\title{Minimum Convex Polygon (MCP)}
\usage{
  rhrMCP(xy, levels = 95, ud = FALSE, cud = FALSE,
    xrange = NA, yrange = NA, res = 100)
}
\arguments{
  \item{xy}{data.frame with two columns. The first column
  contains x coordinaes and the second column contains y
  coordinates}

  \item{levels}{a vector with the percentage of closest
  points to the centroid that are used to calculated MCP}

  \item{ud}{a logical value, whether or not a utilitisation
  distribution should be calculated}

  \item{cud}{a logical value, whether or not a cumulative
  utilitisation distribution should be calculated}

  \item{xrange}{vector of length 2, with xmin and xmax for
  the UD}

  \item{yrange}{vector of length 2, with ymin and ymax for
  the UD}

  \item{res}{the resolution for the ud.}
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

