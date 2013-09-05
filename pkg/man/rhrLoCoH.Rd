\name{rhrLoCoH}
\alias{rhrLoCoH}
\title{Local Convex Hull (LoCoH)}
\usage{
  rhrLoCoH(xy, type = "k", n = 10, levels = 95,
    min.pts = 3, ud = FALSE, cud = FALSE, xrange = NA,
    yrange = NA, res = 100)
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

  \item{type}{one of k, r, a. See details. three types are
  availabe: i) k-nearest neighbours; ii) neighbours within
  radius r; iii) a within cum dist}

  \item{n}{if type is k, number of neaerst neibhers, if
  type is r radius and if type is a cum dist to be used up}

  \item{min.pts}{min.pts}
}
\value{
  object of class \code{SpatialPolygonsDataFrame}
}
\description{
  Local Convex Hull (LoCoH)
}
\details{
  There are three different types available for determining
  the number of neighbors. \itemize{ \item{"k"}{uses the k
  nearest neighbours} \item{"r"}{uses all neighbours within
  a radius r} \item{"a"}{uses all neighbours that can be
  reached within a distance a. The distance to all points
  is calculated and then the cummulatively summed starting
  from the smallest until \code{a} is reached.}}
}
\examples{
data(datSH)
nrow(datSH)
locoh <- rhrLoCoH(datSH[1:100, 2:3], type="k", n=10, level=c(50, 90))
}

