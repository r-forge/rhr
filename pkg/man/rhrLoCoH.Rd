\name{rhrLoCoH}
\alias{rhrLoCoH}
\title{Local Convex Hull (LoCoH)}
\usage{
  rhrLoCoH(xy, type = "k", n = 10, level = 95, min.pts = 3)
}
\arguments{
  \item{xy}{a data frame with at least two columns. The
  first column contains x-coordiantes and the second column
  y-coordinates.}

  \item{type}{one of k, r, a. See details. three types are
  availabe: i) k-nearest neighbours; ii) neighbours within
  radius r; iii) a within cum dist}

  \item{n}{if type is k, number of neaerst neibhers, if
  type is r radius and if type is a cum dist to be used up}

  \item{level}{at which levels should the home range be
  calculated. Numbers between 1 and 100.}

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
locoh <- rhrLoCoH(datSH[, 2:3], type="k", n=100, level=c(50, 90))
spplot(locoh, 'level')
}

