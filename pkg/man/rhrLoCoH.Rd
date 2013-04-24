\name{rhrLoCoH}
\alias{rhrLoCoH}
\title{Local Convex Hull (LoCoH)}
\usage{
  rhrLoCoH(xy, type = "k", n = 10, level = 95, min.pts = 3)
}
\arguments{
  \item{xy}{a data frame with 2 columns. The first column
  contains x-coordiantes, the second one y-coordinates.}

  \item{type}{three types are availabe: i) k-nearest
  neighbours; ii) neighbours within radius r; iii) a within
  cum dist}

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
\author{
  Johannes Signer
}

