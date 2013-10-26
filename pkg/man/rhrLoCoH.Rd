\name{rhrLoCoH}
\alias{rhrLoCoH}
\title{Local Convex Hull (LoCoH)}
\usage{
  rhrLoCoH(xy, type = "k", n = 10, levels = 95, minPts = 3,
    ud = FALSE, cud = FALSE, xrange = NA, yrange = NA,
    res = 100)
}
\arguments{
  \item{xy}{data.frame with two columns. The first column
  contains x coordinates and the second column contains y
  coordinates.}

  \item{levels}{a vector with isopleth levels at which home
  range should be calculated.}

  \item{ud}{a logical value, indicating whether or not a
  utilization distribution should be calculated.}

  \item{cud}{a logical value, indicating whether or not a
  cumulative utilization distribution should be
  calculated.}

  \item{xrange}{vector of length 2, with xmin and xmax for
  the UD}

  \item{yrange}{vector of length 2, with ymin and ymax for
  the UD}

  \item{res}{numeric value, the resolution of the ud.}

  \item{type}{character value, one of k, r, a. Three
  methods to select which neighbours are used for local
  hulls are availabe: i) k-nearest neighbours; ii)
  neighbours within a radius r; iii) neighbours within a
  cummulative dististance a.}

  \item{n}{numeric value, if type is k it is the number of
  neaerst neibhers, if type is r it is the radius that is
  searched and if type is a it is the cummulative distance
  to be used.}

  \item{minPts}{numeric value, the minimum number of
  neighbours required.}
}
\value{
  object of class \code{RhrHREstimator}
}
\description{
  This function estimates an animals home range using Local
  Convex Hulls (LoCoH).
}
\details{
  Three different types available for determining the
  number of neighbors: \itemize{ \item{"k"}{uses the k
  nearest neighbours} \item{"r"}{uses all neighbours within
  a radius r} \item{"a"}{uses all neighbours that can be
  reached within a distance a. The distance to all points
  is calculated and then the cummulatively summed starting
  from the smallest until \code{a} is reached.}}
}
\examples{
\dontrun{
data(datSH)
locoh <- rhrLoCoH(datSH[, 2:3], type="k", n=10, level=c(50, 90))

## area at isopleths
rhrArea(locoh)

## get isopleths
iso <- isopleths(locoh)

## Which parameter were used?
parameters(locoh)
}
}
\author{
  Johannes Signer
}
\references{
  Getz, W. M., & Wilmers, C. C. (2004). A local
  nearest-neighbor convex-hull construction of home ranges
  and utilization distributions. _Ecography_, 27(4),
  489-505.

  Getz, W. M., Fortmann-Roe, S., Cross, P. C., Lyons, A.
  J., Ryan, S. J., & Wilmers, C. C. (2007). LoCoH:
  nonparameteric kernel methods for constructing home
  ranges and utilization distributions. _PloS one_, 2(2),
  e207.
}

