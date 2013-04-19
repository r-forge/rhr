\name{rhrSchoener}
\alias{rhrSchoener}
\title{Schoener's ratio}
\usage{
  rhrSchoener(dat, interval, tolerance = 0, type = "con")
}
\arguments{
  \item{dat}{a data.frame with 3 columns. The first column
  contains x-coordinates, the second column contains y
  coordinates and the third column contains a timestamp as
  \code{POSIXct}.}

  \item{interval}{The interval in seconds}

  \item{tolerace}{The tolerance of the interval, i.e.
  interval +- tolerance}

  \item{type}{either all (for all reachable points within
  time interval), strict (for stricly consecutive points)
  or cons (for relaxed consecutive points).}
}
\value{
  \code{vector} vector of length three. V is Schoeners v, n
  the number of points and m the number of point pairs
  considered to calculated t2.
}
\description{
  Schoener's ratio
}
\examples{
set.seed(123)
rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, 2, "all")
rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, 2, "strict")
rhrSchoener(cbind(rnorm(1000), rnorm(1000), 1:1000), 10, type="con")
}
\author{
  Johannes Signer
}

