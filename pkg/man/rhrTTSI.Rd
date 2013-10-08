\name{rhrTTSI}
\alias{rhrTTSI}
\title{rhrTTSI}
\usage{
  rhrTTSI(dat, interval, ntimes = 3, ...)
}
\value{
  \code{vector} vector of length three. V is Schoeners v, n
  the number of points and m the number of point pairs
  considered to calculated t2.
}
\description{
  This a wrapper around \code{rhrSchoener} to calculate
  time to statistical indpendence from a series of
  intervals
}
\examples{
set.seed(123)
}
\author{
  Johannes Signer
}
\references{
  Swihart, R. and Slade N. 1985, Testing for indpendence of
  observations in animal movement, Ecology, 66(4), 1176 -
  1184
}

