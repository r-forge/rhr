\name{rhrSchoener}
\alias{rhrSchoener}
\title{Schoener's ratio}
\usage{
  rhrSchoener(dat, interval, alpha = 0.25, minM = 10,
    consec = TRUE)
}
\arguments{
  \item{dat}{a data.frame with 3 columns. The first column
  contains x-coordinates, the second column contains y
  coordinates and the third column contains a timestamp as
  \code{POSIXct}.}

  \item{interval}{The interval in seconds}

  \item{alpha}{The alpha value used to calculate the
  critical value}

  \item{minM}{The minimum number of pairs required, if m is
  smaller than this argument it will return NA.}

  \item{consec}{indicates whether or not the observations
  are consecutive}
}
\value{
  \code{vector} vector of length three. V is Schoeners v, n
  the number of points and m the number of point pairs
  considered to calculated t2.
}
\description{
  Schoener's ratio
}
\note{
  This implementation uses the normal distribution as a
  sampling distribution.
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

