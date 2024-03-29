\name{rhrTTSI}
\alias{rhrTTSI}
\title{rhrTTSI}
\usage{
  rhrTTSI(dat, interval, ntimes = 3, ...)
}
\arguments{
  \item{dat}{data.frame, with three columns: x and y
  coordinates and a timestamp in seconds.}

  \item{interval}{numeric value, the initial interval
  considered}

  \item{ntimes}{numeric value, the number of times the
  critical value needs to be exceeded in order to reach
  independence.}

  \item{...}{further arguments passed to
  \code{rhrSchoener}.}
}
\value{
  \code{vector} vector of length three. V is Schoeners v, n
  the number of points and m the number of point pairs
  considered to calculated t2.
}
\description{
  This a wrapper around \code{rhrSchoener} to calculate
  time to statistical indpendence from a series of
  intervals.
}
\examples{
\dontrun{
library(rhr)
data(datSH)
dat <- data.frame(datSH[, 2:3], timestamp=as.numeric(ymd(datSH$day) + hms(datSH$time)))
ttsi <- rhrTTSI(dat, interval=5*60*50)
plot(ttsi)
}
}
\author{
  Johannes Signer
}
\references{
  Swihart, R. and Slade N. 1985, Testing for indpendence of
  observations in animal movement, Ecology, 66(4), 1176 -
  1184
}

