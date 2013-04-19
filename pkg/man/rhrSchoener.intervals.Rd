\name{rhrSchoener.intervals}
\alias{rhrSchoener.intervals}
\title{Schoener's ratio different intervals}
\usage{
  rhrSchoener.intervals(dat, tolerance, n,
    extended = FALSE)
}
\arguments{
  \item{dat}{a data.frame with 3 columns. The first column
  contains x-coordinates, the second column contains y
  coordinates and the third column contains a timestamp as
  \code{POSIXct}.}

  \item{tolerance}{the tolerance at each breakpoint in
  seconds}

  \item{n}{The number of break points, at which to test for
  statistical independence}

  \item{the}{time in seconds for the max breakpoints}

  \item{extended}{Whether or not extended (i.e. all
  reachable neighbours in no sequential order).}
}
\value{
  object of class \code{vector}
}
\description{
  Schoener's ratio different intervals
}
\examples{
cat("hello world")
}
\author{
  Johannes Signer
}

