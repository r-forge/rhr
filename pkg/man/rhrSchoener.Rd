\name{rhrSchoener}
\alias{rhrSchoener}
\title{Schoener's ratio}
\usage{
  rhrSchoener(dat, interval, alpha = 0.25, minM = 10,
    consec = TRUE, meth = 1)
}
\arguments{
  \item{dat}{data.frame with 3 columns. The first column
  contains x coordinates, the second column contains y
  coordinates and the third column contains a timestamp as
  \code{POSIXct}.}

  \item{interval}{numeric value, the interval in seconds.}

  \item{alpha}{numeric value, alpha value used to calculate
  the critical value.}

  \item{minM}{numeric value, the minimum number of pairs
  required, if m is smaller than this argument it will
  return \code{NA}.}

  \item{consec}{locagical value, indicates whether or not
  the observations are consecutive or not.}
}
\value{
  \code{vector} vector of length six. \itemize{
  \item{"V"}{Schoeners V} \item{"m"}{Number of pairs used}
  \item{"r2"}{Mean squared distance from the center of
  activity} \item{"t2"}{Mean squared distance between
  relocations} \item{"cv"}{Critical value} \item{"m"}{The
  time interval in seconds} }
}
\description{
  Schoener's ratio
}
\note{
  This implementation uses the normal distribution as a
  sampling distribution. Relocations are ordered by
  timestamp and then relocations are sampled based on
  \code{interval}. In cases when relocations are not spaced
  equally, i.e. no relocation is available exactly at the
  interval, the relocation directly thereafter is used.
}
\examples{
data(datSH)
dat <- data.frame(datSH[, 2:3], as.numeric(ymd(datSH$day) + hms(datSH$time)))
rhrSchoener(dat, interval=60)
}
\author{
  Johannes Signer
}
\references{
  Swihart, R. and Slade N. 1985, Testing for indpendence of
  observations in animal movement, _Ecology_, 66(4), 1176 -
  1184
}

