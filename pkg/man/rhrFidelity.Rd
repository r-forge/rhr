\name{rhrFidelity}
\alias{rhrFidelity}
\title{Site fidelity}
\usage{
  rhrFidelity(dat, n = 100)
}
\arguments{
  \item{dat}{a data.frame with at least 2 columns. The
  first column contains the x-coordinates, the second
  column contains the y-coordinates}

  \item{n}{the number of bootstrap iterations
  \cite{rodgers2007a}.}
}
\value{
  object of class \code{RhrFidelity}, which is a list of
  length 4. \code{msd.dat} and \code{li.dat} is the mean
  square distance and linearity for the real date.
  \code{msd.sim} and \code{li.sim} are the mean square
  distances and linearities for the simulated trajectories.
}
\description{
  Site fidelity
}
\examples{
dat <- data.frame(x=runif(1000, 0, 100), y=runif(1000, 0, 100))
rhrFidelity(dat, n=500)
}
\author{
  Johannes Signer
}
\references{
  \itemize{ \item{Munger, J. (1984). Home ranges of horned
  lizards (Phrynosoma): circumscribed and exclusive?
  Oecologia, 62(3), 351–360.} \item{Spencer, S., Cameron,
  G., & Swihart, R. (1990). Operationally defining home
  range: temporal dependence exhibited by hispid cotton
  rats. Ecology, 71(5), 1817 – 1822.} \item{White, G. C., &
  Garrott, R. A. (1990). Analysis of Wildlife
  Radio-Tracking Data (p. 383). Academic Press.} }
}

