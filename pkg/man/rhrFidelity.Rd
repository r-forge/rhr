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

  \item{n}{the number of bootstrap iterations.}
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
# simulated data
set.seed(123)
dat <- data.frame(x=runif(1000, 0, 100), y=runif(1000, 0, 100))
rhrFidelity(dat, n=500)

# Example Data
data(datSH)
res <- rhrFidelity(datSH[, 2:3])
}
\author{
  Johannes Signer
}

