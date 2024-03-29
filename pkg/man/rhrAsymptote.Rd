\name{rhrAsymptote}
\alias{rhrAsymptote}
\title{Calculate home rnage asymptote for an object of class RhrHREstimator}
\usage{
  rhrAsymptote(x, ns = seq(100, nrow(dat(x)), 500),
    nrep = 10, tolTotArea = 0.05, nTimes = 5,
    sampling = "sequential")
}
\arguments{
  \item{x}{RhrHREstimator object}

  \item{ns}{numeric vector of the number of samples to be
  taken at each step}

  \item{nrep}{the number of replicates for each sample
  size}

  \item{tolTotArea}{tolerance to the total area (that is
  the area using all points)}

  \item{nTimes}{the number of times the confidence interval
  is required to be within tolerated total area}

  \item{sampling}{this should be either random or
  sequential. See notes for details.}
}
\value{
  An object of class \code{RhrHRAsymptote}
}
\description{
  calculate home range asymptote
}
\note{
  Bootstrapped home ranges are calculated for different
  sample sizes. Starting from very few samples until the
  sample size approaches the total number of points. Home
  range sizes are then plotted against the sample sizes.
  Laver (2005, 2005) suggested to use the following cutoff
  value: the number of location estimates at which the 95
  \% confidence interval of the bootstrapped home-range
  estimates is within a specified percentage of the total
  home range size (that is the size of the home range with
  all relocations) for at least n times. Harris 1990
  suggested to use random sampling for discontinuous radio
  tracking data and sequential sampling for continuous
  radio tracking data.
}
\examples{
\dontrun{
## calculate home range asymptote for kernel density estimation
## first calculate the home range
data(datSH)
hr <- rhrKDE(datSH[, 2:3], h="href")
hra <- rhrAsymptote(hr)
plot(hra)
}
}
\references{
  Peter N Laver. Cheetah of the serengeti plains: a home
  range analysis. Master's thesis, Virginia Polytechnic
  Institute and State University, 2005

  Peter N. Laver and Marcella J. Kelly. A critical review
  of home range studies. The Journal of Wildlife
  Management, 72(1):290-298, 2008
}

