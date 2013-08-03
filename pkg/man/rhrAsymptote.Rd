\name{rhrAsymptote}
\alias{rhrAsymptote}
\title{Calculate home rnage asymptote for an object of class RhrHREstimator}
\usage{
  rhrAsymptote(x, ns = seq(100, nrow(dat(x)), 500),
    nrep = 10, tolTotArea = 0.95, nTimes = 5)
}
\arguments{
  \item{x}{RhrHREstimator object}

  \item{ns}{numeric vector of the number of samples to be
  taken}

  \item{nrep}{the number of replicates for each sample
  size}

  \item{tolTotArea}{tolerance to the total area}

  \item{nTimes}{the number of times the confidence interval
  is required to be within tolerated total area}
}
\value{
  RhrHRAsymptote
}
\description{
  calculate home range asymptote
}

