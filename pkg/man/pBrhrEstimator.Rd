\name{pBrhrEstimator}
\alias{pBrhrEstimator}
\title{Function to do a parallel bootstrap of a RhrEstimator*characteristic}
\usage{
  pBrhrEstimator(xy, estimator, characteristic, nsamples,
    niter, args.estimator = NULL,
    args.characteristic = NULL)
}
\arguments{
  \item{xy}{a data.frame or matrix}

  \item{estimator}{string specifiying the estimator (e.g.
  mcp)}

  \item{characteristic}{string specifiying characteristic
  to bootstrap (e.g. area)}

  \item{nsamples}{how many points should be drawn each
  time}

  \item{niter}{specifiyies the number of bootstrap
  iteration}

  \item{args.estimator}{list of further arguments passed to
  the estimator function}

  \item{args.characteristic}{list of further arguments
  passed to characteristic function}
}
\value{
  list with values of characteristic of length
}
\description{
  Function to do a parallel bootstrap of a
  RhrEstimator*characteristic
}

