\name{plot.RhrHREstimator}
\alias{plot.RhrHREstimator}
\title{plot for RhrHREstimator}
\usage{
  \method{plot}{RhrHREstimator} (x, what = c("ud", "iso"),
    draw = TRUE, useGE = FALSE, ...)
}
\arguments{
  \item{x}{RhrHREstimator object}

  \item{what}{indicates what should be printed. This can be
  either \code{ud} or \code{iso}.}

  \item{draw}{indicates whether the plot should be drawn or
  not. If this is \code{FALSE} a grob is returned.}

  \item{useGE}{indicates wether or not google earth
  satellite images should be use as background}

  \item{...}{none implemented}
}
\description{
  generic plot for RhrHREstimator
}

