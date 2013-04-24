\name{rhrArea.RhrEstimatorKDE}
\alias{rhrArea.RhrEstimatorKDE}
\title{Area for Kernel Density Estimation}
\usage{
  rhrArea.RhrEstimatorKDE(est, level = 95, ud = FALSE,
    raster = FALSE, matrix = FALSE, contour = FALSE)
}
\arguments{
  \item{est}{an object returned from the function rhrKDE}

  \item{level}{The level of the UD at which area is
  calculated}

  \item{ud}{returns only the utility distribution as
  raster}

  \item{raster}{returns a rasterstack with a raster for
  each level}

  \item{matrix}{returns a matrix with predictions}

  \item{contour}{returns contour lines}
}
\value{
  The area
}
\description{
  Area for Kernel Density Estimation
}

