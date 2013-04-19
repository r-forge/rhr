\name{fidelityBase}
\alias{fidelityBase}
\title{Site fidelity multiple animals}
\usage{
  fidelityBase(x, y, n)
}
\arguments{
  \item{dat}{a data.frame with 3 or 4 columns. The first
  column contains x-coordinates, the second column contains
  y coordinates and the third column contains a timestamp
  as \code{POSIXct}. If data is for more than one animal,
  the fourth column should provide ids. In case that there
  are only three}

  \item{n}{the number of montecarlo simulation}
}
\value{
  object of class \code{RhrFidelity}
}
\description{
  Site fidelity multiple animals
}
\examples{
cat("hello world")
}
\author{
  Johannes Signer
}

