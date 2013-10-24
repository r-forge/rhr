\name{dfGrob}
\alias{dfGrob}
\title{dfGrob}
\usage{
  dfGrob(x, start = 0.03, stop = 0.97, digits = 2,
    bodyFont = "")
}
\arguments{
  \item{x}{a data.frame}

  \item{start}{where to start, default is 0.03}

  \item{stop}{where to stop, default is 0.97}

  \item{digits}{to how many digists numbers are rounded}

  \item{...}{additional arguments passed to gp}
}
\description{
  Create a grid representation for an object of class
  data.frame
}
\examples{
grid.newpage()
pushViewport(viewport())
grid.draw(dfGrob(data.frame(id=1:10, class=letters[1:10], stringsAsFactors=FALSE)))
popViewport()
}
\author{
  Johannes Signer
}

