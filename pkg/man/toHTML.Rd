\name{toHTML}
\alias{toHTML}
\alias{toHTML.data.frame}
\alias{toHTML.htest}
\alias{toHTML.summaryDefault}
\title{toHTML: convert R objects as html}
\usage{
  toHTML(x, ...)

  \method{toHTML}{data.frame} (x, cat = TRUE, ...)

  \method{toHTML}{summaryDefault} (x, cap = "", ...)

  \method{toHTML}{htest} (x, cap = NULL, ...)
}
\arguments{
  \item{x}{a R object}

  \item{cap}{caption}

  \item{cat}{warp output with cat}

  \item{...}{additional arguments}
}
\value{
  \code{NULL}

  \code{NULL}

  \code{NULL}
}
\description{
  toHTML
}
\details{
  converts an R object to a data.frame
}

