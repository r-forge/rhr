\name{rhrConvertUnit}
\alias{rhrConvertUnit}
\title{rhrConvertUnit}
\usage{
  rhrConvertUnit(x, inUnit = "m", outUnit = "ha")
}
\arguments{
  \item{x}{a vector}

  \item{inUnit}{input unit}

  \item{outUnit}{output unit}
}
\description{
  Converts areal units
}
\note{
  \code{inUnit} can be one of: \itemize{
  \item{"ido"}{stands for I dont know and will return the
  input} \item{"m"}{meter} \item{"km"}{kilometer}
  \item{"ft"}{feet} \item{"yd"}{yard} \item{"mi"}{mile} }
  \code{outUnit} can be one of: \itemize{
  \item{"sqm"}{square meter} \item{"ha"}{hectar}
  \item{"sqkm"}{square kilometer} \item{"sqft"}{square
  feet} \item{"acres"}{acres} \item{"sqyd"}{square yard}
  \item{"sqmi"}{square mile} \item{"ius"}{input unit
  squared} }
}

