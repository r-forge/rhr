#' rhrAlert
#' Wraps a character as html \code{div} with appropriate alert classe to use with twitter bootstrap.

#' @param x a character
#' @param cat logical, if true results will be \code{cat} to the command line
#' @param class character, giving finer specification of the class, possible values are: error, info, success or warning.
#' @export
#' @return \code{character}

rhrAlert <- function (x, cat = TRUE, class="info") 
{

  if (!class %in% c("info", "error", "success", "warning")) {
    stop("rhrAlert: class: unknown class requested")
  }
  out <- paste0("<div class='alert alert-info'>", x, "</div>")
  if (cat) {
    return(cat(out))
  }
  else {
    return(out)
  }
}
