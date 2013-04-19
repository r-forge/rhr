#' Function to calculate a home range estimator
#' 
#' @param xy matrix or dataframe of dim n * 2, where n is the number of fixes
#' @param estimator the estimator to be calculated
#' @param ... parameters passed to the home range estimator
#' @export
#' @return object of class RhrEstimator
#' @examples a <- rhrEstimator(cbind(rnorm(1000), rnorm(1000)), "MCP")


rhrEstimator <- function(xy, estimator, ...) {
  # To few columns
  if (ncol(xy) < 2) {
    stop("ncol(xy) should be at least 2")
  }

  # To many columns
  if (ncol(xy) > 2) {
    warning("ncol(xy) > 2, assuming x-coords are stored in the first col and y-coords are stored in the second col")
  }

  ## Call to estimator

  # results are stored in res
  res <- list()

  # MCP
  if (tolower(estimator)=="mcp") {
    res$estimator <- "mcp"
    res$estimatorData <- rhrMCP(xy, ...)
    class(res) <- "RhrEstimatorMCP"
  }

  # KDE
  if (tolower(estimator)=="kde") {
    res$estimator <- "kde"
    res$estimatorData <- rhrKDE(xy, ...)
    class(res) <- "RhrEstimatorKDE"

    return(rhrKDE(xy, ...))
  }

  # LOCOH
  if (tolower(estimator) == "locoh") {

    res$estimator <- list()
    argsPassed <- list(...)

    argsRequired <- formals(rhrLoCoH)

    if ("type" %in% names(argsPassed)) {
      res$estimator$type <- argsPassed$type
    } else {
      res$estimator$type <- argsRequired$type
    }

    if ("n" %in% names(argsPassed)) {
      res$estimator$n <- argsPassed$n
    } else {
      res$estimator$n <- argsRequired$n
    }

    res$estimatorData <- rhrLoCoH(xy, ...)
    class(res) <- "RhrEstimatorLoCoH"
  }

  # Return
  return(res)
}

