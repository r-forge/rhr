#' rhrCorrectLevels
#' 
#' correct levels at which a home range should be calculated
#' @param str A string that contains the level, seperated by comas (usually provided through the GUI). If there are no suitable levels, then the default.level is used.
#' @param low lower bound for levels
#' @param upper upperbound for levels
#' @param default.level is the default level
#' @export

rhrCorrectLevels <- function(str, low=1, upper=100, default.level=95) {

  # are there any comas?
  if (grepl(",", str)) {
    # Split the string
    s <- strsplit(str, ",")[[1]]

    # remove empty ones
    s <- s[nchar(s) > 0]
    s <- as.numeric(s)

    # ensure range
    s <- s[s >= low & s <= upper]

    if (length(s) == 0) {
      s <- default.level
    }

    # order and remove duplicates
    s <- s[order(s)]
    s <- unique(s)
  } else { 
    str <- as.numeric(str)
    if (str >= low & str <= upper) {
      s <- str # retrun str, if its between acceptable levels
    } else {
      s <- default.level # return default level
    }  
  }
  return(s)
}
