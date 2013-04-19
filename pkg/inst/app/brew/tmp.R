res <- Rook::Response$new()

if (!is.null(req$POST())) {
  name <- req$POST()[['path']]
  sep <- req$POST()[['sep']]
  skip <- req$POST[['skip']]
  sepDec <- req$POST[['sepDec']]
  if (!is.null(name)) {

    str <- readLines(name$tempfile, n=60)
                                        # asign sep
    if (sep == "coma") {
      sep <- ","
    } else if (sep == "tab") {
      sep <- "\t"
    } else {
      sep <- ";"}

                                        # asign sepDec
    if (sepDec == "coma") {
      sepDec <- ","
    } else {
      sepDec <- "."
    }
    
    # test if a seperator works for the Body
    # each lines requires more than one field and same length
    testBody <- function(str, sep) {
      str <- strsplit(str, sep)
      all(str == str[1]) & (str[1] > 1)
    }

    if (skip != 0) {
      str <- tail(str, -skip)
    }

    if (testBody(str)) {
      # everything is ok
      dat <- read.table(name$tempfile, sep=sep, skip=skip, dec=decSep, stringsAsFactors=FALSE)
      f <- c(names(dat), NA)

      alert(paste0('<b>Data</b> (', name[1], ') successfully loaded'))

      # the frist lines of 
      cat(paste0("<pre>", head(dat), "</pre>"))
    } else {

      alert(paste0('<b>Data</b> (', name[1], ') successfully loaded'))
      cat(paste0("<pre>", head(str), "</pre>"))
    }
      
      


      



      ## possible deliminators
      #delims <- c(d <- c("\t", ",", " ", "\\.", "%", "&", "/", "-", "\\+", "~", "\\?", "!"),
      #            paste0("'", d, "'"),
      #            paste0("\"", d, "\""), 
      #            paste0("'\\s*", d, "\\s*'"),
      #            paste0("\"\\s*", d, "\\s*\""))
      #

      ## check body
      #bodySplit <- lapply(delims, function(x) strsplit(tail(f, -10), x))

      #freq <- lapply(bb, sapply, length)

      ## get rid of the ones where there all 1s
      #delims <- delims[!sapply(freq, function(x) all(x==1))]
      #freq <- freq[!sapply(freq, function(x) all(x==1))]

      ## the true delim should be the one found on all lines in the body with the same frequency
      #delims <- delims[sapply(freq, function(x) all(x==x[1]))]
      #freq <- sapply(lapply(freq, function(x) all(x==x[1])), "[[", 1)
      #trueDelim <- freq[freq == max(freq)]

      ## no delimt could not be detected and the provided doesnt seems to work
      #if (length(trueDelim) == 0) {
      #  alert(paste0("Wrong deliminator: ", sep))
      #} else if (length(trueDelim) > 1) {
      ## It appears that there are 2 suitable deliminator, please edit your data
      #  alert(paste0("Wrong deliminator: ", sep))
      #} else {
      #  if (trueDelim != sep) {
      ## maybe "..." is your deliminator
      #  } else {
      ## everything is good, read it now
      #  }
      #} 

      ## check head


    
  } else {
    cat('<div class="alert alert-error"><b>Something went wrong:</b> No input file selected</div>')
    f <- "no data loaded"

  }
} else if (!is.null(datFromR)) {
  dat <- datFromR
  f <- c(names(datFromR), NA)
  cat(paste0('<div class="alert alert-success"><b>Data</b> read from R</div>'))
} else {
  f <- "no data loaded"
}
