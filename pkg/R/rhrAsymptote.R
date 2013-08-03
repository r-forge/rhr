#' Calculate home rnage asymptote for an object of class RhrHREstimator
#' 
#' calculate home range asymptote
#' @param x RhrHREstimator object
#' @param ns numeric vector of the number of samples to be taken
#' @param nrep the number of replicates for each sample size
#' @param tolTotArea tolerance to the total area
#' @param nTimes the number of times the confidence interval is required to be within tolerated total area
#' @return RhrHRAsymptote
#' @export


rhrAsymptote <- function(x, ns=seq(100, nrow(dat(x)), 500), nrep=10, tolTotArea=0.95, nTimes=5) {

  ## Input checks
  if (!is(x, "RhrHREstimator")) {
    stop("x is not of class RhrHREstimator")
  }

  if (!is(ns, "numeric")) {
    stop("ns is not numeric")
  }

  if (!is(nrep, "numeric")) {
    stop("nrep is not numeric")
  }

  if (nrep <= 0) {
    stop("nrep should be >= 0")
  }

  if (!is(tolTotArea, "numeric")) {
    stop("tolTotArea is not numeric")
  }
  
  if (!is(nTimes, "numeric")) {
    stop("nTimes is not numeric")
  }

  if (!(tolTotArea > 0 & tolTotArea <= 1)) {
    stop("tolTatArea should be between 0 and 1")
  }

  if (nTimes <= 0) {
    stop("nTimes should be > 0")
  }

  if (max(ns) > nrow(dat(x))) {
    ns <- ns[ns <= nrow(dat(x))]
  }

  if (length(ns) <= 1) {
    stop("Requested sample size larger than the number of observation")
  }

  # Which estimator was used
  est <- NA
  est <- switch(parameters(x)$name,
                mcp="rhrMCP",
                kde="rhrKDE",
                locoh="rhrLoCoH")

  providedArgs <- parameters(x)$args
  for (i in seq_along(providedArgs)) if (is.character(providedArgs[[i]])) providedArgs[[i]] <- shQuote(providedArgs[[i]])

  b <- parse(text=paste0(est, "(dat(x)[sample(1:nrow(dat(x)), ", ns, ", replace=FALSE),],",
               paste(names(providedArgs), providedArgs, sep="=",
                     collapse=","), ")"))

  ## use envir in enval, to make sure x is searched at the right place
  bb <- replicate(nrep, lapply(b, eval, envi=environment()), simplify=FALSE)
  bb <- do.call(rbind,
                lapply(bb, function(x)
                       do.call(rbind, lapply(1:length(ns),
                                             function(j) cbind(ns=ns[j], areas(x[[j]])))
                               )
                       )
                )

  
  ## Calculate the total area, i.e. the area with all points
  totalA <- areas(eval(parse(text=paste0(est, "(dat(x),",
                               paste(names(providedArgs), providedArgs, sep="=",
                                     collapse=","), ")")), envir=environment()))
  totalA$lower <- totalA$area * tolTotArea
  totalA$upper <- totalA$area * (2 - tolTotArea)

      
  ## calculate confidence intervals
  confints <- ddply(bb, c("level", "ns"), function(x) t.test(x$area)$conf.int) 

  confints <- split(confints, confints$level)

  ## figure out when the Asymptote was reached at each level j
  ## The asymptote is reached when the both CI are inside the area tolerance for x times
  asymReached <- sapply(seq_along(confints), function(j) {
    ## check if is inside
    isInside <- rle((confints[[j]]$V1 >= totalA[j, "lower"])  &
                    (confints[[j]]$V2 <= totalA[j, "upper"]))

    whereInside <- with(isInside, which(lengths >= nTimes & values))[1]

    ## NA if all outside
    if (length(whereInside) > 0 & !is.na(whereInside)) {
      unique(ns)[sum(isInside$lengths[1:(whereInside-1)]) + nTimes]
    } else {
      NA
    }
  })

  asymReached <- data.frame(level=as.numeric(names(confints)),
                            ns=asymReached)

  ## plot
  out <- list(asymtotes=asymReached, confints=do.call("rbind", confints), hrAreas=bb, call=match.call(),
              params=list(ns=ns, tolTotArea=tolTotArea), totalA=totalA, hrEstimator=x)
  class(out) <- "RhrHRAsymptote"
  out

}


#' print object of rhrHRAsymptote
#' 
#' @usage print(x, ...)
#' @aliases print print.RhrHRAsymptote
#' @param x RhrHRAsymptote
#' @param how should it be printed, this can be either sceen, html or grob
#' @method print RhrHRAsymptote
#' @export

print.RhrHRAsymptote <- function(x, how="screen") {

  if (length(how) > 1) {
    warning("only first element of what is used")
  }

  if (!how %in% c("screen", "html", "grob")) {
    stop("how can only be screen, html or grob")
  }

  if (how == "screen") {
    cat(paste0("class                    : ", class(x)),
        paste0("asymptote calculated for : ", paste0(x$asymtotes$level, collapse=",")),
        paste0("asypotote reached at     : ", paste0(x$asymtotes$ns, collapse=",")),
        sep="\n")
  }

  if (how == "html") {

  }
}


#' plot for RhrHRAsymptote
#' 
#' generic plot for RhrHREstimator
#' @usage plot(x, ...)
#' @aliases plot plot.RhrHRAsymptote
#' @param x RhrHRAsymptote
#' @param draw indicates whether the plot should be drawn or not. If this is  \code{FALSE} a grob is returned.
#' @param ... none implemented
#' @method plot RhrHRAsymptote
#' @export

plot.RhrHRAsymptote <- function(x, draw=TRUE, ...) {

  ## Input checks
  ## to be completed
  require(ggplot2)
  require(reshape2)
  
  cc <- melt(x$confints, id=c("level", "ns"))

  ## totalA
  totalA <- x$totalA
  tolTotArea <- x$params$tolTotArea
  ns <- x$params$ns
  
  dd <- data.frame(xx=rep(unique(x$params$ns), length(unique(x$hrAreas$level))),
                   ymin=rep(totalA$lower, each=length(unique(ns))),
                   ymax=rep(totalA$upper, each=length(unique(ns))),
                   level=rep(totalA$level, each=length(unique(ns))))
  
  ## When were the asymtotes reaches?
  asymR <- x$asymtotes
  asymR <- asymR[complete.cases(asymR), ]

  
  
  p <- ggplot(x$hrAreas, aes(x=ns, y=area, group=ns)) +
    geom_point(alpha=0.5) +  
      geom_ribbon(data=dd, aes(x=xx, ymin=ymin, ymax=ymax, group=NULL, y=NULL, alpha=0.4)) +
        geom_hline(aes(yintercept=area), linetype="dashed", data=totalA)  +
          geom_line(aes(x=ns, y=value, group=variable), alpha=0.5, data=melt(x$confints, id.vars=c("level", "ns"))) 

  if (nrow(asymR) >= 1) {
    p <- p + geom_vline(aes(xintercept=ns), linetype="solid", colour="red", data=asymR) 
  }

  p <- p + facet_wrap(~level, ncol=2, scale="free_y") +
    theme_bw() +
      theme(legend.position="none") +
        labs(x="Number of points", y="Area")

  if (draw) {
    print(p)
  } else {
    return(p)
  }
}

