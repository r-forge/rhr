<%

res <- Response$new()
if (config$todo$doTTSI & config$config$dateTime) {

  ttsi <- config$preAnalysis$ttsi

  ids <- names(datSub)

  ttsiFilenamePlots0 <- paste0("rhr_timeToStatisticalIndependencePlot_id_", ids, ".png")
  ttsiPlots <- list()


  resTTSI <- lapply(datSub, function(x) {
    totalDiff <- diff(range(as.numeric(x$timestamp)))  # get difference between first and last relocation
    ints <- seq(ttsi$interval, totalDiff, ttsi$interval)  # create seq of from 0 total Diff
    resDiff <- list()


    nTimesAboveCriticalValue <- 0
    cvReached <- FALSE
    enoughM <- TRUE

      for (i in seq_along(ints)) {
        resDiff[[i]] <- rhrSchoener(x[, c('lon', 'lat', 'timestamp')], interval=ints[i], consec=ttsi$consec)


        if (is.na(resDiff[[i]]['V'])) {
          break
        }

        ## Check if V is above critical value
        if (resDiff[[i]]['V'] >= resDiff[[i]]['cv']) {
          nTimesAboveCriticalValue <- nTimesAboveCriticalValue + 1
        } else {
          nTimesAboveCriticalValue <- 0
        }

        if (nTimesAboveCriticalValue >= ttsi$ntimes | is.na(resDiff[[i]]['V'])) {
          cvReached <- TRUE
          break
        }

     }

 
    a <- data.frame(do.call("rbind", resDiff))
    a <- a[complete.cases(a), ]
    return(list(dat=a, interval=ints, cvReached=cvReached, cvReachedAt=ints[i]))
 })


  for (i in seq_along(resTTSI)) {
   # Create plots
   plot0 <- grid.grabExpr(plotTTSI(resTTSI[[i]]$dat, cvReached=resTTSI[[i]]$cvReached, cvReachedAt=resTTSI[[i]]$cvReachedAt))

   png(file=file.path(imagepath, ttsiFilenamePlots0[i]))
   grid.draw(plot0)
   dev.off()
   ttsiPlots[[i]] <- plot0

 }



  # --------------------------------------------------------------------------- #
  # Add to report

  res$write(p(paste0("Time to statistical independence calculates at which time intervall Schoeners V reaches the expected value of 2. The x-axis on the plots shows the time intervall in seconds and on the y axis m indicates the number of pairs (i.e., the number segments of the trajectory) and Schoeners V is calcualted as..")))

  for (i in seq_along(resTTSI)) {
    res$write(h3(paste0("TTSI for ", ids[i])))

    
    # was ttsi reached?
    if (resTTSI[[i]]$cvReached) {
      where <- resTTSI[[i]]$cvReachedAt
      resTTSI[[i]]$msg <- paste0("Time to statistical independence was reached at ",
                             where,
                            " seconds, with ", resTTSI[[i]]$dat[which(resTTSI[[i]]$dat$interval == where), 'm'], " pairs")

    } else {
      resTTSI[[i]]$msg <- paste0("Time to statistical independence was not reached")
    }

    res$write(img(paste0(imageurl, ttsiFilenamePlots0[i]), cap=""))
    res$write(alert(resTTSI[[i]]$msg))
  }


} else if (!config$config$dateTime) {
   res$write(alert("Time to statistical independence was requested, but information about date and time of relocations was not provided"))
} else {
   res$write(alert("Time to statistical independence was not requested"))
 }
res$finish()

%>
