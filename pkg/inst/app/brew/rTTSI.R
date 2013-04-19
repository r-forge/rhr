<%

res <- Response$new()
if (config$todo$doTTSI) {

  ttsi <- config$preAnalysis$ttsi

  ids <- names(datSub)

  ttsiFilenamePlots0 <- paste0("rhr_timeToStatisticalIndependencePlot_id_", ids, ".png")
  ttsiPlots <- list()

  resTTSI <- lapply(datSub, function(x) {
    totalDiff <- diff(range(as.numeric(x$timestamp)))  # get difference between first and last relocation
    ints <- seq(ttsi$interval, totalDiff, ttsi$interval)  # create seq of from 0 total Diff
    resDiff <- list()

      for (i in seq_along(ints)) {
        resDiff[[i]] <- rhrSchoener(x[, c('lon', 'lat', 'timestamp')], interval=ints[i], tolerance=ttsi$tolerance, type=ttsi$type)

    #    if (FALSE) {
          if (resDiff[[i]]['V'] >= 2) {
            break
          }
    #    }
     }

 
    a <- data.frame(do.call("rbind", resDiff))
    return(list(dat=a, interval=ints))
 })


  for (i in seq_along(resTTSI)) {
   # Create plots
   plot0 <- grid.grabExpr(plotTTSI(resTTSI[[i]]$dat))

   png(file=file.path(imagepath, ttsiFilenamePlots0[i]))
   grid.draw(plot0)
   dev.off()
   ttsiPlots[[i]] <- plot0

 }



  # --------------------------------------------------------------------------- #
  # Add to report

  ## res$write(h2("Time to statistical independence"))
  res$write(p(paste0("Description of TTSI")))

  for (i in seq_along(resTTSI)) {
    res$write(h3(paste0("TTSI for ", ids[i])))
    # res$write(p(paste0("The critical value of 2 was reached for the first time with a time interval of ", resTTSI[which(resTTSI[, 'V'] >= 2)[1],'interval'], " seconds.")))
    res$write(img(paste0(imageurl, ttsiFilenamePlots0[i]), cap="TTSI"))
  }


} else {
   res$write(alert("Time to statistical independence was not requested"))
}
res$finish()

%>
