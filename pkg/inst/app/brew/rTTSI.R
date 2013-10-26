<%

res <- Response$new()
if (config$todo$doTTSI & config$config$dateTime) {

  ttsi <- config$preAnalysis$ttsi

  ids <- names(datSub)

  ttsiFilenamePlots0 <- paste0("rhr_timeToStatisticalIndependencePlot_id_", ids, ".png")
  ttsiPlots <- list()


  resTTSI <- lapply(datSub, function(x) {
    a <- try(rhrTTSI(x[, c("lon", "lat", "timestamp")], interval=ttsi$interval))
    if (!inherits(a, "try-error")) {
      p <- grid.grabExpr(plot(a))
    } else {
      p <- NA
    }
    list(res=a, plot=p)
  })


  for (i in seq_along(resTTSI)) {
    ## Save plots
    if (!inherits(resTTSI[[i]]$res, "try-error")) {
      png(file=file.path(imagepath, ttsiFilenamePlots0[i]))
      grid.draw(resTTSI[[i]]$plot)
      dev.off()
      ttsiPlots[[i]] <- resTTSI[[i]]$plot
    }

 }



  # --------------------------------------------------------------------------- #
  # Add to report

  res$write(p(paste0("Time to statistical independence calculates at which time intervall Schoeners V reaches the expected value of 2. The x-axis on the plots shows the time intervall in seconds and on the y axis m indicates the number of pairs (i.e., the number segments of the trajectory) and Schoeners V is calcualted as..")))

  for (i in seq_along(resTTSI)) {
    res$write(h3(paste0("TTSI for ", ids[i])))
    if (!inherits(resTTSI[[i]]$res, "try-error")) {
      if (resTTSI[[i]]$res$cvReached) {
        where <- resTTSI[[i]]$res$cvReachedAt
        resTTSI[[i]]$msg <- paste0("Time to statistical independence was reached at ",
                                   where,
                                   " seconds, with ", resTTSI[[i]]$res$dat[which(resTTSI[[i]]$res$dat$interval == where), 'm'], " pairs")

      } else {
        resTTSI[[i]]$msg <- paste0("Time to statistical independence was not reached")
      }
      res$write(img(paste0(imageurl, ttsiFilenamePlots0[i]), cap=""))
    } else {
        resTTSI[[i]]$msg <- paste0("Something went wrong while calculating TTSI")
    }
    res$write(rhrAlert(resTTSI[[i]]$msg))
  }


} else if (!config$config$dateTime) {
   res$write(rhrAlert("Time to statistical independence was requested, but information about date and time of relocations was not provided"))
} else {
   res$write(rhrAlert("Time to statistical independence was not requested"))
 }
res$finish()

%>
