<%

res <- Response$new()
if (config$todo$doCA) {

  ca <- config$estimator$ca

  ## At the moment only kde
  ca$estimator <- "kde"

  resCa <- list()

  for (i in 1:length(datSub)) {
    x <- datSub[[i]]
    resCa[[i]] <- list()

    ## ---------------------------------------------------------------------------- #
    ## MCP

    ## To be implemented

    ## ---------------------------------------------------------------------------- #
    ## KDE

    if ("kde" %in% ca$estimator) {

      kde <- config$estimator$kde

      ## figure out bandwidth
      if (kde['bandwidth'] == "user") {
        h <- as.numeric(kde['bandwidthValue'])
      } else {
        h <- kde['bandwidth']
      }


      config$estimator$kde$xrange <- lapply(datSub, function(x) range(x[, "lon"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))
      config$estimator$kde$yrange <- lapply(datSub, function(x) range(x[, "lat"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))

      resCa[[i]]$kde <- rhrKDE(x[, c("lon", "lat")], h=h, xrange=config$estimator$kde$xrange[[i]],
                                 yrange=config$estimator$kde$yrange, res=as.numeric(kde['resolution']))

### Needs some cleaning up
      resCa[[i]]$kdeCa <- rhrCoreArea(resCa[[i]]$kde)

      p <- plot(resCa[[i]]$kdeCa)
      resCa[[i]]$kdePlot1 <- grid.grabExpr(print(p))

      r1 <- data.frame(rasterToPoints(resCa[[i]]$kdeCa$rast))

      names(r1)[1:3] <- c("x", "y", "z")
      r1$z <- factor(r1$z)
      levels(r1$z) <- c("other", "core area")

      p2 <- ggplot(data=r1) + geom_tile(aes(x=x, y=y, fill=z)) +
          coord_equal() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
            labs(colour="", x=NULL, y=NULL, fill="") + theme_bw() +  scale_fill_manual(values=c("grey73", "#00BA38"))

      resCa[[i]]$kdePlot1 <- grid.grabExpr(print(p))
      resCa[[i]]$kdePlot2 <- grid.grabExpr(print(p2))

      caFilenamePlots1 <-  paste0("rhr_coreArea_plot1_id_", ids, ".png")
      caFilenamePlots2 <- paste0("rhr_coreArea_plot2_id_", ids, ".png")
      caFilenameTIF   <-  paste0("rhr_coreArea_id_", ids, ".tif")
      caFilenameKML   <-  paste0("rhr_coreArea_id_", ids, ".kml")
      caFilenameRda   <-  paste0("rhr_coreArea_id_", ids, ".rda")

      ## plot
      png(file=file.path(imagepath, caFilenamePlots1[i]))
      print(p)
      dev.off()

      png(file=file.path(imagepath, caFilenamePlots2[i]))
      print(p2)
      dev.off()

      ## Save CA as *.RData
      saveRDS(resCa[[i]]$kdeCa$rast, file=file.path(datapath, caFilenameRda[i]))

      ## Save CA as *.tif
      writeRaster(resCa[[i]]$kdeCa$rast, file=file.path(datapath, caFilenameTIF[i]), overwrite=TRUE)
    }
  }

  ## --------------------------------------------------------------------------- #
  ## Add to report
  res$write(p(paste0("Core area is commonly estimated by a certain percentage home range level 93 (e.g., 25 % or 50 %).  However, Seaman & Powell (1996) and Laver & Kelly (2008) argued for an area independent core area estimation. Thus, we implemented the method of Seaman & Powell (1996) where the threshold for defining the core area is the inflction point when plotting cumulative percent home range against its probability of use.")))

  for (i in seq_along(resCa)) {
    res$write(h3(paste0("Core Area for ", ids[i])))

      res$write(img(paste0(imageurl, caFilenamePlots1[i])))
      res$write(img(paste0(imageurl, caFilenamePlots2[i])))
  }



 # ca <- config$estimator$ca

  # resCAs <- lapply(datSub, function(x) { 
    
    # Create output grid
  #  res <- ca$res
  #  
  #  r1 <- data.frame(rasterToPoints((ca.full <- rhrCoreArea(x[ , c("lon", "lat")], ca$res))$rast))
  #  rast <- r1$rast
  #  names(r1)[1:3] <- c("x", "y", "z")
  #  r1$z <- factor(r1$z)
  #  levels(r1$z) <- c("other", "core area")

  #  if (all(r1$z[1] == r1$z)) {
  #    return(list(exit=1, msg="All values are the same"))
  #  }

  #  p1 <- ggplot(data.frame(x=ca.full$pctrange, y=ca.full$pctprob), aes(x=x, y=y)) + geom_line() + xlim(c(0,1)) + ylim(c(0,1)) +
  #    geom_abline(intercept=1, slope=-1) + 
  #    geom_point(aes(x=x,y=y), colour="red", size=2,
  #               data=data.frame(x=ca.full$pctrange[which.max(ca.full$dist)], y=ca.full$pctprob[which.max(ca.full$dist)])) + 
  #    theme_bw() + labs(x="Fraction of Home Range", y="Fraction of maximum Relative Frequency")


  #  p2 <- ggplot(data=r1) + geom_tile(aes(x=x, y=y, fill=z)) +
  #    coord_equal() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  #      labs(colour="", x=NULL, y=NULL, fill="") + theme_bw()


  #  return(list(exit=0, plot1=p1, plot2=p2, rast=ca.full$rast))
  #})

  #ids <- names(datSub)

  #caFilenamePlots1 <-  paste0("rhr_coreArea_plot1_id_", ids, ".png")
  #caFilenamePlots2 <- paste0("rhr_coreArea_plot2_id_", ids, ".png")
  #caFilenameTIF   <-  paste0("rhr_coreArea_id_", ids, ".tif")
  #caFilenameKML   <-  paste0("rhr_coreArea_id_", ids, ".kml")
  #caFilenameRda   <-  paste0("rhr_coreArea_id_", ids, ".rda")
  #caPlots1 <- list()
  #caPlots2 <- list()

  #for (i in seq_along(resCAs)) {

  #  if (resCAs[[i]]$exit == 0) {
  #    # plot
  #    png(file=file.path(imagepath, caFilenamePlots1[i]))
  #    print(resCAs[[i]]$plot1)
  #    dev.off()

  #    png(file=file.path(imagepath, caFilenamePlots2[i]))
  #    print(resCAs[[i]]$plot2)
  #    dev.off()

  #    caPlots1[[i]] <- grid.grabExpr(print(resCAs[[i]]$plot1))
  #    caPlots2[[i]] <- grid.grabExpr(print(resCAs[[i]]$plot2))

  #    # Save CA as *.RData
  #    saveRDS(resCAs[[i]]$rast, file=file.path(datapath, caFilenameRda[i]))

  #    # Save CA as *.tif
  #    writeRaster(resCAs[[i]]$rast, file=file.path(datapath, caFilenameTIF[i]), overwrite=TRUE)
  #  }
  #}


  ## --------------------------------------------------------------------------- #
  ## Add to report
  #res$write(p(paste0("Core area is estimated as are commonly estimated by a certain percentage home range level 93 (e.g. 25 % or 50 %).  However, Seaman & Powell (1996) and Laver & Kelly (2008) argued 94 for an area independent core area estimation. Thus, we implemented the method of 95 Seaman & Powell (1996) where the threshold for dening the core area is the inection point when plotting cumulative percent home range against its probability of use.")))

  #for (i in seq_along(resCAs)) {
  #  res$write(h3(paste0("Core Area for ", ids[i])))

  #  if (resCAs[[i]]$exit == 0) {
  #    res$write(img(paste0(imageurl, caFilenamePlots1[i])))
  #    res$write(img(paste0(imageurl, caFilenamePlots2[i])))
  #  } else {
  #    res$write(alertWarning(resCAs[[i]]$msg))
  #  }
  #}


} else {
  res$write(alert("Core area not requested"))
}
res$finish()

%>
