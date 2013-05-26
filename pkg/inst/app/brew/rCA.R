<%

res <- Response$new()
if (config$todo$doCA) {

  ca <- config$estimator$ca

  resCAs <- lapply(datSub, function(x) { 
    
    # Create output grid
    res <- ca$res
    nr <- ceiling(diff(range(x$lon)) / res)
    nc <- ceiling(diff(range(x$lat)) / res)

    r1 <- raster(nrows=nr, ncols=nc, xmn=range(x$lon)[1], xmx=range(x$lon)[2], ymn=range(x$lat)[1], ymx=range(x$lat)[2])
    r1 <- rasterize(x[, c("lon", "lat")], r1, fun="count")

    obs <- getValues(r1)
    obs <- obs[!is.na(obs)]
    ranks <- order(obs)
    obs <- obs[order(obs)]
    prob <- obs / sum(obs)


    pctprob <- prob / max(prob)

    pctrange <- sapply(prob, function(x) sum(prob >= x) /length(prob))

    
    distancePointLine <- function(x, y, x1, y1, x2, y2) {

                                        # code from: http://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
      normalLength = ((x2 - x1)^2 + (y2 - y1)^2)
      abs((x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)) / normalLength

    }

    dd <- sapply(1:length(prob), function(i) distancePointLine(pctrange[i], pctprob[i], 0, 1, 1, 0))

    
    rast <- r1
    r1 <- data.frame(rasterToPoints(r1))
    names(r1)[1:3] <- c("x", "y", "z")
    r1$z <- as.numeric(r1$z >= obs[which.max(dd)])
    r1$z <- ifelse(is.na(r1$z), 0, r1$z)
    r1$z <- factor(r1$z)
    levels(r1$z) <- c("other", "core area")

    if (all(r1$z[1] == r1$z)) {
      return(list(exit=1, msg="All values are the same"))
    }

    p1 <- ggplot(data.frame(x=pctrange, y=pctprob), aes(x=x, y=y)) + geom_line() + xlim(c(0,1)) + ylim(c(0,1)) +
      geom_abline(intercept=1, slope=-1) + 
      geom_point(aes(x=x,y=y), colour="red", data=data.frame(x=pctrange[which.max(dd)], y=pctprob[which.max(dd)])) + 
      theme_bw() + labs(x="pctrange", y="pctprob")


    p2 <- ggplot(data=r1) + geom_tile(aes(x=x, y=y, fill=z)) +
      coord_equal() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + labs(colour="", x=NULL, y=NULL) + theme_bw()


    return(list(exit=0, plot1=p1, plot2=p2, rast=rast))
  })

  ids <- names(datSub)

  caFilenamePlots1 <-  paste0("rhr_coreArea_plot1_id_", ids, ".png")
  caFilenamePlots2 <- paste0("rhr_coreArea_plot2_id_", ids, ".png")
  caFilenameTIF   <-  paste0("rhr_coreArea_id_", ids, ".tif")
  caFilenameKML   <-  paste0("rhr_coreArea_id_", ids, ".kml")
  caFilenameRda   <-  paste0("rhr_coreArea_id_", ids, ".rda")
  caPlots1 <- list()
  caPlots2 <- list()

  for (i in seq_along(resCAs)) {

    if (resCAs[[i]]$exit == 0) {
      # plot
      png(file=file.path(imagepath, caFilenamePlots1[i]))
      print(resCAs[[i]]$plot1)
      dev.off()

      png(file=file.path(imagepath, caFilenamePlots2[i]))
      print(resCAs[[i]]$plot2)
      dev.off()

      caPlots1[[i]] <- grid.grabExpr(print(resCAs[[i]]$plot1))
      caPlots2[[i]] <- grid.grabExpr(print(resCAs[[i]]$plot2))

      # Save CA as *.RData
      saveRDS(resCAs[[i]]$rast, file=file.path(datapath, caFilenameRda[i]))

      # Save CA as *.tif
      writeRaster(resCAs[[i]]$rast, file=file.path(datapath, caFilenameTIF[i]), overwrite=TRUE)
    }
  }


  # --------------------------------------------------------------------------- #
  # Add to report
  res$write(p(paste0("Description of core area")))

  for (i in seq_along(resCAs)) {
    res$write(h3(paste0("CA for ", ids[i])))

    if (resCAs[[i]]$exit == 0) {
      res$write(img(paste0(imageurl, caFilenamePlots1[i])))
      res$write(img(paste0(imageurl, caFilenamePlots2[i])))
    } else {
      res$write(alertWarning(resCAs[[i]]$msg))
    }
  }


} else {
  res$write(alert("Core area not requested"))
}
res$finish()

%>
