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


      resCa[[i]]$kde <- rhrKDE(xy=x[, c("lon", "lat")], h=h, buffer=as.numeric(config$estimator$kde$buffer), rescale=kde$rescale, res=as.numeric(kde$resolution))


### Needs some cleaning up
      a <- try(resCa[[i]]$kdeCa <- rhrCoreArea(resCa[[i]]$kde))
      if (is(a, "try-error")) {
        cat(a)
      }

      p <- plot(resCa[[i]]$kdeCa)

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

      if (config$config$expTIF) {
      ## Save CA as *.tif
        writeRaster(resCa[[i]]$kdeCa$rast, file=file.path(datapath, caFilenameTIF[i]), overwrite=TRUE)
      }
### debug
      cat("================================================ \n", file=stderr())
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

} else {
  res$write(rhrAlert("Core area not requested"))
}
res$finish()

%>
