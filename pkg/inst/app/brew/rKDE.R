<%

res <- Response$new()
if (config$todo$doKDE) {

  kde <- config$estimator$kde
  kdeLevels <- rhrCorrectLevels(kde$level)

  ## figure out bandwidth
  if (kde$bandwidth == "user") {
    h <- as.numeric(kde$bandwidthValue)
  } else {
    h <- kde$bandwidth
  }

  ## run analysis
  resKDEs <- lapply(seq_along(datSub), function(i) {

    if (config$config$useGM) {
      dat <- SpatialPoints(datSub[[i]][, c('lon', 'lat')])
      proj4string(dat) <- CRS(paste0("+init=epsg:", config$config$epsg))
    } else {
      dat <- datSub[[i]][, c('lon', 'lat')]
    }
    rhrKDE(xy=dat,
           h=h,
           levels=kdeLevels,
           buffer=as.numeric(kde$buffer),
           rescale=kde$rescale,
           res=as.numeric(kde$resolution))})
             
  resKDEsUDs <- lapply(resKDEs, ud)
  resKDEsContours <- lapply(resKDEs, isopleths)

  ids <- names(datSub)
  kdeFilenamePlots <- paste0("rhr_KDE_id_ud_", ids, ".png")
  kdeFilenamePlotsContour <- paste0("rhr_KDE_contour_id_", ids, ".png")
  kdeFilenameTIF <- paste0("rhr_KDE_id_", ids, ".tif")
  kdeFilenameRda <- paste0("rhr_KDE_id_", ids, ".rda")
  kdeContourFilenameKML <- paste0("rhr_KDE_Contour_id_", ids, ".kml")
  kdeContourFilenameRda <- paste0("rhr_KDE_Contour_id_", ids, ".rda")
  kdeContourFilenameShp <- paste0("rhr_KDE_Contour_id_", ids, ".shp")
  kdePlots <- list()
  kdePlotsContours <- list()

  for (i in seq_along(resKDEs)) {

    p <- plot(resKDEs[[i]], what="ud", draw=FALSE)

    ## plot
    png(file=file.path(imagepath, kdeFilenamePlots[i]))
    print(p)
    dev.off()

    kdePlots[[i]] <- grid.grabExpr(print(p))

    ## Contour lines
    p <- plot(resKDEs[[i]], what="iso", draw=FALSE, useGE=config$config$useGM)

    ## Save plots
    png(file=file.path(imagepath, kdeFilenamePlotsContour[i]))
    print(p)
    dev.off()
    kdePlotsContours[[i]] <- grid.grabExpr(print(p))

    ## Save KDE as *.RData
    saveRDS(resKDEsUDs[[i]], file=file.path(datapath, kdeFilenameRda[i]))

    ## Save KDE as *.tif
    if (config$config$expTIF) {
      writeRaster(resKDEsUDs[[i]], file=file.path(datapath, kdeFilenameTIF[i]), overwrite=TRUE)
    }

    ## Save contourLines
    saveRDS(resKDEsContours[[i]], file=file.path(datapath, kdeContourFilenameRda[i]))
    writePolyShape(resKDEsContours[[i]], fn=file.path(datapath, kdeContourFilenameShp[i]))

    ## KML
    if (config$config$expKML) {
        tmcp <- resKDEsContours[[i]] 
        proj4string(tmcp) <- CRS(paste0("+init=epsg:", config$config$epsg))
        tmcp <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
        writeOGR(tmcp, file.name=file.path(datapath, kdeContourFilenameKML[i]), layer="level", driver="KML") 
      }
  }

  ## ---------------------------------------------------------------------------- #
  ## add to report
  res$write(p(paste0("Kernel density estimation (KDE) is one of the most widely used methods to calculate home ranges. In the first step a kernel density estimation is calculated. From the kernel density a utility distribution (UD) is calculated. The resolution was <code>", config$estimator$kde$resolution,  "</code>, the bounding box of relocations was buffered with <code>", config$estimator$kde$buffer, "</code> units and band width was selected through <code>", config$estimator$kde$bandwidth, "</code>.")))
  res$write(cat("<hr>"))

  for (i in seq_along(resKDEs)) {
    res$write(h3(paste0("KDE for ", ids[i])))
    res$write(p(paste0("The bandwidth value was <code>", round(resKDEs[[i]]$parameters$h[1], 2), "</code>.")))

    res$write(img(paste0(imageurl, kdeFilenamePlots[i]), cap=""))
    res$write(img(paste0(imageurl, kdeFilenamePlotsContour[i]), cap=""))

    tt <- data.frame(resKDEsContours[[i]])
    tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
    tt <- tt[, c("level", "area")]
    names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))

    res$write(toHTML(tt))
  }
} else {
  res$write(rhrAlert("KDE not requested"))
}
res$finish()

%>
