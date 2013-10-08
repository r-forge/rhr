<%
res <- Response$new()
if (config$todo$doLocoh) {

    locoh <- config$estimator$locoh
    locohLevels <- rhrCorrectLevels(locoh$level)


    resLocohs <- lapply(datSub, function(x) {
    ## determine k for each animal 
    if (locoh$n) {
        if (locoh$type == "k") {
          locoh$nValue <- sqrt(nrow(x))
        } else if (locoh$type == "a") {
          locoh$nValue <- max(dist(x[, c("lon", "lat")]))
        }
      } 
    if (config$config$useGM) {
      dat <- SpatialPoints(x[, c("lon", "lat")])
      proj4string(dat) <- CRS(paste0("+init=epsg:", config$config$epsg))
    } else {
      dat <- x[, c("long", "lat")]
    }

      try(rhrLoCoH(dat[, c('lon', 'lat')], level=locohLevels, type=locoh$type, n=locoh$nValue))
    })

    ids <- names(datSub)

    locohFilenamePlots <- paste0("rhr_Locoh_id_", ids, ".png")
    locohFilenameSHP   <- paste0("rhr_Locoh_id_", ids, ".shp")
    locohFilenameKML   <- paste0("rhr_Locoh_id_", ids, ".kml")
    locohFilenameRda   <- paste0("rhr_Locoh_id_", ids, ".rda")
    locohPlots         <- list()


    for (i in seq_along(resLocohs)) {

      if (!is(resLocohs[[i]], "try-error")) {

        p <- plot(resLocohs[[i]], useGE=config$config$useGM, what="iso", draw=FALSE)

        png(file=file.path(imagepath, locohFilenamePlots[i]))


        print(p)
        dev.off()
        locohPlots[[i]] <- grid.grabExpr(print(p))

        ## Save RDS
        saveRDS(isopleths(resLocohs[[i]]), file=file.path(datapath, locohFilenameRda[i]))

        ## Save Shp
        writePolyShape(isopleths(resLocohs[[i]]), fn=file.path(datapath, locohFilenameSHP[i]))

        ## KML
        if (config$config$expKML) {
          tlocoh <- isopleths(resLocohs[[i]])
          proj4string(tlocoh) <- CRS(paste0("+init=epsg:", config$config$epsg))
          tlocoh <- spTransform(tlocoh, CRS("+proj=longlat +ellps=sphere +no_defs"))
          writeOGR(tlocoh, file.name=file.path(datapath, locohFilenameKML[i]), layer="level", driver="KML") 
        }
      }
    }


    res$write(p(paste0("Local Convex Hull is a hull based method, where minimum convex polygons are calculated for each point with a given set of neighbors. There are different methods available to determine which neighbors to be used.")))

    for (i in seq_along(resLocohs)) {
      res$write(h3(paste0("Locoh for ", ids[i])))

      if (!is(resLocohs[[i]], "try-error")) {
         res$write(p(paste0("Local Convex Hull type of <code>", resLocohs[[i]]$parameters$type, "</code> was calculated with value of <code>",  formatC(round(resLocohs[[i]]$parameters$n, 2), big.mark=",", format="f", drop0trailing = TRUE), "</code> ", config$config$inUnit, ".")))


        res$write(img(paste0(imageurl, locohFilenamePlots[i]), cap=""))

        tt <- data.frame(isopleths(resLocohs[[i]]))
        tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
         names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
        res$write(toHTML(data.frame(tt)))
      } else {
        alertError("Something went wrong, did you provide enough points?")
      }
    }

} else {
  res$write(alert("LoCoH not requested"))
}
res$finish()

%>
