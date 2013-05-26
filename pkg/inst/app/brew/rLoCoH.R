<%
res <- Response$new()
if (config$todo$doLocoh) {

    locoh <- config$estimator$locoh
    locohLevels <- rhrCorrectLevels(locoh$level)


    resLocohs <- lapply(datSub, function(x) {
      # determine k for each animal 
    if (locoh$n) {
        if (locoh$type == "k") {
          locoh$nValue <- sqrt(nrow(x))
        } else if (locoh$type == "a") {
          locoh$nValue <- max(dist(x[, c("lon", "lat")]))
        }
      } 

      try(rhrEstimator(x[, c('lon', 'lat')], estimator="locoh", level=locohLevels,
                   type=locoh$type, n=locoh$nValue))
    })

    ids <- names(datSub)

    locohFilenamePlots <- paste0("rhr_Locoh_id_", ids, ".png")
    locohFilenameSHP   <- paste0("rhr_Locoh_id_", ids, ".shp")
    locohFilenameKML   <- paste0("rhr_Locoh_id_", ids, ".kml")
    locohFilenameRda   <- paste0("rhr_Locoh_id_", ids, ".rda")
    locohPlots         <- list()


    for (i in seq_along(resLocohs)) {

      if (!is(resLocohs[[i]], "try-error")) {


        tempol <- resLocohs[[i]]$estimatorData

        # Set SRS correct if ggmap to be used
        if (config$config$useGM) {
          proj4string(tempol) <- CRS(paste0("+init=epsg:", config$config$epsg))
          tempol <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
        }

        tempol@data$id = rownames(tempol@data)
        tempol.points = fortify(tempol) #, region="id")
        tempol.df = join(tempol.points, tempol@data, by="id")

        png(file=file.path(imagepath, locohFilenamePlots[i]))

        
        if (config$config$useGM) {

          bb <- bbox(tempol)
          lon <- bb[1,]
          lat <- bb[2,]

          lon2x <- function(lon) lon * pi/180 * 6378137
          lat2y <- function(lat) log(tan(lat * (pi/180)/2 + pi/4)) * 6378137 

          x <- lon2x(extendrange(lon, f=0.03))
          y <- lat2y(extendrange(lat, f=0.03))


          d <- sqrt(diff(x)^2 + diff(y)^2)
          levels <- 1:21

          # http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
          zooms <- 40075000/256 * cos(0) / 2^levels * 256 
          l <- which(abs(zooms - d) == min(abs(zooms - d))) + 1

          p <- ggmap(get_googlemap(center=c(mean(lon), mean(lat)), zoom=l, maptype="hybrid")) + 
            geom_path(size=3, alpha=0.4, data=tempol.df, aes(x=long, y=lat, group=group, color=factor(level))) +
              labs(colour="Level", x="lon", y="lat") +
                geom_path(size=0.2, colour="black", data=tempol.df, aes(x=long, y=lat, group=group)) +
                  scale_color_manual(values=terrain.colors(10)) +
                    theme_bw()
          
        } else {

          p <- ggplot(tempol.df, aes(x=long, y=lat, group=group, color=factor(level))) + 
            geom_point(data=datSub[[i]], aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) + 
              geom_path(size=3, alpha=0.4) +
                labs(colour="Level", x="lon", y="lat") +
                  geom_path(size=0.2, colour="black") +
                    scale_color_manual(values=terrain.colors(10)) + theme_bw() + coord_equal()
        }

        print(p)
        dev.off()
        locohPlots[[i]] <- grid.grabExpr(print(p))

        # Save RDS
        saveRDS(resLocohs[[i]]$estimatorData, file=file.path(datapath, locohFilenameRda[i]))
        # Save Shp
        writePolyShape(resLocohs[[i]]$estimatorData, fn=file.path(datapath, locohFilenameSHP[i]))

        # KML
        if (config$config$expKML) {
          tlocoh <- resLocohs[[i]]$estimatorData 
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
        # NEEDS some more work
         res$write(p(paste0("Local Convex Hull type of <code>", resLocohs[[i]]$estimator$type, "</code> was calculated with value of <code>",  formatC(round(resLocohs[[i]]$estimator$n, 2), big.mark=",", format="f", drop0trailing = TRUE), "</code> ", config$config$inUnit, ".")))

        # res$write(cat(str(resLocohs[[i]]$estimator)))


        res$write(img(paste0(imageurl, locohFilenamePlots[i]), cap=""))

        
        tt <- data.frame(resLocohs[[i]]$estimatorData)
        tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
         names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
        res$write(toHTML(data.frame(tt)))
      } else {
        alertError(resLocohs[[i]])
      }
    }

} else {
  res$write(alert("Locoh not requested"))
}
res$finish()

%>
