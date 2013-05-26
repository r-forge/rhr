<%

res <- Response$new()
if (config$todo$doKDE) {

  kde <- config$estimator$kde
  kdeLevels <- rhrCorrectLevels(kde$level)

  # figure out bandwidth
  if (kde$bandwidth == "user") {
    h <- as.numeric(kde$bandwidthValue)
  } else {
    h <- kde$bandwidth
  }

  config$estimator$kde$xrange <- lapply(datSub, function(x) range(x[, "lon"]) + rep(as.numeric(kde$buffer), 2) * c(-1, 1))
  config$estimator$kde$yrange <- lapply(datSub, function(x) range(x[, "lat"]) + rep(as.numeric(kde$buffer), 2) * c(-1, 1))


  # run analysis
  resKDEs <- lapply(seq_along(datSub), function(i)
                                rhrEstimator(xy=datSub[[i]][, c('lon', 'lat')], estimator="kde",
                                             h=h,
                                             xrange=config$estimator$kde$xrange[[i]],
                                             yrange=config$estimator$kde$yrange[[i]],
                                             resolution=as.numeric(kde$resolution)))
             

  resKDEsUDs <- lapply(resKDEs, function(x) rhrEstimatorCharacteristic(x, "area", as.numeric(kdeLevels), ud=TRUE)$ud)
  resKDEsContours <- lapply(resKDEs, function(x) rhrEstimatorCharacteristic(x, "area", as.numeric(kdeLevels), contour=TRUE)$contour)
  resKDEsAreas <- lapply(resKDEs, function(x) rhrEstimatorCharacteristic(x, "area", as.numeric(kdeLevels))$A)

  ids <- names(datSub)
  kdeFilenamePlots <- paste0("rhr_KDE_id_ud_", ids, ".png")
  kdeFilenamePlotsContour <- paste0("rhr_KDE_contour_id_", ids, ".png")
  kdeFilenameTIF   <- paste0("rhr_KDE_id_", ids, ".tif")
  kdeFilenameRda   <- paste0("rhr_KDE_id_", ids, ".rda")
  kdeContourFilenameKML   <- paste0("rhr_KDE_Contour_id_", ids, ".kml")
  kdeContourFilenameRda   <- paste0("rhr_KDE_Contour_id_", ids, ".rda")
  kdeContourFilenameShp   <- paste0("rhr_KDE_Contour_id_", ids, ".shp")
  kdePlots         <- list()
  kdePlotsContours         <- list()

  for (i in seq_along(resKDEs)) {

    # plot
    png(file=file.path(imagepath, kdeFilenamePlots[i]))
    fixes <- datSub[[i]]

    p <- rasterToPoints(resKDEsUDs[[i]])
    df <- data.frame(p)
    names(df) <- c("x", "y", "ud")
    br <- seq(min(df$ud), max(df$ud), len=8)

    p <- ggplot(data=df) +
      geom_tile(aes(x=x, y=y, fill=ud), alpha=0.7) +
      coord_equal() + scale_x_continuous(expand=c(0,0)) +
      scale_fill_gradient(low="darkblue", high="lightblue",
                          breaks=br) + 
      scale_y_continuous(expand=c(0,0)) + labs(x=NULL, y=NULL) 
    
    print(p)

    dev.off()

    kdePlots[[i]] <- grid.grabExpr(print(p))


    ## Contour lines
    if (config$config$useGM) {
      tempol <- resKDEsContours[[i]]
      proj4string(tempol) <- CRS(paste0("+init=epsg:", config$config$epsg))
      tempol <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
    } else {
      # plot
      tempol <- resKDEsContours[[i]]
    }

    tempol@data$id = rownames(tempol@data)
    tempol.points = fortify(tempol) #, region="id")
    tempol.df = join(tempol.points, tempol@data, by="id")



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
                scale_color_manual(values=terrain.colors(10)) + theme_bw()
    }

    # Save plots
    png(file=file.path(imagepath, kdeFilenamePlotsContour[i]))
    print(p)
    dev.off()
    kdePlotsContours[[i]] <- grid.grabExpr(print(p))

    # Save KDE as *.RData
    saveRDS(resKDEsUDs[[i]], file=file.path(datapath, kdeFilenameRda[i]))

    # Save KDE as *.tif
    writeRaster(resKDEsUDs[[i]], file=file.path(datapath, kdeFilenameTIF[i]), overwrite=TRUE)

    # Save contourLines
    saveRDS(resKDEsContours[[i]], file=file.path(datapath, kdeContourFilenameRda[i]))
    writePolyShape(resKDEsContours[[i]], fn=file.path(datapath, kdeContourFilenameShp[i]))

    # KML
    if (config$config$expKML) {
        tmcp <- resMCPs[[i]]$estimatorData 
        proj4string(tmcp) <- CRS(paste0("+init=epsg:", config$config$epsg))
        tmcp <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
        writeOGR(tmcp, file.name=file.path(datapath, kdeContourFilenameKML[i]), layer="level", driver="KML") 
  }

    # Save KDE as *.kml
   # if (config$config$expKML) {
   #   tkde <- resKDEsUDs[[i]]
   #   proj4string(tkde) <- CRS(paste0("+init=epsg:", config$config$epsg))
   #   data(SAGA_pal)
   #   kml(tkde, colour_scale=SAGA_pal[[1]], file.name=file.path(imagepath, kdeFilenameKML[i]))
   #   #plotKML(tkde, file.name=file.path(imagepath, kdeFilenameKML[i]))
   # }

    # Save KDE as *RData
    #resMCPspdf <- resMCPs[[i]]$estimatorData
    #save(resMCPspdf, file=file.path(imagepath, mcpFilenameDownload[i]))
  }

  # ---------------------------------------------------------------------------- #
  # add to report
  res$write(p(paste0("Kernel density estimation (KDE) is one of the most widely used methods to calculate home ranges. In the first steps a kernel density estimation is calculated. From the kernel density a utility distribution (UD) is calculated. The resolution was <code>", config$estimator$kde$resolution,  "</code>, the bounding box of relocations was buffered with <code>", config$estimator$kde$buffer, "</code> units and band width was selected through <code>", config$estimator$kde$bandwidth, "</code>.")))
  res$write(cat("<hr>"))

  for (i in seq_along(resKDEs)) {
    res$write(h3(paste0("KDE for ", ids[i])))
    res$write(p(paste0("The bandwidth value was <code>", round(resKDEs[[i]]$h$h[1], 2), "</code>.")))

    res$write(img(paste0(imageurl, kdeFilenamePlots[i]), cap=""))
    res$write(img(paste0(imageurl, kdeFilenamePlotsContour[i]), cap=""))

    tt <- data.frame(resKDEsAreas[[i]])
    tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
    names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))

    res$write(toHTML(tt))
  }
} else {
  res$write(alert("KDE not requested"))
}
res$finish()

%>
