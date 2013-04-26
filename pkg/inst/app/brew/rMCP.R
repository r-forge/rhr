<%
res <- Response$new()
if (config$todo$doMCP) {

  mcpLevels <- rhrCorrectLevels(config$estimator$mcp$level)

  resMCPs <- lapply(datSub, function(x) rhrEstimator(x[, c('lon', 'lat')], estimator="mcp", level=mcpLevels))

  mcpFilenamePlots <- paste0("rhr_MCP_id_", ids, ".png")
  mcpFilenameSHP <- paste0("rhr_MCP_id_", ids, ".shp")
  mcpFilenameKML <- paste0("rhr_MCP_id_", ids, ".kml")
  mcpFilenameRda <- paste0("rhr_MCP_id_", ids, ".rda")

  mcpPlots <- list()

  for (i in seq_along(resMCPs)) {

    if (config$config$useGM) {
      tempol <- resMCPs[[i]]$estimatorData
      proj4string(tempol) <- CRS(paste0("+init=epsg:", config$config$epsg))
      tempol <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
    } else {
      # plot
      tempol <- resMCPs[[i]]$estimatorData
    }

    tempol@data$id = rownames(tempol@data)
    tempol.points = fortify(tempol, region="id")
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
        geom_path(size=3, alpha=0.4, data=tempol.df, aes(x=long, y=lat, group=level, color=factor(level))) +
          labs(colour="MCP Level", x="lon", y="lat") +
            geom_path(size=0.2, colour="black", data=tempol.df, aes(x=long, y=lat, group=level)) +
              scale_color_manual(values=terrain.colors(10)) +
                theme_bw()
      
    } else {

      p <- ggplot(tempol.df, aes(x=long, y=lat, group=level, color=factor(level))) + 
        geom_point(data=datSub[[i]], aes(x=lon, y=lat, group=NULL, color=NULL), alpha=0.1) + 
          geom_path(size=3, alpha=0.4) +
            labs(colour="MCP Level", x="lon", y="lat") +
              geom_path(size=0.2, colour="black") +
                scale_color_manual(values=terrain.colors(10)) + theme_bw() +
                  coord_fixed()
    }

    png(file=file.path(imagepath, mcpFilenamePlots[i]))
    print(p)
    dev.off()

    mcpPlots[[i]] <- grid.grabExpr(print(p), wrap=TRUE)

    # Save RDS
    saveRDS(resMCPs[[i]]$estimatorData, file=file.path(datapath, mcpFilenameRda[i]))
    # Save Shp
    writePolyShape(resMCPs[[i]]$estimatorData, fn=file.path(datapath, mcpFilenameSHP[i]))

    # KML
    if (config$config$expKML) {
        tmcp <- resMCPs[[i]]$estimatorData 
        proj4string(tmcp) <- CRS(paste0("+init=epsg:", config$config$epsg))
        tmcp <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
        writeOGR(tmcp, file.name=file.path(datapath, mcpFilenameKML[i]), layer="level", driver="KML") 
  }

  }

  # --------------------------------------------------------------------------- #
  # Add to report
  res$write(p(paste0("Minimum convex polygon (MCP) is one of the older methods to calculate home ranges. It been shown that MCP estimates for home ranges are biased and often overestimated. The points used within a MCP are determined by calculating the centroid of all points and then desired percentage of closest points are selected. With the selected points a MCP is calculated.")))

  res$write(cat('<hr>'))

  for (i in seq_along(resMCPs)) {
    res$write(h3(paste0("MCP for ", ids[i])))
    res$write(img(paste0(imageurl, mcpFilenamePlots[i]), cap=""))
    tt <- data.frame(resMCPs[[i]]$estimatorData)
    tt$area <- formatC(round(tt$area, 2), big.mark=",", format="f", drop0trailing = TRUE)
    names(tt) <- c("Level", "Area")
    res$write(toHTML(tt))
    
  }


} else {
  res$write(alert("MCP not requested"))
}
res$finish()

%>
