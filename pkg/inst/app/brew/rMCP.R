<%
res <- Response$new()
if (config$todo$doMCP) {

 # mcpLevels <- rhrCorrectLevels(config$estimator$mcp$level)

  resMCPs <- lapply(datSub, function(x) {
                                          if (config$config$useGM) {
                                            dat <- SpatialPoints(x[, c("lon", "lat")])
                                            proj4string(dat) <- CRS(paste0("+init=epsg:", config$config$epsg))
                                          } else {
                                            dat <- x[, c("lon", "lat")]
                                          }
                                          rhrMCP(dat, level=config$estimator$mcp$level)
                                        })

  mcpFilenamePlots <- paste0("rhr_MCP_id_", ids, ".png")
  mcpFilenameSHP <- paste0("rhr_MCP_id_", ids, ".shp")
  mcpFilenameKML <- paste0("rhr_MCP_id_", ids, ".kml")
  mcpFilenameRda <- paste0("rhr_MCP_id_", ids, ".rda")

  mcpPlots <- list()

  for (i in seq_along(resMCPs)) {

    p <- plot(resMCPs[[i]], useGE=config$config$useGM, what="iso", draw=FALSE)

    png(file=file.path(imagepath, mcpFilenamePlots[i]))
    print(p)
    dev.off()

    mcpPlots[[i]] <- grid.grabExpr(print(p), wrap=TRUE)

    ## Save RDS
    saveRDS(isopleths(resMCPs[[i]]), file=file.path(datapath, mcpFilenameRda[i]))
    ## Save Shp
    writePolyShape(isopleths(resMCPs[[i]]), fn=file.path(datapath, mcpFilenameSHP[i]))

    ## KML
    if (config$config$expKML) {
      tmcp <- isopleths(resMCPs[[i]])
      proj4string(tmcp) <- CRS(paste0("+init=epsg:", config$config$epsg))
      tmcp <- spTransform(tempol, CRS("+proj=longlat +ellps=sphere +no_defs"))
      writeOGR(tmcp, file.name=file.path(datapath, mcpFilenameKML[i]), layer="level", driver="KML") 
    }

  }

  ## --------------------------------------------------------------------------- #
  ## Add to report
  res$write(p(paste0("Minimum convex polygon (MCP) is one of the older methods to calculate home ranges. It been shown that MCP estimates for home ranges are biased and often overestimated. The points used within a MCP are determined by calculating the centroid of all points and then desired percentage of closest points are selected. With the selected points a MCP is calculated.")))

  res$write(cat('<hr>'))

  for (i in seq_along(resMCPs)) {
    res$write(h3(paste0("MCP for ", ids[i])))
    res$write(img(paste0(imageurl, mcpFilenamePlots[i]), cap=""))
    tt <- data.frame(isopleths(resMCPs[[i]]))
    
    tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
    names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
    res$write(toHTML(tt))
    
  }


} else {
  res$write(alert("MCP not requested"))
}
res$finish()

%>
