<%
res <- Response$new()
if (config$todo$doMCP) {

  alog <- c(alog, catPro("Init mcp", pre=cath2("Minimum convex polygon")))

  for (subcon in seq_along(ares$MCP)) {
    subconParams <- ares$MCP[[subcon]]$params

    for (animal in seq_along(ares$MCP[[subcon]]$animals)) {
      alog <- c(alog, catPro(paste0("starting with mcp for ", ares$MCP[[subcon]]$animals[[animal]]$name)))

      allgood <- tryCatch({

        mcp <- rhrMCP(datSub[[animal]], level=subconParams$level)

        ## Plot
        p <- grid.grabExpr(plot(mcp, useGE=config$config$useGM, what="iso"), warp=TRUE)


        ares$MCP[[subcon]]$animals[[animal]]$plots <- list()
        ares$MCP[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_MCP_id_",
                                                                  ares$MCP[[subcon]]$animals[[animal]]$name, ".png"),
                                                                caption=paste0("Minimum Convex Polygon for animal ", ids[animal]))

        png(file=file.path(imagepath, ares$MCP[[subcon]]$animals[[animal]]$plots[[1]]$filename))
        grid.draw(p)
        dev.off()
        
        ## Tables
        tt <- rhrArea(mcp)
        tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit, config$config$outUnit), 2),
                           big.mark=",", format="f", drop0trailing = TRUE)
        names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
        ares$MCP[[subcon]]$animals[[animal]]$tables <- list()
        ares$MCP[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=tt, caption="Mininum Convex Polygon areas")

        ## results
        saveRDS(mcp, file=file.path(datapath, paste0(paste0("rhr_MCP_id_", ares$MCP[[subcon]]$animals[[animal]]$name, ".rds"))))

        ## Write iso
        writeVect(isopleths(mcp),
                  basename=file.path(datapath, paste0("rhr_MCP_iso_id_", ares$MCP[[subcon]]$animals[[animal]]$name)))
        
        rm(mcp, p)
        gc(); gc()

      }, error=function(e) return(e))

      if (inherits(allgood, "error")) {
        ares$MCP[[subcon]]$animals[[animal]]$exit <- 1
        ares$MCP[[subcon]]$animals[[animal]]$error <- allgood
      } else {
        ares$MCP[[subcon]]$animals[[animal]]$exit <- 0
      }
    }
  }

  alog <- c(alog, catPro("generating html output for mcp"))

  showResultHTML(ares$MCP, config$background$mcp)

} else {
  res$write(rhrAlert("MCP not requested"))
}

res$finish()

%>
