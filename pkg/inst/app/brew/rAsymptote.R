<%

res <- Response$new()
if (config$todo$doAsymptote) {

  asym <- config$preAnalysis$asymptote
  asym$level <- rhrCorrectLevels(asym$level)
  resAsym <- list()

  for (i in 1:length(datSub)) {
    x <- datSub[[i]]
    resAsym[[i]] <- list()

    ## ============================================================================ # 
    ## Are there sufficient points
    
    if (nrow(x) <= (2 * as.numeric(asym$minNPts))) {
      resAsym[[i]]$exit <- 1
      resAsym[[i]]$message <- "To few points"
      next 
    }

    ns <- seq(as.numeric(asym$minNPts), nrow(x), as.numeric(asym$increment))
    resAsym[[i]]$ns <- ns

    ## ---------------------------------------------------------------------------- #
    ## MCP

    if ("mcp" %in% asym$estimator) {
      resAsym[[i]]$mcp <- rhrMCP(x[, c("lon", "lat")], levels=asym$level)

### Needs some cleaning up
      resAsym[[i]]$mcpAsym <- try(rhrAsymptote(resAsym[[i]]$mcp,
                                               ns=ns,
                                               nrep=as.numeric(asym$nIter),
                                               tolTotArea=as.numeric(asym$tolTotArea)/100,
                                               nTimes=as.numeric(asym$nTimes),
                                               sampling=asym$sampling))
      if (!is(resAsym[[i]]$mcpAsym, "try-error")) {
        p <- plot(resAsym[[i]]$mcpAsym, draw=FALSE)
        resAsym[[i]]$mcpPlot <- grid.grabExpr(print(p))

        ## save plot
        png(file=file.path(imagepath, paste0("rhr_AsymptotePlot_id_", ids[i], "_mcp.png")))
        print(p)
        dev.off()
      }
    }
    



    ## ---------------------------------------------------------------------------- #
    ## KDE

    if ("kde" %in% asym$estimator) {

      kde <- config$estimator$kde

      ## figure out bandwidth
      if (kde['bandwidth'] == "user") {
        h <- as.numeric(kde['bandwidthValue'])
      } else {
        h <- kde['bandwidth']
      }


      config$estimator$kde$xrange <- lapply(datSub, function(x) range(x[, "lon"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))
      config$estimator$kde$yrange <- lapply(datSub, function(x) range(x[, "lat"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))

      resAsym[[i]]$kde <- rhrKDE(x[, c("lon", "lat")], levels=asym$level, h=h, xrange=config$estimator$kde$xrange[[i]],
                                 yrange=config$estimator$kde$yrange, res=as.numeric(kde['resolution']))

### Needs some cleaning up
      resAsym[[i]]$kdeAsym <- try(rhrAsymptote(resAsym[[i]]$kde,
                                               ns=ns,
                                               nrep=as.numeric(asym$nIter),
                                               tolTotArea=as.numeric(asym$tolTotArea)/100,
                                               nTimes=as.numeric(asym$nTimes)))
      if (!is(resAsym[[i]]$kdeAsym, "try-error")) {
        p <- plot(resAsym[[i]]$kdeAsym, draw=FALSE)
        resAsym[[i]]$kdePlot <- grid.grabExpr(print(p))

        ## save plot
        png(file=file.path(imagepath, paste0("rhr_AsymptotePlot_id_", ids[i], "_kde.png")))
        print(p)
        dev.off()
      }

    }
  }


### Rewrite results
  res$write(p(paste0("Description of Asymptote")))


  for (i in 1:length(resAsym)) {
    res$write(h3(paste0("Asymptote  for ", ids[i])))
    if ("mcp" %in% asym$estimator) {
      res$write(h4("Minimum Convex Polygon Asymptote"))
      if (!is.null(resAsym[[i]]$mcpAsym)) {
        tt <- resAsym[[i]]$mcpAsym$asymtotes
        tt$ns <- ifelse(is.na(tt$ns), "not reached", tt$ns)
        names(tt) <- c("Level", "Number of Points")
        res$write(toHTML(tt))
        res$write(img(paste0(imageurl, paste0("rhr_AsymptotePlot_id_", ids[i], "_mcp.png")), cap="Asym for MCP"))
        resAsym[[i]]$mcpMsg <- ifelse(any(!is.na(resAsym[[i]]$mcpAsym$asymtotes$ns)), "Assymptote was reached",  "No assymptote reached")

      } else { 
        resAsym[[i]]$mcpMsg <- "Something went wrong, possibly not enough points"
        res$write(alertWarning("Something went wrong, possibly not enough points"))
      }
    }

    if ("kde" %in% asym$estimator) {
      res$write(h4("Kernel Density Estimation Asymptote"))
      if (!is.null(resAsym[[i]]$kdeAsym)) {
        tt <- resAsym[[i]]$kdeAsym$asymtotes
        tt$ns <- ifelse(is.na(tt$ns), "not reached", tt$ns)
        names(tt) <- c("Level", "Number of Points")
        res$write(toHTML(tt))
        resAsym[[i]]$kdeMsg <- ifelse(any(!is.na(resAsym[[i]]$kdeAsym$asymtotes$ns)), "Assymptote was reached",  "No assymptote reached")
        res$write(img(paste0(imageurl, paste0("rhr_AsymptotePlot_id_", ids[i], "_kde.png")), cap="Asym for KDE"))

      } else { 
        resAsym[[i]]$kdeMsg <- "Something went wrong, possibly not enough points"
        res$write(alertWarning("Something went wrong, possibly not enough points"))
      }
    }
  }

} else {
  res$write(alert("The home range asymptote was not requested"))
}
res$finish()

%>
