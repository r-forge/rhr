<%

res <- Response$new()
if (config$todo$doAsymptote) {

  asym <- lapply(config$preAnalysis$asymptote, function(x) tryCatch(as.numeric(x), warning=function(w) x))
  asym$level <- rhrCorrectLevels(asym$level)

  resAsym <- list()


  for (i in 1:length(datSub)) {
    x <- datSub[[i]]
    resAsym[[i]] <- list()

    # ============================================================================ # 
    # Are there sufficient points
    
    if (nrow(x) <= (2 * as.numeric(asym$minNPts))) {
      resAsym[[i]]$exit <- 1
      resAsym[[i]]$message <- "To few points"
      next 
    }

    ns <- rep(seq(as.numeric(asym$minNPts), nrow(x), as.numeric(asym$increment)), each=as.numeric(asym$nIter))
    resAsym[[i]]$ns <- ns

    # ---------------------------------------------------------------------------- #
    # MCP

    if ("mcp" %in% asym$estimator) {
      resAsym[[i]]$mcp <- do.call("rbind", lapply(ns, function(n) data.frame(rhrEstimator(x[sample(1:nrow(x), n), c('lon', 'lat')], estimator="mcp", levels=asym$level)$estimatorData)))

      # Calculate where the asymptote was reached
      # Area with all points:
      totalA <- data.frame(rhrEstimator(x[, c("lon", "lat")], "mcp", level=asym$level)$estimatorData)

      df <- data.frame(ns=rep(ns, each=length(asym$level)), level=resAsym[[i]]$mcp[,1], area=resAsym[[i]]$mcp[,2])

      # calculate confidence intervals
      confints <- ddply(df, c("level", "ns"), function(x) t.test(x$area)$conf.int) 
      confints <- split(confints, confints$level)

      # figure out when the Asymptote was reached at each level j
      # The asymptote is reached when the both CI are inside the area tolerance for x times
      asymReached <- sapply(seq_along(confints), function(j) {
        # check if is inside
	isInside <- rle((confints[[j]]$V1 >= totalA[j,'area'] * (as.numeric(asym$tolTotArea) / 100))  &
                        confints[[j]]$V2 <= (totalA[j,'area'] * (2 - ((as.numeric(asym$tolTotArea) / 100)))))

	whereInside <- with(isInside, which(lengths >= asym$nTimes & values))[1]

        # why the NA?
	if (length(whereInside) > 0 & !is.na(whereInside)) {
          unique(ns)[sum(isInside$lengths[1:(whereInside-1)]) + 1]
	} else {
          NA
        }
      })

      resAsym[[i]]$asymReachedAt <- asymReached <- data.frame(level=asym$level, ns=asymReached, area=totalA$area) 
      

     # # plot
     cc <- melt(do.call("rbind", confints), id=c("level", "ns"))
     
     dd <- data.frame(xx=rep(unique(ns), length(unique(totalA$level))),
                      ymin=rep(totalA$area * (asym$tolTotArea/100), each=length(unique(ns))),
                      ymax=rep(totalA$area * (2 - (asym$tolTotArea /100)), each=length(unique(ns))),
                      level=rep(totalA$level , each=length(unique(ns))))
     


      p <- ggplot(df, aes(x=ns, y=area, group=ns)) +
        geom_hline(aes(yintercept=area), linetype="dashed", data=totalA) + 
        geom_ribbon(data=dd, aes(x=xx, ymin=ymin, ymax=ymax, group=NULL, y=NULL, alpha=0.4)) + 
        geom_line(aes(alpha=0.5))  + 
        geom_line(aes(x=ns, y=value, group=variable), data=cc)

      if (nrow(asymReached[!is.na(asymReached$ns),]) > 0) {
        p <- p + geom_vline(aes(xintercept=ns), linetype="dashed", data=asymReached) + 
        geom_point(aes(x=ns, y=area), colour="red", data=asymReached)  
      }
        p <- p + facet_wrap(~level, ncol=2, scale="free_y") +
        theme_bw() +
        theme(legend.position="none") +
        labs(x="Number of points", y="Area")

      resAsym[[i]]$mcpPlot <- grid.grabExpr(print(p))

                                        # save plot
      png(file=file.path(imagepath, paste0("rhr_AsymptotePlot_id_", ids[i], "_mcp.png")))
      print(p)
      dev.off()
    }

    # ---------------------------------------------------------------------------- #
    # KDE
    if ("kde" %in% asym$estimator) {

      kde <- config$estimator$kde

      # figure out bandwidth
      if (kde['bandwidth'] == "user") {
        h <- as.numeric(kde['bandwidthValue'])
      } else {
        h <- kde['bandwidth']
      }

      config$estimator$kde$xrange <- lapply(datSub, function(x) range(x[, "lon"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))
      config$estimator$kde$yrange <- lapply(datSub, function(x) range(x[, "lat"]) + rep(as.numeric(kde['buffer']), 2) * c(-1, 1))

      resAsym[[i]]$kde <- do.call("rbind", lapply(ns, function(n) rhrEstimatorCharacteristic(
        rhrEstimator(xy=x[sample(1:nrow(x), n), c('lon', 'lat')],
                     estimator="kde",
                     h=h,
                     xrange=config$estimator$kde$xrange[[i]],
                     yrange=config$estimator$kde$yrange[[i]],
                     resolution=as.numeric(kde['resolution'])), "area", asym$level)$A))

      df <- data.frame(ns=rep(ns, each=length(asym$level)), level=resAsym[[i]]$kde[,1], area=resAsym[[i]]$kde[,2])


      # Calculate where the asymptote was reached
      # Area with all points:
      totalA <- rhrEstimatorCharacteristic(
        rhrEstimator(x[, c("lon", "lat")], "kde", 
                     h=h,
                     xrange=config$estimator$kde$xrange[[i]],
                     yrange=config$estimator$kde$yrange[[i]],
                     resolution=as.numeric(kde['resolution'])), "area", asym$level)$A
      totalA <- data.frame(totalA)

      # calculate confidence intervals
      confints <- ddply(df, c("level", "ns"), function(x) t.test(x$area)$conf.int) 
      confints <- split(confints, confints$level)

      # figure out when the Asymptote was reached at each level j
      # The asymptote is reached when the both CI are inside the area tolerance for x times
      asymReached <- sapply(seq_along(confints), function(j) {
        # check if is inside
	isInside <- rle((confints[[j]]$V1 >= totalA[j,'area'] * (as.numeric(asym$tolTotArea) / 100))  &
                        confints[[j]]$V2 <= (totalA[j,'area'] * (2 - ((as.numeric(asym$tolTotArea) / 100)))))

	whereInside <- with(isInside, which(lengths >= asym$nTimes & values))[1]

        # why the NA?
	if (length(whereInside) > 0 & !is.na(whereInside)) {
          unique(ns)[sum(isInside$lengths[1:(whereInside-1)]) + 1]
	} else {
          NA
        }
      })

      resAsym[[i]]$asymReachedAt <- asymReached <- data.frame(level=asym$level, ns=asymReached, area=totalA$area) 
      

     # # plot
     cc <- melt(do.call("rbind", confints), id=c("level", "ns"))
     
     dd <- data.frame(xx=rep(unique(ns), length(unique(totalA$level))),
                      ymin=rep(totalA$area * (asym$tolTotArea/100), each=length(unique(ns))),
                      ymax=rep(totalA$area * (2 - (asym$tolTotArea /100)), each=length(unique(ns))),
                      level=rep(totalA$level , each=length(unique(ns))))
     

      p <- ggplot(df, aes(x=ns, y=area, group=ns)) +
        geom_hline(aes(yintercept=area), linetype="dashed", data=totalA) + 
        geom_ribbon(data=dd, aes(x=xx, ymin=ymin, ymax=ymax, group=NULL, y=NULL, alpha=0.4)) + 
        geom_line(aes(alpha=0.5))  + 
        geom_line(aes(x=ns, y=value, group=variable), data=cc)

      if (nrow(asymReached[!is.na(asymReached$ns),]) > 0) {
        p <- p + geom_vline(aes(xintercept=ns), linetype="dashed", data=asymReached) + 
        geom_point(aes(x=ns, y=area), colour="red", data=asymReached)  
      }
        p <- p + facet_wrap(~level, ncol=2, scale="free_y") +
        theme_bw() +
        theme(legend.position="none") +
        labs(x="Number of points", y="Area")

      resAsym[[i]]$kdePlot <- grid.grabExpr(print(p))

                                        # save plot
      png(file=file.path(imagepath, paste0("rhr_AsymptotePlot_id_", ids[i], "_kde.png")))
      print(p)
      dev.off()
    }


    # ---------------------------------------------------------------------------- #
    # locoh
    if ("locoh" %in% asym$estimator) {

     resAsym[[i]]$locoh <- NA

     #locoh <- config$estimator$locoh

     # # figure out n
     # if (locoh$n == "auto") {
     #   if (locoh$type == "k") {
     #     locoh$nValue <- sqrt(nrow(x))
     #   } else if (locoh$type == "a") {
     #     locoh$nValue <- max(dist(x[, c("lon", "lat")]))
     #   }
     # } 
     # resAsym[[i]]$locoh <- lapply(ns, function(n) data.frame(rhrEstimator(x[sample(1:nrow(x), n), c('lon', 'lat')], estimator="locoh", level=asym$level, type=locoh$type, n=locoh$nValue)$estimatorData))

     # df <- data.frame(ns=rep(ns, each=length(asym$level)), level=resAsym[[i]]$locoh[,1], area=resAsym[[i]]$locoh[,2])
     # p <- ggplot(df, aes(x=ns, y=area, group=ns)) +
     #   geom_line()  + 
     #   facet_wrap(~level, ncol=2, scale="free") +
     #   theme_bw() +
     #   theme(legend.position="none") +
     #   labs(x="Number of points", y="Area")

     # resAsym[[i]]$locohPlot <- grid.grabExpr(print(p))

     # png(file=file.path(imagepath, paste0("asym_", ids[i], "_locoh.png")))
     # print(p)
     # dev.off()
    }
    resAsym[[i]]$exit <- 0
    

  }

  # Rewrite results
  res$write(p(paste0("Description of Asymptote")))

  for (i in 1:length(resAsym)) {
    res$write(h3(paste0("Asymptote for ", ids[i])))

    if (resAsym[[i]]$exit != 0) {
      res$write(alertWarning(resAsym[[i]]$message))
      next
    }

    if (!is.null(resAsym[[i]]$mcp)) {
      res$write(p("For MCP the asymptote was reached after ..."))
      res$write(img(paste0(imageurl, paste0("rhr_AsymptotePlot_id_", ids[i], "_mcp.png")), cap="Asym for MCP"))
    }

    if (!is.null(resAsym[[i]]$kde)) {
      res$write(p("For KDE the asymptote was reached after ..."))
      res$write(img(paste0(imageurl, paste0("rhr_AsymptotePlot_id_", ids[i], "_kde.png")), cap="KDE"))
    }

  }
  


} else {
  res$write(alert("Asymptote not requested"))
}
res$finish()

%>
