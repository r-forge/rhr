<%

res <- Response$new()
if (config$todo$doAsymptote) {

  alog <- c(alog, catPro("Init asymptote", pre=cath2("Home range asymptote")))

  for (subcon in seq_along(ares$Asymptote)) {
    subconParams <- ares$Asymptote[[subcon]]$params

    for (animal in seq_along(ares$Asymptote[[subcon]]$animals)) {
      alog <- c(alog, catPro(paste0("starting with asymptote for ", ares$Asymptote[[subcon]]$animals[[animal]]$name)))

      ## Check if there are enough points
      if (nrow(datSub[[animal]]) <= (2 * as.numeric(subconParams$minNPts))) {
        ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 1
        ares$Asymptote[[subcon]]$animals[[animal]]$error <- "Not enough relocations"
        next 
      }

      ns <- seq(as.numeric(subconParams$minNPts), nrow(datSub[[animal]]), as.numeric(subconParams$increment))

      if (subconParams$estimator == "mcp") {

        if (!is.null(ares$MCP[[1]]$animals[[animal]])) {

          allgood <- tryCatch({
            ## est asym 
            asym <- rhrAsymptote(ares$MCP[[1]]$animals[[animal]]$res, ns=ns,
                                 nrep=as.numeric(subconParams$nIter),
                                 tolTotArea=as.numeric(subconParams$tolTotArea)/100,
                                 nTimes=as.numeric(subconParams$nTimes),
                                 sampling=subconParams$sampling)

            ## Plot
            p <- grid.grabExpr(print(plot(asym, draw=FALSE)))

            ares$Asymptote[[subcon]]$animals[[animal]]$plots <- list()
            ares$Asymptote[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_Asymptote_mcp_id_",
                                                                            ares$Asymptote[[subcon]]$animals[[animal]]$name, ".png"),
                                                                          grob=p,
                                                                          caption=paste0("Asymptote for animal ", ids[animal]))
        
            ## Table
            tt <- asym$asymptote
            tt$ns <- ifelse(is.na(tt$ns), "not reached", tt$ns)
            names(tt) <- c("Level", "Number of Points")

            ares$Asymptote[[subcon]]$animals[[animal]]$tables <- list()
            ares$Asymptote[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=tt, caption="Asymptote")

          }, error=function(e) return(e))

          if (inherits(allgood, "error")) {
            ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 1
            ares$Asymptote[[subcon]]$animals[[animal]]$error <- allgood
          } else {
            ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 0
          }
        } else {
          ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 1
          ares$Asymptote[[subcon]]$animals[[animal]]$error <- "MCP not available"
        }
      }

      if (subconParams$estimator == "kde") {

        if (!is.null(ares$KDE[[1]]$animals[[animal]])) {

          allgood <- tryCatch({
            ## est asym 
            asym <- rhrAsymptote(ares$KDE[[1]]$animals[[animal]]$res, ns=ns,
                                 nrep=as.numeric(subconParams$nIter),
                                 tolTotArea=as.numeric(subconParams$tolTotArea)/100,
                                 nTimes=as.numeric(subconParams$nTimes),
                                 sampling=subconParams$sampling)

            ## Plot
            p <- grid.grabExpr(print(plot(asym, draw=FALSE)))

            ares$Asymptote[[subcon]]$animals[[animal]]$plots <- list()
            ares$Asymptote[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_Asymptote_kde_id_",
                                                                            ares$Asymptote[[subcon]]$animals[[animal]]$name, ".png"),
                                                                          grob=p,
                                                                          caption=paste0("Asymptote for animal ", ids[animal]))
        
            ## Table
            tt <- asym$asymptote
            tt$ns <- ifelse(is.na(tt$ns), "not reached", tt$ns)
            names(tt) <- c("Level", "Number of Points")

            ares$Asymptote[[subcon]]$animals[[animal]]$tables <- list()
            ares$Asymptote[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=tt, caption="Asymptote")

          }, error=function(e) return(e))

          if (inherits(allgood, "error")) {
            ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 1
            ares$Asymptote[[subcon]]$animals[[animal]]$error <- allgood
          } else {
            ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 0
          }
        } else {
          ares$Asymptote[[subcon]]$animals[[animal]]$exit <- 1
          ares$Asymptote[[subcon]]$animals[[animal]]$error <- "kde not available"
        }
      }  # finish kde
    }  # finish animals
  }  # finish subcon
        
  if (config$config$verbose) {
    cat("Generating HTML output for Asymptote \n", file=stderr())
  }

  showResultHTML(ares$Asymptote, config$background$asymptote)

  if (config$config$verbose) {
    cat("Generated HTML output \n", file=stderr())
  }

} else {
  res$write(rhrAlert("The home range asymptote was not requested"))
}
res$finish()

%>
