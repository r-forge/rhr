<%

res <- Response$new()
if (config$todo$doTTSI & config$config$dateTime) {

  
  alog <- c(alog, catPro("Init ttsi", pre=cath2("Time to statistical independence")))

  for (subcon in seq_along(ares$TTSI)) {
    subconParams <- ares$TTSI[[subcon]]$params

    for (animal in seq_along(ares$TTSI[[subcon]]$animals)) {
      alog <- c(alog, catPro(paste0("starting with ttsi for ", ares$TTSI[[subcon]]$animals[[animal]]$name)))

       allgood <- tryCatch({

        ttsi <- rhrTTSI(datSub[[animal]][, c("lon", "lat", "timestamp")], interval=as.numeric(subconParams$interval),
                        ntimes=as.numeric(subconParams$ntimes), 
                        consec=as.logical(subconParams$consec))

        ## Plot
        p <-grid.grabExpr(plot(ttsi))

        ares$TTSI[[subcon]]$animals[[animal]]$plots <- list()
        ares$TTSI[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_TTSI_consec_", subconParams$consec, "_id_",
                                                                   ares$TTSI[[subcon]]$animals[[animal]]$name, ".png"),
                                                                 grob=p,
                                                                 caption=paste0("TTSI ", ids[animal]))
        ## Message
        if (ttsi$cvReached) {
          where <- ttsi$cvReachedAt
          msg <- paste0("Time to statistical independence was reached at ", where,
                                   " seconds, with ", ttsi$dat[which(ttsi$dat$interval == where), 'm'], " pairs")
        } else {
          msg <- paste0("Time to statistical independence was not reached")
        }

        ares$TTSI[[subcon]]$animals[[animal]]$msgs <- list()
        ares$TTSI[[subcon]]$animals[[animal]]$msgs[[1]] <- list(msg)
        
      }, error=function(e) return(e))

      if (inherits(allgood, "error")) {
        ares$TTSI[[subcon]]$animals[[animal]]$exit <- 1
        ares$TTSI[[subcon]]$animals[[animal]]$error <- allgood
      } else {
        ares$TTSI[[subcon]]$animals[[animal]]$exit <- 0
      }
    }
  }

  if (config$config$verbose) {
    cat("Generating HTML output for TTSI \n", file=stderr())
  }

  showResultHTML(ares$TTSI, config$background$ttsi)

  if (config$config$verbose) {
    cat("Generated HTML output \n", file=stderr())
  }

} else if (!config$config$dateTime) {
   res$write(rhrAlert("Time to statistical independence was requested, but information about date and time of relocations was not provided"))
} else {
   res$write(rhrAlert("Time to statistical independence was not requested"))
 }
res$finish()

%>
