<%

res <- Response$new()
if (config$todo$doTTSI) {
  if (config$config$dateTime) {

    
    alog <- c(alog, catPro("Init ttsi", pre=cath2("Time to statistical independence")))

    for (subcon in seq_along(ares$TTSI)) {
      subconParams <- ares$TTSI[[subcon]]$params

      for (animal in seq_along(ares$TTSI[[subcon]]$animals)) {
        alog <- c(alog, catPro(paste0("starting with ttsi for ", ares$TTSI[[subcon]]$animals[[animal]]$name)))

        allgood <- tryCatch({

          ttsi <- rhrTTSI(data.frame(datSub[[animal]])[, c("lon", "lat", "timestamp")], interval=as.numeric(subconParams$interval),
                          ntimes=as.numeric(subconParams$ntimes), 
                          consec=as.logical(subconParams$consec))

          ## Plot
          p <-grid.grabExpr(plot(ttsi))

          ares$TTSI[[subcon]]$animals[[animal]]$plots <- list()
          ares$TTSI[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_TTSI_consec_", subconParams$consec, "_id_",
                                                                     ares$TTSI[[subcon]]$animals[[animal]]$name, ".png"),
                                                                   caption=paste0("TTSI ", ids[animal]))

          png(file=file.path(imagepath, ares$TTSI[[subcon]]$animals[[animal]]$plots[[1]]$filename))
          grid.draw(p)
          dev.off()
          
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

          ## results
          saveRDS(ttsi, file=file.path(datapath, paste0(paste0("rhr_TTSI_id_", ares$TTSI[[subcon]]$animals[[animal]]$name, ".rds"))))

          rm(ttsi, p)
          gc(); gc()
          
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

  } else {
    res$write(rhrAlert("Time to statistical independence was requested, but information about date and time of relocations was not provided"))
  }
} else {
  res$write(rhrAlert("Time to statistical independence was not requested"))
}
res$finish()

%>
