<%
res <- Response$new()
if (config$todo$doLocoh) {

  alog <- c(alog, catPro("Init locoh", pre=cath2("Local convex hull")))

  for (subcon in seq_along(ares$LoCoH)) {
    subconParams <- ares$LoCoH[[subcon]]$params


    for (animal in seq_along(ares$LoCoH[[subcon]]$animals)) {
      alog <- c(alog, catPro(paste0("starting with locoh for ", ares$LoCoH[[subcon]]$animals[[animal]]$name)))

       allgood <- tryCatch({

         ## figure out bandwidth
         if (subconParams$n) {
           if (subconParams$type == "k") {
             subconParams$nValue <- sqrt(nrow(x))
           } else if (subconParams$type == "a") {
             subconParams$nValue <- max(dist(datSub[[animal]][, c("lon", "lat")]))
           }
         } 

         locoh <- rhrLoCoH(datSub[[animal]], level=subconParams$level,
                           type=subconParams$type, n=subconParams$nValue)

         p <- grid.grabExpr(plot(locoh, what="iso", useGE=config$config$useGM))

         ares$LoCoH[[subcon]]$animals[[animal]]$plots <- list()
         ares$LoCoH[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_LoCoH_iso_id_",
                                                                    ares$LoCoH[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  caption=paste0("LoCoH ", ids[animal]))
         
         png(file=file.path(imagepath, ares$LoCoH[[subcon]]$animals[[animal]]$plots[[1]]$filename))
         grid.draw(p)
         dev.off()

         ## Extra params
         ares$LoCoH[[subcon]]$animals[[animal]]$extraParams <- list(nValue=subconParams$nValue)
         
         ## Table
         tt <- rhrArea(locoh)
         tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit,
                                                 config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
         tt <- tt[, c("level", "area")]
         names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
         ares$LoCoH[[subcon]]$animals[[animal]]$tables <- list()
         ares$LoCoH[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=tt, caption="Kernel density estimation areas")

         ## results
         saveRDS(locoh, file=file.path(datapath, paste0(paste0("rhr_LoCoH_id_", ares$LoCoH[[subcon]]$animals[[animal]]$name, ".rds"))))

         ## Write iso
         writeVect(isopleths(locoh),
                   basename=file.path(datapath, paste0("rhr_LoCoH_id_", ares$LoCoH[[subcon]]$animals[[animal]]$name)))
         
         rm(locoh, p)
         gc(); gc()

         
      }, error=function(e) return(e))

      if (inherits(allgood, "error")) {
        ares$LoCoH[[subcon]]$animals[[animal]]$exit <- 1
        ares$LoCoH[[subcon]]$animals[[animal]]$error <- allgood
      } else {
        ares$LoCoH[[subcon]]$animals[[animal]]$exit <- 0
      }
    }
  }


  
  alog <- c(alog, catPro("generating html output for locoh"))

  showResultHTML(ares$LoCoH, config$background$locoh)

} else {
  res$write(rhrAlert("LoCoH not requested"))
}
res$finish()

%>
