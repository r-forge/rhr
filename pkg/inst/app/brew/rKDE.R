<%

res <- Response$new()
if (config$todo$doKDE) {

  alog <- c(alog, catPro("Init kde", pre=cath2("Kernel density estimation")))

  for (subcon in seq_along(ares$KDE)) {
    subconParams <- ares$KDE[[subcon]]$params

    for (animal in seq_along(ares$KDE[[subcon]]$animals)) {

      alog <- c(alog, catPro(paste0("starting with kde for ", ares$KDE[[subcon]]$animals[[animal]]$name)))

      allgood <- tryCatch({

         ## figure out bandwidth
         if (subconParams$bandwidth == "user") {
           h <- as.numeric(subconParams$bandwidthValue)
         } else {
           h <- subconParams$bandwidth
         }

         kde <- rhrKDE(datSub[[animal]], h=h, buffer=as.numeric(subconParams$buffer),
                       rescale=subconParams$rescale, res=as.numeric(subconParams$resolution), levels=subconParams$level)

         ud <- ud(kde)
         iso <- isopleths(kde)

         pud <- grid.grabExpr(plot(kde, what="ud"))
         piso <- grid.grabExpr(plot(kde, what="iso", useGE=config$config$useGM))

         ares$KDE[[subcon]]$animals[[animal]]$plots <- list()
         ares$KDE[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_KDE_ud_id_",
                                                                    ares$KDE[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  caption=paste0("KDE ", ids[animal]))
         png(file=file.path(imagepath, ares$KDE[[subcon]]$animals[[animal]]$plots[[1]]$filename))
         grid.draw(pud)
         dev.off()

         ares$KDE[[subcon]]$animals[[animal]]$plots[[2]] <- list(filename=paste0("rhr_KDE_iso_id_",
                                                                    ares$KDE[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  caption=paste0("KDE ", ids[animal]))

         png(file=file.path(imagepath, ares$KDE[[subcon]]$animals[[animal]]$plots[[2]]$filename))
         grid.draw(piso)
         dev.off()

         ## Extra params
         kp <- parameters(kde)
         ekp <- list(hx=kp$h[1], hy=kp$h[2])

         

         if (kp$method == "lscv") {
           ekp <- c(ekp, list(converged=as.character(kp$converged)))
           ares$KDE[[subcon]]$params$lscvWhichMin <- "global"
           ares$KDE[[subcon]]$params$lscvFailure="smallest"
         }
           
         ares$KDE[[subcon]]$animals[[animal]]$extraParams <- ekp
         
         ## Table
         tt <- rhrArea(kde)
         tt$area <- formatC(round(rhrConvertUnit(tt$area, config$config$inUnit,
                                                 config$config$outUnit), 2), big.mark=",", format="f", drop0trailing = TRUE)
         tt <- tt[, c("level", "area")]
         names(tt) <- c("Level", paste0("Area [", config$config$outUnit, "]"))
         ares$KDE[[subcon]]$animals[[animal]]$tables <- list()
         ares$KDE[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=tt, caption="Kernel density estimation areas")

         ## results
         saveRDS(kde, file=file.path(datapath, paste0(paste0("rhr_KDE_id_", ares$KDE[[subcon]]$animals[[animal]]$name, ".rds"))))

         ## Write iso
         writeVect(isopleths(kde),
                   basename=file.path(datapath, paste0("rhr_KDE_iso_id_", ares$KDE[[subcon]]$animals[[animal]]$name)))
                   
         ## Write ud
         writeRast(ud(kde),
                   basename=file.path(datapath, paste0("rhr_KDE_ud_id_", ares$KDE[[subcon]]$animals[[animal]]$name)))


         rm(kde, piso, pud)
         gc(); gc()

      }, error=function(e) return(e))

      if (inherits(allgood, "error")) {
        ares$KDE[[subcon]]$animals[[animal]]$exit <- 1
        ares$KDE[[subcon]]$animals[[animal]]$error <- allgood
      } else {
        ares$KDE[[subcon]]$animals[[animal]]$exit <- 0
      }
    }
  }


  
  alog <- c(alog, catPro("generating html output for kde"))

  showResultHTML(ares$KDE, config$background$kde)


} else {
  res$write(rhrAlert("KDE not requested"))
}
res$finish()

%>
