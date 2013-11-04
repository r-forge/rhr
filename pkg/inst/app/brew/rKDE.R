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

         kde <- rhrKDE(datSub[[animal]][, c("lon", "lat")], h=h, buffer=as.numeric(subconParams$buffer),
                       rescale=subconParams$rescale, res=as.numeric(subconParams$resolution), levels=subconParams$level)

         ud <- ud(kde)
         iso <- isopleths(kde)

         pud <- grid.grabExpr(plot(kde, what="ud"))
         piso <- grid.grabExpr(plot(kde, what="iso", useGE=config$config$useGM))

         ares$KDE[[subcon]]$animals[[animal]]$plots <- list()
         ares$KDE[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_KDE_ud_id_",
                                                                    ares$KDE[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  grob=pud,
                                                                  caption=paste0("KDE ", ids[animal]))

         ares$KDE[[subcon]]$animals[[animal]]$plots[[2]] <- list(filename=paste0("rhr_KDE_iso_id_",
                                                                    ares$KDE[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  grob=piso,
                                                                  caption=paste0("KDE ", ids[animal]))

         ## results
         ares$KDE[[subcon]]$animals[[animal]]$res <- kde

         ## Extra params
         kp <- parameters(kde)
         ekp <- list(hx=kp$h[1], hy=kp$h[2])

         if (kp$method == "lscv") {
           ekp <- c(ekp, list(converged=kp$converged))
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

         ## Data
         ares$KDE[[subcon]]$animals[[animal]]$data <- list()
         ares$KDE[[subcon]]$animals[[animal]]$data$vect <- list()
         ares$KDE[[subcon]]$animals[[animal]]$data$vect[[1]] <- list(data=iso,
                                                                     filename=paste0("rhr_KDE_iso_id_",
                                                                       ares$KDE[[subcon]]$animals[[animal]]$name))
         ares$KDE[[subcon]]$animals[[animal]]$data$rast <- list()
         ares$KDE[[subcon]]$animals[[animal]]$data$rast[[1]] <- list(data=ud,
                                                                     filename=paste0("rhr_KDE_ud_id_",
                                                                       ares$KDE[[subcon]]$animals[[animal]]$name))

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
