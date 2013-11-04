<%

res <- Response$new()
if (config$todo$doCA) {

  alog <- c(alog, catPro("Init ca", pre=cath2("Core area")))

  for (subcon in seq_along(ares$CA)) {

    subconParams <- ares$CA[[subcon]]$params

    for (animal in seq_along(ares$CA[[subcon]]$animals)) {

      alog <- c(alog, catPro(paste0("starting with ca for ", ares$CA[[subcon]]$animals[[animal]]$name)))

      if (!is.null(ares$KDE[[1]]$animals[[animal]])) {

        allgood <- tryCatch({

          ca <- rhrCoreArea(ares$KDE[[subcon]]$animals[[animal]]$res)

          r1 <- data.frame(rasterToPoints(ca$rast))
          names(r1)[1:3] <- c("x", "y", "z")
          r1$z <- factor(r1$z)
          levels(r1$z) <- c("other", "core area")

          p2 <- ggplot(data=r1) + geom_tile(aes(x=x, y=y, fill=z)) +
            coord_equal() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
              labs(colour="", x=NULL, y=NULL, fill="") + theme_bw() +  scale_fill_manual(values=c("grey73", "#00BA38"))

          p1 <- grid.grabExpr(print(plot(ca)), wrap=TRUE)
          p2 <- grid.grabExpr(print(p2))

          ares$CA[[subcon]]$animals[[animal]]$plots <- list()
          ares$CA[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_CA_plot1_id_",
                                                                    ares$CA[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  grob=p1,
                                                                  caption=paste0("CA ", ids[animal]))

          ares$CA[[subcon]]$animals[[animal]]$plots[[2]] <- list(filename=paste0("rhr_CA_plot2_id_",
                                                                    ares$CA[[subcon]]$animals[[animal]]$name, ".png"),
                                                                  grob=p2,
                                                                  caption=paste0("CA ", ids[animal]))
          ## res
          ares$CA[[subcon]]$animals[[animal]]$res <- ca

          ## data
          ares$CA[[subcon]]$animals[[animal]]$data <- list()
          ares$CA[[subcon]]$animals[[animal]]$data$rast <- list()
          ares$CA[[subcon]]$animals[[animal]]$data$rast[[1]] <- list(data=ca$rast,
                                                                      filename=paste0("rhr_CA_id_",
                                                                        ares$CA[[subcon]]$animals[[animal]]$name))

        }, error=function(e) return(e))

        if (inherits(allgood, "error")) {
          ares$CA[[subcon]]$animals[[animal]]$exit <- 1
          ares$CA[[subcon]]$animals[[animal]]$error <- allgood
        } else {
          ares$CA[[subcon]]$animals[[animal]]$exit <- 0
        }
      } else {
        ares$CA[[subcon]]$animals[[animal]]$exit <- 1
        ares$CA[[subcon]]$animals[[animal]]$error <- "No estimator available"
      }
    }

  }
    
  alog <- c(alog, catPro("generating html output for ca"))

  showResultHTML(ares$CA, config$background$ca)

} else {
  res$write(rhrAlert("Core area not requested"))
}
res$finish()

%>
