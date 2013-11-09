<%
res <- Response$new()
if (config$todo$doSiteFidelity) {
  alog <- c(alog, catPro("Init site fidility", pre=cath2("Site fidelity")))

  for (subcon in seq_along(ares$siteFidelity)) {
    subconParams <- ares$siteFidelity[[subcon]]$params

    for (animal in seq_along(ares$siteFidelity[[subcon]]$animals)) {

      alog <- c(alog, catPro(paste0("starting with site fidelity for ", ares$siteFidelity[[subcon]]$animals[[animal]]$name)))

      allgood <-tryCatch({
        sf <- rhrFidelity(datSub[[animal]], n=subconParams$n)

        ## Plot
        ## msd
        p1 <- ggplot(data.frame(msd=sf$msd.sim), aes(factor("msd"), msd)) +
          geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) +
            theme_bw() + scale_x_discrete(name="MSD", breaks=NULL) +
              scale_y_continuous(name="MSD from Center of Activity (MSD)") + xlab("MSD")
        p1 <- p1 + geom_hline(aes(yintercept = yin, colour="red"), data.frame(yin=sf$msd.dat))

        ## Li
        p2 <- ggplot(data.frame(li=sf$li.sim), aes(factor("li"), li)) + geom_jitter(alpha=0.4) +
          geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="LI", breaks=NULL) +
            scale_y_continuous(name="Linearity Index (LI)") 
        p2 <- p2 + geom_hline(aes(yintercept = yin, colour="red"), data.frame(yin=sf$li.dat))

        p <-grid.grabExpr(grid.arrange(p1,p2, ncol=2, main="Site Fidelity"), warp=TRUE)

        ares$siteFidelity[[subcon]]$animals[[animal]]$plots <- list()
        ares$siteFidelity[[subcon]]$animals[[animal]]$plots[[1]] <- list(filename=paste0("rhr_siteFidelity_id_",
                                                                           ares$siteFidelity[[subcon]]$animals[[animal]]$name, ".png"),
                                                                         caption=paste0("Site fidelity plot", ids[animal]))

        png(file=file.path(imagepath, ares$siteFidelity[[subcon]]$animals[[animal]]$plots[[1]]$filename))
        grid.draw(p)
        dev.off()
        
        ## Tables
        msd.ci <- t.test(sf$msd.sim)$conf.int
        li.ci <- t.test(sf$li.sim)$conf.int
        sftable <- data.frame(Measure=c("LI", "MSD"),
                              Observed=c(sf$li.dat, sf$msd.dat),
                              LCI=c(li.ci[1], msd.ci[1]),
                              UCI=c(li.ci[2], msd.ci[2]), stringsAsFactors=FALSE)
        names(sftable)[3:4] <- c("LCI (95 %)", "UCI (95 %)")

        ares$siteFidelity[[subcon]]$animals[[animal]]$tables <- list()
        ares$siteFidelity[[subcon]]$animals[[animal]]$tables[[1]] <- list(table=sftable, caption="Sitefidelity results")


        ## msg
        if (sf$li.dat < li.ci[1] & sf$msd.dat < msd.ci[1]) {
          msg <- "The mean squared distance from the center of activity and the linearity index of the data are below the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can be assumed and calculation of a home range may be appropriate."
        } else {
          msg <- "Either the mean squared distance from the center of activity (MSD) or the linearity index (LI) of the data are above the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can not be assumed and calculation of home ranges may be problematic."
        }

        ares$siteFidelity[[subcon]]$animals[[animal]]$msgs <- list()
        ares$siteFidelity[[subcon]]$animals[[animal]]$msgs[[1]] <- msg

        ## results
        saveRDS(sf, file=file.path(datapath, paste0(paste0("rhr_SiteFidelity_id_", ares$siteFidelity[[subcon]]$animals[[animal]]$name, ".rds"))))

        rm(sf, p)
        gc(); gc()
        
      }, error=function(e) return(e))

      if (inherits(allgood, "error")) {
        ares$siteFidelity[[subcon]]$animals[[animal]]$exit <- 1
        ares$siteFidelity[[subcon]]$animals[[animal]]$error <- allgood
      } else {
        ares$siteFidelity[[subcon]]$animals[[animal]]$exit <- 0
      }
    }
  }

  showResultHTML(ares$siteFidelity, config$background$siteFidelity)

  alog <- c(alog, catPro("generating html output for site fidelity"))

} else {
  res$write(rhrAlert("Site fidelity not requested"))
}
res$finish()

%>
