<%
res <- Response$new()
if (config$todo$doSiteFidelity) {
  alog <- c(alog, catPro("Init site fidility", pre=cath2("Site fidelity")))

  for (subcon in seq_along(ares$siteFidelity)) {
    subconParams <- ares$siteFidelity[[subcon]]$params

    for (animal in seq_along(ares$siteFidelity[[subcon]]$animals)) {

      alog <- c(alog, catPro(paste0("starting with site fidelity for ", ares$siteFidelity[[subcon]]$animals[[animal]]$name)))

      ares$siteFidelity[[subcon]]$animals[[animal]]$exit <- tryCatch({
        sf <- rhrFidelity(datSub[[animal]][, c("lon", "lat")], n=subconParams$n)

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
                                                                         grob=p,
                                                                         caption=paste0("Site fidelity plot", ids[animal]))
        
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
        
        0
      }, error=function(e) {1})
    }
  }

  if (config$config$verbose) {
    cat("Generating HTML output \n", file=stderr())
  }

  showResultHTML(ares$siteFidelity, config$background$siteFidelity)

  if (config$config$verbose) {
    cat("Generated HTML output \n", file=stderr())
  }

 # sfs <- lapply(datSub, function(x) rhrFidelity(x[, c('lon', 'lat')], n=as.numeric(config$preAnalysis$siteFidelity$n)))

 # ids <- names(datSub)
 # plotSF0 <- paste0("rhr_siteFidelityPlot_id_", ids, ".png")
 # sfPlots <- list()

 # for (i in seq_along(sfs)) {

 #   # plot
 #   filename <- file.path(imagepath, plotSF0[i])
 #   png(file=filename)

 #   ## msd
 #   p1 <- ggplot(data.frame(msd=sfs[[i]]$msd.sim), aes(factor("msd"), msd)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="MSD", breaks=NULL) + scale_y_continuous(name="MSD from Center of Activity (MSD)") + xlab("MSD")
 #   hlineData <- data.frame(yin=sfs[[i]]$msd.dat)
 #   p1 <- p1 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)

 #   ### Li
 #   p2 <- ggplot(data.frame(li=sfs[[i]]$li.sim), aes(factor("li"), li)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="LI", breaks=NULL) + scale_y_continuous(name="Linearity Index (LI)") 
 #   hlineData <- data.frame(yin=sfs[[i]]$li.dat) 
 #   p2 <- p2 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)
##
 #   grid.arrange(p1,p2, ncol=2, main="Site Fidelity")
 #   dev.off()

 #   # store for later
 #   sfPlots[[i]] <- grid.grabExpr(grid.arrange(p1, p2, ncol=2))
 # }

 # ## Add Summary to report

 # res$write(p(paste0("Site fidelity is the concept that animals have preference for one site and not move randomly through space. Two measures were used to asses site fidelity: MSD (mean squared distance from the centroid) and LI (linearity index). Linearity is defined as the distance between the first and last relocation diveded by the total distance travelled. A LI of 1 suggest linear movement and LI < 1 suggests that animal meander. Both MSD and LI were calculated for simulated trajectories (n=",  config$preAnalysis$siteFidelity$n, ") and real data. If both MSD and LI are significantely smaller for the real trajectory than for the simulated trajectories, then an animal exhibits site fidelity.")))

 # for (i in seq_along(sfs)) {
 #   res$write(h3(paste0("Site Fidelity for ", ids[i])))
 #   res$write(img(paste0(imageurl, plotSF0[i]), cap=paste0("Distribution for simulated values of MSD and LI for ", ids[i], ". The red dot indicated the value for MSD and LI from the real trajectory")))

 #   msd.ci <- t.test(sfs[[i]]$msd.sim)$conf.int
 #   li.ci <- t.test(sfs[[i]]$li.sim)$conf.int


 #   fidelity.results <- data.frame(Measure=c("LI", "MSD"),
 #                                  Observed=c(sfs[[i]]$li.dat, sfs[[i]]$msd.dat),
 #                                  LCI=c(li.ci[1], msd.ci[1]),
 #                                  UCI=c(li.ci[2], msd.ci[2]), stringsAsFactors=FALSE)
 #   names(fidelity.results)[3:4] <- c("lower 95 % confidence interval", "upper 95 % confidence interval")
 #   sfs[[i]]$fidelity.results <- fidelity.results


 #   
 #   res$write(toHTML(fidelity.results))

 #   if (sfs[[i]]$li.dat < li.ci[1] && sfs[[i]]$msd.dat < msd.ci[1]) {
 #     sfs[[i]]$msg <- "The mean squared distance from the center of activity and the linearity index of the data are below the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can be assumed and calculation of a home range may be appropriate."
 #   } else {
 #     sfs[[i]]$msg <- "Either the mean squared distance from the center of activity (MSD) or the linearity index (LI) of the data are above the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can not be assumed and calculation of home ranges may be problematic."
 #   }

 #  res$write(rhrAlert(sfs[[i]]$msg))

 # }
} else {
  res$write(rhrAlert("Site fidelity not requested"))
}
res$finish()

%>
