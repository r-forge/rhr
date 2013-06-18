<%
res <- Response$new()
if (config$todo$doSiteFidelity) {

  sfs <- lapply(datSub, function(x) rhrFidelity(x[, c('lon', 'lat')], n=as.numeric(config$preAnalysis$siteFidelity$n)))

  ids <- names(datSub)
  plotSF0 <- paste0("rhr_siteFidelityPlot_id_", ids, ".png")
  sfPlots <- list()

  for (i in seq_along(sfs)) {

    # plot
    filename <- file.path(imagepath, plotSF0[i])
    png(file=filename)

    ## msd
    p1 <- ggplot(data.frame(msd=sfs[[i]]$msd.sim), aes(factor("msd"), msd)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="MSD", breaks=NULL) + scale_y_continuous(name="MSD from Center of Activity (MSD)") + xlab("MSD")
    hlineData <- data.frame(yin=sfs[[i]]$msd.dat)
    p1 <- p1 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)

    ### Li
    p2 <- ggplot(data.frame(li=sfs[[i]]$li.sim), aes(factor("li"), li)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="LI", breaks=NULL) + scale_y_continuous(name="Linearity Index (LI)") 
    hlineData <- data.frame(yin=sfs[[i]]$li.dat) 
    p2 <- p2 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)
#
    grid.arrange(p1,p2, ncol=2, main="Site Fidelity")
    dev.off()

    # store for later
    sfPlots[[i]] <- grid.grabExpr(grid.arrange(p1, p2, ncol=2))
  }

  ## Add Summary to report

  res$write(p(paste0("Site fidelity is the concept that animals have preference for one site and not move randomly through space. Two measures were used to asses site fidelity: MSD (mean squared distance from the centroid) and LI (linearity index). Linearity is defined as the distance between the first and last relocation diveded by the total distance travelled. A LI of 1 suggest linear movement and LI < 1 suggests that animal meander. Both MSD and LI were calculated for simulated trajectories (n=",  config$preAnalysis$siteFidelity$n, ") and real data. If both MSD and LI are significantely smaller for the real trajectory than for the simulated trajectories, then an animal exhibits site fidelity.")))

  for (i in seq_along(sfs)) {
    res$write(h3(paste0("Site Fidelity for ", ids[i])))
    res$write(img(paste0(imageurl, plotSF0[i]), cap=paste0("Distribution for simulated values of MSD and LI for ", ids[i], ". The red dot indicated the value for MSD and LI from the real trajectory")))

    msd.ci <- t.test(sfs[[i]]$msd.sim)$conf.int
    li.ci <- t.test(sfs[[i]]$li.sim)$conf.int


    fidelity.results <- data.frame(Measure=c("LI", "MSD"),
                                   Observed=c(sfs[[i]]$li.dat, sfs[[i]]$msd.dat),
                                   LCI=c(li.ci[1], msd.ci[1]),
                                   UCI=c(li.ci[2], msd.ci[2]), stringsAsFactors=FALSE)
    names(fidelity.results)[3:4] <- c("lower 95 % confidence interval", "upper 95 % confidence interval")
    sfs[[i]]$fidelity.results <- fidelity.results


    
    res$write(toHTML(fidelity.results))

    if (sfs[[i]]$li.dat < li.ci[1] && sfs[[i]]$msd.dat < msd.ci[1]) {
      sfs[[i]]$msg <- "The mean squared distance from the center of activity and the linearity index of the data are below the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can be assumed and calculation of home ranges makes sense"
    } else {
      sfs[[i]]$msg <- "Either the mean squared distance from the center of activity (MSD) or the linearity index (LI) of the data are above the 95 % confidence interval of the simulated random trajectories. Therefore, site fidelity can not be assumed and calculation of home ranges may be problematic"
    }

   res$write(alert(sfs[[i]]$msg))

  }
} else {
  res$write(alert("Site fidelity not requested"))
}
res$finish()

%>
