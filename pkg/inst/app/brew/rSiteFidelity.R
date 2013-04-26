<%
res <- Response$new()
if (config$todo$doSiteFidelity) {

#  sfInit(parallel=TRUE, cpus=3, type="SOCK")
#  sfLibrary(rhr)

  #sfs <- sfLapply(split(data.frame(lon=rnorm(1e4), lat=rnorm(1e4)), sample(c(0,1), 1e4, T)), function(x) rhrFidelity(x[, c('lon', 'lat')], n=200))

  sfs <- lapply(datSub, function(x) rhrFidelity(x[, c('lon', 'lat')], n=as.numeric(config$preAnalysis$siteFidelity$n)))

#  sfStop()
  
  ids <- names(datSub)
  plotSF0 <- paste0("rhr_siteFidelityPlot_id_", ids, ".png")
  sfPlots <- list()

  for (i in seq_along(sfs)) {
  # plot
    filename <- file.path(imagepath, plotSF0[i])
    png(file=filename)
    ## msd
    p1 <- ggplot(data.frame(msd=sfs[[i]]$msd.sim), aes(factor("msd"), msd)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="") + scale_y_continuous(name="Mean Square Distance")
    hlineData <- data.frame(yin=sfs[[i]]$msd.dat)
    p1 <- p1 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)

    ### Li
    p2 <- ggplot(data.frame(li=sfs[[i]]$li.sim), aes(factor("li"), li)) + geom_jitter(alpha=0.4) + geom_boxplot(alpha=0.3) + theme_bw() + scale_x_discrete(name="") + scale_y_continuous(name="Linearity Index")
    hlineData <- data.frame(yin=sfs[[i]]$li.dat)
    p2 <- p2 + geom_hline(aes(yintercept = yin, colour="red"), hlineData)
#
    grid.arrange(p1,p2, ncol=2, main="Site Fidelity")
    dev.off()

    # store for later
    sfPlots[[i]] <- grid.grabExpr(grid.arrange(p1, p2, ncol=2))
  }

  ## Add Summary to report
  res$write(h1("Pre HR"))
  res$write(h2("Site Fidelity"))

  res$write(p(paste0("Site fidelity is the concept that animals have preference for one site and not move randomly through space. Two measures were used to asses site fidelity: MSD (mean squared distance from the centroid) and LI (linearity index). Linearity is defined as the distance between the first and last relocation diveded by the total distance travelled. A LI of 1 suggest linear movement and LI < 1 suggests that animal meander. Both MSD and LI were calculated for simulated trajectories (n=",  config$preAnalysis$siteFidelity$n, ") and real data. If both MSD and LI are significantely smaller for the real trajectory than for the simulated trajectories, then an animal exhibits site fidelity.")))

  for (i in seq_along(sfs)) {
    res$write(h3(paste0("Site Fidelity for ", ids[i])))
    res$write(img(paste0(imageurl, plotSF0[i]), cap=paste0("Distribution for simulated values of MSD and LI for ", ids[i], ". The red dot indicated the value for MSD and LI from the real trajectory")))

    res$write(p(paste0("Summary of the simulations for MSD and LI for ", ids[i], ".")))
    res$write(toHTML(summary(sfs[[i]]$li.sim), cap="Linearity"))
    res$write(toHTML(summary(sfs[[i]]$msd.sim), cpa="MSD"))

    res$write(p("Below t-test to support the the boxplots"))
    res$write(toHTML(t.test(sfs[[i]]$li.sim, mu=sfs[[i]]$li.dat), cap=""))
    res$write(toHTML(t.test(sfs[[i]]$msd.sim, mu=sfs[[i]]$msd.dat), cap=""))
  }
} else {
  res$write(alert("Site fidelity not requested"))
}
res$finish()

%>
