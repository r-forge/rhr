<%
res <- Response$new()


filename <- file.path(docpath, "params.pdf")

width   <- 8.3
height  <- 11.7
  
pdf(filename, width=width,height=height)
  
grid.newpage()

# Outermost margins
pushViewport(viewport(x=unit(35, "mm"),
                      y=unit(12.5, "mm"),
                      width=unit(163.5, "mm"),
                      height=unit(272, "mm"),
                      just=c("left", "bottom"),
                      gp=gpar(lineheight=1, fontsize=11)))
    # page frame        
    grid.rect(gp=gpar(lwd=0.2))

# ---------------------------------------------------------------------------- #
# Title Page
    ## Title
    pushViewport(viewport(y=unit(220, "mm"), width=unit(153.5, "mm"), height=unit(50, "mm"), just="bottom"))

        grid.text("Reproducible Homerange Analysis", y=0.7, gp=gpar(fontface="bold", cex=2))
        grid.text(paste0("(", date(), ")"),  y=unit(0.4, "npc"), gp=gpar(cex=0.9))
        grid.text("based on the rhr package",  y=unit(0.4, "npc") - unit(2.2, "lines"), gp=gpar(cex=0.9))
    popViewport()

    ## Description
    pushViewport(viewport(y=unit(200, "mm"), width=unit(153.5, "mm"), height=unit(20, "mm"), just="bottom"))
        grid.lines(c(0.1,0.9), c(1,1), gp=gpar(lwd=0.1))
        pushViewport(viewport(width=0.8))
            grid.text("This is an automatically generated file with all parameters and settings, in order",
                      x=0, just="left",
                      y=0.7, gp=gpar(cex=0.9))
            grid.text("to enable later replication of the same analysis given the same data set.",
                      x=0, y=unit(0.7, "npc") - unit(1, "lines"),
                      just="left", gp=gpar(cex=0.9))
        popViewport()
        grid.lines(c(0.1,0.9), c(0.1,0.1), gp=gpar(lwd=0.1))

    popViewport()

    ## Input data heading
    pushViewport(viewport(y=unit(190, "mm"), width=unit(153.5, "mm"), height=unit(10, "mm"), just="bottom"))
        grid.text("Input data", just="left", x=0, gp=gpar(fontface="bold", fontsize=18))
    popViewport()

    # Filename and format
    pushViewport(viewport(y=unit(180, "mm"), width=unit(153.5, "mm"), height=unit(10, "mm"), just="bottom"))
        grid.text("Relocations read from: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)
        grid.text(config$fileName, y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0.7, gp=gpar(fontfamily="mono"))
    popViewport()

    # Remap fields
    # strShorten, make sure field names from user input are not to long
    pushViewport(viewport(y=unit(140, "mm"), width=unit(153.5, "mm"), height=unit(30, "mm"), just="bottom"))
        # title
        grid.text("This filed contained the following column names, which were remaped to:",
                  y=unit(1, "npc") - unit(1, "lines"),
                  just=c("left", "bottom"), x=0)
        # id
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.00, label=config$mapFields$id, gp=gpar(fontfamily="mono"))
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.25, label="to", gp=gpar(fontface="italic"))
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.30, label="id")
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.40, label="this is the animal id")
        # lon
        grid.text(y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0.00, label=config$mapFields$lon, gp=gpar(fontfamily="mono"))
        grid.text(y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0.25, label="to", gp=gpar(fontface="italic"))
        grid.text(y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0.30, label="lon")
        grid.text(y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0.40, label="this is the lon")
        # lat
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.00, label=config$mapFields$lat, gp=gpar(fontfamily="mono"))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.25, label="to", gp=gpar(fontface="italic"))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.30, label="lat")
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.40, label="this is the lat")

        # date
        grid.text(y=unit(1, "npc") - unit(6, "lines"), just=c("left", "bottom"), x=0.00, label=config$mapFields$date['date'], gp=gpar(fontfamily="mono"))
        grid.text(y=unit(1, "npc") - unit(6, "lines"), just=c("left", "bottom"), x=0.25, label="to", gp=gpar(fontface="italic"))
        grid.text(y=unit(1, "npc") - unit(6, "lines"), just=c("left", "bottom"), x=0.30, label="date")
        grid.text(y=unit(1, "npc") - unit(6, "lines"), just=c("left", "bottom"), x=0.40, label="this is the date")

        # time
        grid.text(y=unit(1, "npc") - unit(7, "lines"), just=c("left", "bottom"), x=0.00, label=config$mapFields$time['time'], gp=gpar(fontfamily="mono"))
        grid.text(y=unit(1, "npc") - unit(7, "lines"), just=c("left", "bottom"), x=0.25, label="to", gp=gpar(fontface="italic"))
        grid.text(y=unit(1, "npc") - unit(7, "lines"), just=c("left", "bottom"), x=0.30, label="time")
        grid.text(y=unit(1, "npc") - unit(7, "lines"), just=c("left", "bottom"), x=0.40, label="this is the time")
    popViewport()


    ## spatial bounding box
    pushViewport(viewport(y=unit(110, "mm"), width=unit(153.5, "mm"), height=unit(20, "mm"), just="bottom"))
        grid.text("The spatial bounding box was: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)
        # bbox total
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.00, label=paste("xmin:", config$spBbx['xmin']))
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.25, label=paste("xmax:", config$spBbx['xmax']))
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.50, label=paste("ymin:", config$spBbx['ymin']))
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.75, label=paste("ymax:", config$spBbx['ymax']))

        grid.text("The restricted spatial bounding box was: ", y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0)
        # bbox restricted
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.00, label=paste("xmin:", config$spBbxRestricted['xmin']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.25, label=paste("xmax:", config$spBbxRestricted['xmax']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.50, label=paste("ymin:", config$spBbxRestricted['ymin']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.75, label=paste("ymax:", config$spBbxRestricted['ymax']))

    popViewport()

    ## spatial bounding box
    pushViewport(viewport(y=unit(85, "mm"), width=unit(153.5, "mm"), height=unit(20, "mm"), just="bottom"))
        grid.text("The temporal bounding box was: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)
        # bbox total
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.00, label=paste("tmin: ", config$temporalBbx$tmin))
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.50, label=paste("tmax: ", config$temporalBbx$tmax))

        grid.text("The restricted temporal bounding box was: ", y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0)
        # bbox restricted
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.00, label=paste("tmin: ", config$temporalBbxRestricted$tmin))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.50, label=paste("tmax: ", config$temporalBbxRestricted$tmax))

    popViewport()

    ## Number of relocations
    pushViewport(viewport(y=unit(65, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
        grid.text("Number of relcations: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)

        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.00, label="Whole dataset")
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.70, label=config$n$initialN, gp=gpar(fontfamily="mono"))

        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.00, label="Restricted dataset")
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.70, label=config$n$restrictedN, gp=gpar(fontfamily="mono"))
    popViewport()

    # footer
    pushViewport(viewport(y=unit(0, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
        grid.lines(c(0,1), c(1,1))
        grid.text("Input data", x=0, just="left", gp=gpar(cex=0.7))
        grid.text("Page 1 of 3", x=1, just="right", gp=gpar(cex=0.7))
    popViewport()
popViewport()
grid.newpage()

# ============================================================================ # 
# 2nd to nth page
# Prepare everything

repItems <- list()
repH <- c()

# ---------------------------------------------------------------------------- # 
## Site fidelity

if (config$todo$doSiteFidelity) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Site fidelity") -> rSF.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rSF.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Number of simulated trajectories") -> rSF.niter0
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label=config$preAnalysis$siteFidelity$n) -> rSF.niter1

  # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rSF.header, rSF.input, rSF.niter0, rSF.niter1))
  repH <- c(repH, 20)
  
  # Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> rSF.res
  repItems[[length(repItems) + 1]] <- rSF.res 
  repH <- c(repH, 10)

  # add results
  for (i in seq_along(sfs)) {
    # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 10)

    # summary Linearity
    repItems[[length(repItems) + 1]] <- summaryGrob(summary(sfs[[i]]$li.sim))
    repH <- c(repH, 12)

    # summary MSD
    repItems[[length(repItems) + 1]] <- summaryGrob(summary(sfs[[i]]$msd.sim))
    repH <- c(repH, 12)

    # plots
    p <- sfPlots[[i]]
    repItems[[length(repItems) + 1]] <- p
    repH <- c(repH, 80)

    # ttest
    repItems[[length(repItems) + 1]] <- ttestGrob(t.test(sfs[[i]]$li.sim, mu=sfs[[i]]$li.dat))
    repH <- c(repH, 12)

    repItems[[length(repItems) + 1]] <- ttestGrob(t.test(sfs[[i]]$msd.sim, mu=sfs[[i]]$msd.dat))
    repH <- c(repH, 12)
  }
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Site fidelity not requested") -> rSF.not
  repItems[[length(repItems) + 1]] <- rSF.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## TTSI

if (config$todo$doTTSI) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Time To Statistical Independence") -> rTTSI.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rTTSI.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rTTSI.level0
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label="100") -> rTTSI.level1

 # # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rTTSI.header, rTTSI.input, rTTSI.level0, rTTSI.level1))
  repH <- c(repH, 20)
 # 
 # # Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> rTTSI.res
  repItems[[length(repItems) + 1]] <- rTTSI.res 
  repH <- c(repH, 10)

 # # add results
  for (i in seq_along(ttsiPlots)) {
 #   # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 5)

 #   # plots
    repItems[[length(repItems) + 1]] <- ttsiPlots[[i]]
    repH <- c(repH, 80)
  }
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Time To Statistical Independence") -> rTTSI.not
  repItems[[length(repItems) + 1]] <- rTTSI.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## Asymptote

if (config$todo$doAsymptote) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Asymptote") -> rAsym.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rAsym.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rAsym.level0	      
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label="100") -> rAsym.level1

 ## add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rAsym.header, rAsym.input, rAsym.level0, rAsym.level1))
  repH <- c(repH, 20)
 # 

 ## Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> Asym.res
  repItems[[length(repItems) + 1]] <- Asym.res 
  repH <- c(repH, 10)

 ## Add plots
  for (i in seq_along(resAsym)) {

    # header
      repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                  textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                  linesGrob(y=c(0,0))))
      repH <- c(repH, 5)

    if (resAsym[[i]]$exit != 0) {
      repItems[[length(repItems) + 1]] <- textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), label=resAsym[[i]]$message) 
      repH <- c(repH, 10)
      next
    }

    
    if ("mcp" %in% config$preAnalysis$asymptote$estimator) {
      ## header

      ## plots MCP
      repItems[[length(repItems) + 1]] <- textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), label="Minimum Convex Polygon") 
      repH <- c(repH, 5)

      repItems[[length(repItems) + 1]] <- resAsym[[i]]$mcpPlot
      repH <- c(repH, 80)
    }
      if ("kde" %in% config$preAnalysis$asymptote$estimator) {

        ## plots KDE
        repItems[[length(repItems) + 1]] <- textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), label="KDE") 
        repH <- c(repH, 5)

        repItems[[length(repItems) + 1]] <- resAsym[[i]]$kdePlot
        repH <- c(repH, 80)
      }

  }



  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Asymptote not requested") -> rAsym.not
  repItems[[length(repItems) + 1]] <- rAsym.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## Core Area

if (config$todo$doCA) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Core Area Estimation") -> rCA.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rCA.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rCA.level0
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label="100") -> rCA.level1

  # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rCA.header, rCA.input, rCA.level0, rCA.level1))
  repH <- c(repH, 20)
  
  # Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> CA.res
  repItems[[length(repItems) + 1]] <- CA.res 
  repH <- c(repH, 10)

  # add results
  for (i in seq_along(datSub)) {
    # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 5)

    if (resCAs[[i]]$exit == 0) {

      # plots
      repItems[[length(repItems) + 1]] <- caPlots1[[i]]
      repH <- c(repH, 80)

      repItems[[length(repItems) + 1]] <- caPlots2[[i]]
      repH <- c(repH, 80)

    } else {
      textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label=resCAs[[i]]$msg) -> rCA
      repItems[[length(repItems) + 1]] <- rCA 
      repH <- c(repH, 10)
    }
  }
    
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Core Area not requested") -> rCA.not
  repItems[[length(repItems) + 1]] <- rCA.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## MCP

if (config$todo$doMCP) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Minimum Convex Polygon") -> rMCP.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rMCP.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rMCP.level0
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label=paste0(rhrCorrectLevels(config$estimator$mcp$level), collapse=",")) -> rMCP.level1

 # # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rMCP.header, rMCP.input, rMCP.level0, rMCP.level1))
  repH <- c(repH, 20)
 # 
 # # Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> rMCP.res
  repItems[[length(repItems) + 1]] <- rMCP.res 
  repH <- c(repH, 10)

 # # add results
  for (i in seq_along(mcpPlots)) {
 #   # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 5)

 #   # plots
    repItems[[length(repItems) + 1]] <- mcpPlots[[i]]
    repH <- c(repH, 80)
  }
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Minimum convex polygon not requested") -> rMCP.not
  repItems[[length(repItems) + 1]] <- rMCP.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## KDE

if (config$todo$doKDE) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Kernel Density Estimation") -> rKDE.header
  # textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rKDE.input
  # textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rKDE.level0
  # textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label="100") -> rKDE.level1

 # # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rKDE.header))
  repH <- c(repH, 10)

 # # Results
 # textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> KDE.res
 # repItems[[length(repItems) + 1]] <- KDE.res 
 # repH <- c(repH, 10)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "level",
                                                 config$estimator$kde$level))
  repH <- c(repH, 5)

  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "resolution",
                                                 config$estimator$kde$resolution))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "buffer",
                                                 config$estimator$kde$buffer))
  repH <- c(repH, 5)

  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "bandwidth type",
                                                 config$estimator$kde$bandwidth))
  repH <- c(repH, 5)

  # add results
  for (i in seq_along(kdePlots)) {
    # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 5)

    # plots
    repItems[[length(repItems) + 1]] <- kdePlots[[i]]
    repH <- c(repH, 80)
    # areas
    repItems[[length(repItems) + 1]] <- dfGrob(data.frame(resKDEsAreas[[i]]), start=0.3, stop=0.8)
    repH <- c(repH, ((nrow(resKDEsAreas[[i]]) * 5) + 10))

    

    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("KDE", "bandwidth value",
                                                   round(resKDEs[[i]]$h$h), 2))
    repH <- c(repH, 5)
  }
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Kernel density estimation not requested") -> rKDE.not
  repItems[[length(repItems) + 1]] <- rKDE.not 
  repH <- c(repH, 15)
}

# ---------------------------------------------------------------------------- # 
## LoCoH

if (config$todo$doLocoh) {
  # infoblock
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontsize=18), label="Local convex hull") -> rLocoh.header
  textGrob(y=unit(1, "npc") - unit(3, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Input") -> rLocoh.input
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.0, just=c("left", "bottom"), label="Level") -> rLocoh.level0
  textGrob(y=unit(1, "npc") - unit(4, "lines"), x=0.7, just=c("left", "bottom"), gp=gpar(fontfamily="mono"), label="100") -> rLocoh.level1

 # # add to pending
  repItems[[length(repItems) + 1]] <- gTree(children=gList(rLocoh.header, rLocoh.input, rLocoh.level0, rLocoh.level1))
  repH <- c(repH, 20)
 # 
 # # Results
  textGrob(y=unit(1, "npc") - unit(1, "lines"), x=0.0, just=c("left", "bottom"), gp=gpar(fontface="bold"), label="Results") -> Locoh.res
  repItems[[length(repItems) + 1]] <- Locoh.res 
  repH <- c(repH, 10)

  # add results
  for (i in seq_along(locohPlots)) {
    # header
    repItems[[length(repItems) + 1]] <- gTree(children=gList(
                                                textGrob(x=0.0, label=names(datSub)[i], just=c("left", "bottom")),
                                                linesGrob(y=c(0,0))))
    repH <- c(repH, 5)

    # plots
    repItems[[length(repItems) + 1]] <- locohPlots[[i]]
    repH <- c(repH, 80)
    # areas
    tmp <- data.frame(resLocohs[[i]]$estimatorData)
    repItems[[length(repItems) + 1]] <- dfGrob(tmp, start=0.3, stop=0.8)
    repH <- c(repH, ((nrow(tmp) * 5) + 10))
  }
  
} else {
  textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="italic"), label="Local convex hull not requested") -> rLocoh.not
  repItems[[length(repItems) + 1]] <- rLocoh.not 
  repH <- c(repH, 15)
}

# ============================================================================ #
# Summary of parameters

repItems[[length(repItems) + 1]] <- textGrob(y=0.5, just=c("left", "center"), x=0, gp=gpar(fontface="bold"), label="Summary of parameters used")
repH <- c(repH, 20)

if (config$todo$doSiteFidelity) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Site fidelity", "n simulated trajectories",
                                                 config$preAnalysis$siteFidelity$n))
  repH <- c(repH, 5)
}

if (config$todo$doTTSI) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("TTSI", "time interval",
                                                 config$preAnalysis$ttsi$interval))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("TTSI", "tolerance",
                                                 config$preAnalysis$ttsi$tolerance))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("TTSI", "type",
                                                 config$preAnalysis$ttsi$type))
  repH <- c(repH, 5)
}


if (config$todo$doAsymptote) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "minum number of points",
                                                 config$preAnalysis$asymptote$minNPts))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "number of iterations",
                                                 config$preAnalysis$asymptote$nIter))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "increment per iteration",
                                                 config$preAnalysis$asymptote$increment))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "estimator",
                                                 config$preAnalysis$asymptote$estimator))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "number of replications",
                                                 config$preAnalysis$asymptote$nTimes))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "level",
                                                 config$preAnalysis$asymptote$level))
  repH <- c(repH, 5)
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Asymptote", "tolerance to total area",
                                                 config$preAnalysis$asymptote$tolTotArea))
  repH <- c(repH, 5)

  if ("kde" %in% config$preAnalysis$asymptote$estimator) {

    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("Asymptote - kde", "resolution",
                                                   config$estimator$kde$resolution))
    repH <- c(repH, 5)
    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("Asymptote - kde", "buffer",
                                                   config$estimator$kde$buffer))
    repH <- c(repH, 5)
    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("Asymptote - kde", "bandwidth",
                                                   config$estimator$kde$bandwidth))
    repH <- c(repH, 5)

    if (config$estimator$kde$bandwidth == "user") {
      repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                   just=c("left", "bottom"),
                                                   gp=gpar(fontfamily="mono"),
                                                   label=c("Asymptote - kde", "bandwidth value",
                                                     config$estimator$kde$bandwidthValue))
      repH <- c(repH, 5)
    }
  }
}

if (config$todo$doMCP) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("MCP", "level",
                                                 config$estimator$mcp$level))
  repH <- c(repH, 5)
}

if (config$todo$doKDE) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "level",
                                                 config$estimator$kde$level))
  repH <- c(repH, 5)

  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "resolution",
                                                 config$estimator$kde$resolution))
  repH <- c(repH, 5)

  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "buffer",
                                                 config$estimator$kde$buffer))
  repH <- c(repH, 5)
  
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("KDE", "bandwidth",
                                                 config$estimator$kde$bandwidth))
  repH <- c(repH, 5)

  if (config$estimator$kde$bandwidth == "user") {
    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("KDE", "bandwidth value",
                                                   config$estimator$kde$bandwidthValue))
    repH <- c(repH, 5)
  }
}

if (config$todo$doLocoh) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("LoCoH", "level",
                                                 config$estimator$locoh$level))
  repH <- c(repH, 5)

  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("LoCoH", "type",
                                                 config$estimator$locoh$type))
  repH <- c(repH, 5)

  if (!config$estimator$locoh$n) {
    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("LoCoH", "n value",
                                                   config$estimator$locoh$nValue))
  } else {
    repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                                 just=c("left", "bottom"),
                                                 gp=gpar(fontfamily="mono"),
                                                 label=c("LoCoH", "n value",
                                                   "auto"))
  }
  repH <- c(repH, 5)
}

if (config$todo$doCA) {
  repItems[[length(repItems) + 1]] <- textGrob(x=c(0.01, 0.3, 0.8),
                                               just=c("left", "bottom"),
                                               gp=gpar(fontfamily="mono"),
                                               label=c("Core Area", "resolution",
                                                 config$estimator$ca$resolution))
  repH <- c(repH, 5)
}
# ============================================================================ #
# Plot it

# on which page does every element go?
whichpage <- 1
onWhichPage <- c()
spaceOnPage <- 250

for (i in seq_along(repH)) {
  if (spaceOnPage - repH[i] >= 0) {
    spaceOnPage <- spaceOnPage - repH[i]
  } else {
    spaceOnPage <- 250 - repH[i]
    whichpage <- whichpage + 1
  }
  onWhichPage <- c(onWhichPage, whichpage)
}

nPages <- max(onWhichPage)

# Create report
for (page in 1:nPages) {
    pushViewport(viewport(x=unit(35, "mm"),
                          y=unit(12.5, "mm"),
                          width=unit(163.5, "mm"),
                          height=unit(272, "mm"),
                          just=c("left", "bottom"),
                          gp=gpar(lineheight=1, fontsize=11)))

        # extract and heights for this page
        thisPageRepItems <- repItems[onWhichPage == page]
        thisPageRepH <- repH[onWhichPage == page]

        # page frame        
        grid.rect(gp=gpar(lwd=0.2))

        # content viewport
        pushViewport(viewport(y=unit(15, "mm"), width=unit(153.5, "mm"), height=unit(257, "mm"), just="bottom"))

        # Add items to page
        for (i in 1:length(thisPageRepItems)) {
          if (i == 1) {
            where <- 255
          } else {
            where <- 255 - sum(thisPageRepH[1:i-1])
          }
          pushViewport(viewport(y=unit(where, "mm"), width=unit(153.5, "mm"), height=unit(thisPageRepH[i], "mm"), just="top"))
             grid.draw(thisPageRepItems[[i]])
             ## Debugging only
             # grid.text(paste(where, thisPageRepH[i]), gp=gpar(col="red", fontface=1))
             ## Debugging only
             # grid.rect(gp=gpar(col="red"))
          popViewport()
        }
        popViewport()

        # footer
        pushViewport(viewport(y=unit(0, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
            grid.lines(c(0,1), c(1,1))
            grid.text("Input data", x=0, just="left", gp=gpar(cex=0.7))
            grid.text(paste0("Page ", page + 1, " of ", nPages + 1), x=1, just="right", gp=gpar(cex=0.7))
        popViewport()
    popViewport()
    grid.newpage()
}


# ============================================================================ #
# Session info
pushViewport(viewport(width=0.95, height=0.95))
grid.rect(gp=gpar(lwd=0.2))
pushViewport(viewport(width=0.95))

ystart <- 0.95
grid.text("R Session Info:", y=ystart, just="left", x=0, gp=gpar(fontface="bold", cex=1.5))

a <- sessionInfo()

loc <- strsplit(a$locale, ";")[[1]]
loc.lines <- cumsum(nchar(loc)) %/% 55
loc.str <- paste(tapply(loc, loc.lines, function(x) paste(x, collapse="; ")), collapse="\n")

basePkg <- a$basePkgs
basePkg.lines <- cumsum(nchar(basePkg)) %/% 55
basePkg.str <- paste(tapply(basePkg, basePkg.lines, function(x) paste(x, collapse="; ")), collapse="\n")

otherPkg <- sapply(a$otherPkgs, function(x) paste0(x$Package, "_", x$Version))
otherPkg.lines <- cumsum(nchar(otherPkg)) %/% 55
otherPkg.str <- paste(tapply(otherPkg, otherPkg.lines, function(x) paste(x, collapse="; ")), collapse="\n")

loadedPkg <- sapply(a$loadedOnly, function(x) paste0(x$Package, "_", x$Version))
loadedPkg.lines <- cumsum(nchar(loadedPkg)) %/% 55
loadedPkg.str <- paste(tapply(loadedPkg, loadedPkg.lines, function(x) paste(x, collapse="; ")), collapse="\n")


grid.text(paste0(a$R.version$version.string, "\n", "Plattform: ", a$R.version$platform, "(", strsplit(a$R.version$arch, "_")[[1]][1], "-bit)\n\nlocale:\n", loc.str, "\n\nattached base packages:\n", basePkg.str, "\n\nother attached packages:\n", otherPkg.str, "\n\nloaded viar a namespace (and not attached):\n", loadedPkg.str  ), x=0, y=unit(ystart, "npc") - unit(3, "lines"), just=c("left", "top"), gp=gpar(fontfamily="mono"))

popViewport()

pushViewport(viewport(width=0.98, height=unit(1.2, "lines"), y=0, x=0.5, just=c("center", "bottom")))
grid.lines(c(0,1), c(1,1))
grid.text("R-SessionInfo", x=0, just="left", gp=gpar(cex=0.7))
grid.text("Page 3 of 3", x=1, just="right", gp=gpar(cex=0.7))
popViewport()


popViewport()

dev.off()

res$write(cat(paste0("<a href='", docurl, "params.pdf'>Download Report</a>")))
res$finish()

%>
