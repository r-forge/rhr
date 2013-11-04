<%

res <- Response$new()

filename <- file.path(docpath, "params.pdf")

width   <- 8.3
height  <- 11.7
  
pdf(filename, width=width,height=height)
  
grid.newpage()

## Outermost margins
pushViewport(viewport(x=unit(35, "mm"),
                      y=unit(12.5, "mm"),
                      width=unit(163.5, "mm"),
                      height=unit(272, "mm"),
                      just=c("left", "bottom"),
                      gp=gpar(lineheight=1, fontsize=11)))
    ## page frame        
    grid.rect(gp=gpar(lwd=0.2))

## ============================================================================ # 
## Global settings

global <- list()
global$h1 <- list()
global$h1$gpar <- gpar(fontface="bold", fontsize=18)
global$h1$grob <- function(x) textGrob(x=0.0, y=0.2, just=c("left", "bottom"), gp=global$h1$gpar, label=x) 

global$h1$size <- 18

global$h2$gpar <- gpar(fontsize=16)
global$h2$size <- 12
global$h2$grob <- function(x) {
  textGrob(x=0.0, y=0.2, just=c("left", "bottom"), gp=global$h2$gpar, label=x) 
}

global$h3$grob <- function(x) {
  gTree(children=gList(
          textGrob(x=0.0, y=0.3, label=x, just=c("left", "bottom"), gp=gpar(fontsize=14)),
                   linesGrob(y=c(0.2,0.2))))
      }
global$h3$size <- 12
global$line$size <- 5.5

global$hr <- function() linesGrob(y=c(0.2, 0.2))
global$hrsize <- 3.3
    

global$textbox <- function(str) {
    ## Recom
    str <- strsplit(str, " ")[[1]]
    strLines <- cumsum(nchar(str)) %/% 65
    strLinesMax <- max(strLines)
    str <- paste(tapply(str, strLines, function(x) paste(x, collapse=" ")), collapse="\n")

    strGrob <- textGrob(label=str) 

    ## Draw a Green Box to highlight key findings
    strBorderGrob <- rectGrob(width=0.95, height=unit(global$line$size * strLinesMax + 6, "mm"),
                              gp=gpar(fill="darkgreen", alpha=0.34))
    
    list(obj=gTree(children=gList(strBorderGrob, strGrob)),
         size=global$line$size * strLinesMax + 14)
  }

showResultGrid <- function(context, name) {

  obj <- list()
  sz <- list()

  obj[[1]] <- global$h1$grob(name)
  sz[[1]] <- global$h1$size

  ## cycle through subcontexts
  for (subcon in seq_along(context)) {

    ## If more than one subcontext is available give detials about them and extra headings, elso just continue
    hasSeveralContexts <- length(context) > 1

    if (hasSeveralContexts) {
      obj[[length(obj) + 1]] <- global$h2$grob(context[[subcon]]$param$subContext)
      sz[[length(sz) + 1]] <- global$h2$size
    }

    obj[[length(obj) + 1]] <- global$h3$grob("Settings")
    sz[[length(sz) + 1]] <- global$h3$size

    ## Write parameters
    params <- params2df(context[[subcon]]$params[c(-1, -2)])
    obj[[length(obj) + 1]] <- dfGrob(params, bodyFont="mono")
    sz[[length(sz) + 1]] <- global$line$size * nrow(params) + 5

    for (animal in seq_along(context[[subcon]]$animals)) {
      thisAnimal <- context[[subcon]]$animals[[animal]]


      obj[[length(obj) + 1]] <- global$h3$grob(paste0("Results for animal: ", thisAnimal$name))
      sz[[length(sz) + 1]] <- global$h3$size

      if (thisAnimal$exit == 0) {
        ## Extra params
        if (!is.na(thisAnimal$extraParams)) {
          params <- params2df(thisAnimal$extraParams)
          obj[[length(obj) + 1]] <- dfGrob(params, bodyFont="mono")
          sz[[length(sz) + 1]] <- global$line$size * nrow(params) + 5
        }

        ## Plots
        if (!is.na(thisAnimal$plots)) {
          for (p in seq_along(thisAnimal$plots)) {
            ## Write the plot
            obj[[length(obj) + 1]]  <- thisAnimal$plots[[p]]$grob
            sz[[length(sz) + 1]] <- 80
            
          }
        }

        ## Tables
        if (!is.na(thisAnimal$tables)) {
          for (table in seq_along(thisAnimal$tables)) {

            tt <- thisAnimal$tables[[table]]$table
            tt[, 1] <- as.character(tt[, 1])

            obj[[length(obj) + 1]] <- dfGrob(tt, bodyFont="mono")
            sz[[length(sz) + 1]] <- global$line$size * nrow(thisAnimal$tables[[table]]$table)
          }
        }

        ## Messages
        if (!is.na(thisAnimal$msgs)) {
          for (msg in seq_along(thisAnimal$msgs)) {
            ## include the table
            m <- global$textbox(thisAnimal$msgs[[msg]][[1]])
            obj[[length(obj) + 1]] <- m$obj
            sz[[length(sz) + 1]] <- m$size
          }
        }
      } else {
        m <- global$textbox(as.character(thisAnimal$error))
        obj[[length(obj) + 1]] <- m$obj
        sz[[length(sz) + 1]] <- m$size
      }
    }
  }
  return(list(obj=obj, size=sz))
}


showAnimalsGrid <- function(animals, name) {

  obj <- list()
  sz <- list()

  obj[[1]] <- global$h1$grob(name)
  sz[[1]] <- global$h1$size

  ## cycle through subcontexts
  for (animal in animals) {

    obj[[length(obj) + 1]] <- global$h3$grob(paste0("Data for animal: ", animal$summary$name))
    sz[[length(sz) + 1]] <- global$h3$size

    ## Write parameters
    params <- params2df(animal$summary[-1])
    obj[[length(obj) + 1]] <- dfGrob(params, bodyFont="mono")
    sz[[length(sz) + 1]] <- global$line$size * nrow(params) + 5

  }
  return(list(obj=obj, size=sz))
}
## ============================================================================ # 
## Prepare 2nd to nth page

repItems <- list()
repH <- c()

## ---------------------------------------------------------------------------- # 
## From estimator
m <- showAnimalsGrid(ares$animals, "Information about animals") 
repItems <- c(repItems, m$obj)
repH <- c(repH, m$size)

## ---------------------------------------------------------------------------- # 
## From estimator

## Site fidelity
name <- "Site fidelity"
if (config$todo$doSiteFidelity) {
  if (config$config$verbose) {
    cat("* writing pdf for site fidelity \n", file=stderr())
  }
  m <- showResultGrid(ares$siteFidelity, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
  
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

## TTSI
name <- "Time to statistical independence"
if (config$todo$doTTSI) {
  if (config$config$verbose) {
    cat("* writing pdf for ttsi \n", file=stderr())
  }
  m <- showResultGrid(ares$TTSI, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}


### MCP
name <- "Minimum convex polygon"
if (config$todo$doMCP) {
  if (config$config$verbose) {
    cat("* writing pdf for mcp \n", file=stderr())
  }
  m <- showResultGrid(ares$MCP, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

### KDE
name <- "Kernel density estimation"
if (config$todo$doKDE) {
  if (config$config$verbose) {
    cat("* writing pdf for kernel density \n", file=stderr())
  }
  m <- showResultGrid(ares$KDE, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

### LoCoH
name <- "Local convex hull"
if (config$todo$doLocoh) {
  if (config$config$verbose) {
    cat("* writing pdf for locoh \n", file=stderr())
  }
  m <- showResultGrid(ares$LoCoH, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

## Asymptote
name <- "Asymptote"
if (config$todo$doAsymptote) {
  if (config$config$verbose) {
    cat("* writing pdf for asymptote \n", file=stderr())
  }
  m <- showResultGrid(ares$Asymptote, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

### CA
name <- "Core area"
if (config$todo$doCA) {
  if (config$config$verbose) {
    cat("* writing pdf for core area \n", file=stderr())
  }
  m <- showResultGrid(ares$CA, name) 
  repItems <- c(repItems, m$obj)
  repH <- c(repH, m$size)
} else {
  repItems[[length(repItems) + 1]] <- global$h1$grob(name)
  repH[[length(repH) + 1]] <- global$h1$size
  m <- global$textbox(paste0(name, " not requested.")) 
  repItems[[length(repItems) + 1]] <- m$obj
  repH[[length(repH) + 1]] <- m$size + 2
}

## ============================================================================ #
## Plot it

## Unlist repH
repH <- unlist(repH)

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

    ## Filename and format
    pushViewport(viewport(y=unit(180, "mm"), width=unit(153.5, "mm"), height=unit(10, "mm"), just="bottom"))
        grid.text("Relocations read from: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)
        grid.text(config$fileName, y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0.7, gp=gpar(fontfamily="mono"))
    popViewport()

    ## Remap fields
    ## strShorten, make sure field names from user input are not to long
    pushViewport(viewport(y=unit(140, "mm"), width=unit(153.5, "mm"), height=unit(30, "mm"), just="bottom"))
        # title
        grid.text("This file contained the following column names, which were remapped to:",
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
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"), x=0.40, label="this is the latitude")

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
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.00, label=paste("xmin:", config$spBbxRestricted['xmin']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.25, label=paste("xmax:", config$spBbxRestricted['xmax']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.50, label=paste("ymin:", config$spBbxRestricted['ymin']))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.75, label=paste("ymax:", config$spBbxRestricted['ymax']))

    popViewport()

    ## Temporal bounding box
    if (config$config$dateTime) {
    pushViewport(viewport(y=unit(85, "mm"), width=unit(153.5, "mm"), height=unit(20, "mm"), just="bottom"))
        grid.text("The temporal bounding box was: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)
        ## bbox total
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.00, label=paste("tmin: ", config$temporalBbx$tmin))
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.50, label=paste("tmax: ", config$temporalBbx$tmax))

        grid.text("The restricted temporal bounding box was: ", y=unit(1, "npc") - unit(4, "lines"), just=c("left", "bottom"), x=0)
        ## bbox restricted
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.00, label=paste("tmin: ", config$temporalBbxRestricted$tmin))
        grid.text(y=unit(1, "npc") - unit(5, "lines"), just=c("left", "bottom"),
                  x=0.50, label=paste("tmax: ", config$temporalBbxRestricted$tmax))
    popViewport()
  }

    ## Number of relocations
    pushViewport(viewport(y=unit(65, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
        grid.text("Number of relocations: ", y=unit(1, "npc") - unit(1, "lines"), just=c("left", "bottom"), x=0)

        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.00,
                  label="Whole dataset")
        grid.text(y=unit(1, "npc") - unit(2, "lines"), just=c("left", "bottom"), x=0.70,
                  label=config$n$initialN, gp=gpar(fontfamily="mono"))

        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.00, label="Restricted dataset")
        grid.text(y=unit(1, "npc") - unit(3, "lines"), just=c("left", "bottom"), x=0.70, label=config$n$restrictedN, gp=gpar(fontfamily="mono"))
    popViewport()

    ## footer
    pushViewport(viewport(y=unit(0, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
        grid.lines(c(0,1), c(1,1))
        grid.text("Input data", x=0, just="left", gp=gpar(cex=0.7))
        grid.text(paste0("Page 1 of ", nPages + 2), x=1, just="right", gp=gpar(cex=0.7))
    popViewport()
popViewport()
grid.newpage()

## ============================================================================ #
## Next pages

for (page in 1:nPages) {
    pushViewport(viewport(x=unit(35, "mm"),
                          y=unit(12.5, "mm"),
                          width=unit(163.5, "mm"),
                          height=unit(272, "mm"),
                          just=c("left", "bottom"),
                          gp=gpar(lineheight=1, fontsize=11)))

        ## extract and heights for this page
        thisPageRepItems <- repItems[onWhichPage == page]
        thisPageRepH <- repH[onWhichPage == page]

        ## page frame        
        grid.rect(gp=gpar(lwd=0.2))

        ## content viewport
        pushViewport(viewport(y=unit(15, "mm"), width=unit(153.5, "mm"), height=unit(257, "mm"), just="bottom"))

        ## Add items to page
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

        ## footer
        pushViewport(viewport(y=unit(0, "mm"), width=unit(153.5, "mm"), height=unit(15, "mm"), just="bottom"))
            grid.lines(c(0,1), c(1,1))
            grid.text("Input data", x=0, just="left", gp=gpar(cex=0.7))
            grid.text(paste0("Page ", page + 1, " of ", nPages + 2), x=1, just="right", gp=gpar(cex=0.7))
        popViewport()
    popViewport()
    grid.newpage()
}


## ============================================================================ #
## Session info
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


grid.text(paste0(a$R.version$version.string, "\n", "Plattform: ", a$R.version$platform, "(", strsplit(a$R.version$arch, "_")[[1]][1], "-bit)\n\nlocale:\n", loc.str, "\n\nattached base packages:\n", basePkg.str, "\n\nother attached packages:\n", otherPkg.str, "\n\nloaded via a namespace (and not attached):\n", loadedPkg.str  ), x=0, y=unit(ystart, "npc") - unit(3, "lines"), just=c("left", "top"), gp=gpar(fontfamily="mono"))

popViewport()

pushViewport(viewport(width=0.98, height=unit(1.2, "lines"), y=0, x=0.5, just=c("center", "bottom")))
grid.lines(c(0,1), c(1,1))
grid.text("R-SessionInfo", x=0, just="left", gp=gpar(cex=0.7))
grid.text(paste0("Page ", nPages + 2, " of ", nPages + 2), x=1, just="right", gp=gpar(cex=0.7))
popViewport()


popViewport()

dev.off()

res$write(cat(paste0("<a href='", docurl, "params.pdf'>Download Report</a>")))

res$finish()

%>
