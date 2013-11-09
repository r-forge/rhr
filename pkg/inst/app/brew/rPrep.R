<%

params2df <- function(params) {
  params <- lapply(params, function(x) if (length(x) > 1) paste0(x, collapse=",") else x)
  data.frame(Parameter=names(params), Value=unlist(params), stringsAsFactors=FALSE)
}

res <- Response$new()

alog <- c()
alog <- c(alog, catPro("Preparing analysis", pre=cath1("Starting with rhr analysis")))

## ============================================================================ # 
## update Config

## Add file name to config
if (!is.null(datFromR)) {
  config$inputFile$filename <- 'data loaded from R'
} else {
  config$inputFile$filename <- nameInputFile$filename
  config$inputFile$hasHeader <- hasHeader
  config$inputFile$sep <- sep
  config$inputFile$decSep <- sepDec
  config$inputFile$skip <- skip
}


## ============================================================================ # 
## Provide data

## Do subset
## subset time & space

alog <- c(alog, catPro("splitting data"))


## ============================================================================ # 
## Validate and correct input
alog <- c(alog, catPro("validate input"))
config$analysis$mcp$level   <- rhrCorrectLevels(config$analysis$mcp$level)
config$analysis$kde$level   <- rhrCorrectLevels(config$analysis$kde$level)
config$analysis$locoh$level <- rhrCorrectLevels(config$analysis$locoh$level)


## ============================================================================ #
## Create outdirs
alog <- c(alog, catPro("create out dirs"))

outdirAnalysis <- file.path(outdir, paste0("rhr_run_", format(Sys.time(), format="%Y-%m-%d-%H-%M-%S")))

dir.create(outdirAnalysis)
dir.create(file.path(outdirAnalysis, "plots"))
dir.create(file.path(outdirAnalysis, "data"))
dir.create(file.path(outdirAnalysis, "doc"))

## Delete files from last run
file.remove(list.files(imagepath, full.names=TRUE))
file.remove(list.files(datapath, full.names=TRUE))
file.remove(list.files(docpath, full.names=TRUE))

## ============================================================================ #
## Results
alog <- c(alog, catPro("preparing results"))

## create res list
ares <- list()

brewResults <- function(name) {
  list(name=name,  # name of the animal
       exit=1,  # exit code, 0 all good, > 0 something went wrong
       error="No analysis performed",  # error msg if any
       plots=NA,  # list of plots, each plot has: a filename, a caption and a grob
       msgs=NA,  # additional msg from the analysis
       tables=NA,  # list of tables, each table (data.frame) and caption
       data=NA,  # list of data, that can potentially be written as shape files, raster etc
       extraParams=NA,  # list of data, that can potentially be written as shape files, raster etc
       res=NA)  # The actual result is stored here for later reuse
}

### Prepare animals
ares$animals <- list()
alog <- c(alog, catPro("setting up animal data"))

### Perform final subset on the data
if (config$config$dateTime) {
  ## Date and time was provided
  datSub <- datrm[datrm$timestamp >= ymd_hms(config$temporalBbxRestricted$tmin) &
                  datrm$timestamp <= ymd_hms(config$temporalBbxRestricted$tmax) &
                  datrm$lat >= config$spBbxRestricted$ymin &
                  datrm$lat <= config$spBbxRestricted$ymax &
                  datrm$lon >= config$spBbxRestricted$xmin &
                  datrm$lon <= config$spBbxRestricted$xmax, ]
} else {
  ## No date or time was provided
  datSub <- datrm[datrm$lat >= config$spBbxRestricted$ymin &
                  datrm$lat <= config$spBbxRestricted$ymax &
                  datrm$lon >= config$spBbxRestricted$xmin &
                  datrm$lon <= config$spBbxRestricted$xmax, ]
}

## subset only analyse animals that are wanted
datSub <- datSub[datSub$id %in% config$animal$id[as.logical(config$animal$include)],]

## Split
datSub <- split(datSub, datSub$id)
ids <- names(datSub)

## Cycle through animals
for (i in seq_along(ids)) {
  alog <- c(alog, catPro(paste0("preparing animal: ", ids[i])))
  
  ## Save everything here
  ares$animals[[i]] <- list()
  ares$animals[[i]]$summary <- list()

  ## name
  ares$animals[[i]]$summary$name <- ids[i]

  ## initial number of rows
  ares$animals[[i]]$summary$initNRows <- nrow(datSub[[i]])

  ## number of duplicates
  if (config$config$dateTime) {
    ares$animals[[i]]$summary$duplicatesN <- sum(duplicated(datSub[[i]][, c("lon", "lat", "timestamp")]))
  } else {
    ares$animals[[i]]$summary$duplicatesN <- sum(duplicated(datSub[[i]][, c("lon", "lat")]))
  }
  
  ares$animals[[i]]$summary$duplicatesWhatTodo <- config$config$duplicates
  
  if (config$config$duplicates == "jitter") {
      x <- datSub[[i]]
      whichDubplicates <- duplicated(x[, c("lat", "lon")])
      x[whichDubplicates, 'lat'] <- jitter(x[whichDubplicates, 'lat'])
      x[whichDubplicates, 'lon'] <- jitter(x[whichDubplicates, 'lon'])
      datSub[[i]] <- x
    } else if (config$config$duplicates == "remove") {
      x <- datSub[[i]]
      whichDubplicates <- duplicated(x[, c("lat", "lon")])
      datSub[[i]] <- x[!whichDubplicates,]
    }

  ## number of missing cases
  ares$animals[[i]]$summary$nMissingCases <- sum(!complete.cases(datSub[[i]]))


  ## Order
  if (config$config$dateTime) {
    datSub[[i]] <- datSub[[i]][order(datSub[[i]]$timestamp), ]
  }

  ## Make Spatial
  allgood <- tryCatch({
    alog <- c(alog, catPro("projecting data"))
    datSub[[i]] <- SpatialPointsDataFrame(datSub[[i]][, c("lon", "lat")], data=datSub[[i]])

    if (config$config$inEpsg != -1) {
      proj4string(datSub[[i]]) <- CRS(paste0("+init=epsg:", config$config$inEpsg))
    }
    if (config$config$outEpsg != -1) {
      ## Reporject
      alog <- c(alog, catPro(paste0("reprojecting data from ", config$config$inEpsg, " to ", config$config$outEpsg)))
      datSub[[i]] <- spTransform(datSub[[i]], CRS(paste0("+init=epsg:", config$config$outEpsg)))
    }
  }, error=function(e) e)

  if (inherits(allgood, "error")) {
    log <- c(log, catPro(paste0("failed with projecting and/or transforming data",
                                allgood$message)))
  } else {
    log <- c(log, catPro("succeeded with projecting and/or transomfing data"))
  }

  ## Final number of points
  ares$animals[[i]]$summary$finalNRows <- nrow(datSub[[i]])
}


## ============================================================================ # 
## Go through contexts
if (config$todo$doSiteFidelity) {
  ares$siteFidelity <- list()
  ares$siteFidelity[[1]] <- list()

  ## Subcontexts - none for site fidelity
  ares$siteFidelity[[1]]$params <- c(list(context="Site Fidelity", subContext=NA, name="siteFidelity"),
                                    config$analysis$siteFidelity)
  ares$siteFidelity[[1]]$animals <- lapply(ids, brewResults)
  
}

if (config$todo$doTTSI) {
  ares$TTSI <- list()

  ## Subcontexts - consec or non cesec
  variesOver <- unlist(config$analysis$ttsi$consec)
  for (i in seq_along(variesOver)) {

    thisParams <- config$analysis$ttsi
    thisParams$consec <- variesOver[i]
    ares$TTSI[[i]] <- list()
    
    ares$TTSI[[i]]$params <- c(list(context="Time to statistical independence (TTSI)",
                                           subContext=paste0("TTSI with ", ifelse(variesOver[i], "consecutive ",
                                             "non consecutive "), "observations"), name="ttsi"),
                                    thisParams)
  
    ares$TTSI[[i]]$animals <- lapply(ids, brewResults)
  }

}

if (config$todo$doMCP) {
  ares$MCP <- list()
  ares$MCP[[1]] <- list()

  ## Subcontexts - none for mcp
  ares$MCP[[1]]$params <- c(list(context="Minimum Convex Polygon(MCP)", subContext=NA, name="mcp"),
                                    config$analysis$mcp)
  
  ares$MCP[[1]]$animals <- lapply(ids, brewResults)

}

if (config$todo$doKDE) {
  ares$KDE <- list()

  ## Subcontexts - bandwidth, rescale
  variesOver0 <- unlist(config$analysis$kde$bandwidth)
  variesOver1 <- unlist(config$analysis$kde$rescale)
  idx <- 1

  for (i in seq_along(variesOver0)) {
    for (j in seq_along(variesOver1)) {
      thisParams <- config$analysis$kde
      thisParams$bandwidth <- variesOver0[i]
      thisParams$rescale <- variesOver1[j]
      ares$KDE[[idx]] <- list()
      
      ares$KDE[[idx]]$params <- c(list(context="Kernel Density Estimation (KDE)",
                                             subContext=paste0("KDE with: ", variesOver0[i], " bandwidth and rescaling: ",
                                               variesOver1[j]), name="kde"),
                                        thisParams)
      
      ares$KDE[[idx]]$animals <- lapply(ids, brewResults)
      idx <- idx + 1
    }
  }

}


if (config$todo$doLocoh) {
  ares$LoCoH <- list()

  ## Subcontexts - three types, different values
  variesOver0 <- config$analysis$locoh$type
  idx <- 1

  for (i in seq_along(variesOver0)) {
      thisParams <- config$analysis$locoh
      thisParams$type <- variesOver0[i]
      ares$LoCoH[[idx]] <- list()
      
      ares$LoCoH[[idx]]$params <- c(list(context="Local Convex Hull (LoCoH)",
                                        subContext=paste0("LoCoH type: ", variesOver0[i]), name="locoh"),
                                        thisParams)
      ares$LoCoH[[idx]]$animals <- lapply(ids, brewResults)
      idx <- idx + 1
  }

}

if (config$todo$doAsymptote) {
  ares$Asymptote <- list()

  ## Subcontexts - over all estimator and their subcontexts
  variesOver0 <- unlist(config$analysis$asymptote$estimator)
  idx <- 1

  for (i in seq_along(variesOver0)) {
      thisParams <- config$analysis$asymptote
      thisParams$estimator <- variesOver0[i]
      ares$Asymptote[[idx]] <- list()
      
      ares$Asymptote[[idx]]$params <- c(list(context="Asymptote",
                                        subContext=paste0("Asymptote for: ", variesOver0[i]), name="asymptote"),
                                        thisParams)
      ares$Asymptote[[idx]]$animals <- lapply(ids, brewResults)
      idx <- idx + 1
  }
}

if (config$todo$doCA) {
  ares$CA <- list()
  ares$CA[[1]] <- list()

  ## Subcontexts - none for CA
  ares$CA[[1]]$params <- c(list(context="Core Area (CA)", subContext=NA, name="ca"),
                                    config$analysis$ca)
  
  ares$CA[[1]]$animals <- lapply(ids, brewResults)

}

## Show animals

for (i in seq_along(ares$animals)) {
  res$write(h3(paste0("Animal: ", ares$animals[[i]]$summary$name)))
  res$write(rhrToHTML(changeToLongNames(params2df(ares$animals[[i]]$summary[-1]), config$config$longNames$animals)))
}


## Functions

writeVect <- function(x,
                       basename="",
                       formats=config$config$expVectExt,
                       fn=paste0(basename, ".", config$config$expVectExt),
                       driver=(config$config$expVectDriver)) {
  for (f in seq_along(formats)) {
    if (f == "kml") {
      ## we need to convert SRS back to geographical
      x <- spTransform(x, CRS("+init=epsg:4326"))
    } 

      writeOGR(obj=x,
               dsn=fn[f], 
               layer=basename(basename),
               driver=driver[f])
  }
}

writeRast <- function(x,
                      basename="",
                      formats=config$config$expRastDriver,
                      fn=paste0(basename, ".", config$config$expRastExt)) {
  for (f in seq_along(formats)) {
    writeRaster(x=x,
                filename=fn[f], 
                formrat=formats[f])
  }
}

showResultHTML <- function(context, des) {

  res$write(p(des))
  res$write(cat('<hr>'))

  ## cycle through subcontexts
  for (subcon in seq_along(context)) {

    ## If more than one subcontext is available give detials about them and extra headings, elso just continue
    hasSeveralContexts <- length(context) > 1

    if (hasSeveralContexts) {
      ## heading and params for each context
      res$write(h3(context[[subcon]]$param$subContext))
    }

    ## Write parameters
    ## We do not need the first three entries, because they are only used for internal reference
    res$write(rhrToHTML(changeToLongNames(params2df(context[[subcon]]$params[c(-1, -2, -3)]),
                                          config$config$longNames[[context[[subcon]]$params$name]])))

    
    for (animal in seq_along(context[[subcon]]$animals)) {
      thisAnimal <- context[[subcon]]$animals[[animal]]


      ## Results for each animal
      res$write(cat('<hr>'))
      res$write(h4(paste0("Results for animal: ", thisAnimal$name)))

      if (thisAnimal$exit == 0) {
        ## Extra params
        if (!is.na(thisAnimal$extraParams)) {
          res$write(rhrToHTML(changeToLongNames(params2df(thisAnimal$extraParams),
                                                config$config$longNames[[context[[subcon]]$params$name]])))
        }

        ## Plots
        if (!is.na(thisAnimal$plots)) {
          for (p in seq_along(thisAnimal$plots)) {

            ## include the plot
            res$write(img(paste0(imageurl, thisAnimal$plots[[p]]$filename),
                          cap=thisAnimal$plots[[p]]$cpation))
      
          }
        }

        ## Tables
        if (!is.na(thisAnimal$tables)) {
          for (table in seq_along(thisAnimal$tables)) {

            ## include the table
            res$write(rhrToHTML(thisAnimal$tables[[table]]$table,
                                cap=thisAnimal$tables[[table]]$caption))
          }
        }

        ## Messages
        if (!is.na(thisAnimal$msgs)) {
          for (msg in seq_along(thisAnimal$msgs)) {
            ## include the table
            res$write(p(thisAnimal$msgs[[msg]]))
          }
        }
      } else {
        res$write(rhrAlert(thisAnimal$error, class="error"))
      }
    }
  }
}


res$finish()

%>
