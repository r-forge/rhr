<%

res <- Response$new()

## ============================================================================ # 
## Provide data

## Do subset
## subset time & space

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

## Add the number of relocations to the config list
config$n$restrictedN <- nrow(datSub)

## subset only take the once that are requested
datSub <- datSub[datSub$id %in% config$animal$id[as.logical(config$animal$include)],]

## Split
datSub <- split(datSub, datSub$id)

## Ids
ids <- names(datSub)

## ============================================================================ # 
## Deal with duplicates
## There are three options
## 1. Delete duplicates
## 2. Add random noise
## 3. Do nothing
if (config$config$duplicates == "jitter") {
  for (i in seq_along(datSub)) {
    x <- datSub[[i]]
    whichDubplicates <- duplicated(x[, c("lat", "lon")])
    x[whichDubplicates, 'lat'] <- jitter(x[whichDubplicates, 'lat'])
    x[whichDubplicates, 'lon'] <- jitter(x[whichDubplicates, 'lon'])
    datSub[[i]] <- x
  }
} else if (config$config$duplicates == "remove") {
  for (i in seq_along(datSub)) {
    x <- datSub[[i]]
    whichDubplicates <- duplicated(x[, c("lat", "lon")])
    datSub[[i]] <- x[!whichDubplicates,]
  }
}

## Remove animals with less than 10 points
datSub <- datSub[sapply(datSub, nrow) >= 10]


## Order data for each animal by timestamp if provided
if (config$config$dateTime) {
  for (i in seq_along(datSub)) {
    datSub[[i]] <- datSub[[i]][order(datSub[[i]]$timestamp), ]
  }
}

## ============================================================================ # 
## Validate and correct input
config$estimator$mcp$level   <- rhrCorrectLevels(config$estimator$mcp$level)
config$estimator$kde$level   <- rhrCorrectLevels(config$estimator$kde$level)
config$estimator$locoh$level <- rhrCorrectLevels(config$estimator$locoh$level)



## ============================================================================ #
## Create outdirs
outdirAnalysis <- file.path(outdir, paste0("rhr_run_", format(Sys.time(), format="%Y-%m-%d-%H-%M-%S")))

dir.create(outdirAnalysis)
dir.create(file.path(outdirAnalysis, "plots"))
dir.create(file.path(outdirAnalysis, "data"))
dir.create(file.path(outdirAnalysis, "doc"))

## Delete files from last run
file.remove(list.files(imagepath, full.names=TRUE))
file.remove(list.files(datapath, full.names=TRUE))
file.remove(list.files(docpath, full.names=TRUE))

res$finish()

%>
