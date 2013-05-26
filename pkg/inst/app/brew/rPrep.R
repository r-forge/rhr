<%

res <- Response$new()

# ============================================================================ # 
# Provide data

# Do subset
# subset time & space

if (config$config$dateTime) {
datSub <- datrm[datrm$timestamp >= ymd_hms(config$temporalBbxRestricted$tmin) &
                datrm$timestamp <= ymd_hms(config$temporalBbxRestricted$tmax) &
                datrm$lat >= config$spBbxRestricted$ymin &
                datrm$lat <= config$spBbxRestricted$ymax &
                datrm$lon >= config$spBbxRestricted$xmin &
                datrm$lon <= config$spBbxRestricted$xmax, ]
} else {
datSub <- datrm[datrm$lat >= config$spBbxRestricted$ymin &
                datrm$lat <= config$spBbxRestricted$ymax &
                datrm$lon >= config$spBbxRestricted$xmin &
                datrm$lon <= config$spBbxRestricted$xmax, ]
}

# Add the number of relocations to the config list
config$n$restrictedN <- nrow(datSub)

# subset ind
datSub <- datSub[datSub$id %in% config$animal$id[as.logical(config$animal$include)],]
datSub <- split(datSub, datSub$id)



## Ids
ids <- names(datSub)

# ============================================================================ #
# Parallel processing
# check if cluster is still running
#while (sfIsRunning()) {
#  sfStop()
#}
#
#  res$write(cat(config$config$ncores))
#  res$write(cat(class(config$config$ncores)))
#if (config$config$ncores > 1) {
#  # we want parallel
#
#  res$write(h2(config$config$ncores))
#  res$write(h2("................."))
#  res$write(h2(class(config$config$ncores)))
#  #sfInit(parallel=TRUE, cpus=2, type="SOCK", useRscript=TRUE)
#  #sfInit(parallel=TRUE, cpus=config$config$ncores, type="SOCK")
# #sfInit(parallel=TRUE, cpus=2, type="SOCK")
#  #sfInit(parallel=FALSE)
#
#} else {
#
#  sfInit(parallel=FALSE)
#
#}
#
#
## lib to slaves
# sfLibrary(rhr)
##sfExport("datSub")
#sfExport("config")

# ============================================================================ #
# Create outdirs
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
