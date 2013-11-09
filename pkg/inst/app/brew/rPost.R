<%

res <- Response$new()

res$write(h2("Files"))
res$write(p(paste0("All data and files were automatically copied to: <code>", outdirAnalysis, "</code>")))


## Do I have to export the data
if (config$config$expData) {
  write.csv(do.call(rbind.data.frame, datSub), file=file.path(datapath, "data_used.csv"), row.names=FALSE)
}

## Write Log
if (config$config$writeLog) {
  writeLines(c(plog, alog), con=file.path(docpath, "log.txt"))
}

## Write Config
saveRDS(config, file=file.path(datapath, "config.rds"))

## ============================================================================ #
## copy files

## plot
file.copy(list.files(imagepath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "plots"))

## data
file.copy(list.files(datapath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "data"))

## plots
file.copy(list.files(docpath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "doc"))

res$write(p(paste0("Copied files to <code>", outdirAnalysis, "</code>")))

res$finish()

# ============================================================================ # 
# Remove data
#rm(datSub)

%>
