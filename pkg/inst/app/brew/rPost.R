<%

res <- Response$new()

res$write(h2("Writing files"))
res$write(p(paste0("All files will be automatically copied to: <code>", outdirAnalysis, "</code>")))

## Cycle through all analytical steps
for (context in ares) {
  for (subcontext in context) {
    for (animal in subcontext$animals) {
      if (!is.na(subcontext$params$subContext)) {
        prefix <- subcontext$params$subContext
      } else {
        prefix <- subcontext$params$context
      }
      ## any data to write?
      if (!is.na(animal$data)) {
        res$write(cat("<ul>"))
        ## any vector data?
        if (!is.null(animal$data$vect)) {
          for (vect in animal$data$vect) {
            for (format in 1:length(config$config$expVectDo)) {
              if (as.logical(config$config$expVectDo[format])) {
                tryCatch({
                  writeOGR(obj=vect$data,
                           dsn=file.path(datapath, paste0(vect$filename, ".", config$config$expVectExt[format])),
                           layer=vect$filename,
                           driver=config$config$expVectDriver[format])
                  res$write(cat(paste0("<li> ", prefix, " successfully written: ",
                                       paste0(vect$filename, ".", config$config$expVectExt[format]), "</li>")))
                }, error=function(e) {
                  res$write(cat(paste0("<li> error while writing: ",
                                       paste0(prefix, " ", vect$filename, ".", config$config$expVectExt[format]), "</li>")))
                })
              }
            }
          }
        }
        ## any raster data?
        if (!is.null(animal$data$rast)) {
          for (rast in animal$data$rast) {
            for (format in 1:length(config$config$expRastDo)) {
              if (as.logical(config$config$expRastDo[format])) {
                tryCatch({
                  writeRaster(x=rast$data,
                              filename=file.path(datapath, paste0(rast$filename, ".", config$config$expRastExt[format])),
                              formrat=config$config$expRastDriver[format])
                  res$write(cat(paste0("<li> ", prefix, " successfully written: ",
                                       paste0(rast$filename, ".", config$config$expRastExt[format]), "</li>")))
                }, error=function(e) {
                  res$write(cat(paste0("<li> error while writing: ",
                                       paste0(prefix, " ", rast$filename, ".", config$config$expRastExt[format]), "</li>")))
                })
              }
            }
          }
        }
        ## any text data?
        if (!is.null(animal$data$text)) {
          res$write(p("now we would write some text data"))
        }
      }
      res$write(cat("</ul>"))
    }
  }
}

## Do I have to export the data
if (config$config$expData) {
  write.csv(do.call(rbind.data.frame, datSub), file=file.path(datapath, "data_used.csv"), row.names=FALSE)
}

## Write Log
if (config$config$writeLog) {
  writeLines(c(plog, alog), con=file.path(docpath, "log.txt"))
}
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
rm(datSub)

%>
