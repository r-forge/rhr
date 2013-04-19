<%

res <- Response$new()

# ============================================================================ # 
# Remove data
rm(datSub)

# ============================================================================ #
# copy files

# plot
file.copy(list.files(imagepath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "plots"))

file.copy(list.files(datapath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "data"))

file.copy(list.files(docpath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "doc"))

res$write(cat("Copied fiels <code>", outdirAnalysis, "</code>"))

# ============================================================================ #
# Make data available for download

res$write(h2("Files for Download"))


fn <- list.files(datapath)
fnRda <- fn[grepl("*.rda$", fn)]
fnKml <- fn[grepl("*.kml$", fn)]
fnTif <- fn[grepl("*.tif$", fn)]

fnShp <- fn[!fn %in% c(fnRda, fnKml, fnTif)]
fnShp <- fnShp[!grepl("*.png", fnShp)]
fnShp <- fnShp[!grepl("*.pdf", fnShp)]

res$write(h3("Rdata"))
res$write(cat("<ul>"))
res$write(cat(paste0("<li><a href='", dataurl, fnRda, "'>", fnRda, " </a></li>")))
res$write(cat("</ul>"))

if (config$config$expKML) {
  res$write(h3("Kml"))
  res$write(cat("<ul>"))
  res$write(cat(paste0("<li><a href='", dataurl, fnKml, "'>", fnKml, " </a></li>")))
  res$write(cat("</ul>"))
}

if (config$todo$doKDE | config$todo$doCA) {
  res$write(h3("Tif"))
  res$write(cat("<ul>"))
  res$write(cat(paste0("<li><a href='", dataurl, fnTif, "'>", fnTif, " </a></li>")))
  res$write(cat("</ul>"))
  res$write(h3("Shape file"))
  res$write(cat("<ul>"))
  res$write(cat(paste0("<li><a href='", dataurl, fnShp, "'>", fnShp, " </a></li>")))
  res$write(cat("</ul>"))
}

if (config$todo$doLocoh | config$todo$doMCP) {
  res$write(h3("Shape file"))
  res$write(cat("<ul>"))
  res$write(cat(paste0("<li><a href='", dataurl, fnShp, "'>", fnShp, " </a></li>")))
  res$write(cat("</ul>"))
}

res$finish()

%>
