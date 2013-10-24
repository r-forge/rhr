<%

res <- Response$new()


# ============================================================================ #
# copy files

# plot
file.copy(list.files(imagepath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "plots"))

file.copy(list.files(datapath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "data"))

file.copy(list.files(docpath, full.names=TRUE, ignore.case=TRUE),
          file.path(outdirAnalysis, "doc"))

res$write(cat("Copied files to <code>", outdirAnalysis, "</code>"))

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


for (i in seq_along(names(datSub))) {

  fnRda.this <- fnRda[grepl(names(datSub)[i], fnRda)]
  fnKml.this <- fnKml[grepl(names(datSub)[i], fnKml)]
  fnTif.this <- fnTif[grepl(names(datSub)[i], fnTif)]
  fnShp.this <- fnShp[grepl(names(datSub)[i], fnShp)]

  res$write(h3(names(datSub[i])))
  res$write(cat("<ul>"))
  if (length(fnRda.this) > 0) res$write(cat(paste0("<li><a href='", dataurl, fnRda.this, "'>", fnRda.this, " </a></li>")))
  if (length(fnKml.this) > 0) res$write(cat(paste0("<li><a href='", dataurl, fnKml.this, "'>", fnKml.this, " </a></li>")))
  if (length(fnTif.this) > 0) res$write(cat(paste0("<li><a href='", dataurl, fnTif.this, "'>", fnTif.this, " </a></li>")))
  if (length(fnShp.this) > 0) res$write(cat(paste0("<li><a href='", dataurl, fnShp.this, "'>", fnShp.this, " </a></li>")))
  res$write(cat("</ul>"))
}


res$finish()

# ============================================================================ # 
# Remove data
rm(datSub)

%>
