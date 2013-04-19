<%

  ## Add file name to config
    if (!is.null(datFromR)) {
      name <- 'data loaded from R'
    } else {
      config$fileName <- name$filename
    }

  # check so no empty fields are passed
  config$mapFields$id <- id <- req$POST()[['id']] 
  config$mapFields$lat <- lat <- req$POST()[['lat']]
  config$mapFields$lon <- lon <- req$POST()[['lon']]
  config$mapFields$date$date <- date <- req$POST()[['date']]
  config$mapFields$date$dateFormat <- dateformat <- req$POST()[['dateformat']]
  config$mapFields$time$time <- time <- req$POST()[['time']]
  config$mapFields$time$timeFormat <- timeformat <- req$POST()[['timeformat']]

  date.parsed <- eval(parse(text=paste0(dateformat, "(dat[, date])")))
  time.parsed <- eval(parse(text=paste0(timeformat, "(dat[, time])")))  
  timestamp <- date.parsed + time.parsed

  datrm <- data.frame(id=dat[, id], lat=dat[, lat], lon=dat[, lon], timestamp=timestamp, stringsAsFactors=FALSE)
 
  #--- datsubSplit <- datsub <- datrm
  

  ## Add animal ids to config
  config$animal$ids <- unique(datrm$id)
  config$animal$include <- rep(1, length(config$animal$ids))

  ## Add bbx to config
  config$spBbx$xmin <- min(datrm$lon)
  config$spBbx$xmax <- max(datrm$lon)
  config$spBbx$ymin <- min(datrm$lat)
  config$spBbx$ymax <- max(datrm$lat)
  config$n$initialN <- nrow(datrm)

  ## Prep
  config$spBbxRestricted$xmin <- min(datrm$lon)
  config$spBbxRestricted$xmax <- max(datrm$lon)
  config$spBbxRestricted$ymin <- min(datrm$lat)
  config$spBbxRestricted$ymax <- max(datrm$lat)
  config$n$restrictedN <- nrow(datrm)

  config$temporalBbxRestricted$tmin <- as.character(min(datrm$timestamp))
  config$temporalBbxRestricted$tmax <- as.character(max(datrm$timestamp))

  config$temporalBbx$tmin <- as.character(min(datrm$timestamp))
  config$temporalBbx$tmax <- as.character(max(datrm$timestamp))


%>
