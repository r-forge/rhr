<%
# Get data

if (!is.null(a <- req$POST()[['timemin']])) config$temporalBbxRestricted$tmin <- as.character(a)
if (!is.null(a <- req$POST()[['timemax']])) config$temporalBbxRestricted$tmax <- as.character(a)
if (!is.null(a <- req$POST()[['lonmin']])) config$spBbxRestricted$xmin <- a
if (!is.null(a <- req$POST()[['lonmax']])) config$spBbxRestricted$xmax <- a
if (!is.null(a <- req$POST()[['latmin']])) config$spBbxRestricted$ymin <- a
if (!is.null(a <- req$POST()[['latmax']])) config$spBbxRestricted$ymax <- a
if (!is.null(a <- req$POST()[['doAnimal']])) config$animal$include <- fromJSON(a)


res <- Response$new()

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

  # subset ind
datSub <- datSub[datSub$id %in% config$animal$id[as.logical(config$animal$include)],]

# ============================================================================ #
# Results

res$write(printSpRestriction(datrm, datSub,
                           lonmin=config$spBbxRestricted$xmin,
                           lonmax=config$spBbxRestricted$xmax,
                           latmin=config$spBbxRestricted$ymin,
                           latmax=config$spBbxRestricted$ymax))
if (config$config$dateTime) {
res$write(printTemporalRestriction(datrm, datSub,
                                   tmin=config$temporalBbxRestricted$tmin,
                                   tmax=config$temporalBbxRestricted$tmax))
}

# checkboxes wether or not select an animal
res$write(h3("Use the following animals:"))
res$write(cat('<div class="btn-group">'))
res$write(cat('<button type="button" id="btnCheckAll1" class="btn btn-mini">Check All</button>'))
res$write(cat('<button type="button" id="btnUncheckAll1" class="btn btn-mini">Uncheck All</button>'))
res$write(cat('</div><br><br>'))

# Wrte animals
res$write(cat(paste0('<table class="table table-striped"><tr><th>include</th><th>id</th><th>n (total)</th><th>n (current)</th></tr>',
                   paste0('<tr><td><input type="checkbox" name="selectAnimal" value="', config$animal$id, '"',
                          ifelse(config$animal$include, 'checked="checked"', '' ), '><td>',
                          config$animal$id, "</td><td>",
                          sapply(config$animal$id, function(x) sum(datrm$id == x)), "</td><td>",
                          sapply(config$animal$id, function(x) sum(datSub$id == x)),
                          "</td></tr>", collapse=""), "</table>", collapse="")))



res$write(cat('<div class="btn-group">'))
res$write(cat('<button type="button" type="button" id="btnCheckAll" class="btn btn-mini">Check All</button>'))
res$write(cat('<button type="button" id="btnUncheckAll" class="btn btn-mini">Uncheck All</button>'))
res$write(cat('</div><br><br>'))

res$write(cat('<button type="button" id="restrictionApply" class="btn btn-primary">Apply Restriction</button>'))
# res$write(cat('<button type="button" id="restrictionReset" class="btn">Reset</button>'))
res$finish()

rm(datSub)
%>
