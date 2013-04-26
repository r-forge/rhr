.e <- new.env()

#' GUI
#' 
#' Starts an interactive GUI
#' @param dat a data frame with the tracking data
#' @param outdir directory where results are saved
#' @param ... fruther arguments
#' @export
rhrGUI <- function(dat=NULL, outdir=getwd(), ...){

  # check if dat is ok
  if (!is.null(dat)) {
    if (ncol(dat) < 3) {
      stop("dat needs at least 3 columns")
    }
    
    if (nrow(dat) < 25) {
      stop("dat needs at least 25 rows")
    }


    if (is(dat, "SpatialPointsDataFrame")) {
      tc <- coordinates(dat)
      dat <- data.frame(x=tc[,1], y=tc[,2], dat)
    }

    if (!is(dat, "data.frame")) {
      stop("dat not of class data.frame or SpatialPointsDataFrame")
    }
  }

  dir.create(file.path(tempdir(),'plots'), showWarnings=FALSE)
  dir.create(file.path(tempdir(),'data'), showWarnings=FALSE)
  dir.create(file.path(tempdir(),'doc'), showWarnings=FALSE)
  
  #if (!exists(".e")){
    .e$s <- Rhttpd$new()
  #} else { 
  #  warning("Previouse session is now closed")
  #  .e$s$stop()
  #  .e$s$remove(all=TRUE)
  #}

  
  app <- Builder$new( Static$new( urls = c('/css','/img','/images','/js'),
                                 root = system.file("app", package="rhr")
                                 ),
    		     Static$new(urls=c('/plots', '/data', '/doc'), root=tempdir()),
                     Brewery$new( url='/brew',
                                 root= system.file("app", package="rhr"),
                                 imagepath=file.path(tempdir(), 'plots'),
                                 imageurl="../plots/",
                                 datapath=file.path(tempdir(), 'data'),
                                 dataurl="../data/",
                                 docpath=file.path(tempdir(), 'doc'),
                                 docurl="../doc/",
                                 datFromR=dat, 
                                 outdir=outdir
                                 ),
                     Redirect$new('/brew/load.rhtml')
                     )
  .e$s$launch(app=app, name='rhr') 
}


