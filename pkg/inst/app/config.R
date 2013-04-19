# Load library
library(ggplot2)
library(RJSONIO)
library(rhr)

dir.create(file.path(tempdir(),'plots'), showWarnings=FALSE)
dir.create(file.path(tempdir(),'data'), showWarnings=FALSE)
dir.create(file.path(tempdir(),'doc'), showWarnings=FALSE)

  
app <- Builder$new( Static$new( urls = c('/css','/img','/images','/js'),
                               root = system.file("app", package="rhr")
                               ),
                   Static$new(urls=c('/plots', '/data', '/doc'), root=tempdir()),
                   Brewery$new( url='/brew',
                               root= ".",
                               imagepath=file.path(tempdir(), 'plots'),
                               imageurl="../plots/",
                               datapath=file.path(tempdir(), 'data'),
                               dataurl="../data/",
                               docpath=file.path(tempdir(), 'doc'),
                               docurl="../doc/",
                               datFromR=NULL, 
                               outdir="~/"
                               ),
                   Redirect$new('/brew/load.rhtml')
)
