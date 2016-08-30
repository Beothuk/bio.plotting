#' @title load.environment
#' @description This function sets a bunch of parameters and outlines the package requirements for the bio.plotting package
#' This way, a variety of functions can use them without having to pass them through function calls
#' @return a list of parameters - \code{p.plotting}'
#' @examples
#'  p.plotting = bio.plotting::load.environment() 
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
load.environment = function( libs = NULL, p.plotting = NULL ) {

    if (is.null(p.plotting)) p.plotting = list()
  
  p.plotting$project.name = 'bio.plotting'
  p.plotting$project.outdir.root = project.datadirectory( p.plotting$project.name, 'R' ) #required for interpolations and mapping
  
  rlibs = RLibrary ( "sp", "rgeos", "maptools", "mapdata", "classInt")
  blibs = bioLibrary( "bio.plotting") 
  
  if (!is.null(libs)) RLibrary(libs)
  if ( exists("libs", p.plotting) ) libs = c(libs, p.plotting$libs)
  
  p.plotting$libs = unique( c( libs, rlibs, blibs ) )
  
  p.plotting$libs = c(rlibs)
  
  #bio.plotting
    #make.basemap()
    p.plotting$df = NULL
    p.plotting$auto.setlimits = F
    p.plotting$x.limits = c(-70,-54)
    p.plotting$y.limits = c(41,50)
    p.plotting$add.DFO = T
    p.plotting$crs.out = '+init=epsg:2220'
    
    #add.points()
    #df, 
    #p.plotting$basemap.Info
    p.plotting$lat.field = 'LATITUDE'
    p.plotting$long.field = 'LONGITUDE'
    p.plotting$plot.field = 'EST_COMBINED_WT.SUM'
    p.plotting$plot.field.pretty = NULL
    p.plotting$show.legend = T
    p.plotting$pnt.style = 19
    p.plotting$nclasses = 5
    p.plotting$cexMax = 3
    
    #get.DFO.areas()
    p.plotting$DFO.areas = c('St_Ann','Gully','Vazella_Emerald','Vazella_Sambro','Lophelia', 'NE_Channel')
    p.plotting$DFO.detailed = F
  
  return(p.plotting)
}