#' @title load_environment
#' @description This function sets a bunch of parameters and outlines the package requirements for the bio.plotting package
#' This way, a variety of functions can use them without having to pass them through function calls
#' @return a list of parameters - \code{p.plotting}
#' @examples
#'  p.plotting = bio.plotting::load.environment() 
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
load_environment = function( libs = NULL, p.plotting = NULL ) {
  
  if (is.null(p.plotting)) p.plotting = list()
  
  p.plotting$project.name = 'bio.plotting'
  p.plotting$project.outdir.root = project.datadirectory( p.plotting$project.name, 'R' ) #required for interpolations and mapping
  
  rlibs = RLibrary ( "sp", "rgeos", "maptools", "mapdata", "classInt", "rgdal")
  blibs = bioLibrary( "bio.plotting") 
  
  if (!is.null(libs)) RLibrary(libs)
  if ( exists("libs", p.plotting) ) libs = c(libs, p.plotting$libs)
  
  p.plotting$libs = unique( c( libs, rlibs, blibs ) )
  
  #make.basemap()
  p.plotting$df = NULL   #this, in combination with auto.setlimits allows the map to automatically match the extent of the data
  p.plotting$auto.setlimits = F  
  p.plotting$x.limits = c(-70,-54)
  p.plotting$y.limits = c(41,50)
  p.plotting$crs.out = '+init=epsg:2220'
  
  #add.points()
  p.plotting$lat.field = 'LATITUDE'
  p.plotting$lon.field = 'LONGITUDE'
  p.plotting$plot.field = 'EST_COMBINED_WT.SUM'
  p.plotting$plot.field.pretty = p.plotting$plot.field #used by the legend
  p.plotting$show.legend = T
  p.plotting$pnt.style = 21
  p.plotting$cexMax = 1
  p.plotting$pnt.col = 'black'
  p.plotting$pnt.bg = 'red'
  p.plotting$use.buckets = F
  p.plotting$nclasses = 3
  
  #get.known.areas()
  p.plotting$known.areas = NULL 
  p.plotting$known.areas.add = T
  p.plotting$known.areas.detailed = F
  
  return(p.plotting)
}