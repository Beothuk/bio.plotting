#' @title load_plotting_environment
#' @description This function sets a bunch of parameters and outlines the package requirements for the bio.plotting package
#' This way, a variety of functions can use them without having to pass them through function calls
#' @return a list of parameters - \code{p.plotting}
#' @examples
#'  p.plotting = load_plotting_environment()
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
load_plotting_environment = function( ) {
  
  if (!exists("p.plotting")) p.plotting = list()
  
  #make.basemap()
  p.plotting$df = NULL   
  p.plotting$auto.setlimits = F  
  p.plotting$x.limits = c(-70,-54)
  p.plotting$y.limits = c(41,50)
  p.plotting$crs.out = '+init=epsg:2220' #UTMZ20N
  
  #add.points()
  p.plotting$lat.field = 'LATITUDE'
  p.plotting$lon.field = 'LONGITUDE'
  p.plotting$plot.field = NULL
  p.plotting$plot.field.pretty = NULL
  p.plotting$show.legend = F
  p.plotting$pnt.style = 21
  p.plotting$pt.cex.min = 1
  p.plotting$pt.cex.max = 2
  p.plotting$pnt.col = 'black'  #point outline
  p.plotting$pnt.bg = 'red'
  p.plotting$use.buckets = T
  p.plotting$nclasses = 3
  
  # #get.known.areas() #on hold till central source works
  # p.plotting$known.areas = NULL 
  # p.plotting$known.areas.add = T
  # p.plotting$known.areas.detailed = T
  # p.plotting$gis.archive.root.path = NULL
  
  return(p.plotting)
}