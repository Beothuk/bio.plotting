#' @title add.points
#' @description This function adds points to an existing plot.  Data is symbolized by a selected field, and a legend can be drawn
#' @param \code{df} the data to be aggregated
#' @param \code{basemap.Info} a SpatialPolygons object (identifying the boundaries and projection of an existing plot)
#' @param \code{lat.field} the default is \code{'LATITUDE'}. the name of the field holding latitude values (in decimal degrees)
#' @param \code{long.field} the default is \code{'LONGITUDE'}.  the name of the field holding longitude values (in decimal degrees)
#' @param \code{plot.field} the default is \code{'EST_COMBINED_WT.SUM'}.  The field on which to symbolize the data.
#' @param \code{plot.field.pretty} the default is \code{NULL}. A nice name to describe \code{plot.field} (used by the legend)
#' @param \code{show.legend} the default is \code{T}. Displays a legend for the added points.
#' @param \code{pnt.style} the default is \code{19}. Determines the style of the points (any valid value for \code{pch} is acceptable)
#' @param \code{nclasses} the default is \code{5}. Determines how many "bins" to use to display the data.  
#' @param \code{cexMax} the default is \code{3}. Determines the maximum size of the points to be drawn (any valid value for \code{cex} is 
#' acceptable)
#' @return a data frame 'allCatchInfo'
#' @examples
#'  add.points(df, map, lat.field='LAT', lon.field='LON', plot.field='WEIGHT_SMOOTHED', plot.field.pretty='WEIGHT', classes=10, cexMax=1.5)
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
add.points<-function(df, basemap.Info, p.plotting=p.plotting ){
  #get plotting parameters
  #p.plotting <<- bio.plotting::load.environment() 
    lat.field = p.plotting$lat.field
    long.field = p.plotting$long.field
    plot.field = p.plotting$plot.field
    plot.field.pretty = p.plotting$plot.field.pretty
    show.legend = p.plotting$show.legend
    pnt.style = p.plotting$pnt.style
    nclasses = p.plotting$nclasses
    cexMax = p.plotting$cexMax
  
  if (is.null(plot.field.pretty)) plot.field.pretty=plot.field
  # require(bio.utilities)
  # require(classInt)
  df$ORD = seq.int(nrow(df))
  classes = classIntervals(df[,plot.field], n=nclasses, style= "quantile", dataPrecision=0)
  colcode = findColours(classes, c("#c7e9b4","#41b6c4","#225ea8","#081d58")) #colorblind-friendly yellow-blue
  colour.df = data.frame(varname=classes$var,colcode, ptSizer=findCols(classes))
  colour.df$ptSizer = as.numeric(colour.df$ptSizer)
  names(colour.df)[names(colour.df)=="varname"] <- plot.field
  df=merge( df,unique(colour.df), all.x = T)
  df = df[order(df$ORD),]

  #following line assumes data is latlong
  df.xy = df[,c(long.field,lat.field)]
  df.sp <- SpatialPointsDataFrame(coords = df.xy, data = df, proj4string = CRS("+init=epsg:4326"))
  #uses basemap info to reproject to appropriate proj
  df.sp.tr=spTransform(df.sp,CRS(basemap.Info@proj4string@projargs))
  plot(df.sp.tr, col = df.sp.tr@data$colcode, pch = pnt.style, cex = (df.sp.tr@data$ptSizer/nclasses)*cexMax, add=T)
   if (show.legend){
    leg.pos=c(min(basemap.Info@bbox[1,]),max(basemap.Info@bbox[2,]))
    ptSizes=U(df.sp.tr$ptSizer/nclasses)*cexMax
    legend(#leg.pos[1], leg.pos[2], 
          'topleft',
           title = plot.field.pretty,
           pch=pnt.style,
           col=c(attr(colcode, "palette"),"white"),
           pt.bg =c(attr(colcode, "palette"),"white"),
           pt.cex=ptSizes,
           legend = c(gsub(",","-",names(attr(colcode, "table"))),"no data"))
   }
  }