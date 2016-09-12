#' @title add_points
#' @description This function adds points to an existing plot.  The advantage over \code{points(...)} is that by including a map object as 
#' input, it will automatically reproject the points, and trim them such that only positions that fall within the map boundaries are shown.  
#' The user is notified of how many positions lay outside of the map boundaries.   
#' Additionally, the input data can either be symbolized generically (i.e. one style), or it can classify the points according to the values
#' stored in a field identified by the user using the flag \code{use.buckets}.  If \{code{use.buckets=T}, data is symbolized by a selected 
#' field, and the colour and size of the points varies with the values in \code{plot.field}.
#' @param \code{p.plotting} a list derived from \code{bio.plotting::load_environment()} holding some default variables
#' @param \code{df} the data to be plotted
#' @param \code{basemap.Info} a SpatialPolygons object (identifying the boundaries and projection of an existing plot). If none is provided, a default \code{make_basemap()} object will be used. object
#' @param \code{lat.field} the default is \code{NULL}. The name of the field holding latitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param \code{lon.field} the default is \code{NULL}.  The name of the field holding longitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param \code{plot.field} the default is \code{NULL}.  The field on which to symbolize the data.  If unspecified, the value from p.plotting will be used.
#' @param \code{pnt.style} the default is \code{NULL}. Determines the style of the points (any valid value for \code{pch} is acceptable).  If unspecified, the value from p.plotting will be used.
#' @param \code{pnt.col} the default is \code{NULL}. This determines the colour outlining the points.  If unspecified, the value from p.plotting will be used.
#' @param \code{pnt.bg} the default is \code{NULL}.  Applies only when \code{show.legend = T}.  Determines the colour of the points.  If unspecified, the value from p.plotting will be used.
#' @param \code{cexMax} the default is \code{NULL}. When \code{use.buckets = F}, this determines the size of points, and when \code{use.buckets = T}, this determines the maximum size of the points to be drawn.  If unspecified, the value from p.plotting will be used.
#' @param \code{show.legend} the default is \code{NULL}.  Determines whether or not a legend will be displayed.  If unspecified, the value from p.plotting will be used.
#' @param \code{plot.field.pretty} the default is \code{NULL}.  Applies only when \code{use.buckets = F}. This is a nice name to describe \code{plot.field} which will be displayed in the legend.  If unspecified, the value from p.plotting will be used.
#' @param \code{use.buckets} the default is \code{NULL}. If \code{use.buckets = F}, all points are identical, but if \code{use.buckets = T}, data points are scaled, and teh colour intensity varies according to the value of \code{plot.field}.  If unspecified, the value from p.plotting will be used.
#' @param \code{nclasses} the default is \code{NULL}. Applies only when \code{use.buckets = T}.  Determines how many "bins" to use to display the data.  If unspecified, the value from p.plotting will be used. 
#' @return NULL, but notifies the user of how many positions lay outside of the map boundaries.
#' @examples
#'  add.points(p.plotting, df, map, lat.field='LAT', lon.field='LON', plot.field='WEIGHT_SMOOTHED', plot.field.pretty='WEIGHT', use.buckets=T, classes=10, cexMax=1.5)
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
add_points<-function(p.plotting=p.plotting, df, basemap.Info =  NULL, lat.field=NULL, lon.field = NULL, plot.field = NULL, 
                     pnt.style = NULL, pnt.col = NULL, pnt.bg = NULL, cexMax= NULL,
                     show.legend = NULL, plot.field.pretty = NULL, 
                     use.buckets=NULL, nclasses=NULL ){
  
  if (is.null(lat.field)) lat.field = p.plotting$lat.field
  if (is.null(lon.field)) lon.field = p.plotting$lon.field
  if (is.null(plot.field)) plot.field = p.plotting$plot.field
  if (is.null(pnt.style)) pnt.style = p.plotting$pnt.style
  if (is.null(pnt.col)) pnt.col = p.plotting$pnt.col
  if (is.null(pnt.bg)) pnt.bg = p.plotting$pnt.bg
  if (is.null(cexMax)) cexMax = p.plotting$cexMax
  
  if (is.null(use.buckets)) use.buckets = p.plotting$use.buckets
  if (is.null(nclasses)) nclasses = p.plotting$nclasses
  if (is.null(show.legend)) show.legend = p.plotting$show.legend
  if (is.null(plot.field.pretty)) plot.field.pretty = p.plotting$plot.field

  if (is.null(basemap.Info)) basemap.Info = make_basemap(p.plotting)
  
  df.xy = df[,c(lon.field,lat.field)]
  df.sp <- SpatialPointsDataFrame(coords = df.xy, data = df, proj4string = CRS("+init=epsg:4326"))
  df.sp.tr=spTransform(df.sp,CRS(basemap.Info@proj4string@projargs))
  df.sp.tr$over=over(df.sp.tr,basemap.Info)
  print(paste0(NROW(df.sp.tr[is.na(df.sp.tr$over),])," of ", NROW(df.sp.tr), " positions lie outside of the map"))
  df.sp.tr = df.sp.tr[!is.na(df.sp.tr$over),]
  df.sp.tr$over=NULL
  if (use.buckets){
    df.sp.tr$ORD = seq.int(nrow(df.sp.tr))
    df.classes = as.data.frame(df.sp.tr)
    df.classes = df.classes[,c("ORD", plot.field)]
    classes = classIntervals(df.classes[,plot.field], n=nclasses, style= "quantile", dataPrecision=0)
    colcode = findColours(classes, c("#c7e9b4","#41b6c4","#225ea8","#081d58")) #colorblind-friendly yellow-blue
    colour.df = data.frame(varname=classes$var,colcode, ptSizer=findCols(classes))
    colour.df$ptSizer = as.numeric(colour.df$ptSizer)
    names(colour.df)[names(colour.df)=="varname"] <- plot.field
    df.sp.tr=merge( df.sp.tr,unique(colour.df), all.x = T)
    df.sp.tr = df.sp.tr[order(df.sp.tr$ORD),]
  }
    #Maybe overwrite these defaults
    ptnames = plot.field.pretty
    ptlegend = "data"
    
    if (use.buckets){
      pnt.bg = df.sp.tr@data$colcode
      cexMax = sort(unique((df.sp.tr@data$ptSizer/nclasses)))*cexMax
      ptnames = names(attr(colcode, "table"))
      ptlegend = gsub(",","-",ptnames)
    }

  plot(df.sp.tr, col = pnt.col, bg= pnt.bg, pch = pnt.style, cex = cexMax, add=T)
    
    if (show.legend){
      leg.pos=c(min(basemap.Info@bbox[1,]),max(basemap.Info@bbox[2,]))
      legend(
        'topleft',
        title = plot.field.pretty,
        pch=pnt.style,
        col=c(pnt.col,"white"),
        pt.bg =c(pnt.bg,"white"),
        pt.cex=cexMax,
        legend = ptlegend
        )
    }
}