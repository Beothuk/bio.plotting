#' @title add_points
#' @description This function adds points to an existing plot.  The advantage over \code{points(...)} is that by including a map object as 
#' input, it will automatically reproject the points, and trim them such that only positions that fall within the map boundaries are shown.  
#' The user is notified of how many positions lay outside of the map boundaries.   
#' Additionally, the input data can either be symbolized generically (i.e. one style), or it can classify the points according to the values
#' stored in a field identified by the user using the flag \code{use.buckets}.  If \{code{use.buckets=T}, data is symbolized by a selected 
#' field, and the colour and size of the points varies with the values in \code{plot.field}.
#' @param df the data to be plotted
#' @param basemap.Info a SpatialPolygons object (identifying the boundaries and projection of an existing plot). If none is provided, a default \code{make_basemap()} object will be used. object
#' @param lat.field the default is \code{NULL}. The name of the field holding latitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param lon.field the default is \code{NULL}.  The name of the field holding longitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param plot.field the default is \code{NULL}.  The field on which to symbolize the data.  If unspecified, the value from p.plotting will be used.
#' @param pnt.style the default is \code{NULL}. Determines the style of the points (any valid value for \code{pch} is acceptable).  If unspecified, the value from p.plotting will be used.
#' @param pnt.col the default is \code{NULL}. This determines the colour outlining the points.  If unspecified, the value from p.plotting will be used.
#' @param pnt.bg the default is \code{NULL}.  Applies only when \code{show.legend = T}.  Determines the colour of the points.  If unspecified, the value from p.plotting will be used.
#' @param pt.cex.min the default is \code{NULL}. When \code{use.buckets = F}, this determines the size of the points, and when \code{use.buckets = T}, this determines the minimum size of the points to be drawn.  If unspecified, the value from p.plotting will be used.
#' @param pt.cex.max the default is \code{NULL}. When \code{use.buckets = F}, this is ignored, and when \code{use.buckets = T}, this determines the maximum size of the points to be drawn.  If unspecified, the value from p.plotting will be used.
#' @param show.legend the default is \code{NULL}.  Determines whether or not a legend will be displayed.  If unspecified, the value from p.plotting will be used.
#' @param plot.field.pretty the default is \code{NULL}.  Applies only when \code{use.buckets = F}. This is a nice name to describe \code{plot.field} which will be displayed in the legend.  If unspecified, the value from p.plotting will be used.
#' @param use.buckets the default is \code{NULL}. If \code{use.buckets = F}, all points are identical, but if \code{use.buckets = T}, data points are scaled, and teh colour intensity varies according to the value of \code{plot.field}.  If unspecified, the value from p.plotting will be used.
#' @param nclasses the default is \code{NULL}. Applies only when \code{use.buckets = T}.  Determines how many "bins" to use to display the data.  If unspecified, the value from p.plotting will be used. 
#' @return NULL, but notifies the user of how many positions lay outside of the map boundaries.
#' @importFrom sp over
#' @importFrom sp spTransform
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom classInt classIntervals
#' @importFrom classInt findColours
#' @importFrom classInt findCols
#' @importFrom graphics plot
#' @importFrom graphics legend
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
add_points<-function(df, basemap.Info =  NULL, lat.field=NULL, lon.field = NULL, plot.field = NULL, 
                     pnt.style = NULL, pnt.col = NULL, pnt.bg = NULL, pt.cex.min=NULL, pt.cex.max= NULL,
                     show.legend = NULL, plot.field.pretty = NULL, 
                     use.buckets=NULL, nclasses=NULL ){
  if (!exists("p.plotting")) p.plotting=load_plotting_environment()
  if (is.null(lat.field)) lat.field = p.plotting$lat.field
  if (is.null(lon.field)) lon.field = p.plotting$lon.field
  if (is.null(plot.field)) plot.field = p.plotting$plot.field
  if (is.null(pnt.style)) pnt.style = p.plotting$pnt.style
  if (is.null(pnt.col)) pnt.col = p.plotting$pnt.col
  if (is.null(pnt.bg)) pnt.bg = p.plotting$pnt.bg
  if (is.null(pt.cex.min)) pt.cex.min = p.plotting$pt.cex.min
  if (is.null(pt.cex.max)) pt.cex.max = p.plotting$pt.cex.max
  
  if (is.null(use.buckets)) use.buckets = p.plotting$use.buckets
  if (is.null(nclasses)) nclasses = p.plotting$nclasses
  if (is.null(show.legend)) show.legend = p.plotting$show.legend
  if (is.null(plot.field.pretty)) plot.field.pretty = p.plotting$plot.field
  
  if (is.null(basemap.Info)) basemap.Info = make_basemap(p.plotting)
  
  df.xy = df[,c(lon.field,lat.field)]
  df.sp <- SpatialPointsDataFrame(coords = df.xy, data = df, proj4string = CRS("+init=epsg:4326"))
  df.sp.tr=spTransform(df.sp,CRS(basemap.Info@proj4string@projargs))
  df.sp.tr$over=over(df.sp.tr,basemap.Info)
  n.validpts = NROW(df.sp.tr)
  print(paste0(NROW(df.sp.tr[is.na(df.sp.tr$over),])," of ", n.validpts, " positions lie outside of the map"))
  df.sp.tr = df.sp.tr[!is.na(df.sp.tr$over),]
  df.sp.tr$over=NULL
  if (n.validpts<nclasses & use.buckets==TRUE) {
    use.buckets=FALSE
    print("Too little data to bucket - using generic symbolization")
  }
  if (use.buckets){
    df.sp.tr$ORD = seq.int(nrow(df.sp.tr))
    df.classes = as.data.frame(df.sp.tr)
    df.classes = df.classes[,c("ORD", plot.field)]
    classes = classIntervals(df.classes[,plot.field], n=nclasses, style= "quantile", dataPrecision=0)
    #colcode = findColours(classes, c("#c7e9b4", "#41b6c4","#225ea8","#081d58")) #colorblind-friendly yellow-blue
    colcode = findColours(classes, c("#edf8b1","#7fcdbb", "#2c7fb8")) #colorblind-friendly yellow-blue
    colour.df = data.frame(varname=classes$var,colcode, ptSizer=findCols(classes))
    colour.df$ptSizer = as.numeric(colour.df$ptSizer)
    names(colour.df)[names(colour.df)=="varname"] <- plot.field
    df.sp.tr=merge( df.sp.tr,unique(colour.df), all.x = T)
  }
  #Maybe overwrite these defaults
  ptnames = plot.field.pretty
  leg.labels = "data"
  pnt.cex = pt.cex.min
  leg.pnt.bg = pnt.bg
  leg.pt.cex = pnt.cex
  
  if (use.buckets){
    #evenly space classes between the min and max point sizes
    df.sp.tr@data$ptSizer = pt.cex.min + (((df.sp.tr@data$ptSizer*pt.cex.min)-pt.cex.min)*((pt.cex.max-pt.cex.min)/(nclasses-1)))
    pnt.cex = df.sp.tr@data$ptSizer
    pnt.bg = df.sp.tr@data$colcode
    leg.labels = names(attr(colcode, "table"))
    leg.labels = gsub(",","-",leg.labels)
    leg.data = unique(df.sp.tr@data[c("ptSizer","colcode")])
    leg.data = leg.data[order(leg.data$ptSizer),]
    leg.pnt.bg = leg.data$colcode
    leg.pt.cex = leg.data$ptSizer
  }
  
  sp::plot(df.sp.tr, col = pnt.col, bg= pnt.bg, pch = pnt.style, cex = pnt.cex, add=T)
  if (show.legend){
    leg.pos=c(min(basemap.Info@bbox[1,]),max(basemap.Info@bbox[2,]))
    legend(
      'topleft',
      title = plot.field.pretty,
      pch=pnt.style,
      col=pnt.col,
      pt.bg =leg.pnt.bg,
      pt.cex=leg.pt.cex,
      legend = leg.labels
    )
  }
}