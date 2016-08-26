#' @title make.basemap
#' @description This function creates a simple basemap on which to plot data.  The plot consists of a (filled) coastline, with a labelled lat-lon grid.  DFO Marine areas can be also be added.  No local GIS files are used.
#' @param \code{df} default is \code{NULL}.  You can optionally send a dataframe to this function, which can be used in conjunction with
#' @param \code{auto.setlimits} to automatically set the map extent.
#' @param \code{auto.setlimits} default is \code{F}. This controls the extent of the resultant map.  If \code{T} (and a data frame with values for \code{LATITUDE} and \code{LONGITUDE} has been sent), it will use the data to determine the plot boundaries (plus a 10% padding factor).
#' @param \code{x.limits} default is \code{c(-70,-54)}. Default longitudes for extent.
#' @param \code{y.limits} default is \code{c(41,50)}. Default latitudes for extent.
#' @param \code{add.DFO} default is \code{c("St_Ann","Gully","Vazella_Emerald","Vazella_Sambro","Lophelia", "NE_Channel")}.  
#' @param \code{crs.out} default is \code{"+init=epsg:2220"} (UTM Zone 20 N).  This is the desired projection of the final plot.  
#' @return a SpatialPolygons object corresponding to the bounding box of the plot.  
#' @note Bathymetry will be added to this in the near future, and the function call will be modified to include a flag that will indicate whether or not it should be plotted.  Many values are possible and/or appropriate for the value of \code{crs.out}, depending on the data. Check \code{\link{http://spatialreference.org/ref/epsg/}} for more options.
#' @family plotting
#' @export
#' @note .
make.basemap = function(df=NULL, auto.setlimits=F, x.limits=c(-70,-54), y.limits=c(41,50),
                         add.DFO = c("St_Ann","Gully","Vazella_Emerald","Vazella_Sambro","Lophelia", "NE_Channel"),
                         crs.out="+init=epsg:2220"){
  
  require(sp)       #Polygon/coordinates
  require(rgeos)    #gintersection
  require(maptools) #map2spatialpolygons
  require(mapdata) 
  
  crs.in="+init=epsg:4326"
  
  if (auto.setlimits==T & !is.null(df)){
    #find range, pad it by 25% and round it to nearest  degree
    x.limits = round(extendrange(range(df$LONGITUDE), f=0.10)/1)*1
    y.limits = round(extendrange(range(df$LATITUDE), f=0.10)/1)*1
  }
  
  
  limits = data.frame(X = x.limits, Y = y.limits) 
  coordinates(limits) = c("X", "Y")
  proj4string(limits) = CRS(crs.in)
  
  boundbox = SpatialPolygons(list(Polygons(list(Polygon(cbind(
    xx = c(limits$X[2], seq(limits$X[2],limits$X[1],length=200),seq(limits$X[1],limits$X[2],length=200)),
    yy = c(limits$Y[2], seq(limits$Y[1],limits$Y[1],length=200), seq(limits$Y[2],limits$Y[2],length=200))))),
    ID = "bb")), 
    proj4str = CRS(crs.in))
  boundbox.pr = spTransform(boundbox,crs.out)
  #make slightly bigger bbox to reserve space in final
  boundbox2 = SpatialPolygons(list(Polygons(list(Polygon(cbind(
    xx = c(limits$X[2]+1, seq(limits$X[2]+1,limits$X[1]-1,length=200), seq(limits$X[1]-1,limits$X[2]+1,length=200)),
    yy = c(limits$Y[2]+1, seq(limits$Y[1]-1,limits$Y[1]-1,length=200), seq(limits$Y[2]+1,limits$Y[2]+1,length=200))))),
    ID = "bb2")), 
    proj4str = CRS(crs.in))
  boundbox2.pr = spTransform(boundbox2,crs.out)
  
  #'using the clipped data (pre-projection), capture information for the grid,
  #'including information about the gridlines, as well as their labels
  #'
  #'the hack below helps in cases where the data results in a map less than 1deg by 1 deg - in which case, the gridlines and labels need to 
  #'be more frequent
  #'by1 and by2 relate to the gridline labels
  #'by1_1 and by2_1 relate to the gridlines themselves
  if (boundbox@"bbox"[3] - boundbox@"bbox"[1] < 2){
    by1=0.5
    by1_1=0.25
  }
  if (boundbox@"bbox"[4] - boundbox@"bbox"[2] < 2){
    by2=0.5
    by2_1=0.25
  }

  the.grid = gridat(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=by1), norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=by2))
  grid.pr = spTransform(the.grid, CRS(crs.out))
  these.gridlines = gridlines(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=by1_1), norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=by2_1))
  these.gridlines.pr = spTransform(these.gridlines, CRS(crs.out))
  
  coastline = map( "worldHires", regions=c("Canada","USA", "Greenland"), xlim=x.limits,ylim=y.limits, col="blue", fill=T, plot=F, resolution=0) 
  coastline.sp = map2SpatialPolygons(coastline, IDs=coastline$names,proj4string=CRS(crs.in))
  coastline.sp = spTransform(coastline.sp, CRS( crs.out ))
  coastline.sp.clip = gIntersection(gBuffer(coastline.sp, byid=TRUE, width=0), spTransform(boundbox,CRS( crs.out )))
  
  
  #get the desired DFO areas
  if (length(add.DFO)>0) DFO.areas=get.DFO.areas(add.DFO)
  
  par(mar=c(2,2,1,1),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  plot(boundbox2.pr, border="transparent", add=F, lwd=1) #add transparent boundbox first to ensure all data shown
  plot(coastline.sp.clip, col="navajowhite2", border="navajowshite4", lwd=0.5, axes=F, add=T )  #add coastline
  for (o in 1:length(DFO.areas)){
    plot(gIntersection(gBuffer(spTransform(DFO.areas[[o]], CRS(crs.out)), byid=TRUE, width=0), spTransform(boundbox,CRS( crs.out ))), border="olivedrab4", lwd=0.5, add=T)
  }
  plot(these.gridlines.pr, col="grey77", lty=2, lwd=0.5, add=T)                           #gridlines
  text(coordinates(grid.pr), pos=grid.pr$pos, labels=parse(text=as.character(the.grid$labels)), offset=0.2, col="black", cex=1)
  plot(boundbox.pr, border="black", add=T, lwd=1) #add actual boundbox
  return(boundbox.pr)
}