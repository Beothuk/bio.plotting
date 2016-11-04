#' @title make_basemap
#' @description This function creates a simple basemap on which to plot data.  The plot consists of a (filled) coastline, with a labelled
#' lat-lon grid.  When a dataframe is provided, the extents of the plot will automatically match the extent of the supplied data (with a 15 percent buffer around the edges).
#' Additionally, the output projection of the plot can be specified (e.g. \code{"+init=epsg:2220"} would result in UTM Zone 20 N, while
#' \code{"+init=epsg:4326"} would be an unprojected WGS84 plot. Many values are possible and/or appropriate for the value of \code{crs.out},
#' depending on the data. Check \code{\link[sp]{CRS-class}} for more options.
#' @param df default is \code{NULL}.  You can optionally send a dataframe with values for \code{LATITUDE} and \code{LONGITUDE} to this function, which can be have the plot extent match
#' the data.
#' @param crs.out default is \code{'+init=epsg:2220'} (UTM Zone 20 N).  This is the desired projection of the final plot.
#' @param x.limits default is \code{c(-70, -54)} but an appropriate value would be in the form of \code{c(-70,-54)}. These are the default
#' bounding longitudes.
#' @param y.limits default is \code{c(41, 50)} but an appropriate value would be in the form of  is \code{c(41,50)}. These are the default
#' bounding latitudes for extent.
#' @param add.isobaths default is \code{FALSE}.  If TRUE, adds the following isobaths (m) to the plot:
#'  "10"   "20"   "50"   "75"   "100"  "200"  "250"  "300"  "400"  "500"  "600"  "700"  "750"  "800"
#'  "900"  "1000" "1200" "1250" "1400" "1500" "1750" "2000" "2500" "3000" "4000" "5000"
#' @return a SpatialPolygons object corresponding to the bounding box of the plot.
#' @importFrom grDevices xy.coords colorRampPalette
#' @importFrom graphics text par plot.default
#' @importFrom sp CRS gridlines gridat is.projected coordinates coordinates<- proj4string<- spTransform SpatialPolygons Polygon Polygons
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom utils data
#' @importFrom methods as
#' @import mapdata
#' @family plotting
#' @export
make_basemap = function(df = NULL,
                        crs.out = '+init=epsg:2220',
                        x.limits = c(-70, -54),
                        y.limits = c(41, 50),
                        add.isobaths = FALSE
                        # ,
                        # known.areas = c('Gully', 'St_Ann', 'Vazella_Emerald', 'Vazella_Sambro', 'NE_Channel', 'Musquash'),
                        # get.detailed = F
)
  
{
  crs.in = "+init=epsg:4326"
  if (!is.null(df)) {
    df2 = df_qc_spatial(df)
    cat(paste0("\nDropped ",nrow(df) - nrow(df2)," records where the coordinates were invalid."))
    df = df2
    x.limits = range(df$LONGITUDE)
    y.limits = range(df$LATITUDE)
  }
  
  if (diff(y.limits) <= 1) {
    #tiny map
    y.maj = 0.25
    y.min = 0.05
  } else if (diff(y.limits) <= 4) {
    y.maj = 0.5
    y.min = 0.25
  } else{
    y.maj = 1
    y.min = 0.5
  }

  if (diff(x.limits) <= 1) {
    x.maj = 0.25
    x.min = 0.05
  } else if (diff(x.limits) <= 4) {
    x.maj = 0.5
    x.min = 0.25
  } else{
    x.maj = 1
    x.min = 0.5
  }

  if (!is.null(df)) {

    #find range, pad it by amount determined above, and round to nice value
    x.limits = round(c((min(df$LONGITUDE)-(0.5*x.maj)), (max(df$LONGITUDE)+(0.5*x.maj)))/ x.maj) * x.maj
    y.limits = round(c((min(df$LATITUDE)-(0.5*y.maj)), (max(df$LATITUDE)+(0.5*y.maj))) / y.maj) * y.maj
  }
  limits = data.frame(X = x.limits, Y = y.limits)
  sp::coordinates(limits) = c("X", "Y")
  sp::proj4string(limits) = sp::CRS(crs.in)

  boundbox = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    cbind(
      xx = c(
        max(limits$X),
        seq(max(limits$X), min(limits$X), length = 200),
        seq(min(limits$X), max(limits$X), length = 200)
      ),
      yy = c(
        max(limits$Y),
        seq(min(limits$Y), min(limits$Y), length = 200),
        seq(max(limits$Y), max(limits$Y), length = 200)
      )
    )
  )),
  ID = "bb")),
  proj4string = sp::CRS(crs.in))
  boundbox.pr = sp::spTransform(boundbox, crs.out)
  boundbox2 = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(
    cbind(
      xx = c(
        max(limits$X) + x.min,
        seq(max(limits$X) + x.min, min(limits$X) - x.min, length = 200),
        seq(min(limits$X) - x.min, max(limits$X) + x.min, length = 200)
      ),
      yy = c(
        max(limits$Y) + x.min,
        seq(min(limits$Y) - x.min, min(limits$Y) - x.min, length = 200),
        seq(max(limits$Y) + x.min, max(limits$Y) + x.min, length = 200)
      )
    )
  )),
  ID = "bb2")),
  proj4string = sp::CRS(crs.in))
  boundbox2.pr = sp::spTransform(boundbox2, crs.out)
  the.grid = sp::gridat(
    boundbox,
    easts = seq(boundbox@"bbox"[1], boundbox@"bbox"[3], by = x.maj),
    norths = seq(boundbox@"bbox"[2], boundbox@"bbox"[4], by = y.maj)
  )
  grid.pr = sp::spTransform(the.grid, sp::CRS(crs.out))
  these.gridlines = sp::gridlines(
    boundbox,
    easts = seq(boundbox@"bbox"[1], boundbox@"bbox"[3], by = x.min),
    norths = seq(boundbox@"bbox"[2], boundbox@"bbox"[4], by = y.min)
  )
  these.gridlines.pr = sp::spTransform(these.gridlines, sp::CRS(crs.out))

  coastline = -1
  coastline = tryCatch(
    {
      maps::map(
        "worldHires",
        regions = c("Canada", "USA", "France", "Greenland"),
        xlim = x.limits,
        ylim = y.limits,
        col = "blue",
        fill = T,
        plot = F,
        resolution = 0
      )
    },
    error=function(cond){
      #this present since we'd get errors if plotting offshore areas
      return(-1)
    }
  )
  if (class(coastline) == "map"){
    coastline.sp = maptools::map2SpatialPolygons(coastline,
                                                 IDs = coastline$names,
                                                 proj4string = sp::CRS(crs.in))
    coastline.sp = sp::spTransform(coastline.sp, sp::CRS(crs.out))
    coastline.sp.clip = rgeos::gIntersection(rgeos::gBuffer(coastline.sp, byid = TRUE, width = 0),
                                             boundbox.pr
                                             #sp::spTransform(boundbox, sp::CRS(crs.out))
                                             )
  }

  if (add.isobaths){
    data(isobaths)
    index.isobaths = row.names(isobaths)
    colfunc <- colorRampPalette(c("lightskyblue1", "skyblue3", "steelblue4"))
    isobath.cols = colfunc(length(index.isobaths)) #make colours for all possible isobaths
    col.df= data.frame(index.isobaths,isobath.cols)
    isobaths.pr = sp::spTransform(isobaths, sp::CRS(crs.out))
    isobath.clip = rgeos::gIntersection(rgeos::gBuffer(isobaths.pr, byid = TRUE, width = 0.01),
                                        boundbox.pr, byid = TRUE, drop_lower_td = TRUE)
    isobath.clip = as(isobath.clip, 'SpatialLines')
    used.isobaths = gsub(" bb", "", row.names(isobath.clip))
    used.isobath.cols = col.df[col.df$index.isobaths %in% used.isobaths,]$isobath.cols
  }
  par(
    mar = c(1, 1, 1, 1),
    xaxs = "i",
    yaxs = "i",
    cex.axis = 1.3,
    cex.lab = 1.4
  )
  sp::plot(boundbox2.pr,
           border = "transparent",
           add = F,
           lwd = 1) #add transparent boundbox first to ensure all data shown
  if (add.isobaths) {
    if (!is.null(isobath.clip)) {
      sp::plot(isobath.clip, add=T, col=used.isobath.cols)
    }
  }
  # if (known.areas[1] != ""){
  #     plot.areas=get_known_areas(known.areas, get.detailed, crs.out)
  #     plot.areas.clip = rgeos::gIntersection(rgeos::gBuffer(plot.areas, byid = TRUE, width = 0),  boundbox.pr
  #                                            #sp::spTransform(boundbox, sp::CRS(crs.out))
  #                                            )
  #       sp::plot(plot.areas.clip, border = "grey72", add=TRUE)
  #  }
  if(exists("coastline.sp.clip")){
  if (class(coastline.sp.clip)=="SpatialPolygons"){
    sp::plot(
      coastline.sp.clip,
      col = "navajowhite2",
      border = "navajowhite4",
      lwd = 0.5,
      axes = F,
      add = T
    )
  }
  }
  sp::plot(
    these.gridlines.pr,
    col = "grey77",
    lty = 2,
    lwd = 0.5,
    add = T
  )
  text(
    sp::coordinates(grid.pr),
    pos = grid.pr$pos,
    labels = parse(text = as.character(the.grid$labels)),
    offset = 0.2,
    col = "black",
    cex = 1
  )
  sp::plot(boundbox.pr,
           border = "black",
           add = T,
           lwd = 1) #add actual boundbox
  return(boundbox.pr)
}
