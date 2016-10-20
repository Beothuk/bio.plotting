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
#' @return a SpatialPolygons object corresponding to the bounding box of the plot.
#' @note Bathymetry will be added to this in the near future, and the function call will be modified to include a flag that will indicate
#' whether or not it should be plotted.
#' @importFrom grDevices xy.coords
#' @importFrom graphics text par plot.default
#' @importFrom sp CRS gridlines gridat is.projected coordinates coordinates<- proj4string<- spTransform SpatialPolygons Polygon Polygons
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @import mapdata
#' @family plotting
#' @export
make_basemap = function(df = NULL,
                        crs.out = '+init=epsg:2220',
                        x.limits = c(-70, -54),
                        y.limits = c(41, 50)
                        )
                        {
                          crs.in = "+init=epsg:4326"

                          if (!is.null(df)) {
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

                          coastline = maps::map(
                            "worldHires",
                            regions = c("Canada", "USA", "France", "Greenland"),
                            xlim = x.limits,
                            ylim = y.limits,
                            col = "blue",
                            fill = T,
                            plot = F,
                            resolution = 0
                          )
                          #
                          coastline.sp = maptools::map2SpatialPolygons(coastline,
                                                             IDs = coastline$names,
                                                             proj4string = sp::CRS(crs.in))
                          coastline.sp = sp::spTransform(coastline.sp, sp::CRS(crs.out))
                          coastline.sp.clip = rgeos::gIntersection(rgeos::gBuffer(coastline.sp, byid = TRUE, width =
                                                                      0),
                                                            sp::spTransform(boundbox, sp::CRS(crs.out)))

                          par(
                            mar = c(1, 1, 1, 1),
                            xaxs = "i",
                            yaxs = "i",
                            cex.axis = 1.3,
                            cex.lab = 1.4
                          )
                          #lines(boundbox2.pr@polygons[[1]]@Polygons[[1]]@coords, col="transparent")
                          sp::plot(boundbox2.pr,
                                   border = "transparent",
                                   add = F,
                                   lwd = 1) #add transparent boundbox first to ensure all data shown

                          if (!is.null(coastline.sp.clip))
                            sp::plot(
                              coastline.sp.clip,
                              col = "navajowhite2",
                              border = "navajowhite4",
                              lwd = 0.5,
                              axes = F,
                              add = T
                            )  #add coastline

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
