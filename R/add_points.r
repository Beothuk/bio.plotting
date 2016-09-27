#' @title add_points
#' @description This function adds points to an existing plot.  The advantage over \code{points(...)} is that by including a map object as
#' input, it will automatically reproject the points, and trim them such that only positions that fall within the map boundaries are shown.
#' The user is notified of how many positions lay outside of the map boundaries.
#' Additionally, the input data can either be symbolized generically (i.e. one style), or it can classify the points according to the values
#' stored in a field identified by the user using the flag \code{use.buckets}.  If \{code{use.buckets=T}, data is symbolized by a selected
#' field, and the colour and size of the points varies with the values in \code{plot.field}.
#' @param df the dataframe to be plotted
#' @param basemap.Info a SpatialPolygons object (identifying the boundaries and projection of an existing plot). If none is provided, a default \code{make_basemap()} object will be used. object
#' @param lat.field the default is \code{'LATITUDE'}. The name of the field holding latitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param lon.field the default is \code{'LONGITUDE'}.  The name of the field holding longitude values (in decimal degrees).  If unspecified, the value from p.plotting will be used.
#' @param plot.field the default is \code{NULL}.  The field on which to symbolize the data.  If unspecified, the data can only be symbolized generically.
#' @param plot.field.pretty the default is \code{NULL}.  Applies only when \code{use.buckets = F}. This is a nice name to describe \code{plot.field} which will be displayed in the legend.  If unspecified, the value from p.plotting will be used.
#' @param pnt.style the default is \code{21}. Determines the style of the points (any valid value for \code{pch} is acceptable).  If unspecified, the value from p.plotting will be used.
#' @param pnt.col the default is \code{'black'}. This determines the colour outlining the points.  If unspecified, the value from p.plotting will be used.
#' @param pnt.bg the default is \code{'red'}.  Applies only when \code{show.legend = T}.  Determines the colour of the points.  If unspecified, the value from p.plotting will be used.
#' @param pt.cex.min the default is \code{1}. When \code{use.buckets = F}, this determines the size of the points, and when \code{use.buckets = T}, this determines the minimum size of the points to be drawn.  If unspecified, the value from p.plotting will be used.
#' @param pt.cex.max the default is \code{2}. When \code{use.buckets = F}, this is ignored, and when \code{use.buckets = T}, this determines the maximum size of the points to be drawn.  If unspecified, the value from p.plotting will be used.
#' @param show.legend the default is \code{FALSE}.  Determines whether or not a legend will be displayed.  If unspecified, the value from p.plotting will be used.
#' @param use.buckets the default is \code{TRUE}. If \code{use.buckets = F}, all points are identical, but if \code{use.buckets = T}, data points are scaled, and the colour intensity varies according to the value of \code{plot.field}.  If unspecified, the value from p.plotting will be used.
#' @param use.colours the default is \code{TRUE}. If \code{use.colour = FALSE}, all points will be coloured using the value of \code{pnt.bg}.  If set to \code{TRUE}, the the intensity of the colour will also scale with the size of the markers, according to the value of \code{plot.field}.  If unspecified, the value from p.plotting will be used.
#' @param nclasses the default is \code{3}. Applies only when \code{use.buckets = T}.  Determines how many "bins" to use to display the data.  If unspecified, the value from p.plotting will be used.
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
add_points <-
  function(df,
           basemap.Info =  NULL,
           lat.field = "LATITUDE",
           lon.field = "LONGITUDE",
           plot.field = NULL,
           plot.field.pretty = NULL,
           pnt.style = 21,
           pnt.col = 'black',
           pnt.bg = 'red',
           pt.cex.min = 1,
           pt.cex.max = 2,
           show.legend = FALSE,
           use.buckets = TRUE,
           use.colours = TRUE,
           nclasses = 3) {
    if (exists("p.plotting")){
      lat.field = p.plotting$lat.field
      lon.field = p.plotting$lon.field
      pnt.style = p.plotting$pnt.style
      pnt.col = p.plotting$pnt.col
      pnt.bg = p.plotting$pnt.bg
      pt.cex.min = p.plotting$pt.cex.min
      pt.cex.max = p.plotting$pt.cex.max
      use.buckets = p.plotting$use.buckets
      nclasses = p.plotting$nclasses
      show.legend = p.plotting$show.legend
      plot.field = p.plotting$plot.field
      if (is.null(basemap.Info))
        basemap.Info = make_basemap(p.plotting)
    }
    if (is.null(basemap.Info))
      basemap.Info = make_basemap()

    #still no plot field?  use an existing field, but don't symbolize by it
    if (is.null(plot.field)) {
      if (use.buckets == TRUE)
        cat(
          "You set use.buckets=TRUE, but didn't indicate a field to symbolize.  To avoid this, please set a
          valid field to plot in p.plotting$plot.field.  In the meantime, plotting generically...\n"
        )
      plot.field = lat.field
      plot.field.pretty = "Generic"
      use.buckets = FALSE
    }
    df.xy = df[, c(lon.field, lat.field)]
    df.sp <-
      SpatialPointsDataFrame(
        coords = df.xy,
        data = df,
        proj4string = CRS("+init=epsg:4326")
      )
    df.sp.tr = spTransform(df.sp, CRS(basemap.Info@proj4string@projargs))
    df.sp.tr$over = over(df.sp.tr, basemap.Info)
    n.invalidpts = NROW(df.sp.tr[is.na(df.sp.tr$over), ])
    #print(n.validpts)
    n.validpts = NROW(df.sp.tr[!is.na(df.sp.tr$over), ])
    #print(n.invalidpts)
    if (n.invalidpts > 0)
      print(paste0(
        n.validpts,
        " of ",
        NROW(df.sp.tr),
        " positions lie outside of the map"
      ))
    df.sp.tr = df.sp.tr[!is.na(df.sp.tr$over), ]
    df.sp.tr$over = NULL
    if (n.validpts > 3 & n.validpts > nclasses & use.buckets == TRUE) {
      use.buckets = TRUE
    } else{
      if (use.buckets == TRUE)
        print("Too little data to bucket - using generic symbolization")
      use.buckets = FALSE
    }
    if (use.buckets) {
      df.sp.tr$ORD = seq.int(nrow(df.sp.tr))
      df.classes = as.data.frame(df.sp.tr)
      df.classes = df.classes[, c("ORD", plot.field)]
      classes = classIntervals(
        df.classes[, plot.field],
        n = nclasses,
        style = "quantile",
        dataPrecision = 0
      )
      if (use.colours) {
        colcode = findColours(classes, c("#edf8b1", "#7fcdbb", "#2c7fb8")) #colorblind-friendly yellow-blue
      } else{
        colcode = findColours(classes, c(pnt.bg, pnt.bg)) #hack to use bg colour only
      }
      colour.df = data.frame(varname = classes$var,
                             colcode,
                             ptSizer = findCols(classes))
      colour.df$ptSizer = as.numeric(colour.df$ptSizer)
      names(colour.df)[names(colour.df) == "varname"] <- plot.field
      df.sp.tr = merge(df.sp.tr, unique(colour.df), all.x = T)
    }
    #Maybe overwrite these defaults
    ptnames = plot.field.pretty
    leg.labels = "data"
    pnt.cex = pt.cex.min
    leg.pnt.bg = pnt.bg
    leg.pt.cex = pnt.cex

    if (use.buckets) {
      #evenly space classes between the min and max point sizes
      df.sp.tr@data$ptSizer = pt.cex.min + (((df.sp.tr@data$ptSizer * pt.cex.min) -
                                               pt.cex.min) * ((pt.cex.max - pt.cex.min) / (nclasses - 1)))
      pnt.cex = df.sp.tr@data$ptSizer
      pnt.bg = df.sp.tr@data$colcode
      leg.labels = names(attr(colcode, "table"))
      leg.labels = gsub(",", "-", leg.labels)
      leg.data = unique(df.sp.tr@data[c("ptSizer", "colcode")])
      leg.data = leg.data[order(leg.data$ptSizer), ]
      leg.pnt.bg = leg.data$colcode
      leg.pt.cex = leg.data$ptSizer
    }

    sp::plot(
      df.sp.tr,
      col = pnt.col,
      bg = pnt.bg,
      pch = pnt.style,
      cex = pnt.cex,
      add = T
    )
    if (show.legend) {
      leg.pos = c(min(basemap.Info@bbox[1, ]), max(basemap.Info@bbox[2, ]))
      legend(
        'topleft',
        title = plot.field.pretty,
        pch = pnt.style,
        col = pnt.col,
        pt.bg = leg.pnt.bg,
        pt.cex = leg.pt.cex,
        legend = leg.labels
      )
    }
  }
