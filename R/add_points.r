#' @title add_points
#' @description This function adds points to an existing plot.  The advantage over \code{points(...)} is that by including a map object as
#' input, it will automatically reproject the points, and trim them such that only positions that fall within the map boundaries are shown.
#' The user is notified of how many positions lay outside of the map boundaries.
#' Additionally, the input data can either be symbolized generically (i.e. one style), or it can classify the points according to the values
#' stored in a field identified by the user using the flag \code{use.buckets}.  If \code{use.buckets=T}, data is symbolized by a selected
#' field, and the colour and size of the points varies with the values in \code{plot.field}.
#' @param df the dataframe to be plotted
#' @param basemap a SpatialPolygons object (identifying the boundaries and projection of an existing plot). If none is provided, a default \code{make_basemap()} object will be used. object
#' @param lat.field the default is \code{'LATITUDE'}. The name of the field holding latitude values (in decimal degrees).
#' @param lon.field the default is \code{'LONGITUDE'}.  The name of the field holding longitude values (in decimal degrees).
#' @param plot.field the default is \code{'EST_COMBINED_WT'}.  The field on which to symbolize the data.  If unspecified, the data can only be symbolized generically.
#' @param plot.field.pretty the default is \code{NULL}.  Applies only when \code{use.buckets = F}. This is a nice name to describe \code{plot.field} which will be displayed in the legend.
#' @param pnt.style the default is \code{21}. Determines the style of the points (any valid value for \code{pch} is acceptable).
#' @param pnt.col the default is \code{'black'}. This determines the colour outlining the points.
#' @param pnt.bg the default is \code{'red'}.  Applies only when \code{show.legend = T}.  Determines the colour of the points.
#' @param pnt.cex.min the default is \code{1}. When \code{use.buckets = F}, this determines the size of the points, and when \code{use.buckets = T}, this determines the minimum size of the points to be drawn.
#' @param pnt.cex.max the default is \code{2}. When \code{use.buckets = F}, this is ignored, and when \code{use.buckets = T}, this determines the maximum size of the points to be drawn.
#' @param show.legend the default is \code{FALSE}.  Determines whether or not a legend will be displayed.
#' @param leg.pos the default is \code{'topleft'}.  Determines where on the plot the legend will be displayed (if \code{show.legend=TRUE}). Valid values are 'topleft','topright','bottomleft','bottomright'.
#' @param use.buckets the default is \code{TRUE}. If \code{use.buckets = F}, all points are identical, but if \code{use.buckets = T}, data points are scaled, and the colour intensity varies according to the value of \code{plot.field}.
#' @param use.colours the default is \code{TRUE}. If \code{use.colour = FALSE}, all points will be coloured using the value of \code{pnt.bg}.  If set to \code{TRUE}, the the intensity of the colour will also scale with the size of the markers, according to the value of \code{plot.field}.
#' @param colour.ramp the default is \code{c("#edf8b1", "#7fcdbb", "#2c7fb8")}, which is color-blind friendly. You can use hex values, or colour names (e.g. \code{c('blue','pink')}), but there must be at least 2 colour present.
#' @param bucket.style the default is \code{quantile} chosen style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#' @param bucket.fixed.breaks the default is \code{NULL} if the \code{bucket.style} is set to "fixed", a vector of the desired upper ends of the desired breaks must be supplied (e.g. \code{c(2, 5, 10, 100, 500.10000, 100000)})
#' @param nclasses the default is \code{3}. Applies only when \code{use.buckets = T}.  Determines how many "bins" to use to display the data.
#' @return NULL, but notifies the user of how many positions lay outside of the map boundaries.
#' @importFrom sp over
#' @importFrom sp plot
#' @importFrom graphics plot
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom classInt classIntervals
#' @importFrom classInt findColours
#' @importFrom classInt findCols
#' @importFrom graphics legend
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
add_points <-
  function(df,
           basemap =  NULL,
           lat.field = "LATITUDE",
           lon.field = "LONGITUDE",
           plot.field = NULL,
           plot.field.pretty = NULL,
           pnt.style = 21,
           pnt.col = 'black',
           pnt.bg = 'red',
           pnt.cex.min = 1,
           pnt.cex.max = 2,
           show.legend = TRUE,
           leg.pos = 'topleft',
           use.buckets = TRUE,
           use.colours = TRUE,
           colour.ramp = c("#edf8b1", "#7fcdbb", "#2c7fb8"),
           bucket.style = 'quantile',
           bucket.fixed.breaks = NULL,
           nclasses = 3) {
    if (is.null(basemap))
      basemap = make_basemap(df)

    nullsymb = 3
    nullsymbsize = pnt.cex.min/2.5

    get_pnt_size <- function(origpt, pnt.cex.min = pnt.cex.min, pnt.cex.max = pnt.cex.max, nclasses = nclasses){
      return(pnt.cex.min + (((origpt * pnt.cex.min) - pnt.cex.min) * ((pnt.cex.max - pnt.cex.min) / (nclasses - 1))))
    }

    #still no plot field?  use an existing field, but don't symbolize by it
    if (is.null(plot.field) | (! plot.field %in% colnames(df))) {
      if (use.buckets == TRUE)
        cat(
"There's a problem symbolizing the data by a plot.field.  Either you didn't identify a field, or you
wrote a fieldname that doesn't exist.  To avoid this, please set a valid field to plot.field.
In the meantime, plotting generically...\n"
        )
      plot.field = lat.field
      plot.field.pretty = "<no plot field>"
      use.buckets = FALSE
      legend.df2 = data.frame(categdesc = "data",
                              symbol = pnt.style,
                              ptSizer = pnt.cex.min,
                              categcol = pnt.bg)

    }
    if (is.null(plot.field.pretty)) plot.field.pretty=plot.field
    df.xy = df[, c(lon.field, lat.field)]
    df.sp <-
      sp::SpatialPointsDataFrame(
        coords = df.xy,
        data = df,
        proj4string = sp::CRS("+init=epsg:4326")
      )
    df.sp.tr = sp::spTransform(df.sp, sp::CRS(basemap@proj4string@projargs))
    df.sp.tr$over = sp::over(df.sp.tr, basemap)
    n.invalidpts = NROW(df.sp.tr[is.na(df.sp.tr$over), ])
    df.sp.tr@data$symbol = nullsymb

    n.invalidpts = NROW(df.sp.tr[is.na(df.sp.tr$over), ])
    #unique values of plot field
    u.values = NROW(unique(df.sp.tr@data[!is.na(df.sp.tr@data[plot.field]), ][plot.field]))
    if (n.invalidpts > 0)
      print(paste0(
        n.invalidpts,
        " of ",
        NROW(df.sp.tr),
        " positions lie outside of the map"
      ))
    df.sp.tr = df.sp.tr[!is.na(df.sp.tr$over), ]
    df.sp.tr$over = NULL
    if (u.values > 3 & u.values > nclasses & use.buckets == TRUE) {
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
      df.classes = df.classes[!is.na(df.classes[plot.field])&(df.classes[plot.field]>0),]
      classes = classInt::classIntervals(
        df.classes[, plot.field],
        n = nclasses,
        style = bucket.style,
        fixedBreaks = bucket.fixed.breaks,
        dataPrecision = 0
      )
      if (use.colours) {
        colcode = classInt::findColours(classes, colour.ramp) #colorblind-friendly yellow-blue
      } else{
        colcode = classInt::findColours(classes, c(pnt.bg, pnt.bg)) #hack to use bg colour only
      }
      #get the possible symbology values from the classes - these may not exist in the data
      symbol.df = data.frame(
        ptsize = seq(1,length(classes$brks)-1),
        symbol = rep(pnt.style,length(classes$brks)-1),
        categ = classes$brks[1:length(classes$brks)-1],
        categdesc = names(attr(colcode, "table")),
        categcol = attr(colcode, "palette")
      )

      symbol.df$ptSizer = get_pnt_size(symbol.df$ptsize, pnt.cex.min, pnt.cex.max, nclasses)

      colour.df = data.frame(varname = classes$var,
                             colcode,
                             ptsize = classInt::findCols(classes))
      colour.df$ptsize = as.numeric(colour.df$ptsize)
      df.sp.tr@data = merge(df.sp.tr@data, unique(colour.df), by.x=plot.field, by.y='varname', all.x = T)
      df.sp.tr@data = df.sp.tr@data[order(df.sp.tr@data$ORD),]

      df.sp.tr@data$ptSizer = get_pnt_size(df.sp.tr@data$ptsize, pnt.cex.min, pnt.cex.max, nclasses)

      pnt.cex = df.sp.tr@data$ptSizer
      pnt.bg = df.sp.tr@data$colcode
      leg.data = symbol.df[c("ptSizer", "categcol")]
      leg.pnt.bg = leg.data$colcode
      leg.pt.cex = leg.data$ptSizer
    }else{
      symbol.df = data.frame(
        ptSizer = pnt.cex.min,
        symbol = pnt.style,
        #categ = classes$brks[1:length(classes$brks)-1],
        categdesc = "data",
        categcol = pnt.bg
      )
      df.sp.tr@data$ptSizer = pnt.cex.min
    }

    df.sp.tr@data$symbol[!is.na(df.sp.tr@data[plot.field]) & (df.sp.tr@data[plot.field]>0)] = pnt.style
    df.sp.tr@data$symbol[is.na(df.sp.tr@data[plot.field]) | (df.sp.tr@data[plot.field]==0)] = nullsymb
    df.sp.tr@data$ptSizer[is.na(df.sp.tr@data[plot.field]) | (df.sp.tr@data[plot.field]==0)]= nullsymbsize
    sp::plot(
      df.sp.tr,
      col = pnt.col,
      bg = pnt.bg,
      pch = df.sp.tr@data$symbol,
      cex = df.sp.tr@data$ptSizer,
      add = T
    )
    if (show.legend) {
      legend.df = symbol.df[c("symbol","categdesc", "ptSizer", "categcol")]
      legend.df=legend.df[order(legend.df[,c('symbol')],legend.df[,c('ptSizer')]),]

      if (any(df.sp.tr@data$symbol ==nullsymb)){
        nullrow = data.frame(symbol=nullsymb, categdesc = "null", ptSizer= nullsymbsize, categcol="#000000")
        legend.df2 = do.call("rbind", list(nullrow, legend.df))
      }

      #create this first so we can detect height and width for placement
      leg.scratch =   graphics::legend(
        title = plot.field.pretty,
        legend = legend.df2$categdesc,
        inset = 2,
        x=  max(basemap@bbox[1, ]),
        y = max(basemap@bbox[2, ]),
        bty="o",y.intersp=0.75,x.intersp=0.75,
        xjust= 0,
        yjust=0.5,
        pt.lwd=0.6,
        plot = F
      )
      y.bmp = diff(basemap@bbox[2, ])*0.125
      x.bmp = diff(basemap@bbox[1, ])*0.075
      if (leg.pos == 'topleft'){
        leg.x= min(basemap@bbox[1, ])+x.bmp
        leg.y = max(basemap@bbox[2, ])-(leg.scratch$rect$h)+y.bmp
      }else if (leg.pos == 'topright'){
        leg.x= max(basemap@bbox[1, ])-(leg.scratch$rect$w+x.bmp)
        leg.y = max(basemap@bbox[2, ])-(leg.scratch$rect$h)+y.bmp
      } else  if (leg.pos == 'bottomleft'){
        leg.x= min(basemap@bbox[1, ])+x.bmp
        leg.y = min(basemap@bbox[2, ])+(1.5*y.bmp)
      }else if (leg.pos == 'bottomright'){
        leg.x= max(basemap@bbox[1, ])-(leg.scratch$rect$w+x.bmp)
        leg.y =  min(basemap@bbox[2, ])+(1.5*y.bmp)
      }
      graphics::legend(
        title = plot.field.pretty,
        legend = legend.df2$categdesc,
        x= leg.x,
        y = leg.y,
        bty="o",y.intersp=0.75,x.intersp=0.75,
        xjust= 0,
        yjust=0.5,
        pch=legend.df2$symbol, pt.cex=legend.df2$ptSizer,
        pt.lwd=0.6,
        pt.bg =legend.df2$categcol,
        col ='black',
        plot = T
      )
    }
  }
