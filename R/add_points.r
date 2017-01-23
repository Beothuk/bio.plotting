#' @title add_points
#' @description This function adds points to an existing plot.  The advantage over \code{points(...)} is that by including a map object as
#' input, it will automatically reproject the points, and trim them such that only positions that fall within the map boundaries are shown.
#' The user is notified of how many positions lay outside of the map boundaries.
#' Additionally, the input data can either be symbolized generically (i.e. one style), or if a \code{bin.style} is elected, data will be
#' symbolized according to the values of an identified field.  Both the colour and size of the points can vary with the values in \code{plot.field}.
#' @param df the dataframe to be plotted
#' @param basemap a SpatialPolygons object (identifying the boundaries and projection of an existing plot). If none is provided, a default \code{make_basemap()} object will be used. object
#' @param lat.field the default is \code{'LATITUDE'}. The name of the field holding latitude values (in decimal degrees).
#' @param lon.field the default is \code{'LONGITUDE'}.  The name of the field holding longitude values (in decimal degrees).
#' @param plot.field the default is \code{'EST_COMBINED_WT'}.  The field on which to symbolize the data.  If unspecified, the data can only be symbolized generically.
#' @param plot.field.pretty the default is \code{NULL}.  Applies only when \code{bin.style = F}. This is a nice name to describe \code{plot.field} which will be displayed in the legend.
#' @param pnt.style the default is \code{21}. Determines the style of the points (any valid value for \code{pch} is acceptable).
#' @param pnt.outline the default is \code{'black'}. This determines the colour outlining the points.
#' @param pnt.fill the default is \code{'red'}.  Applies only when \code{show.legend = T}.  Determines the colour of the points.
#' @param pnt.cex.min the default is \code{1}. When \code{bin.style = F}, this determines the size of the points, and when \code{bin.style = T}, this determines the minimum size of the points to be drawn.
#' @param pnt.cex.max the default is \code{2}. When \code{bin.style = F}, this is ignored, and when \code{bin.style = T}, this determines the maximum size of the points to be drawn.
#' @param show.legend the default is \code{FALSE}.  Determines whether or not a legend will be displayed.
#' @param leg.pos the default is \code{'topleft'}.  Determines where on the plot the legend will be displayed (if \code{show.legend=TRUE}). Valid values are 'topleft','topright','bottomleft','bottomright'.
#' @param colour.ramp the default is \code{c("#edf8b1", "#7fcdbb", "#2c7fb8")}, which is color-blind friendly. If this vector exists, data will be symbolized along a gradient of colours as the values in `plot.field` increase. Otherwise, all data will be symbolized as the value selected for `pnt.fill`. You can use hex values, or colour names (e.g. \code{c('blue','pink')}), but there must be at least 2 colour present.
#' @param size.ramp the default is \code{TRUE} this indicates whether or not the point size should increase with increasing values of \code{plot.field}
#' @param bin.style the default is \code{fixed} chosen style: one of "NULL",  fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks".  If \code{NULL}, all points will be displayed  identically, but if a grouping style is selected, the data points will be grouped according to the values in \code{plot.field}.
#' @param fixed.breaks.bins the default is \code{c(1,100,1000,100000)} if the \code{bin.style} is set to "fixed", a vector of the desired upper ends of the desired breaks must be supplied (e.g. \code{c(2, 5, 10, 100, 500.10000, 100000)})
#' @param trim.fixed.breaks the default is \code{TRUE}  In cases where the \code{fixed.breaks.bins} vector extends beyond the data values, this parameter removes unused categories so that the largest values receive the maximum symbology (i.e. largest pnt.cex.max and extreme colour.ramp colour)
#' @param trim.legend the default is \code{TRUE} This ensure that only values that exist in the data are shown in the legend.
#' @param nbins the default is \code{3}. Applies only when a \code{bin.style} is selected.  Determines how many "bins" to use to display the data.
#' @param save.plot the default is \code{FALSE}.  If FALSE, the plot is displayed, if TRUE, it is saved to the working directory as a bio.plotting.png.
#' @return a SpatialPolygons object  (identifying the boundaries and projection of an existing plot).  It also notifies the user of how many positions lay outside of the map boundaries.
#' @importFrom sp CRS over plot spTransform SpatialPointsDataFrame
#' @importFrom graphics plot legend plot.new
#' @importFrom classInt classIntervals findColours findCols
#' @importFrom grDevices dev.off png
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
           pnt.fill = '#1874CD',
           pnt.outline = '#333333',
           pnt.cex.min = 1,
           pnt.cex.max = 2,
           show.legend = TRUE,
           leg.pos = 'topleft',
           colour.ramp = c("#edf8b1", "#7fcdbb", "#2c7fb8"),
           size.ramp = TRUE,
           bin.style = 'fixed',
           fixed.breaks.bins = c(1, 4, 32, 256, 2045, 16336, 130496, 1042432, 8327186),
           trim.fixed.breaks = TRUE,
           trim.legend = TRUE,
           nbins = 3,
           save.plot = FALSE) {
    
    #null symbology below
    nullPtType = 3
    nullPtSize = 1/2.5
    nullSymbCol = "#ABABAB"
    nullSymbOutline = "#ABABAB"

    get_pnt_size <- function(origpt, pnt.cex.min = pnt.cex.min, pnt.cex.max = pnt.cex.max, nbins = nbins){
      return(pnt.cex.min + (((origpt * pnt.cex.min) - pnt.cex.min) * ((pnt.cex.max - pnt.cex.min) / (nbins - 1))))
    }
    if (save.plot) {
      plot.new()
      png('bio.plotting.png', width = 800, height=800)
    }
    cangroup = FALSE
    df = df_qc_spatial(df, lat.field, lon.field)
    if (is.null(basemap)) basemap = make_basemap(df, lat.field=lat.field, lon.field=lon.field)
    
    #no plot field provided -
    if (is.null(plot.field)) plot.field = 'noneprovided'
    if (plot.field == 'noneprovided' | !(plot.field %in% colnames(df))) {
      if (requireNamespace("bio.datawrangling",quietly = FALSE)) {
        plot.field = bio.datawrangling::read_mind()[2]
        if (is.null(plot.field.pretty)) plot.field.pretty = plot.field
        cangroup = TRUE
        cat("\nNo plot.field was indicated, assuming ",plot.field)
      }else{
        cangroup = FALSE
        if (!is.null(bin.style)){
          #no plot.field determined, but want bins
          cat(
"\nThere's a problem symbolizing the data by a plot.field.  Either you didn't identify a field, or you
wrote a fieldname that doesn't exist.  To avoid this, please set a valid field to plot.field.
In the meantime, plotting generically..."
          )
          plot.field = lat.field
          plot.field.pretty = "<no plot field>"
          bin.style = NULL
        }
      }
    } else {
      cangroup = TRUE
    }
    #we've decided whether or not we can group the data, so...
    u.values = NROW(unique(df[!is.na(df[plot.field]), ][plot.field]))
    if ((u.values > 3) & (u.values > nbins) & (!is.null(bin.style))) {
      cangroup = TRUE
    } else{
      cangroup = FALSE
      if (!is.null(bin.style)) cat("\nToo little data to bin - using generic symbolization")
    }
    #create spatial data and project it
    df.sp = df[, c(lon.field, lat.field)]
    df.sp <-
      sp::SpatialPointsDataFrame(
        coords = df.sp,
        data = df,
        proj4string = sp::CRS("+init=epsg:4326")
      )
    rm(df)
    df.sp = sp::spTransform(df.sp, sp::CRS(basemap@proj4string@projargs))
    #check if points are contained by basemap
    df.sp$over = sp::over(df.sp, basemap)
    n.invalidpts = NROW(df.sp[is.na(df.sp$over), ])
    if (n.invalidpts > 0)
      cat(paste0("\n", n.invalidpts, " of ", NROW(df.sp), " positions lie outside of the map"))
    df.sp = df.sp[!is.na(df.sp$over), ]
    df.sp$over = NULL

    if (is.null(plot.field.pretty)) plot.field.pretty=plot.field
    #set defaults

    if (cangroup == FALSE | is.null(bin.style)){
      #want/must plot generically
      df.sp@data$ptType = pnt.style
      df.sp@data$ptSize = pnt.cex.min
      df.sp@data$ptFill = pnt.fill
      df.sp@data$ptOutline = pnt.outline
      legend.df = data.frame(categdesc = "data",
                             ptType = pnt.style,
                             ptSize = pnt.cex.min,
                             ptFill = pnt.fill,
                             ptOutline = pnt.outline)
    }else{
      df.sp$ORD = seq.int(nrow(df.sp))
      df.classes = as.data.frame(df.sp)
      df.classes = df.classes[, c("ORD", plot.field)]
      df.classes = df.classes[!is.na(df.classes[plot.field])&(df.classes[plot.field]>0),]

      if (bin.style == 'fixed' & trim.fixed.breaks == TRUE){
        #get rid of unused classes prior to creating symbols
        min.bin = min(fixed.breaks.bins[fixed.breaks.bins>max(df.classes[, plot.field])])
        fixed.breaks.bins = fixed.breaks.bins[fixed.breaks.bins <= min.bin]
      }
      if (bin.style == 'fixed'){
      classes = classInt::classIntervals(
        df.classes[, plot.field],
        n = nbins,
        style = bin.style,
        fixedBreaks = fixed.breaks.bins,
        dataPrecision = 0)
    }else{
      classes = classInt::classIntervals(
        df.classes[, plot.field],
        n = nbins,
        rtimes = 3,
        style = bin.style,
        dataPrecision = 0)
    }
      if (class(colour.ramp) == "character" & length(colour.ramp)>1) {
        ptFill = classInt::findColours(classes, colour.ramp) #colorblind-friendly yellow-blue
       } else{
         ptFill = classInt::findColours(classes, c(pnt.fill, pnt.fill)) #hack to use bg colour only
       }
      #get the possible symbology values from the classes - these may not exist in the data
      symbol.df = data.frame(
        ptType = rep(pnt.style,length(classes$brks)-1),
        ptSize = get_pnt_size(seq(1,length(classes$brks)-1), pnt.cex.min, pnt.cex.max, nbins),
        ptFill = attr(ptFill, "palette"),
        ptOutline = pnt.outline,
        categ = classes$brks[1:length(classes$brks)-1],
        categdesc = gsub(",","-",names(attr(ptFill, "table")))
      )
      if (size.ramp == FALSE) symbol.df$ptSize = pnt.cex.min
      colour.df = data.frame(varname = classes$var,
                             ptFill,
                             ptSize = classInt::findCols(classes),
                             ptType = pnt.style,
                             ptOutline = pnt.outline)
      df.sp@data = merge(df.sp@data, unique(colour.df), by.x=plot.field, by.y='varname', all.x = T)
      df.sp@data = df.sp@data[order(df.sp@data$ORD),]
      df.sp@data$ptSize = get_pnt_size(df.sp@data$ptSize, pnt.cex.min, pnt.cex.max, nbins)
      if (size.ramp == FALSE) df.sp@data$ptSize = pnt.cex.min


      #assemble legend values, add nulls
      legend.df= symbol.df[c("categdesc","ptType","ptSize", "ptFill", "ptOutline")]
      

    }     
    legend.df = rbind.data.frame(c("null", nullPtType, nullPtSize, nullSymbCol, nullSymbOutline),legend.df)
    legend.df$ptType = as.numeric(legend.df$ptType)
    legend.df$ptSize = as.numeric(legend.df$ptSize)
    legend.df = legend.df[order(legend.df$ptType, legend.df$ptSize),]
    #add nulls - this was previously only in the cangroup section
    df.sp@data$ptType[is.na(df.sp@data[plot.field]) | (df.sp@data[plot.field]==0)] = nullPtType
    df.sp@data$ptSize[is.na(df.sp@data[plot.field]) | (df.sp@data[plot.field]==0)]= nullPtSize
    df.sp@data$ptFill[is.na(df.sp@data[plot.field]) | (df.sp@data[plot.field]==0)]= nullSymbCol
    df.sp@data$ptOutline[is.na(df.sp@data[plot.field]) | (df.sp@data[plot.field]==0)]= nullSymbOutline
    #only keep legend entries that exist in data
    if (trim.legend) legend.df = legend.df[legend.df$ptSize %in% df.sp@data$ptSize,]

    sp::plot(
      df.sp,
      col = df.sp@data$ptOutline,
      bg = df.sp@data$ptFill,
      pch = df.sp@data$ptType,
      cex = df.sp@data$ptSize,
      add = T
    )
    if (show.legend) {
      #create this first so we can detect height and width for placement
      leg.scratch =   graphics::legend(
        title = plot.field.pretty,
        legend = legend.df$categdesc,
        inset = 2,
        x=  max(basemap@bbox[1, ]),
        y = max(basemap@bbox[2, ]),
        bty="o",y.intersp=0.75,x.intersp=0.75,
        xjust= 0,
        yjust=0.5,
        pt.lwd=0.6,
        pch=legend.df$ptType,
        pt.cex=legend.df$ptSize,
        pt.bg =legend.df$ptFill,
        col =legend.df$ptOutline,
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
        legend = legend.df$categdesc,
        inset = 2,
        x= leg.x,
        y = leg.y,
        bty="o",y.intersp=0.75,x.intersp=0.75,
        xjust= 0,
        yjust=0.5,
        pch=legend.df$ptType,
        pt.cex=legend.df$ptSize,
        pt.lwd=0.6,
        pt.bg =legend.df$ptFill,
        col =legend.df$ptOutline,
        plot = T
      )
    }
    if (save.plot) dev.off()
    return(basemap)
  }
