#' @title bucket_o_plots
#' @description This function takes filtered data (from \code{bio.datawrangling::data_filter()}), 
#' and generates a series of plots.  Two fields must be provided by the user - one specifies the 
#' field for which to generate plots (\code{plot.by}), and the second specifies which field contains 
#' the values to plot (\code{plot.value}).
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param plot.by default is NULL.  This field determine what the focus of series of plots will be -
#' for example, selecting YEAR would generate plots by year, while selecting SPEC would generate a 
#' series of plots by species.  This can be provided in the function call, or a selection box will 
#' prompt the user.
#' @param plot.value default is NULL.  This field determine what field should be used to symbolize 
#' the data.  Only numeric values are allowed.  This can be provided in the function call, or a 
#' selection box will prompt the user.
#' @param crs.out default is \code{'+init=epsg:2220'} (UTM Zone 20 N).  This is the desired 
#' projection of the final plot. Specifying \code{'+init=epsg:4326'} will generate the rectangular
#' plot common to the VDC.
#' @param x.limits default is \code{c(-70, -54)} but an appropriate value would be in the form of 
#' \code{c(-70,-54)}. These are the default
#' bounding longitudes.
#' @param y.limits default is \code{c(41, 50)} but an appropriate value would be in the form of  is 
#' \code{c(41,50)}. These are the default bounding latitudes for extent.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude values (in decimal degrees)
#' @param bin.style the default is \code{fixed} chosen style: one of "NULL",  fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks".  If \code{NULL}, all points will be displayed  identically, but if a grouping style is selected, the data points will be grouped according to the values in \code{plot.field}.
#' @param fixed.breaks.bins the default is \code{c(1,100,1000,100000)} if the \code{bin.style} is set to "fixed", a vector of the desired upper ends of the desired breaks must be supplied (e.g. \code{c(2, 5, 10, 100, 500.10000, 100000)})
#' @param trim.fixed.breaks the default is \code{TRUE}  In cases where the \code{fixed.breaks.bins} vector extends beyond the data values, this parameter removes unused categories so that the largest values receive the maximum symbology (i.e. largest pnt.cex.max and extreme colour.ramp colour)
#' @param save.plot the default is \code{FALSE}.  If FALSE, the plot is displayed, if TRUE, it is 
#' saved to the working directory as a bio.plotting.png.
#' @return no data, but a series of plots
#' @family db_specific
#' @importFrom stats aggregate
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot.new
#' @importFrom stats complete.cases
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note This line is here to prevent an error message claiming the export is mult-line
bucket_o_plots <- function(db = 'rv', plot.by = NULL, plot.value = NULL,
                           crs.out = '+init=epsg:2220',
                           x.limits = c(-72, -56),
                           y.limits = c(40, 47.5),
                           lat.field = "LATITUDE",
                           lon.field = "LONGITUDE",
                           save.plot = FALSE,
                           bin.style = 'fixed',
                           fixed.breaks.bins = c(1, 4, 32, 256, 2045, 16336, 130496, 1042432, 8327186),
                           trim.fixed.breaks = TRUE){
  if (!requireNamespace("bio.plotting")) {
    return(NULL)
  }else{
  promptUser = FALSE
  if (is.null(plot.by) | is.null(plot.value)) promptUser = TRUE
  #' save the data as it was in a new environment; and
  #' get a df of all of the available fields, the tables they come from, and how many unique values 
  #' are in each field
  virgin = new.env()
  cols <-  data.frame(FIELD = NA, CLAS = NA, TAB = NA)
  tab= ds_all[[.GlobalEnv$db]]$tables

#CLAS is not working because times are returning 2 classes! 
  for (k in 1:length(tab)){
    assign(tab[k], get(tab[k]), envir = virgin)
    colrows = data.frame(FIELD = colnames(get(tab[k])), 
                         #the magic below is kludgy since dates return multiple classes
                         CLAS = unname(rapply(lapply(get(tab[k]),class), function(x) x[1], how = "unlist")),
                         TAB = rep(tab[k], length(colnames(get(tab[k])))))
    m = lapply(get(tab[k]),class)
    cols = rbind(cols, colrows)
  }
  cols = cols[complete.cases(cols),]
  cols = cols[order(cols$FIELD, cols$CLAS, cols$TAB),]
  #' keep just one row per FIELD
  cols = cols[!duplicated(cols$FIELD), ]
  
  if (is.null(plot.by)){
    sel.field = select.list(
      cols$FIELD,
      multiple = F,
      graphics = T,
      title = 'What field do you want to generate plots for?'
    )
    
    
    if (sel.field=="") stop("Cancelled by user")
    plot.by = gsub('\\s\\([^)]*\\)', '\\1', sel.field)
  }
  if (is.null(plot.value)){  
    plot.value = select.list(
      cols[(cols$CLAS %in% c('numeric','integer','double')) & (!cols$FIELD %in% plot.by),]$FIELD,
      multiple = F,
      graphics = T,
      title = 'What field should the bubble size reflect?'
    )
  }

  chosen = cols[cols$FIELD ==plot.by,]
  chosen.table = get(chosen[[3]])
  entries=sort(unique(chosen.table[chosen[[1]]][,1]))
 
   
  for (j in 1:length(entries)){
    cat(paste0("\nWorking on ",plot.by," = '", entries[j],"'"))
    if (save.plot) {
      plot.new()
      png('bio.plotting.png', width = 800, height=800)
    }
    
    assign(chosen[[3]],chosen.table[chosen.table[[plot.by]]==entries[j],], envir = .GlobalEnv)
    self_filter(db)
    df.plot = summarize_catches(db=db, valid.coords = TRUE, quiet=TRUE)
    #added a try block in case no data falls in the plot area
    bm = bio.plotting::make_basemap(x.limits=x.limits, y.limits=y.limits, crs.out = crs.out)
    try(
      bio.plotting::add_points(df.plot, basemap=bm,
                               bin.style = bin.style, fixed.breaks.bins=fixed.breaks.bins,
                               trim.fixed.breaks = trim.fixed.breaks, plot.field = plot.value, lat.field = lat.field,
                               lon.field = lon.field,
                               plot.field.pretty = entries[j])
      ,silent = TRUE
    )
    

    if (save.plot) dev.off()
    thisbm= NULL
    for (m in 1:length(tab)){
      assign(tab[m], virgin[[tab[m]]], envir = .GlobalEnv)
    }
  }
  }
}
