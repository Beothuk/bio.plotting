#' @title assess_privacy
#' @description This function takes filtered data (from \code{bio.datawrangling::data_filter()}),
#' and generates a series of plots.  Two fields must be provided by the user - one specifies the
#' field for which to generate plots (\code{aggregate.by}), and the second specifies which field contains
#' the values to plot (\code{aggregate.fields}).
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param aggregate.by default is NULL.  This field determine what the focus of series of plots will be -
#' for example, selecting YEAR would generate plots by year, while selecting SPEC would generate a
#' series of plots by species.  This can be provided in the function call, or a selection box will
#' prompt the user.
#' @param aggregate.fields default is NULL.  This field determine what field should be used to symbolize
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
#' @param make.shapes default is \code{TRUE}.  This controls whether or not the function generates
#' a shapefile for the polygon that is being assessed.  If FALSE, no shapefile is created.
#' @param include.poly.aggs default is \code{TRUE}. Sometime you may want the polygon just to show
#' which areas meet the privacy constraints, and you don't want to include the aggegated values for
#' each area.  Changing this to \code{FALSE} will prevent the aggregate values from being added to
#' the polygon.
#' @param debug default is \code{FALSE}. By default, the privacy checks can only be applied to 
#' commercial data.  This allows you to apply it to other datasets.
#' @return no data
#' @family db_specific
#' @importFrom stats aggregate
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics plot.new
#' @importFrom stats complete.cases
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note This line is here to prevent an error message claiming the export is mult-line
assess_privacy <- function(db = NULL, aggregate.by = NULL, aggregate.fields = NULL,
                               crs.out = '+init=epsg:2220',
                               x.limits = c(-72, -56),
                               y.limits = c(40, 47.5),
                               lat.field = "LATITUDE",
                               lon.field = "LONGITUDE",
                               make.shapes = TRUE,
                               include.poly.aggs = TRUE, 
                               debug = FALSE){

  if (!db %in% c('isdb','marfis') & !debug) stop("\nOnly Commercial data requires this analysis")
      ts = format(Sys.time(), "%Y%m%d_%H%M")
    promptUser = FALSE
    if (is.null(aggregate.by) | is.null(aggregate.fields)) promptUser = TRUE
    #' save the data as it was in a new environment; and
    #' get a df of all of the available fields, the tables they come from, and how many unique values
    #' are in each field
    virgin = new.env()
    cols =  data.frame(FIELD = NA, CLAS = NA, TAB = NA)
    tab = ds_all[[.GlobalEnv$db]]$tables

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

    if (is.null(aggregate.by)){
      sel.field = select.list(
        cols$FIELD,
        multiple = F,
        graphics = T,
        title = 'Assess data by unique values of which field?'
      )

      if (sel.field=="") stop("Cancelled by user")
      aggregate.by = gsub('\\s\\([^)]*\\)', '\\1', sel.field)
    }
    if (is.null(aggregate.fields)){
      aggregate.fields = select.list(
        cols[(cols$CLAS %in% c('numeric','integer','double')) & (!cols$FIELD %in% aggregate.by),]$FIELD,
        multiple = T,
        graphics = T,
        title = 'What data fields should be aggregated?'
      )
    }
    chosen = cols[cols$FIELD ==aggregate.by,]
    chosen.table = get(chosen[[3]])
    entries=sort(unique(chosen.table[chosen[[1]]][,1]))
    for (j in 1:length(entries)){
      cat(paste0("\nWorking on ",aggregate.by," = '", entries[j],"'"))

      #limit the data to a single value for the selected field (e.g. a single species, year, etc)
      assign(chosen[[3]],chosen.table[chosen.table[[aggregate.by]]==entries[j],], envir = .GlobalEnv)
      self_filter(db)
      df.loop = summarize_catches(db=db, valid.coords = TRUE, quiet=TRUE)
      #make NAFO_screen for this selection (and write it out)
      poly_screen = aggregator_poly(df = df.loop, polygon.shapefile ='NAFOSubunits',
                                    agg.field.poly = c('NAFO_BEST'), agg.fields = aggregate.fields,
                                    name.bit = entries[j],
                                    make.shapes = make.shapes,
                                    include.poly.aggs = FALSE)
      poly_screen = spTransform(poly_screen, CRS('+init=epsg:4326')) #to geographic to match pts
      poly_screenname = paste0('screenedNAFO_',entries[j],'_', ts)
      #retain only the polygons that had data in them f
      poly_screen  = subset(poly_screen, !is.na(CAN_SHOW))
      assign(poly_screenname, poly_screen, envir = .GlobalEnv)
      #subset poly to the ones with enough unique private records
      allowed.poly  = subset(poly_screen, CAN_SHOW == 'YES')

      if (nrow(allowed.poly)>0){
        #make df spatial and clip to acceptable polygons
        df.loop.sp = df_to_sp(df.loop)
        df.loop.allowed <- df.loop.sp[allowed.poly, ]
        #aggregate to 2 minute squares
        if (nrow(df.loop.allowed)){
          cells2m = aggregator(df.loop.allowed@data, agg.fields = aggregate.fields, agg.minutes = 2, sens.fields = NULL)
          cells2m.sp = df_to_sp(cells2m)
          if (make.shapes) cells2m.sp = prepare_shape_fields(cells2m.sp)
          cellname = paste0('screened2min_',entries[j],'_', ts)
          assign(cellname, cells2m.sp, envir = .GlobalEnv)
          if (make.shapes) writeOGR(cells2m.sp, ".", cellname, driver="ESRI Shapefile", overwrite_layer=TRUE)
        }

      }
      for (m in 1:length(tab)){
        assign(tab[m], virgin[[tab[m]]], envir = .GlobalEnv)
      }
    }
  }
