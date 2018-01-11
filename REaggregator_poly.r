#' @title aggregator_poly
#' @description At this time, data with privacy considerations must be aggregated such that each 
#' polygon has a minimum of 5 unique values for sensitive fields like Licenses, License Holders, and 
#' Vessels.  This function takes a dataframe and shapefile and for each polygon in the 
#' shapefile calculates 1) aggregate values for a number of (user-specified) fields , and 2) 
#' how many unique values exist in each polygon for each of a number of sensitive fields. A 
#' shapefile is generated with all of the data, as well as a field indicating whether or not the 
#' data can be displayed.
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for 
#' \code{db} should be provided
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param rule.of default is \code{5} Whether or not a polygon is flagged as "CAN_SHOW" or not 
#' depends on the presence of a threshold number of unique values for certain sensitive fields.  
#' This parameter sets that threshold.
#' @param agg.fields the default is \code{NULL}, but if a db is provided, they 
#' will be guessed.  These are the fields on which to aggregate (i.e. generate 
#' values of \code{MEAN}, \code{COUNT} and \code{SUM}).
#' @param sens.fields the defaults are \code{NULL}  These are fields
#' to which the "rule of 5" should be applied. The Treasury Secretariat states that when data is 
#' shown to the public, certain fields must have at least 5 unique values for these fields 
#' aggregated together. When run, this function will look at these fields, and calculate how many 
#' unique values exist for each.  It will then populate a field 'TOTUNIQUE' with the minimum number 
#' of unique values of all the assessed fields. If this is 5 or more, a field called 'CAN_SHOW' will 
#' be marked as 'YES' (otherwise it will be 'NO').
#' @param create.shp default is \code{FALSE}.   This controls whether or not to
#' create a shapefile of the NAFO zones showing the aggregated data (and whether or not 
#' data within each zone can be shown at all.
#' @return a SpatialPolygonsDataFrame, and generates a shapefile
#' @family aggregation
#' @importFrom stats aggregate
#' @importFrom rgdal writeOGR
#' @importFrom sp CRS
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp over
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function is not a replacement for your brain.  If sensitive fields have names that are 
#' different than what is provided in the \code{sen.fields}, they will not be detected, or 
#' included in the checks.  Please make very sure you correctly identify such fields.
#' @export
aggregator_poly <- function(
  df= NULL, 
  db = NULL, 
  lat.field = 'LATITUDE',
  lon.field = 'LONGITUDE',
  rule.of = 5,
  agg.fields = NULL,
  sens.fields = NULL,
  create.shp = FALSE
){
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  
  if (is.null(df) & is.null(db)){
    df = summarize_catches(ds_all[[.GlobalEnv$db]]$db)
    sens.fields = ds_all[[.GlobalEnv$db]]$field_private
    agg.fields = ds_all[[.GlobalEnv$db]]$field_default
  }
  df = df_qc_spatial(df, lat.field, lon.field, FALSE)
  #make the points spatial
  sp::coordinates(df) = c(lon.field, lat.field)
  sp::proj4string(df) = sp::CRS("+proj=longlat +datum=WGS84")

  theshape = data("NAFOSubunits")

#  theshape = readRDS(path.expand('~/sf_git/Maritimes/Mar.datawrangling/data/NAFO.rda'))

  #add the poly fields to each point
  df@data = cbind(df@data,sp::over( df, theshape , fn = NULL))
  
  df.poly = df@data
  
  #verify that field values can be aggregated - change NA to 0
  for (i in 1:length(agg.fields)) {
    if (nrow(df.poly[is.na(df.poly[, agg.fields[i]]), ]) > 0)
      df.poly[is.na(df.poly[, agg.fields[i]]), ][, agg.fields[i]] <- 0
  }
  #ensure they're numeric
  df.poly[agg.fields] <- sapply(df.poly[agg.fields], as.numeric)
  
  #aggregate - calculate statistics by polygon area
  #this df will be merged to the original poly
  df.agg = as.data.frame(as.list(aggregate(
    df.poly[agg.fields],
    by = df.poly[c('NAFO_BEST')],
    FUN = function(x)
      c(
        MEAN = round(mean(x), 4),
        CNT = round(length(x), 4),
        SUM = round(sum(x), 4)
      )
  )))
  
  df.agg[,2:ncol(df.agg)] <- sapply(df.agg[,2:ncol(df.agg)], as.numeric)
  #how many unique values of each sensitive field per poly? 
  df.agg.sens = as.data.frame(as.list(aggregate(
    df.poly[intersect(sens.fields, colnames(df.poly))],
    by = df.poly[c('NAFO_BEST')],
    FUN = function(x)
      c(
        CNT = round(length(unique(x)), 4)
      )
  )))

  df.agg.sens$TOTUNIQUE = apply(as.data.frame(df.agg.sens[,2:ncol(df.agg.sens)]), 1, min)
  df.agg.sens$CAN_SHOW[df.agg.sens$TOTUNIQUE>=rule.of] = 'YES'
  df.agg.sens[is.na(df.agg.sens$CAN_SHOW),]$CAN_SHOW<-'NO'
  
  df.agg = merge(df.agg, df.agg.sens)
  if (create.shp){
    attribs = prepare_shape_fields(df.agg)
    theshape = sp::merge(theshape, attribs)
    shpname = paste0('screened_NAFO_', ts)
    writeOGR(theshape, ".", shpname, driver="ESRI Shapefile", overwrite_layer=TRUE)
    cat(paste0("\nCreated shapefile ", getwd(), .Platform$file.sep, shpname,".shp"))
  }
  return(df.agg)
}