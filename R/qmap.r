#' @title qmap
#' @description qmap ('quickmap') is a simple script for quickly generating plots from bio.datawrangling datasets.
#' @param db This identifies the dataset you want to plot.  Valid values are 'obs'
#' (observer) and 'rv' (groundfish/research survey).
#' @param save.plot the default is \code{FALSE}.  If FALSE, the plot is displayed, if TRUE, it is saved to the working directory as a bio.plotting.png.
#' @return list of the filters that were applied to generate the remaining dataset
#' @family extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom bio.datawrangling summarize_catches
#' @export
qmap <- function(df=NULL, save.plot=F){

psi = read_mind()
db = psi[1]
plot.field.db = psi[2]

#check if df looks like an aggregate, if so, change plot.fields
  
  if (is.null(df)) {
    df= bio.datawrangling::summarize_catches(db, all.details = FALSE)
  }
  if (plot.field.db %in% colnames(df)) {
    basemap = add_points(df, plot.field = plot.field.db, save.plot=save.plot)
  }else{
    #maybe it's aggregated data?  Try appending ".SUM"
    plot.field.db = paste0(plot.field.db,".SUM")
    basemap = add_points(df, plot.field = plot.field.db, save.plot=save.plot)
  }
  
return(basemap)
}
