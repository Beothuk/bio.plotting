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
qmap <- function(db, save.plot=F){
  if (db=='obs'){
    plot.field.db = 'EST_COMBINED_WT'
  }else if (db=='rv'){
      plot.field.db='TOTNO'
    }
  df= bio.datawrangling::summarize_catches(db, all.details = FALSE)
  add_points(df, plot.field = plot.field.db, save.plot=save.plot)
}
