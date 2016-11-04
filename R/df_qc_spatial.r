#' @title df_qc_spatial
#' @description This function drops points that can't be plotted from a data frame.
#' @param df the dataframe to be cleaned
#' @param return.bad the default is \code{FALSE}.  Normally, only the plottable points are returned.
#' If TRUE, only the rejected points are returned.
#' @return data.frame contents depend of the return.bad parameter
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_qc_spatial <-function(df, return.bad=FALSE){
  pass = df[!is.na(df$LATITUDE) & !is.na(df$LONGITUDE),]
  pass = pass[pass$LATITUDE > -90 & pass$LATITUDE < 90,]
  pass = pass[pass$LONGITUDE > -180 & pass$LONGITUDE < 180,]
  if (return.bad){
    fail = rbind(df, pass)
    fail = fail[!duplicated(fail,fromLast = FALSE)&!duplicated(fail,fromLast = TRUE),] 
    return(fail)
  }else{
    return(pass)
  }
}