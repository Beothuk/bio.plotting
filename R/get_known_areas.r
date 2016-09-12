#' @title get_known_areas
#' @description This function facilitates the addition of a number of definitive/authoritative/canonical spatial areas.  These may be 
#' fishing areas, habitat areas, boundaries, etc.  The important thing is that the areas are from a definitive source.  
#' Multiple areas can be specified, and the \code{known.areas.detailed} flag can be set to either show only the outer boundary, or the 
#' detailed polygons within it.  Areas can be specified by individually by name, or they can be included in a "group", in which case, 
#' calling the group will return all group members.  An individual layer can be in many groups.  Area identified are case-insensitive.
#' 
#' @param \code{known.areas} default is \code{NULL}. Valid values include the following:
#' \itemize{
#'   \item Bowtie
#'   \item LFA40
#'   \item Haddock_Closed_Area
#'   \item Vazella_Emerald
#'   \item Vazella_Sambro
#'   \item Lophelia
#'   \item Right_Whale_Habitat
#'   \item Gully
#'   \item Bottlenose_Habitat
#'   \item NE_Channel
#'   \item St_Ann
#'   \item Musquash
#'   \item DFO_Regions
#'   \item DFO_Regions_Lines
#'   \item US_strata
#'   \item Mar_strata
#'   \item NAFO
#' }
#' Valid groups include:
#' \itemize{
#'   \item mpa (Gully and Musquash)
#'   \item cca (Vazella_Emerald, Vazella_Sambro, NE_Channel and Gully)
#'   \item critical_habitat (Bottlenose and Righ Whale Critical Habitats)
#'   \item Strata (US and Maritimes strata)
#' }
#' @param \code{known.areas.detailed} default is \code{F}.  Some areas are complex and contains sub-areas, and may not be appropriate for all plots.
#' \code{known.areas.detailed  =T} will plot all of complexity, while \code{known.areas.detailed  = F} will only plot the outlines.
#' @return list of SpatialPolygons
#' @examples
#'  plot(get.known.areas('Gully')[[1]])
#' @family plotting
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note As it is now written, this function references paths that will not be visible to most people.  The intent is that these canonical
#' data layers will be moved to a location accessible by all staff.
#' @export
get_known_areas <- function(p.plotting = p.plotting, known.areas=NULL, known.areas.detailed= NULL)
{

  if (is.null(known.areas)) known.areas = p.plotting$known.areas
  if (is.null(known.areas.detailed)) known.areas.detailed = p.plotting$known.areas.detailed

  known.areas = tolower(known.areas)
  
valid.areas=list(
  list(area='bowtie', groups=NULL, 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Fisheries/FisheriesConservationAreas_SOURCE_2007/Original_Data/Bowtie.shp'),
  list(area='lfa40', 
       groups=NULL, 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Fisheries/FisheriesConservationAreas_SOURCE_2007/Original_Data/LFA40.shp'),
  list(area='haddock_closed_area', groups=NULL, 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Fisheries/FisheriesConservationAreas_SOURCE_2007/Original_Data/HaddockBox.shp'),
  list(area='vazella_emerald', groups=c("cca"), 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/SpongeConservationAreas_OCMD_2013/OfficialPublic_Data/Emerald_SpongeConservationArea.shp'),
  list(area='vazella_sambro', groups=c("cca"), 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/SpongeConservationAreas_OCMD_2013/OfficialPublic_Data/SambroBank_SpongeConservationArea.shp'),
  list(area='lophelia', groups=c("cca"), 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/CoralConservationAreas_OCMD_2006/OfficialPublic_Data/Lophelia_cca.shp'),
  list(area='right_whale_habitat', groups=c("critical_habitat"), 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/CritHabitat_NARightWhale_SARMD_2010/OfficialPublic_Data/NorthAtlanticRightWhale_CH.shp'),
  list(area='gully', groups=c("mpa","cca"), 
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/MarineProtectedArea_Gully_OCMD_2004/OfficialPublic_Data/Gully_mpa.shp'),
  list(area='bottlenose_habitat', groups=c("critical_habitat"),
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/CritHabitat_BottleNosedWhale_SARMD_2009/OfficialPublic_Data/NorthernBottlenoseWhale_CH.shp'),
  list(area='ne_channel', groups=c("cca"),
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/CoralConservationAreas_OCMD_2006/OfficialPublic_Data/NEChannel_cca.shp'),
  list(area='st_ann', groups=NULL,
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/MarineProtectedArea_StAnnBankAOI_OCMD_2012/InternalOnly_Data/SAB_Zones_NRCAN.shp'),
  list(area='musquash', groups=c("mpa"),
       path='/home/mike/Shares/OCMD/OCMD/01_Data/Management_Areas/Conservation_Areas/MarineProtectedArea_Musquash_OCMD_2008/OfficialPublic_Data/Musquash_mpa_region.shp'),
  list(area='dfo_regions', groups=NULL,
       path='/home/mike/Shares/R_PED/Shared/Spatial/Management_Areas/FederalRegions/DFO_Regions.shp'),
  list(area='dfo_regions_lines', groups=NULL,
       path='/home/mike/Shares/R_PED/Shared/Spatial/Management_Areas/FederalRegions/DFO_Regions_Lines84_UTM20.shp'),
  list(area='us_strata', groups='strata',
       path='/home/mike/Shares/R_PED/Shared/Spatial/Science/Strata/USNEFSC/EcoMon_Strata.shp'),
  list(area='mar_strata', groups='strata',
       path='/home/mike/Shares/R_PED/Shared/Spatial/Science/Strata/ped_groundfish/MaritimesRegionEcosystemAssessmentStrata(2014-).shp'),
  list(area='nafo', groups=NULL,
       path='/home/mike/Shares/R_PED/Shared/Spatial/Management_Areas/Fisheries/NAFO/Lines_NAFO.shp')
  )

  get_area = function(path=NULL){
    this = readOGR(dsn=dirname(path), layer=sub("[.][^.]*$", "", basename(path)), verbose=F)
    if (known.areas.detailed == F & length(this)>1){
      lps=getSpPPolygonsLabptSlots(this)
      IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
      this   <- unionSpatialPolygons(this,IDOneBin)
    }
    return(this)
  }
  
  these.areas=list()
  for (i in 1:length(known.areas)){
    for (j in 1:length(valid.areas)){
      if (known.areas[i]== valid.areas[[j]][[1]] | known.areas[i] %in% valid.areas[[j]][[2]]){
        these.areas[[paste0(known.areas[i],"_",j)]] = get_area(valid.areas[[j]][[3]])
      }
    }
  } 
    return(these.areas)
}
