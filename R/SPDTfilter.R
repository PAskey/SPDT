#' A function to facilitate rapid filtering SPDTdata to specific cases beyond the biological references (lakes, years, project)
#'
#'
#' This is convenience data filtering and cleaning process when doing exploratory analysis or plotting with SPDTdata. 
#'
#'
#' @title SPDTfilter
#' @name SPDTfilter
#' @keywords SPDT
#' @export
#' @param Projects an optional character string or character vector describing the Project_Name from Assessments table in SLD.
#' @param Lk_yrs an optional character string or character vector of the Lake-years to filter the data set to. Must be in format WBID_YYYY (e.g. "01100OKAN_2020" or for multiples c("01100OKAN_2020", "01598LNTH_2018"))
#' @param Lake_Names an optional character string or character vector describing the lakes to include in data.
#' @param WBIDs an optional character string or character vector describing the WBIDs to include in data.
#' @param Years an optional integer or vector describing the sampling years to include in data.
#' @param Regions an optional character string or character vector describing the management regions to include in data.
#' @examples
#' #Must be connected to VPN if working remotely
#' 
#' #Download all data with clipped fish
#' SPDTdata(Contrast = "Strain")
#' 
#' #Creates a character vector of lake-years that exist in SPDT data call that were sampled in Mathew lake.
#' SPDTfilter(Waterbody_Names = "MATHEW")
#' 
#' #Typically we would call this within the SPDTdata (or SPDTplot) call
#' SPDTdata(Contrast = "Strain", filters = SPDTfilter(Waterbody_Names = "MATHEW"))
#
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTfilter<- function(Projects = NULL, Lk_yrs = NULL, Waterbody_Names = NULL, WBIDs = NULL, Years = NULL, Regions = NULL){

  
#Lk_yr is the unit unique to all potential filters. Create a complete vector of lake-years that apply  

    Projly = Assessments$Lk_yr[Assessments$Project_Name %in% Projects]
    Lakely = Biological$Lk_yr[Biological$Waterbody_Name%in%Waterbody_Names]
    WBIDly = Biological$Lk_yr[Biological$WBID%in%WBIDs]
    Yearsly = Biological$Lk_yr[Biological$Year%in%Years]
    Regionsly = Biological$Lk_yr[Biological$Region%in%Regions]
    
    return(unique(c(Projly, Lakely,WBIDly, Yearsly, Regionsly)))
    
}
