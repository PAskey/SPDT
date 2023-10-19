#' A convenience function to test whether a clip is unique
#' 
#' This function is used to assess whether a fish captured with a clip is a unique group or was the clip repeated.must be a certain age class, or
#' It checks the historical stocking records of a lake to see if the same clip exists in a specified number of years

#' 
#' @title uniqueClip
#' @name uniqueClip
#' @keywords SPDT; stocked; releases
#' @export
#'
#' @param Clip a character string describing a specific fin clip
#' @param Species character string following BC standard species codes
#' @param Year an integer value of the assessment year to be checked
#' @param WBID A unique waterbody identifier for the lake being assessed
#' @param t_minus A number to specify how far back to check for the same clip (default is 6)
#' 
#' @examples
#' Must be connected to VPN if working remotely, and must have already loaded the Releases data table
#' SD2R()
#' uniqueClip("AD","RB",2018, "01598LNTH")
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

uniqueClip<- function(Clip, Species, Year, WBID, t_minus = 6){
  
  if(!exists("Releases")){stop("Need to start with a data load from SLD to make release data avaiable in your enviro e.g. SLD2R() or SPDTdata()")}
  
  data = Releases%>%dplyr::select(Clip, Species, Year, WBID, sby_code)
  
  c = Clip; spp = Species; t = Year; wb = WBID 
  unique = data%>%dplyr::filter(Clip == c, Species == spp, 
                                    Year %in% c((t-t_minus):t), 
                                    WBID == wb)
 
  out = list("unique" = nrow(unique)==1, "occurences" = nrow(unique), "brood_years" = unique(unique$sby_code))
  return(out)
}
    