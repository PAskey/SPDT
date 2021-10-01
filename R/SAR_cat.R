#' A function to partition release sizes into discrete categories
#' 
#' This function takes a number (release size) and assigns it to another number category. 
#' This is useful since a difference of 1g between stocking groups is a big deal for a 1g fry release size, but a rounding error for large yearlings.
#' A default set of categories has been created. The function may be updated to offer options in the future.
#' 
#' @title SAR_cat
#' @name SAR_cat
#' @keywords SPDT; gillnet; stocking, release size
#' @export
#' @param SAR a number or vector of fish release sizes (in g or other sizes)
#' @examples
#' Must be connected to VPN if working remotely
#' 
#'
#'#Create a vector of size-at-release values - SARs
#'SARs = c(1:100)
#'
#'#Re-categorize these into broader size bins depending on sizes. 
#'Default bins based on common release sizes and release size targets.
#'SAR_cat = SAR_cat(SARs)
#'SAR_cat
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

SAR_cat = function(SAR){
  SAR_cat = dplyr::case_when(SAR<=5.5 ~ plyr::round_any(SAR,1),
                             SAR>5.5&SAR<=22.5 ~ plyr::round_any(SAR,5),
                             SAR>22.5&SAR<65 ~ plyr::round_any(SAR,10),
                             TRUE ~ plyr::round_any(SAR, 25))
  return(SAR_cat)
}