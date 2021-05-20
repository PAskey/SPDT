#' A function to correct for gillnet selectivity
#' 
#' This function estimates the relative size-dependent vulnerability of trout to the standard RIC gillnets.
#' The function is used to multiply observed catches by a multiplier proportional to the fish fork length. 
#' In the future the selectivity function can be made more robust to account for non-standard combinations of gillnet panels.
#' Using a simply multiplier to correct for selectivity, only works with good sample sizes, because 0 * anything = 0.
#' 
#' 
#' @title RICselect
#' @name SPDTdata
#' @keywords SPDT
#' @export
#' @param panels a vector of mesh sizes used in the gillnet assessment. This is assumed constant across nets.
#' @examples
#' RICselect()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

RICselect<- function(Length_mm){
  
  p = fastmatch::fmatch(Length_mm, gn_select_lookup$p)
}