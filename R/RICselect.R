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
#' @param Length_mm an integer or vector of lengths for which to calculate relative probability of capture by RIC gillnet.
#' @examples
#' RICselect()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

RICselect<- function(Length_mm){
  
  p = select_lookup$p[select_lookup$Length_mm %in% Length_mm]
  return(p)
}