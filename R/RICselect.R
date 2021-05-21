#' A function to correct for gillnet selectivity
#' 
#' This function estimates the relative size-dependent vulnerability of trout to the standard RIC gillnets.
#' The function is used to multiply observed catches by a multiplier proportional to the fish fork length. 
#' In the future the selectivity function can be made more robust to account for non-standard combinations of gillnet panels.
#' Using a simple multiplier to correct for selectivity only works with good sample sizes, because 0 * anything = 0.
#' 
#' 
#' @title RICselect
#' @name RICselect
#' @keywords SPDT; gillnet; selectivity
#' @export
#' @param FLengths an integer or vector of fork lengths in mm for which to calculate relative probability of capture by RIC gillnet.
#' @examples
#' Must be connected to VPN if working remotely
#' 
#'
#'#Create a vector of fish lengths from 50 to 650mm
#'Fish_lengths = c(50:650)
#'
#'#Estimate the relative probability of capture for each of the fish lengths
#'pvals = RICselect(Fish_lengths)
#'
#'#Plot the selectivity function
#'plot(pvals~Fish_lengths)
#'
#'#However, you can access this selectivity function directly by simply typing
#'select_lookup
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

RICselect<- function(FLengths){
  p = select_lookup$p[fastmatch::fmatch(FLengths, select_lookup$Length_mm)]
  p
}