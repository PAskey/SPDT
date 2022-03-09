#' Lookup table to convert the ageing notations to integer ages
#'
#' An unfortuantely growing lookup table for non-conforming age notations.
#'
#' @format A data frame with 128 rows and 2 variables:
#' \describe{
#'   \item{Ages}{Ages, Character string describing age(#) and growth(+) or spawning events(s)}
#'   \item{Int.Aget}{Integer Age, Integer converted from the Age character string}
#' }
#' @source PAskey personal copy
"Ages"

#' Lookup table to convert the strain names to codes
#'
#' Aids in standardization between SLD and Paris.
#'
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{stock_strain_loc_name}{Strain name, character string fully describing strain source population}
#'   \item{Strain}{Strain code, Codes used to describe the strains}
#' }
#' @source PAskey personal copy
"Strain_code_LU"

#' A set of parameters ro estimate relative vulnerability to capture by gillnet dependent on fish size
#'
#' Aids in standardization of strain comparisons when groups are different sizes.
#'
#' @format A list with 4 parts: RIC_meshes, theta, rel.power are for using predict_Millar() and p_gam are stored gam model predictions
#' \describe{
#'   \item{RIC_meshes}{A vector of standard meshes in in used in RIC gillnets, numeric value}
#'   \item{theta}{A vecotr of 5 parameters needed for bilognorm Millar model}
#'   \item{rel.power}{A vector of 7 parameters to describe relative fishing power of each mesh for bilognorm Millar model}
#'   \item{p_gam}{A named vector describing the relative vulnerability to capture by gillnets for each fork length (mm) using a GAM model on mark-recap data} 
#' }
#' @source PAskey personal copy
"RIC_param"
