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