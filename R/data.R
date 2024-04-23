#' Lookup table to convert the ageing notations to integer ages
#'
#' An unfortunately growing lookup table for non-conforming age notations.
#'
#' @format A data frame with 128 rows and 2 variables:
#' \describe{
#'   \item{Ages}{Ages, Character string describing age(#) and growth(+) or spawning events(s)}
#'   \item{Int.Aget}{Integer Age, Integer converted from the Age character string}
#' }
#' @source PAskey personal copy with .csv stored in data-raw
"Ages"

#' Lookup table to convert the strain names to codes
#'
#' Aids in standardization between SLD and Paris.
#'
#' @format A data frame with 235 rows and 4 variables:
#' \describe{
#'   \item{Species}{Species code, Codes used to describe the species}
#'   \item{stock_strain_loc_name}{Strain name, character string fully describing strain source population}
#'   \item{Strain}{Strain code, Codes used to describe the strains}
#'   \item{N}{Observations, AN integer describing the number of releases of this strain up to Jan 2022}
#' }
#' @source PAskey personal copy with .csv stored in data-raw
"Strain_code_LU"

#' A set of parameters to estimate relative vulnerability to capture by gillnet dependent on fish size
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
#' @source PAskey personal copy. Estimation process described in Gillnet selectivity RMarkdown document on tech committees sharepoint.
"RIC_param"

#' BC Fish Naming Codes and Data
#'
#' Lookup table with species codes and additional grouping columns for scientific naming, spawn timing, recreational, invasive and/or conservation values, etc. Also see fishbc package (https://github.com/poissonconsulting/fishbc).
#'
#' @format A data frame with 174 rows and 23 variables:
#' \describe{
#'   \item{species_id}{Species identification key from SMall Lakes Database (SLD) numeric}
#'   \item{species_former_name}{Character string describing any former names fo rthe same species}
#'   \item{species_name}{charater string of current accpeted species name}
#'   \item{species_code}{character string for standard ( BC RISC) accepted code abbreviation for species name}
#'   \item{sportfish_group}{character string classifying species by the recreational, invasive, etc. values}
#'   \item{subfamily}{character string classifying species by the subfamily}
#'   \item{stocked_species}{A logical indicating whether the species has been stocked in BC at least once}
#'   \item{spawn_season}{character string describing whether the species spawn period is in spring or fall}
#'   \item{Code}{A character vector of the unique provincial fish code.}
#'\item{CommonName}{A character vector of the common name.}
#'\item{Class}{A character vector of the class.}
#'\item{Order}{A character vector of the order.}
#'\item{Family}{A character vector of the family.}
#'\item{Genus}{A character vector of the genus.}
#'\item{Species}{A character vector of the species.}
#'\item{Subspecies}{A character vector of the subspecies.}
#'\item{Species2}{A character vector of the second species if a hybrid.}
#'\item{Extant}{A logical vector indicating whether any of the fishes are extant in British Columbia.}
#'\item{Native}{A logical vector indicating whether any of the fishes are native to British Columbia.}
#'\item{Marine}{A logical vector indicating whether a life-stage of any of the fishes occur in saltwater in British Columbia.}
#'\item{Yellow}{A logical vector indicating whether any of the fishes are yellow listed in British Columbia.}
#'\item{Blue}{A logical vector indicating whether any of the fishes are blue listed in British Columbia.}
#'\item{Red}{A logical vector indicating whether any of the fishes are red listed in British Columbia.}
#'\item{CDCode}{A character vector specifying the BC Conservation Data Centre species code.}
#' }
#' @source PAskey personal copy with .csv stored in data-raw
"Spp_code_group_LU"

