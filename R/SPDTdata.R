#' A function to filter and format SLD data to cases with clipped experimental stocking groups. 
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#' This is the final data filtering and cleaning process after SLD2R(), and linkClips(). 
#' These two previous functions are called within this function, so there is no need to repeat.
#' This function returns the data tables from linkClips, plus new refined data sets.
#' "idf" is the individual level data frame, for all clipped fish meeting your filtering selections.
#' "gdf" is the grouped data frame, for all clipped groups meeting your filtering selections.
#' Our database will not likely ever have filtered summaries to this level, so this function will be used and updated in to the future.
#' #Data is filtered by the experimental contrast (Genotype, Strain, SAR) being investigated,
#' and the performance metric of interest (Growth, Survival, Maturation).
#'
#' @title SPDTdata
#' @name SPDTdata
#' @keywords SPDT
#' @export
#' @param Species species (use standard code in quotations) to select, default is "RB"
#' @param Contrast contrast of interest can be any of: "Genotype", "Strain", "SAR". Default is "Strain". 
#' @param Metric Performance metric of interest, can be any of: "Growth" (default), "Survival" or "Maturation"
#' @examples
#' SPDTdata()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTdata <- function(Species = "RB", Contrast = "Strain", Metric = "Growth"){

linkClips()
  

  
  
idf <- Biological%>%
          dplyr::filter(!is.na(.data$Clip)&!is.null(.data$Clip)&.data$Clip != "NONE")%>%#remove non-clips
          dplyr::mutate(NetX = 1/RICselect(Length_mm))
#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. 
gdf <- idf%>%
  dplyr::group_by(.data$Waterbody_Name, .data$WBID, .data$Year, .data$Lk_yr, .data$Species, .data$Strain, .data$Genotype, .data$Int.Age, .data$Clip, .data$sby_code)%>%
  dplyr::summarize(mean_FL = mean(.data$Length_mm, na.rm = TRUE),sd_FL = sd(.data$Length_mm, na.rm = TRUE),
                    mean_wt = mean(Weight_g, na.rm = TRUE), sd_wt = sd(Weight_g, na.rm = TRUE),
                    N = dplyr::n(),
                    NetXN = sum(NetX),
                    p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE),
                    avg_sample_date = as.Date(mean(.data$Date),format='%d%b%Y'),
                    avg_rel_date=as.Date(mean(.data$avg_rel_date),format='%d%b%Y'),
                    SAR = mean(.data$SAR)#SAR is size at release in grams
                  )%>%
  dplyr::ungroup()


gdf = dplyr::full_join(gdf, Xnew, 
                by = c("Waterbody_Name", "WBID", "Year", "Lk_yr", "Int.Age", "Species", "Strain","Genotype", "sby_code", "Clip"))%>%
                dplyr::filter(Clip != "")%>%
                dplyr::mutate(N = replace(N, is.na(N), 0))

idf<<-idf
gdf<<-gdf

#no longer need Xnew from linkClips
rm(Xnew, envir = .GlobalEnv)


}