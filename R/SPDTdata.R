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
#'
#' @title SPDTdata
#' @name SPDTdata
#' @keywords SPDT
#' @export
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' SPDTdata()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTdata <- function(){

linkClips()
  

  
  
idf <- Biological%>%
          dplyr::filter(!is.na(.data$Clip)&!is.null(.data$Clip)&.data$Clip != "NONE")#remove non-clips

#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. 
gdf <- idf%>%
  dplyr::group_by(.data$Waterbody_Name, .data$WBID, .data$Year, .data$Lk_yr, .data$Species, .data$Strain, .data$Genotype, .data$Int.Age, .data$Clip, .data$sby_code, .data$N_rel, .data$SAR, .data$avg_rel_date)%>%
  dplyr::summarize(mean_FL = mean(.data$Length_mm, na.rm = TRUE),sd_FL = sd(.data$Length_mm, na.rm = TRUE),
                    mean_wt = mean(Weight_g, na.rm = TRUE), sd_wt = sd(Weight_g, na.rm = TRUE),
                    N = dplyr::n(),
                    NetX_FL = stats::weighted.mean(.data$Length_mm, .data$NetX, na.rm = TRUE),
                    NetX_wt = stats::weighted.mean(.data$Weight_g, .data$NetX, na.rm = TRUE),
                    NetXN = sum(NetX),
                    p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE),
                    avg_sample_date = as.Date(mean(.data$Date),format='%d%b%Y')
                  )%>%
  dplyr::ungroup()

#The only reason for this full_join() is to add in the 0 counts.
gdf = dplyr::full_join(gdf, Xnew[,c("Waterbody_Name", "WBID", "Year", "Lk_yr", "Int.Age", "Species", "Strain","Genotype", "sby_code", "Clip", "Quantity", "g_size")], 
                by = c("Waterbody_Name", "WBID", "Year", "Lk_yr", "Int.Age", "Species", "Strain","Genotype", "sby_code", "Clip", "N_rel"="Quantity", "SAR"="g_size"))%>%
                dplyr::filter(Clip != "")%>%
                dplyr::mutate(N = replace(N, is.na(N), 0), NetXN = replace(NetXN, is.na(NetXN), 0))

idf<<-idf
gdf<<-gdf

#no longer need Xnew from linkClips
rm(Xnew, envir = .GlobalEnv)


}