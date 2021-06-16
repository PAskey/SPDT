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
#'Several new columns are present. Anything prefixed by "NetX" means that variable has been expanded to account for gillnet selectivity.
#'SAR if for size-at-release in grams, and SAR_cat has categorized those release sizes.
#'The code creating categories for the SAR_cat has bigger categories as the size increases and is as follows:
#'
#'dplyr::mutate(SAR_cat = dplyr::case_when(SAR<=6 ~ plyr::round_any(SAR,1),
#'                                         SAR>6&SAR<=14.5 ~ plyr::round_any(SAR,2),
#'                                         SAR>14.5&SAR<=27.5 ~ plyr::round_any(SAR,5),
#'                                         SAR>27.5&SAR<65 ~ plyr::round_any(SAR,10),
#'                                         TRUE ~ plyr::round_any(SAR, 25))
#' You could create our own categorical system and column if needed
#'                                         
#'
#'
#' @title SPDTdata
#' @name SPDTdata
#' @keywords SPDT
#' @export
#' @param Spp a character string or character vector for BC species code. This will filter data to only that species.
#' @param Strain a character string or character vector describing the strain code (SPDTdata format) for source population. This will filter to only those strains listed
#' @param Contrast a character string describing the experimental contrast, which must be a field in the SPDTdata (e.g. "Strain", "SAR_cat", "Genotype" are the 3 possibilities now).
#' Entering a value for contrast will filter to lake years that had fish present from a co-stocking event of groups varying in your contrast variable.
#' @examples
#' #Must be connected to VPN if working remotely
#' 
#' #Download all data with clipped fish
#' SPDTdata()
#' 
#' #Download only KO data
#' SPDTdata(Spp = "KO")
#' 
#' #Download any data from RB, of either HF, CL or BW strain that were stocked as a strain comparison
#' SPDTdata(Spp = "RB", Strains = c("HF", "CL", "BW"), Contrast = "Strain")
#' 
#' #Download all data from lake years that had a size-at-release comparison for RB
#' SPDTdata(Spp = "RB", Contrast = "SAR_cat")
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTdata <- function(Spp = NULL, Strains = NULL, Contrast = NULL){

linkClips()

#Initial filters
idf<-subset(Biological,!is.na(Clip)&!is.null(Clip)&Clip != "NONE")
clipsum <- subset(clipsum, !is.na(Clip)&!is.null(Clip)&Clip != "NONE")

if (!is.null(Spp)) {
  idf = subset(idf, Species %in% Spp)
  clipsum = subset(clipsum, Species %in% Spp)
}

if (!is.null(Strains)) {
  idf = subset(idf, Strain %in% Strains)
  clipsum = subset(clipsum, clipStrains %in% Strains)
}




#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. 
gdf <- idf%>%
  dplyr::group_by(.data$Waterbody_Name, .data$WBID, .data$Year, .data$Lk_yr, .data$Capture_Method, .data$Species, .data$Strain, .data$Genotype, .data$Int.Age, .data$Clip, .data$sby_code, .data$N_rel, .data$SAR, .data$avg_rel_date)%>%
  dplyr::summarize(mean_FL = mean(.data$Length_mm, na.rm = TRUE),sd_FL = sd(.data$Length_mm, na.rm = TRUE),
                    mean_wt = mean(Weight_g, na.rm = TRUE), sd_wt = sd(Weight_g, na.rm = TRUE),
                    N = dplyr::n(),
                    NetX_FL = stats::weighted.mean(.data$Length_mm, .data$NetX, na.rm = TRUE),
                    NetX_wt = stats::weighted.mean(.data$Weight_g, .data$NetX, na.rm = TRUE),
                    NetXN = sum(NetX),
                    p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE)
                  )%>%
  dplyr::ungroup()


#The only reason for this full_join() is to add in the 0 counts.
#Using clipsum instead of Xnew should keep release date and sample date and better cross reference when multiple release ids for one release group.
clipsum<-clipsum%>%
  dplyr::filter(n_sby == 1)%>%
  dplyr::mutate(clipsbys = as.integer(clipsbys))

gdf = dplyr::full_join(gdf, clipsum[,c("Waterbody_Name", "WBID", "Lk_yr", "Year","Int.Age", "Species", "clipStrains","clipGenos", "clipsbys", "Clip", "N_rel", "SAR", "avg_rel_date")], 
                       by = c("Waterbody_Name", "WBID", "Lk_yr", "Year","Int.Age", "Species", "Strain"="clipStrains","Genotype"= "clipGenos", "sby_code"="clipsbys", "Clip", "N_rel", "SAR"))%>%
  dplyr::filter(Clip != "", Lk_yr%in%idf$Lk_yr)%>%
  dplyr::mutate(N = replace(N, is.na(N), 0), 
                NetXN = replace(NetXN, is.na(NetXN), 0),
                avg_rel_date = pmax(avg_rel_date.x, avg_rel_date.y, na.rm = TRUE))%>%
  dplyr::select(-c(avg_rel_date.x, avg_rel_date.y))

#A lookup to add in average sampling date for each lake year.
quick_Lu <-idf%>%dplyr::group_by(Lk_yr)%>%
  dplyr::summarize(avg_sample_date = as.Date(mean(.data$Date),format='%d%b%Y'))%>%
  dplyr::ungroup()

gdf = dplyr::left_join(gdf, quick_Lu, by = c("Lk_yr"))



idf <- idf%>%
  dplyr::mutate(SAR_cat = dplyr::case_when(SAR<=6 ~ plyr::round_any(SAR,1),
                                SAR>6&SAR<=14.5 ~ plyr::round_any(SAR,2),
                                SAR>14.5&SAR<=27.5 ~ plyr::round_any(SAR,5),
                                SAR>27.5&SAR<65 ~ plyr::round_any(SAR,10),
                                TRUE ~ plyr::round_any(SAR, 25)
                                )
         )

gdf <- gdf%>%
  dplyr::mutate(SAR_cat = dplyr::case_when(SAR<=6 ~ plyr::round_any(SAR,1),
                                           SAR>6&SAR<=14.5 ~ plyr::round_any(SAR,2),
                                           SAR>14.5&SAR<=27.5 ~ plyr::round_any(SAR,5),
                                           SAR>27.5&SAR<65 ~ plyr::round_any(SAR,10),
                                           TRUE ~ plyr::round_any(SAR, 25)
  )
  )

#Sections to pull specific cnotrast years
if (!is.null(Contrast)) {

Contrast_possible = c("Genotype", "SAR_cat", "Strain")

controls = setdiff(Contrast_possible, Contrast)

#dplyr::group_by_at(c(4:7,10,11))%>%
exps <- gdf%>%
  dplyr::group_by(Lk_yr, Int.Age, !!!rlang::syms(controls))%>%
  dplyr::summarize(Ncontrasts = length(unique(na.omit(get(Contrast)))), Nclips = length(unique(na.omit(Clip))))%>%
  dplyr::filter(Nclips>=Ncontrasts&Ncontrasts>1)%>%
  droplevels()

idf<-subset(idf, Lk_yr%in%exps$Lk_yr)
gdf<-subset(gdf, Lk_yr%in%exps$Lk_yr)
}


idf<<-idf
gdf<<-gdf


}