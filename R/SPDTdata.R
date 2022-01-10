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

#Initial filters. Keep CLip == "NONE" because experimental fish in Yellow (KO) and maybe elsewhere were non-clips.
  #Not sure if this will cause an issue
#idf<-subset(Biological,!is.na(Clip)&!is.null(Clip)&Clip !="NOREC")#&Clip != "NONE"
idf = Biological
#clipsum <- subset(clipsum, !is.na(Clip)&!is.null(Clip)&Clip !="NOREC")#&Clip != "NONE"


if (!is.null(Spp)) {
  idf = subset(idf, Species %in% Spp)
  clipsum = subset(clipsum, Species %in% Spp)
}

if (!is.null(Strains)) {
  idf = subset(idf, Strain %in% Strains)
  clipsum = subset(clipsum, clipStrains %in% Strains)
}

#Categorize release size to facilitate finding true contrasts and analyzing
#Code below create categories that increase in width as size increases.  
idf <- idf%>%
  dplyr::mutate(SAR_cat = SAR_cat(SAR),
                Season = metR::season(Date),
                Season = dplyr::recode_factor(Season, MAM = "Spring", JJA = "Summer", SON = "Fall", DJF = "Winter"))


#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. 
gdf <- idf%>%
  dplyr::group_by(.data$Waterbody_Name, .data$WBID, .data$Year, .data$Lk_yr, .data$Season,.data$Capture_Method, .data$Species, .data$Strain, .data$Genotype, .data$Int.Age, .data$Clip, .data$sby_code, .data$N_rel, .data$SAR, .data$SAR_cat, .data$avg_rel_date)%>%
  dplyr::summarize( mean_FL = mean(.data$Length_mm[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    sd_FL = sd(.data$Length_mm[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    mean_wt = mean(Weight_g[.data$Outlier%in%c(0,NA)], na.rm = TRUE), 
                    sd_wt = sd(Weight_g[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    N = dplyr::n(),
                    N_outliers = sum(.data$Outlier, na.rm = TRUE),
                    NetX_FL = stats::weighted.mean(.data$Length_mm[.data$Outlier%in%c(0,NA)], .data$NetX[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    NetX_wt = stats::weighted.mean(.data$Weight_g[.data$Outlier%in%c(0,NA)], .data$NetX[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    NetXN = sum(NetX),
                    p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE)
                  )%>%
  dplyr::ungroup()


#The only reason for this next section that merges clipsum
#Is to track 0 counts for specific clips during sampling.


#Using clipsum instead of Xnew should keep release date and sample date and better cross reference when multiple release ids for one release group.
clipsum<-clipsum%>%
  dplyr::filter(n_sby == 1)%>%
  dplyr::mutate(clipsbys = as.integer(clipsbys))

#Vector of unique sampling events by lake-year, season and method 
#Can't add avg_sampling date within gdf, because will be different for each strain, age, etc.
uni_events = idf%>%
  #filter out indoor capture methods like HATCH, etc. and FFSBC "lakes"
  dplyr::filter(!(Capture_Method%in%c('HATCH', 'LAB','UNK')))%>%#to filter out FFSBC lakes, !grepl("FFSBC",WBID)
  dplyr::group_by(Lk_yr, Season, Capture_Method)%>%
  dplyr::summarize(avg_sample_date = mean(.data$Date,na.rm = TRUE))%>%
  dplyr::ungroup()

#MAYBE CHANGE THIS TO A RIGHT JOIN?
#Join this with clipsum, so all releases that should appear in a sampling event are tracked.
clipsum = dplyr::left_join(uni_events, clipsum, by = 'Lk_yr')

gdf = dplyr::full_join(gdf, clipsum[,c("Waterbody_Name", "WBID", "Lk_yr", "Year", "Season", "Capture_Method", "Int.Age", "Species", "clipStrains","clipGenos", "clipsbys", "Clip", "N_rel", "SAR", "cur_life_stage_code","avg_rel_date")], 
                       by = c("Waterbody_Name", "WBID", "Lk_yr", "Year", "Season", "Capture_Method", "Int.Age", "Species", "Strain"="clipStrains","Genotype"= "clipGenos", "sby_code"="clipsbys", "Clip", "N_rel", "SAR"))%>%
  dplyr::filter(Clip != "", Lk_yr%in%idf$Lk_yr)%>%
  dplyr::mutate(N = replace(N, is.na(N), 0), 
                NetXN = replace(NetXN, is.na(NetXN), 0),
                avg_rel_date = pmax(avg_rel_date.x, avg_rel_date.y, na.rm = TRUE))%>%
  dplyr::select(-c(avg_rel_date.x, avg_rel_date.y))

#A lookup to add in average sampling date for each lake year and Capture method.
#quick_Lu <-idf%>%dplyr::group_by(Lk_yr, Capture_Method)%>%
#  dplyr::summarize(avg_sample_date = as.POSIXct(mean(.data$Date),format='%d%b%Y'))%>%
#  dplyr::ungroup()

#gdf = dplyr::left_join(gdf, quick_Lu, by = c("Lk_yr", "Capture_Method"))

#Have to add in average sample data at this point, otherwise it is NA for all cases of non-clips, etc.
gdf = dplyr::left_join(gdf, uni_events, by = c("Lk_yr", "Season", "Capture_Method"))

#Another lookup to only keep 0 catch if a comparison group was captured
#This was done in linkclips but had gdff instead fo gdf by accident
#quick_Lu <-idf%>%dplyr::group_by(WBID, Year, sby_code)%>%dplyr::summarize(Lk_yr_sby = paste(WBID, Year, sby_code, sep = "_"))
#gdf <- gdf%>%dplyr::mutate(Lk_yr_sby = paste(WBID, Year, sby_code, sep = "_"))
##NEED TO FINISH SECTION HERE

#Sections to pull specific contrast years
if (!is.null(Contrast)) {

Contrast_possible = c("Genotype", "Strain", "SAR_cat")

controls = dplyr::setdiff(Contrast_possible, Contrast)

##MAYBE USE N_DISTINCT TO CONTROL FOR FACTOR LEVELS BEING COUNTED INSTEAD OF VALUES?
exps <- gdf%>%
  dplyr::group_by(Lk_yr, Int.Age, !!!rlang::syms(controls))%>%
  dplyr::summarize(Ncontrasts = length(unique(na.omit(get(Contrast)))), Nclips = length(unique(na.omit(Clip))))%>%
  dplyr::filter(Nclips>=Ncontrasts&Ncontrasts>1)%>%
  droplevels()

idf<-subset(idf, Lk_yr%in%exps$Lk_yr)
gdf<-subset(gdf, Lk_yr%in%exps$Lk_yr)

#Create a wide format data set for comparing relative catch
#First only use groups that are recruited to gillnets (>150mm).
predf = gdf%>%
  dplyr::filter(mean_FL>150)%>%
  dplyr::group_by(Waterbody_Name, Lk_yr, sby_code, Int.Age, !!!rlang::syms(Contrast_possible))%>%
  dplyr::summarize(groups = dplyr::n(), N = sum(N), xN = sum(NetXN), Nr = sum(N_rel))%>%
  dplyr::filter(!grepl(",",get(Contrast)))%>%#remove group that included multipe levels within contrast
  dplyr::arrange(desc(get(Contrast)))%>%
  dplyr::ungroup()

  

cats = predf%>%
  dplyr::pull(get(Contrast))%>%unique()%>%sort(decreasing = TRUE)

#SAR_cat we want in numeric order. For strain or genotype we want 2N and BW at back end.
if(Contrast == "SAR_cat"){
  cats = sort(cats)
  predf = dplyr::arrange(predf, get(Contrast))
  }

#i = 1
#j = 3
df = NULL

for(i in 1:(length(cats)-1)){
  for(j in (i+1):length(cats)){
    Cons = c(i,j)
    Con = paste0(cats[i],"vs",cats[j])
    dfnew = predf%>%
      dplyr::filter(get(Contrast) %in% cats[c(i,j)])%>%#Can switch to filter by delta
      dplyr::mutate(Comparison = Con)%>%
      tidyr::pivot_wider(names_from = {{Contrast}}, values_from = c(xN, Nr, N))%>%#, names_sort = TRUE
      dplyr::rename(a_xN = 9, b_xN = 10, a_Nr=11, b_Nr = 12, a_N = 13, b_N = 14)%>%
      dplyr::rowwise()%>%
      dplyr::filter(!is.na(sum(a_xN, b_xN, a_Nr, b_Nr)))%>%
      dplyr::mutate(Recap_Ratio = a_xN/(b_xN), Release_Ratio = a_Nr/(b_Nr), a = cats[i], b = cats[j], N = sum(a_N, b_N))%>%
      dplyr::ungroup()
    df = rbind(df, dfnew)
    
  }
}

wide_df = df%>%
  dplyr::rowwise()%>%
  dplyr::mutate(surv_diff=Recap_Ratio/Release_Ratio,
                LCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), a_Nr/(a_Nr+b_Nr))$conf.int[1], 
                UCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), a_Nr/(a_Nr+b_Nr))$conf.int[2],
                Sig_p = 0.05>stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0),a_Nr/(a_Nr+b_Nr))$p.value)%>%
  dplyr::mutate(LCI = LCI/(1-LCI), 
                UCI = UCI/(1-UCI))

#Add in average relative survival (log odds) for each comparison.
wide_df = wide_df%>%group_by(Comparison)%>%mutate(avg_surv = exp(mean(log(surv_diff))))%>%ungroup()

#Put into global environment. These only appear if Contrast is not NULL.
wide_df<<-wide_df
controls<<-controls

}

#Remove cohorts years where nothing is observed
#This coding should be used earlier to remove strain experiments that appear with size experiments
idf$Lk_yr_age = paste0(idf$Lk_yr, "_", idf$Int.Age)
gdf$Lk_yr_age = paste0(gdf$Lk_yr, "_", gdf$Int.Age)
gdf = gdf%>%
  dplyr::filter(Lk_yr_age %in% idf$Lk_yr_age)

#Put data frames into the global environment
idf<<-idf
gdf<<-gdf


#Add function parameters to the global environment
Spp<<-Spp
Strains<<-Strains
Contrast<<-Contrast


gc()
}
