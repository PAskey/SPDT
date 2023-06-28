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
#' @param Spp an optional character string or character vector for BC species code (e.g. "RB" or c("KO", "EB"), etc.). This will filter data to only that species.
#' @param Contrast an optional character string describing the experimental contrast, which must be a field in the SPDTdata (e.g. "Strain", "SAR_cat", "Genotype" are the 3 possibilities now).
#' Entering a value for contrast will filter to lake years that had fish present from a co-stocking event of groups varying in your contrast variable.
#' @param Strains an optional character string or character vector describing the strain code (SPDTdata format e.g. "RB" for Rainbow Trout) for source population. This will filter to only those strains listed
#' @param Genotypes an optional character string or character vector to filter data to specific genotypes (e.g. 2n or AF3n)
#' @param Project an optional character describing the Project_Name from Assessments table in SLD.
#' @param Lk_yrs an optional character string or character vector of the Lake-years to filter the data set to. Must be in format WBID_YYYY (e.g. "01100OKAN_2020" or for multiples c("01100OKAN_2020", "01598LNTH_2018"))
#' @param Data_source a TRUE FALSE value to indicate whether to load data form the SLD, or just use data tables in the Environment.
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


SPDTdata <- function(Spp = NULL, Contrast = NULL, Strains = NULL, Genotypes = NULL, Project = NULL, Lk_yrs = NULL, Data_source = TRUE){

  if(is.null(Contrast)){stop("Must define a 'Contrast' for SPDTdata() function, see ?SPDTdata, for all data use SLD2R() instead")}
  
  if(Data_source == TRUE){linkClips()}
  
  if(!exists("Biological")|!exists("clipsum")){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}
  


#Initial filters. Keep Clip == "NONE" because experimental fish in Yellow (KO) and maybe elsewhere were non-clips.

 #The Biological Table from the SLD through linkClips() is always the same regardless of other parameters, and be used as a raw data check. 
  #Alternatively if Data_source is set to "FALSE" then analyst uses a Biological Table that is loaded in the RStudio environment, which could be from the SLD or a spreadsheet, etc.
idf = Biological  
  



if (!is.null(Spp)) {
  idf = subset(idf, Species %in% Spp)
  clipsum = subset(clipsum, Species %in% Spp)
}

if (!is.null(Strains)) {
  idf = subset(idf, Strain %in% Strains)
  clipsum = subset(clipsum, clipStrains %in% Strains)
}

if (!is.null(Genotypes)) {
  idf = subset(idf, Genotype %in% Genotypes)
  clipsum = subset(clipsum, clipGenos %in% Genotypes)
}

if (!is.null(Lk_yrs)|!is.null(Project)) {
  Pro_lk_yr = Assessments$Lk_yr[Assessments$Project_Name %in% Project]
  Lk_yrs = c(Lk_yrs,Pro_lk_yr)
  idf = subset(idf, Lk_yr %in% Lk_yrs)
  clipsum = subset(clipsum, Lk_yr %in% Lk_yrs)
} 

#Categorize release size to facilitate finding true contrasts and analyzing
#Code below create categories that increase in width as size increases.  
idf <- idf%>%
  dplyr::mutate(SAR_cat = SAR_cat(SAR),
                Season = metR::season(Date),
                Season = dplyr::recode_factor(Season, MAM = "Spring", JJA = "Summer", SON = "Fall", DJF = "Winter"),
                Dec.Age = round(Int.Age+as.numeric(Season)/4, 2)
                )


#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. 
gdf <- idf%>%
  dplyr::group_by(.data$Waterbody_Name, .data$WBID, .data$Year, .data$Lk_yr, .data$Season, .data$Capture_Method, .data$Species, .data$Strain, .data$Genotype, .data$Int.Age, .data$Dec.Age, .data$sby_code, .data$Clip, .data$N_rel, .data$Rel_ha, .data$SAR, .data$SAR_cat, .data$avg_rel_date)%>%
  dplyr::summarize( mean_FL = mean(.data$Length_mm[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    sd_FL = sd(.data$Length_mm[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    mean_wt = mean(Weight_g[.data$Outlier%in%c(0,NA)], na.rm = TRUE), 
                    sd_wt = sd(Weight_g[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
                    mean_K = mean(.data$K[.data$Outlier%in%c(0,NA)], na.rm = TRUE),
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
clipsum = dplyr::left_join(uni_events, clipsum, by = 'Lk_yr')%>%
          dplyr::mutate(Dec.Age = round(Int.Age+as.numeric(Season)/4, 2))%>%#this add dec.age to 0 counts
          dplyr::filter(!is.na(Species))#Remove cases that did not match a stocking event.
###################################################################################################################
gdf = dplyr::full_join(gdf, clipsum[,c("Waterbody_Name", "WBID", "Lk_yr", "Year", "Season", "Capture_Method", "Int.Age", "Dec.Age", "Species", "clipStrains","clipGenos", "clipsbys", "Clip", "N_rel","Rel_ha", "SAR", "cur_life_stage_code","avg_rel_date")], 
                       by = c("Waterbody_Name", "WBID", "Lk_yr", "Year", "Season", "Capture_Method", "Int.Age", "Dec.Age", "Species", "Strain"="clipStrains","Genotype"= "clipGenos", "sby_code"="clipsbys", "Clip", "N_rel","Rel_ha", "SAR"))%>%
  dplyr::filter(Lk_yr%in%idf$Lk_yr)%>%#Clip != "", Pulled this out to keep species comparisons
  dplyr::filter(dplyr::case_when(Contrast != "Species"~ Clip != "", TRUE ~ !is.na(N_rel) ))%>%#, TRUE ~ Clip %in% unique(Clip)
  dplyr::mutate(N = replace(N, is.na(N), 0), 
                NetXN = replace(NetXN, is.na(NetXN), 0),
                avg_rel_date = pmax(avg_rel_date.x, avg_rel_date.y, na.rm = TRUE),
                SAR_cat = SAR_cat(SAR))%>%
  dplyr::select(-c(avg_rel_date.x, avg_rel_date.y))
#####################################################################################################################

#Have to add in average sample data at this point, otherwise it is NA for all cases of non-clips, etc.
gdf = dplyr::left_join(gdf, uni_events, by = c("Lk_yr", "Season", "Capture_Method"))

#Sections to pull specific contrast years
if (!is.null(Contrast)) {

Contrast_possible = c("Genotype", "Strain", "SAR_cat")

controls = dplyr::setdiff(Contrast_possible, Contrast)

#Find set of experiments 'exps' comparing the contrast of interest that exist in the database
#If the contrast is species, then not worth worrying about other controls (Maybe Genotype?Add later if want) and clips
if(Contrast != "Species"){
##MAYBE USE N_DISTINCT TO CONTROL FOR FACTOR LEVELS BEING COUNTED INSTEAD OF VALUES?
exps <- gdf%>%
  dplyr::filter(!grepl(",",get(Contrast)))%>%#discount groups that included multiple levels within contrast (they are always separated by commas)
  dplyr::group_by(Lk_yr, Int.Age, !!!rlang::syms(controls))%>%
  dplyr::summarize(Ncontrasts = length(unique(na.omit(get(Contrast)))), Nclips = length(unique(na.omit(Clip))))%>%
  dplyr::filter(Nclips>=Ncontrasts&Ncontrasts>1)%>%
  droplevels()
}else{
  Contrast_possible = c("Species", "Genotype")#Could add genotype later
  exps <- gdf%>%
    dplyr::filter(!grepl(",",get(Contrast)))%>%#discount groups that included multiple levels within contrast (they are always separated by commas)
    dplyr::group_by(Lk_yr, Int.Age)%>%
    dplyr::summarize(Ncontrasts = length(unique(na.omit(get(Contrast)))))%>%
    dplyr::filter(Ncontrasts>1)%>%
    droplevels() 
}

idf<-subset(idf, Lk_yr%in%exps$Lk_yr)%>%dplyr::filter(!grepl(",",get(Contrast)))
gdf<-subset(gdf, Lk_yr%in%exps$Lk_yr)%>%dplyr::filter(!grepl(",",get(Contrast)))

#Let's try an effort table for standard gillnet data only for now. Data looks terrible quality.
#Assume 7 panels and overnight when not recorded but SGN indicated
Nets = Nets%>%dplyr::filter(sample_design_code == "SGN")%>%
  tidyr::replace_na(list(no_net_panels = 7, overnight_yn= "Y"))

#Capitalization
Nets$overnight_yn = stringr::str_to_upper(Nets$overnight_yn)

Net_effort = Nets%>%
              dplyr::filter(Lk_yr %in% gdf$Lk_yr, method == "Gillnet", sample_design_code == "SGN")%>%#This should be changed to "GN" to match other tables
              dplyr::group_by(Waterbody_Name, Lk_yr, Start_Date, net_id, no_net_panels)%>%
              dplyr::summarize(soak_hrs = mean(soak_hrs))#corrects for different soak times for same net with different species

#Have to count net panels and divide by 7 because standard nets were split into multiple sections in some cases.
SGN_E = Net_effort%>%
            dplyr::group_by(Waterbody_Name, Lk_yr)%>%
            dplyr::summarize(Net_nights = sum(no_net_panels)/7, Net_hours = as.numeric(crossprod(no_net_panels,soak_hrs)/7))%>%
            dplyr::ungroup()

#Create a wide format data set for comparing relative catch. May want to add sby_code back in as grouper? THis messes up species comparisons.
#First only use groups that are recruited to gillnets (>150mm).
predf = gdf%>%
  #dplyr::filter(mean_FL>150)%>%
  dplyr::group_by(Lk_yr, Waterbody_Name,Year, Season, Capture_Method, Int.Age, !!!rlang::syms(Contrast_possible))%>%#, sby_code
  dplyr::summarize(groups = dplyr::n(), N = sum(N), xN = sum(NetXN), Nr = sum(N_rel))%>%
  dplyr::filter(!grepl(",",get(Contrast)))%>%#remove group that included multiple levels within contrast
  dplyr::arrange(desc(get(Contrast)))%>%
  dplyr::ungroup()

  
#The distint catgories found in the contrast
cats = predf%>%
  dplyr::pull(get(Contrast))%>%unique()%>%sort(decreasing = TRUE)

#SAR_cat we want in numeric order. For strain or genotype we want 2N and BW at back end as they are the "base case" higher smple size and survival.
if(Contrast == "SAR_cat"){
  cats = sort(cats)
  predf = dplyr::arrange(predf, get(Contrast))
  }

#i = 1
#j = 2
#calculations so that spreading an renaming columns works even when grouping columns change(i.e. for species groupings)
df = NULL
spread = c("xN", "Nr", "N")
Lspread = length(spread)
maxcols = as.integer(ncol(predf)-(Lspread-1)+(Lspread*2-1))#In the loop below this will be the max with of spread dataframe

for(i in 1:(length(cats)-1)){
  for(j in (i+1):length(cats)){
    Cons = c(i,j)
    Con = paste0(cats[i],"vs",cats[j])
    dfnew = predf%>%
      dplyr::filter(get(Contrast) %in% cats[c(i,j)])%>%#Can switch to filter by delta
      dplyr::mutate(Comparison = Con)%>%
      tidyr::pivot_wider(names_from = {{Contrast}}, values_from = tidyselect::all_of(spread))%>%#, names_sort = TRUE
      dplyr::rename(a_xN = maxcols-5, b_xN = maxcols-4, a_Nr=maxcols-3, b_Nr = maxcols-2, a_N = maxcols-1, b_N = maxcols)%>%
      dplyr::rowwise()%>%
      dplyr::filter(0<(sum(a_xN, b_xN)), !is.na(sum(a_xN, b_xN, a_Nr, b_Nr)))%>%
      dplyr::mutate(Recap_Ratio = a_xN/(b_xN), Release_Ratio = a_Nr/(b_Nr), a = cats[i], b = cats[j], N = sum(a_N, b_N))%>%
      dplyr::ungroup()
    df = rbind(df, dfnew)
    
  }
}

if(nrow(df)>0){
wide_df = df%>%
  dplyr::rowwise()%>%
  dplyr::mutate(surv_diff=Recap_Ratio/Release_Ratio,
                LCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), a_Nr/(a_Nr+b_Nr))$conf.int[1], 
                UCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), a_Nr/(a_Nr+b_Nr))$conf.int[2],
                Sig_p = 0.05>stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0),a_Nr/(a_Nr+b_Nr))$p.value)%>%
  dplyr::mutate(LCI = LCI/(1-LCI), 
                UCI = UCI/(1-UCI))

#Add in average relative survival (log odds) for each comparison.
wide_df = wide_df%>%
  dplyr::group_by(Comparison, Int.Age)%>%
  dplyr::mutate(avg_surv = exp(mean(log(surv_diff))))%>%
  dplyr::ungroup()

#Put into global environment. These only appear if Contrast is not NULL.
wide_df<<-wide_df
}
controls<<-controls

}

#Removes cohorts years where nothing is observed> STILL WANT THIS?
#This coding should be used earlier to remove strain experiments that appear with size experiments
idf$Lk_yr_age = paste0(idf$Lk_yr, "_", idf$Int.Age)
gdf$Lk_yr_age = paste0(gdf$Lk_yr, "_", gdf$Int.Age)

gdf = gdf%>%
  dplyr::filter(Lk_yr_age %in% idf$Lk_yr_age)

#Use gillnet effort data to get CPUE when possible.
gdf = dplyr::left_join(gdf,SGN_E[,c('Lk_yr','Net_nights')], by = "Lk_yr")%>%
      dplyr::mutate(CPUE = N/Net_nights)

#Put data frames into the global environment
idf<<-idf
gdf<<-gdf
SGN_E<<-SGN_E


#Add function parameters to the global environment
Spp<<-Spp
Strains<<-Strains
Contrast<<-Contrast


if(!exists("wide_df")){message("WARNING: There were no controlled co-stocking events in the database for the specified contrast. Therefore, no wide_df exists, no survival plot can be constructed.")}

invisible(gc())
}
