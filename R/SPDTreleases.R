#' A function to load, clean and standardize release data for SPDT analysis. 
#' Function only usable by staff who have a connection to SLD.
#'
#'
#' This is a non-exported funciton to simplify code using the releases table to understand all possible stocked groups that could be in a lake at any given time.
#' In cases where clips are unique, then fields for age, strain, genotype are updated.
#' This function was previously withing lines of exported functions: SLD2R() and linkClips().But to make code more tractable, it has been put into it's own function.
#' Ultimately, as upload filters and cleaning are improved in the main database, this function could become obsolete.
#'
#' @title SPDTreleases
#' @name SPDTreleases
#' @keywords SPDT; releases; stocked; clips
#' 
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' SPDTreleases()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
SPDTreleases <- function(){

  #Check data has been loaded.
  if(!exists("Biological")|!exists("Releases")){stop("Need to start with a data load to make SLD releases and biological available to this function")}
  
  #Step 1. Clean and standardize release data

  #INclude age at release when it is a straight forward calculation
  #In "case, when" statement, later statements do not replace earlier, so go from specific to general. Basically start with the exceptions
  #Fix pre 2012 Brood year issue for FV
  Releases <- Releases%>%dplyr::mutate(Releases, sby_code = dplyr::if_else(.data$Strain == "FV" & .data$sby_code <=2012 & so_origin_code =="H", .data$sby_code-1,.data$sby_code))
  
  Releases <- Releases%>%dplyr::mutate(
    Int.Age = dplyr::case_when(
      #there are a few specific cases where we know age for sure just based on the age description, but only a few (some are not accurate)
      .data$g_size > 0 & .data$g_size < 2 ~ 0L,
      .data$sby_code < 1 & .data$cur_life_stage_code =="FG" ~0L,
      .data$cur_life_stage_code %in% c("EG", "EE", "FF", "FR")& .data$g_size <5 ~ 0L,
      .data$sby_code < 1 & .data$cur_life_stage_code %in% c("YE", "YE+", "FG", "FFG")& .data$g_size >5& .data$g_size <500 ~ 1L,#Only trust YE designation if don't have sby_code
      #Now with Fraser Valleys it gets really confusing. This is NOT correct 100% of the time.
      .data$stock_strain_loc_name == "FRASER VALLEY" & .data$cur_life_stage_code %in% c("EG", "EE", "FF", "FR")&g_size <10 ~ 0L,
      TRUE ~ sby2age(.data$Species, .data$sby_code, .data$Year),
      )
  )

  Releases = Releases%>%
    dplyr::mutate(SprFall = dplyr::if_else(lubridate::month(rel_Date)>8,"Fall","Spr"),
                  Clip = dplyr::na_if(Clip,c("")))
  
  group_cols <- c("Region", "Waterbody_Name","WBID", "Species", "Year","sby_code","Int.Age", "Strain","Genotype","Clip","cur_life_stage_code", "SprFall")
  
  #First just group together cases of multiple relids for the same group type of fish to the same lake and time.Pretty slow function, so reduced to essential summary variables

  Releases <- Releases%>%
    #group by Spr or Fall in case 2 trips on different days of same fish
    dplyr::group_by(!!!rlang::syms(c(group_cols)))%>%
    dplyr::summarise(#rel_id = paste0(unique(.data$rel_id),collapse = ","), 
                     #rel_temp = mean(.data$rel_waterbody_temp_c, na.rm = TRUE),
                     #rel_ph = mean(.data$rel_waterbody_ph, na.rm = TRUE),
                     rel_Date = mean(.data$rel_Date, na.rm = TRUE), 
                     g_size = sum(Quantity*g_size)/sum(Quantity),
                     Quantity = sum(Quantity)
                     )%>%
   dplyr::ungroup()

  
  #Add in some per ha calculations. Faster to keep these separate from summarize
  ##THIS TABLE IS ADDED TO ENVIRO_______________________________________________
  Releases = Releases%>%
    dplyr::inner_join(dplyr::select(Lakes,WBID, Area),by = "WBID")%>%
    dplyr::mutate(SAR_cat = SAR_cat(g_size),
                  Quantity_ha = round(Quantity/Area,0), 
                  Biom_ha = round(Quantity_ha*g_size/1000,1))%>%
    dplyr::ungroup()
  
  ##Add in a variable to document how many years the current stocking prescription has been stable (same as previous years)
#Group based on a per lake-year stocking prescription across, species, strains, life-stages. Strain is an unknown impact and variable for species other than RB, so disregard for other species 
  Stable = Releases%>%
    dplyr::mutate(Strain = dplyr::if_else(Species == "RB", Strain, Species))%>%#Only keep unique strains for RB.
    dplyr::group_by(WBID, Year)%>%
    dplyr::summarise(
      Strain_rel = paste(sort(unique(Strain)),collapse = ','),
      Geno_rel = paste(sort(unique(Genotype)),collapse = ','),
      Total_Quantity = round(sum(Quantity),-2))%>%
    dplyr::ungroup() 
  
  
  Stable = transform(Stable, Stable_yrs = stats::ave(Total_Quantity, data.table::rleid(WBID, Strain_rel, Geno_rel, Total_Quantity), FUN = seq_along))%>%
    dplyr::select(WBID, Year, Stable_yrs)
  
  #If insert Stable years here then will be different for each release event within sample year
  #Releases = dplyr::left_join(Releases, Stable, by = c('WBID','Year')) 
  
  
  #Step 2. Find the list of sampled lake-years in the Biological table that could be linked to releases.
  
  
  endage = 6# Set the max age you expect to retrieve stocked fish in a lake. This value is overwritten if fish were aged at an older age from a given lake at any point in time. So this is more like an average maximum age sampled.
  
  #The Sampled_only variable filters lakes and releases to cases where a lake has been assessed (data exists in the Biological Table). However, some cases in the Biological table are not true in-lake sampling events.We will remove these from here and all further analyses. If they are wanted use SLD2R() only.
  

  maxxages = Biological%>%
    #dplyr::filter(!Capture_Method %in% c("UNK","Pre-Release Len_Wt", "HATCH","LAB"))%>%
    #anti_join(data.frame(Capture_Method = c("UNK", "Pre-Release Len_Wt", "HATCH", "LAB")), by = "Capture_Method")%>%
    dplyr::semi_join(Releases, by = c("WBID", "Species"))%>%
    dplyr::group_by(WBID, Year, Lk_yr, Species) %>%
    dplyr::summarise(max = max(endage,max(Int.Age, na.rm = T)), .groups = "drop")%>%
    dplyr::group_by(WBID,Species) %>%
    dplyr::mutate(max = max(max, na.rm = T))%>%
    suppressWarnings()


  #Step 3. From those sampled lake years find the sequence of previous stocking years to that lake that could have fish in the lake at the time of stocking. Search back to oldest age observed or the endage.
      
  Sampled = maxxages%>%
    dplyr::group_by(WBID, Lk_yr, Year,Species) %>%
    dplyr::mutate(YearSeq = purrr::map(Year, ~seq((. - max), .))) %>%
    tidyr::unnest(YearSeq) %>%
    dplyr::ungroup() %>%
    dplyr::select(WBID, Species, Sample_Year = Year, Year = YearSeq)
    #dplyr::left_join(.,Stable, by = c('WBID','Year'))%>%
    #dplyr::select(WBID, Species, Sample_Year = Year, Stable_sample = Stable_yrs, Year = YearSeq)%>%
    #tidyr::replace_na(list(Stable_sample = 0))
  
 
  #Step 4. Go back to releases and filter down to the matching lake-year combinations
  
  
   #This is a much faster way to filter than using interaction()
  ##THIS TABLE IS ADDED TO ENVIRO_______________________________________________
  Rel_sampled = Releases %>%
    dplyr::inner_join(Sampled, by = c("WBID", "Species","Year"), relationship = "many-to-many")%>%
    dplyr::mutate(Rel_Age = Int.Age, Int.Age = (Sample_Year-Year)+Rel_Age)
  
  #Insert Stable years as a variable to releases
  #Rel_sampled = dplyr::left_join(Rel_sampled, Stable, by = c('WBID','Sample_Year'='Year')) 
  
  
  #Step 5. Link individual fish back to their stocking records. If they are aged, the we can narrow down to release event(s) in one year to one lake.
  
  
  aged_in_lake = Rel_sampled%>%
    dplyr::group_by(!!!rlang::syms(c("Sample_Year",setdiff(group_cols, c("Year","sby_code","cur_life_stage_code", "SprFall","Strain","Genotype")))))%>%
    dplyr::summarise(
      sby_rel = paste0((unique(sby_code)),collapse = ','),
      Strain_rel = paste0((unique(Strain)),collapse = ','),
      Geno_rel = paste0((unique(Genotype)),collapse = ','),
      AF = all(grepl("F",Genotype)),
      Sterile = all(grepl("3",Genotype)),
      LS_rel = paste0((unique(cur_life_stage_code)),collapse = ','),
      wt_rel = round(sum(.data$g_size*.data$Quantity)/sum(.data$Quantity),1),
      N_ha_rel = sum(Quantity_ha),
      avg_rel_date = round(mean(rel_Date), unit = "day"))%>%
    dplyr::mutate(Poss_Age = Int.Age)%>%
    dplyr::ungroup()
  
  #If they are not aged, then in most cases there are multiple stocking events to a given lake that could have resulted in the sampled fish. Unless it had a uniquely identifiable clip.

  #The second block builds off of the age-in-lake instead of Rel_sampled, so that the binning of N_ha_rel across strains is complete and not confounded as much across years..
  unaged_in_lake = aged_in_lake%>%
    dplyr::group_by(!!!rlang::syms(c("Sample_Year",setdiff(group_cols, c("Year","sby_code","Int.Age","cur_life_stage_code", "SprFall", "Strain","Genotype")))))%>%
    dplyr::summarise(
      sby_rel = paste0(unique(sby_rel),collapse = ','),
      Strain_rel = paste0(unique(unlist(strsplit(Strain_rel, ","))),collapse = ','),
      Geno_rel = paste0(unique(unlist(strsplit(Geno_rel, ","))),collapse = ','),
      AF = all(AF),
      Sterile = all(Sterile),
      LS_rel = paste0(unique(unlist(strsplit(LS_rel, ","))),collapse = ','),
      wt_rel = round(dplyr::if_else(dplyr::n()==1|(sd(wt_rel)/mean(wt_rel))<0.5, mean(wt_rel),NA),1),
      N_ha_rel = round(dplyr::if_else(dplyr::n()==1|(sd(N_ha_rel)/mean(N_ha_rel))<0.15,mean(N_ha_rel),NA),0),
      avg_rel_date = round(mean(avg_rel_date), unit = "day"),
      Poss_Age = paste0(sort(unique(Int.Age)),collapse = ',')
    )%>%
    dplyr::mutate(Int.Age = NA)%>%#This is for biological records with Int.Age == NA
    #dplyr::mutate(Int.Age = as.integer(dplyr::if_else(grepl(",", Poss_Age),NA,Poss_Age)))%>%
    dplyr::ungroup() 
  
  
  
  
##THIS TABLE IS ADDED TO ENVIRO_______________________________________________  
Link_rel = rbind(aged_in_lake, unaged_in_lake)%>%
  dplyr::rename(Year = Sample_Year)%>%
  dplyr::left_join(.,Stable, by = c('WBID','Year'))%>%
  dplyr::mutate(Lk_yr = paste0(WBID,"_",Year))%>%
  tidyr::replace_na(.,list(Stable_yrs = 0))%>%
  dplyr::select(-c("Region","Waterbody_Name"))

Releases <<-Releases#All releases in database cleaned and standardized
Rel_sampled <<-Rel_sampled#Releases associated with sampled lakes in database cleaned and standardized
Link_rel<<-Link_rel#Releases summarized into the in-lake observable and un-observable release characteristics, so that each individual fish can potentially be tied to a specific or group of releases into that lake


}
