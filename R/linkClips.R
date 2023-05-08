#' A function to load, clean and standardize SLD data for SPDT analysis. 
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#' This is the second data filtering and cleaning process after SLD2R() to reduce down to biological data that can be tied to releases. 
#' Takes SLD2R() data and uses clip information to tie fish back to stocking event, age, strain, genotype where possible.
#' Make sure your VPN is running, so that the database can be accessed by the function.
#' The Biological record count remains the same, but includes information about the potential stocking event(s) tied to each fish.
#' In cases where clips are unique, then fields for age, strain, genotype are updated if black. Otherwise a list of possibilities can be found
#' in "clipAges, clipStrains, clipGenos. 
#' All other data tables only include data that can be linked to the Biological data, either same Assessment_Key, or rel_id.
#' Any data Tables you have open as Assessments, Nets, Lakes, Biological, Releases will be replaced with versions from this function.
#' Lookup tables for integer ages and strain codes are included as part of package and can by called as Ages, Strain_code_LU
#' Ultimately, as upload filters and cleaning are improved in the main database, this function will become obsolete.
#'
#' @title linkClips
#' @name linkClips
#' @keywords SPDT; clips
#' @export
#' @param Sampled_only a logical TRUE/FALSE indicating whether to reduce data tables to records associated with a sampling event with Biological data. 
#' If true (default) then Assessments, Nets, Lakes and Releases tables are all reduced to Lake-years that can cross reference to Biological records.
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' linkClips()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
linkClips <- function(Sampled_only = TRUE){

SLD2R()
  


#Create a grouping variable for lake and brood year to evaluate co-stocking and linkages to Biological data. 
#Add age when released to facilitate linking to ages within Biological data
spring_spwn = as.character(expression(ACT, BS, CRS, CT, GR, MG, RB, ST, TR, WCT, WP, WSG))#expression adds quotations to each element
fall_spwn = as.character(expression(AS, BL, BT, DV, EB, GB, KO, LT, LW))

#INclude age at release when it is a straight forward calculation
#In "case, when" statement, later statements do not replace earlier, so go from specific to general. Basically start with the exceptions
Releases <- Releases%>%dplyr::mutate(
                              Int.Age = dplyr::case_when(
                                #there are a few specific cases where we know age for sure just based on the age description, but only a few (some are not accurate)
                                .data$g_size > 0 & .data$g_size < 2 ~ 0L,
                                .data$cur_life_stage_code %in% c("EG", "EE", "FF", "FR")& .data$g_size <5 ~ 0L,
                                .data$sby_code == 0 & .data$cur_life_stage_code %in% c("YE", "YE+", "FG", "FFG")& .data$g_size >5& .data$g_size <500 ~ 1L,#Only trust YE designation if don't have sby_code
                                #Now with Fraser Valleys it gets really confusing. This is NOT correct 100% of the time.
                                .data$stock_strain_loc_name == "FRASER VALLEY" & .data$cur_life_stage_code %in% c("EG", "EE", "FF", "FR")&g_size <10 ~ 0L,
                                .data$stock_strain_loc_name == "FRASER VALLEY" & .data$g_size <500 ~ 1L,
                                .data$stock_strain_loc_name == "FRASER VALLEY" & .data$g_size > 500 ~ .data$Year-.data$sby_code,
                                #First put NAs for remaining  cases where brood year will not help us
                                .data$sby_code == 0 ~ NA_integer_,
                                #Now simple brood year based ages.
                                # A few cases where supposedly released before brood year.
                                (.data$Year - .data$sby_code) > 0 & .data$Species %in% fall_spwn ~ .data$Year - .data$sby_code - 1L,#1L is so all numbers conform to integer type
                                (.data$Year - .data$sby_code) >= 0 & .data$Species %in% spring_spwn ~ .data$Year - .data$sby_code,
                                TRUE ~ NA_integer_),
                              )


#First just group together cases of multiple relids for the same group type of fish to the same lake and time.
#this seems quite slow.
Releases = Releases%>%
  mutate(Qtr = quarter(rel_Date))%>%#group by quarter in case 2 trips on different days of same fish
  group_by(across(c(-rel_id,-rel_Date, -stock_source_loc_name, -Quantity, -Weight, -g_size,-rel_waterbody_temp_c,-rel_waterbody_ph)))%>%
  summarize(rel_id = paste(unique(.data$rel_id),collapse = ","), 
            rel_Date = mean(.data$rel_Date, na.rm = TRUE), 
            stock_source_loc_name = paste(unique(.data$stock_source_loc_name),collapse = ","),
            Quantity = sum(Quantity), Weight = sum(Weight), g_size = sum(Quantity*g_size)/sum(Quantity))%>%
  select(-Qtr, ag_description)


Releases = Releases%>%
  dplyr::inner_join(dplyr::select(Lakes,WBID, Area),by = "WBID")%>%
  dplyr::mutate(Quantity_ha = Quantity/Area, Biom_ha = (Weight/Area))

#Store a copy of all releases in case of interest
All_Releases = Releases


#REDUCE DOWN TO RELEASES RELEVANT TO CROSS REFERNCE WITH LAKES IN BIOLOGICAL DATA
Releases <- Releases[Releases$WBID%in%Biological$WBID,]
#Only Releases that have brood year or release size info included.
Releases <- Releases[!(Releases$sby_code==0&Releases$g_size==0),]


#Expand releases dataframe into future, so we create a row of data for each potential age that a release group may exist in a lake
#All possible lake_years where a given release may be observed "xage" years into the future

maxxage = 6#the max age you expect to retrieve stocked fish.
X <- Releases#Set up starting dataframe X for loop
Xnew <- NULL#Add an additional dataframe for each year projected
#Loop thru each projected year and append updated dataframe rows (mostly repeated data, except for age, year and lake_year)
for(i in 0:maxxage){
  X$Int.Age = Releases$Int.Age+i
  X$Year = Releases$Year+i
  X$Lk_yr = paste(X$WBID,"_",X$Year, sep = "")
  Xnew <- rbind(Xnew, X)
}

rm(X)#X dataframe was just created for looping and can be removed.

#Notice that Xnew is a new data frame (xage + 1) times longer than Releases (+1 because we include 0 in the loop)

#Let's filter down to the stocking records that are predicted to appear in the Biological Table,
#and for consistency remove fish >maxxage for fish that were stocked at 1+ (or older)
Xnew <- Xnew[Xnew$Lk_yr%in%Biological$Lk_yr&Xnew$Int.Age <= maxxage,]

#Now we have a list of release records that were filtered to lakes and times matching assessments in the Biological table
#Then we expanded over the life span (well we cut off at 6 so really old fish will be lost) of the stocked fish
#(new data row for each age), so that we can cross reference a stocking event to a fish observed at any age in the biological table
#NeXt step is try and cross reference those stocking records to individual fish (so we can verify strain, genotype, etc.)


########################################################################################################################
###CLEANING TASKS SHOULD BE REMOVED EVENTUALLY

#First for cases where no clip, need to be consistent in NONE, NA, Null to be able to cross reference, so change all of these to NA
#This could be a problem where clips were not checked (currently NA), but the vast majority of NAs are truly not clipped.
#The other problem is when NA clip stocked but natural recruitment exists.
Biological$Clip[Biological$Clip == "NONE"]<-NA
Xnew$Clip[Xnew$Clip == ""]<-NA


#Similar issue where people have put UNK instead of NA or none. UNK should be for non-readable clips from fish know to be clipped.
Clip_yrs = Biological%>%
  dplyr::filter(!Clip%in%c("NOREC","UNK",NA))%>%
  dplyr::group_by(Lk_yr, Species)%>%
  dplyr::summarize(Nclips = length(unique(Clip)))%>%
  dplyr::mutate(Lk_yr_spp = paste0(Lk_yr,Species))%>%
  suppressMessages()
  
#So if there were no clips recorded at all in that Lk-yr and species group, then change UNK or NOREC to NA
Biological = Biological%>%dplyr::mutate(Lk_yr_spp = paste0(Lk_yr,Species))
Biological$Clip[Biological$Clip%in%c("NOREC","UNK",NA)&!Biological$Lk_yr_spp%in%Clip_yrs$Lk_yr_spp]<-NA

#Biological = Biological%>%dplyr::select(-Lk_yr_spp)

#########################################################################################################################
#How many clips are unique so that age does not need to be known?
#Group release data by lake, year, species, clip to create a summary (this will allow for replacing erroneous ages)
#of potential stocking records that could be assigned to those grouping variables (ideally just one - if unique clip or consistent stocking prescription)

group_cols <- c("Waterbody_Name","WBID", "Lk_yr", "Year","Species", "Clip")

clipsum <- Xnew%>%dplyr::group_by(!!!rlang::syms(group_cols))%>%
                  dplyr::summarize(nrel_ids = length(unique(.data$rel_id)), #number of rel_ids, these are not unique to stocking group 
                                   n_sby = length(unique(na.omit(.data$sby_code))), #number of brood years
                                    n_sry = length(unique(.data$Year)), #number of release years
                                    nStrains = length(unique(.data$Strain)), #number of Strains
                                    nGenos = length(unique(.data$Genotype)), #number of genotypes
                                    cliprel_ids = paste(unique(.data$rel_id),collapse = ","),
                                    clipStrains = paste(sort(unique(.data$Strain)), collapse = ","),
                                    clipGenos = paste(sort(unique(.data$Genotype)), collapse = ","),
                                    clipAges = paste(unique(.data$Int.Age), collapse = ","),
                                    clipsbys = paste(unique(.data$sby_code), collapse = ","),
                                    #The following variables are only valid if the n.. columns are unique (=1)
                                    #However adding an if_else caused problems with dates, and so clean these up later
                                    N_rel = sum(.data$Quantity),#Calculate number released if unique group
                                    SAR = round(sum(.data$g_size*.data$Quantity)/.data$N_rel,2), #Calculate mean weight at release
                                    cur_life_stage_code = paste(unique(.data$cur_life_stage_code), collapse = ","),
                                   avg_rel_date = mean(.data$rel_Date, na.rm = TRUE))%>%
                  dplyr::ungroup()%>%
  suppressMessages()


#Select release, lake-year combinations where clip leads to unique stocking group
Uniqueclips <-clipsum%>%
  dplyr::filter_at(dplyr::vars(n_sby, n_sry, nStrains, nGenos), dplyr::all_vars(. == 1))%>%#Filters to cases where all of the listed columns are unique. A value of 1.
  droplevels()#Drop all levels not in the list so we correctly filter Biological

#NA clips are not unique if there were no other clips present and/or natural recruitment (which is tough to know)
NA_clips = Uniqueclips%>%dplyr::filter(is.na(Clip))%>%dplyr::pull(Lk_yr)

#There are some cases where a non-clip us used as a clip where no natural recruitment exists (e.g. KO in Yellow lake)
#Filter out NAs if the size range in NAs is greater than on age class.
NA_nonuniques = Biological%>%dplyr::filter(is.na(Clip))%>%
  dplyr::group_by(Lk_yr_spp)%>%
  dplyr::summarize(minL = min(Length_mm, na.rm = T), maxL = max(Length_mm, na.rm = T),perc = (maxL- minL)/mean(minL,maxL))%>%
  dplyr::filter(perc>1)%>%
  dplyr::pull(Lk_yr_spp)%>%
  unique()%>%
  suppressWarnings()

#Drop these NA clips from uniqueclips
Uniqueclips = Uniqueclips%>%dplyr::filter(!paste0(Lk_yr,Species,NA)%in%paste0(NA_nonuniques,NA))

#In non-unique groups, we must remove all erroneous summary calculations of stocking averages and sums
nonunique <- dplyr::anti_join(clipsum, Uniqueclips)%>%
  dplyr::mutate(across(
    .cols = n_sby:avg_rel_date,
    .fns = ~replace(., TRUE, NA_integer_)
  ))%>%suppressMessages()


#Now we can take the Biological table a subset a chunk of it that matches the cases in the unique clips (whether ages have been entered or not).
#It is known that there are at least some cases where clipped fish have the wrong age entered (See Englishman Lake)
#That is why we first do a join with all the data regardless of whether age is entered, and then fill out the rest for just unaged fish
Bio_unique = dplyr::inner_join(Biological, Uniqueclips, by = group_cols)

#We create a subset of the Biological data without these uniquely assigned individuals to continue refining
Biosub = dplyr::anti_join(Biological, Bio_unique, by = colnames(Biological))

Biounaged = Biosub%>%dplyr::filter(is.na(.data$Int.Age))#All of the unaged fish that do not have unique group can at least be linked with possibilities

Biounaged = dplyr::left_join(Biounaged, nonunique, by = group_cols)

Bioaged = Biosub%>%dplyr::filter(!is.na(.data$Int.Age))

#Also could possibly add ages from clips if there is a gap of 1 year or more between just 2 or 3 possible ages

#New (overwriting) clip summary that includes ages as a grouping variable
group_cols <- c("Waterbody_Name","WBID", "Lk_yr", "Year","Species", "Clip", "Int.Age")

clipsum <- Xnew%>%dplyr::group_by(!!!rlang::syms(group_cols))%>%
                  dplyr::summarize(nrel_ids = length(unique(.data$rel_id)), #number of rel_ids, these are not unique to stocking group 
                                  n_sby = length(unique(na.omit(.data$sby_code))), #number of brood years
                                  n_sry = length(unique(.data$Year)), #number of release years
                                  nStrains = length(unique(.data$Strain)), #number of Strains
                                  nGenos = length(unique(.data$Genotype)),
                                  cliprel_ids = paste(unique(.data$rel_id),collapse = ","),
                                  clipStrains = paste(sort(unique(.data$Strain)), collapse = ","),
                                  clipGenos = paste(sort(unique(.data$Genotype)), collapse = ","),
                                  clipAges = paste(unique(.data$Int.Age), collapse = ","),
                                  clipsbys = paste(unique(.data$sby_code), collapse = ","),
                                  #The following variables are only valid if the n.. columns are unique (=1)
                                  #However adding an if_else caused problems with dates, and so clean these up later
                                  N_rel = sum(.data$Quantity),#Calculate number released if unique group
                                  SAR = sum(.data$g_size*.data$Quantity)/.data$N_rel, #Calculate mean weight at release
                                  cur_life_stage_code = paste(unique(.data$cur_life_stage_code), collapse = ","),
                                  avg_rel_date = mean(.data$rel_Date, na.rm = TRUE))%>%
                        dplyr::ungroup()%>%
  suppressWarnings()

#Identify unique stocking groups, in order to remove summary stats from non-unique groups
Uniqueclips <-clipsum%>%
  dplyr::filter(!paste0(Lk_yr,Species,NA)%in%paste0(NA_nonuniques,NA))%>%
  dplyr::filter_at(dplyr::vars(.data$n_sby, .data$n_sry, .data$nStrains, .data$nGenos), dplyr::all_vars(. == 1))%>%#Filters to cases where all of the listed columns are unique. A value of 1.
  droplevels()#Drop all levels not in the list so we correctly filter Biological

nonunique <- dplyr::anti_join(clipsum, Uniqueclips)%>%
                dplyr::mutate(N_rel = as.integer(NA),
                              SAR = as.numeric(NA),
                              cur_life_stage_code = as.character(NA),
                              avg_rel_date = as.POSIXct(NA)
                             )%>%
  suppressMessages()

#Bring them back together
clipsum = rbind(Uniqueclips, nonunique)

#Now we can match these to Biological data that had ages whether cases are unique or not.
#in non-unique cases it will list potential options.
Bioaged = dplyr::left_join(Bioaged, clipsum, by = group_cols)

#We now have three different data sub sets that add up to the same total records as Biological

Biological = rbind(Bio_unique, Biounaged, Bioaged)

#OK NOW HAVE CLIP BASED RELEASES DATA.
#THE ONY THING MAYBE NOT 100% ACCURATE WOULD BE FV BECASUE OF THEIR WEIRD BROOD YEARS

#Now for all cases where the clip makes strain, genotype, or age unique, then insert and/or replace values in those columns. In the case of age, put clip as age method.
#################Problem where NA clips appear unique but there is natural recruitment
#Add data where a single possible outcome from clip information. This does not currently attempt to correct erroneous ages, and only infills NA values in that case.
#DO NOT ATTEMPT FOR NA CLIPS UNTIL ALL NATURAL RECRUIT LAKES IDENTIFIED
Biological<-suppressWarnings(Biological%>%
  dplyr::mutate(
  sby_code = dplyr::if_else(!is.na(Clip)&.data$n_sby == 1L&!is.na(as.integer(.data$clipsbys)), as.integer(.data$clipsbys), .data$sby_code),
  Int.Age = dplyr::if_else(!is.na(Clip)&.data$n_sby == 1L&!is.na(as.integer(.data$clipAges)), as.integer(.data$clipAges), .data$Int.Age),
  #Strain = dplyr::if_else(.data$nStrains == 1L&!is.na(.data$clipStrains), .data$clipStrains, .data$Strain),
  #Genotype = dplyr::if_else(.data$nGenos == 1L&!is.na(.data$clipGenos), .data$clipGenos, .data$Genotype)
  #Because of known cases of mix of AF and 2N with same clip but recorded as one in data (see Premier 2014), replace data clips with true mix
  Strain = dplyr::if_else(!is.na(Clip)&!is.na(.data$clipStrains), .data$clipStrains, .data$Strain),
  Genotype = dplyr::if_else(!is.na(Clip)&!is.na(.data$clipGenos), .data$clipGenos, .data$Genotype)
  ))
#I think the warning NA introduced by coercion can safely be ignored from google research. There are less NA values at the end of this code than when started.
#suppressWarnings() must wrap everything, otherwise does not run, and warning is suppressed

#remove all data columns that are just tallies, and other columns not needed for growth/survival data analysis
Biological<-Biological%>%dplyr::select(-c(n_sby, n_sry, nStrains, nGenos, Scale, Otolith, DNA_ID, ATUS, Family_Group))
#Update Lk_sby with new age info.
Biological$Lk_sby = paste(paste(Biological$WBID,"_",Biological$sby_code, sep = ""))

#Add in stocking density info
Biological$Rel_ha = round(Biological$N_rel/Lakes$Area[match(Biological$WBID, Lakes$WBID)],1)
clipsum$Rel_ha = round(clipsum$N_rel/Lakes$Area[match(clipsum$WBID, Lakes$WBID)],1)

Biological<<-Biological
clipsum<<-clipsum




if(Sampled_only==TRUE){
Assessments<<- Assessments[Assessments$Assessment_Key%in%Biological$Assessment_Key,]
Lakes<<-Lakes[Lakes$WBID%in%Biological$WBID,]
Nets<<-Nets[Nets$Assessment_Key%in%Biological$Assessment_Key,]
Releases<<-Releases
}

if(Sampled_only==FALSE){
  Assessments<<- Assessments
  Lakes<<-Lakes
  Nets<<-Nets
  Releases<<-All_Releases
}


}
