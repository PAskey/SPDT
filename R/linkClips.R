#' A function to load, clean and standardize SLD data for SPDT analysis. 
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#' This is the second data filtering and cleaning process after SLD2R() to reduce down to stocking data that can be tied to releases. 
#' Takes SLD2R() data and uses clip information to tie fish back to stocking event, age, strain, gentotype where possible.
#' Make sure your VPN is running, so that the database can be accessed by the function.
#' The Biological record count remains the same, but includes information about the potential stocking event(s) tied to each fish.
#' In cases where clips are unique, then fields for age, strain, genotype are updated if black. Otherwise a list of possibilities can be found
#' in "clipAges, clipStrains, clipGenos. 
#' All other data tables only include data that can be linked to the Biological data, either same Assessment_Key, or rel_id.
#' Anydata Tables you have open as Assessments, Nets, Lakes, Biological, Releases will be replaced with versions form this function.
#' Lookup tables for integer ages and strain codes are included as part of package and can by called as Ages, Strain_code_LU
#' Ultimately, as upload filters and cleaning are improved in the main database, this function will become obsolete.
#'
#' @title linkClips
#' @name linkClips
#' @keywords SPDT; clips
#' @export
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' linkClips()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
linkClips <- function(){

SLD2R()
  
#Just look at releases that are from lakes in the Biological Table
Releases <- Releases[Releases$WBID%in%Biological$WBID,]
#Only Releases that have brood year or release size info included.
Releases <- Releases[!(Releases$sby_code==0&Releases$g_size==0),]



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
                                .data$sby_code == 0 & .data$cur_life_stage_code %in% c("YE", "YE+", "FG", "FFG")& .data$g_size <500 ~ 1L,#Only trust YE designation if don't have sby_code
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

#Now we have a list of release records that covers all observed stocked fish in the Biological table.
#NeXt step is try and cross reference those stocking records to individual fish (so we can verify strain, genotype, etc.)


#First for cases where no clip, need to be consistent in NONE, NA, Null to be able to cross reference, so change all of these to NA
#This could be a problem where clips were not checked (currently NA), but the vast majority of NAs are truely not clipped.
Biological$Clip[Biological$Clip == "NONE"]<-NA
Xnew$Clip[Xnew$Clip == ""]<-NA

#How many clips are unique so that age does not need to be known?
#Group release data by lake, year, species, clip to create a summary (this will allow for replacing erroneous ages)
#of potential stocking records that could be assigned to those grouping variables (ideally just one - if unique clip or consistent stocking prescription)

group_cols <- c("Waterbody_Name","WBID", "Lk_yr", "Year","Species", "Clip")

clipsum <- Xnew%>%dplyr::group_by(!!!syms(group_cols))%>%
                  dplyr::summarize(nrel_ids = length(unique(.data$rel_id)), #number of rel_ids, these are not unique to stocking group 
                                   n_sby = length(unique(na.omit(.data$sby_code))), #number of brood years
                                    n_sry = length(unique(.data$Year)), #number of release years
                                    nStrains = length(unique(.data$Strain)), #number of Strains
                                    nGenos = length(unique(.data$Genotype)), #number of genotypes
                                    cliprel_ids = paste(unique(.data$rel_id),collapse = ","),
                                    clipStrains = paste(unique(.data$Strain), collapse = ","),
                                    clipGenos = paste(unique(.data$Genotype), collapse = ","),
                                    clipAges = paste(unique(.data$Int.Age), collapse = ","),
                                    clipsbys = paste(unique(.data$sby_code), collapse = ","),
                                    N_rel = ifelse(.data$nStrains == 1&.data$nGenos == 1, sum(.data$Quantity),NA),#Calculate number released if unique group
                                    SAR = sum(.data$g_size*.data$Quantity)/.data$N_rel, #Calculate mean weight released if unique group
                                    avg_rel_date = mean(.data$rel_Date))%>%
                        dplyr::ungroup()


#reduce summary to cases where clip leads to unique stocking group
Uniqueclips <-clipsum%>%
  dplyr::filter_at(dplyr::vars(.data$n_sby, .data$n_sry, .data$nStrains, .data$nGenos), dplyr::all_vars(. == 1))%>%#Filters to cases where all of the listed columns are unique. A value of 1.
  droplevels()#Drop all levels not in the list so we correctly filter Biological

#Now we can take the Biological table a subset a chunk of it that matches the cases in the unique clips (whether ages have been entered or not).
#It is known that there are at least some cases where clipped fish have the wrong age entered (See Englishman Lake)
#That is why we first do a join with all the data regardless of whether age is entered, and then fill out the rest for just unaged fish
Bio_unique = dplyr::inner_join(Biological, Uniqueclips, by = group_cols)

#We create a subset of the Biological data without these uniquely assigned individuals to continue refining
Biosub = dplyr::anti_join(Biological, Bio_unique, by = colnames(Biological))

Biounaged = Biosub%>%dplyr::filter(is.na(.data$Int.Age))#All of the unaged fish that do not have unique group can at least be linked with possibilities

Biounaged = dplyr::left_join(Biounaged, clipsum, by = group_cols)

Bioaged = Biosub%>%dplyr::filter(!is.na(.data$Int.Age))

#Also could possibly add ages from clips if there is a gap of 1 year or more between just 2 or 3 possible ages

#New (overwriting) clip summary that includes ages as a grouping variable
group_cols <- c("Waterbody_Name","WBID", "Lk_yr", "Year","Species", "Clip", "Int.Age")

clipsum <- Xnew%>%dplyr::group_by(!!!syms(group_cols))%>%
                  dplyr::summarize(nrel_ids = length(unique(.data$rel_id)), #number of rel_ids, these are not unique to stocking group 
                                  n_sby = length(unique(na.omit(.data$sby_code))), #number of brood years
                                  n_sry = length(unique(.data$Year)), #number of release years
                                  nStrains = length(unique(.data$Strain)), #number of Strains
                                  nGenos = length(unique(.data$Genotype)),
                                  cliprel_ids = paste(unique(.data$rel_id),collapse = ","),
                                  clipStrains = paste(unique(.data$Strain), collapse = ","),
                                  clipGenos = paste(unique(.data$Genotype), collapse = ","),
                                  clipAges = paste(unique(.data$Int.Age), collapse = ","),
                                  clipsbys = paste(unique(.data$sby_code), collapse = ","),
                                  N_rel = ifelse(.data$nStrains == 1&.data$nGenos == 1, sum(.data$Quantity),NA),#Calcualte number released if unique group
                                  SAR = sum(.data$g_size*.data$Quantity)/.data$N_rel, #Calculate mean weight released if unique group
                                  avg_rel_date = mean(.data$rel_Date))%>%
                        dplyr::ungroup()


#Now we can match these to Biological data that had ages whether cases are unique or not.
#in non-unique cases it will list potential options.
Bioaged = dplyr::left_join(Bioaged, clipsum, by = group_cols)

#We now have three different data sub sets that add up to the same total records as Biological

Biological = rbind(Bio_unique, Biounaged, Bioaged)

#OK NOW HAVE CLIP BASED RELEASES DATA.
#THE ONY THING MAYBE NOT 100% ACCURATE WOULD BE FV BECASUE OF THEIR WEIRD BROOD YEARS

#Now for all cases where the clip makes strain, genotype, or age unique, then insert and/or replace values in those columns. In the case of age, put clip as age method.

#Add data where a single possible outcome from clip information. This does not currently attempt to correct erroneous ages, and only infills NA values in that case.
Biological<-suppressWarnings(Biological%>%
  dplyr::mutate(
  sby_code = dplyr::if_else(.data$n_sby == 1L&!is.na(as.integer(.data$clipsbys)), as.integer(.data$clipsbys), .data$sby_code),
  Int.Age = dplyr::if_else(.data$n_sby == 1L&!is.na(as.integer(.data$clipAges)), as.integer(.data$clipAges), .data$Int.Age),
  Strain = dplyr::if_else(.data$nStrains == 1L&!is.na(.data$clipStrains), .data$clipStrains, .data$Strain),
  #Genotype = dplyr::if_else(.data$nGenos == 1L&!is.na(.data$clipGenos), .data$clipGenos, .data$Genotype)
  #Because of known cases of mix of AF and 2N with same clip but recorded as one in data (see Premier 2014), replace data clips with true mix
  Genotype = dplyr::if_else(!is.na(.data$clipGenos), .data$clipGenos, .data$Genotype)
  ))
#I think the warning NA introduced by coercion can safely be ignored from google research. There are less NA values at the end of this code than when started.
#suppressWarnings() must wrap everything, otherwise does not run, and warning is suppressed

#remove all data columns that are just tallies, and othe rcolumns not needed for growth/survival data analysis
Biological<-Biological%>%dplyr::select(-c(.data$n_sby, .data$n_sry, .data$nStrains, .data$nGenos, .data$Scale, .data$Otolith, .data$DNA_ID, .data$ATUS, .data$Family_Group))
#Update Lk_sby with new age info.
Biological$Lk_sby = paste(paste(Biological$WBID,"_",Biological$sby_code, sep = ""))

Assessments<<- Assessments[Assessments$Assessment_Key%in%Biological$Assessment_Key,]
Lakes<<-Lakes[Lakes$WBID%in%Biological$WBID,]
Nets<<-Nets[Nets$Assessment_Key%in%Biological$Assessment_Key,]
Biological<<-Biological
Releases<<-Releases
clipsum<<-clipsum

}
