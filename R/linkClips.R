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
#' @param Data_source a TRUE FALSE value to indicate whether to load data form the SLD, or just use data tables in the Environment.
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' linkClips()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
linkClips <- function(Sampled_only = TRUE, Data_source = TRUE){

if(Data_source == TRUE){SLD2R()}
if(!exists("Biological")|!exists("Releases")){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}
  
#Remove NA or non-fish species codes
Biological = Biological%>%dplyr::filter(Species%in%Spp_code_group_LU$species_code)

#Clean and standardize releases so they can be linked back to the individual fish in the biological table.
SPDT:::SPDTreleases()

#Clean up NOREC and UNK Clip entries in lake-years where no clips should be present anyways.
Clipsrel = Link_rel%>%
  dplyr::group_by(Lk_yr, Species)%>%
  dplyr::summarise(Nclips = sum(!is.na(Clip)&is.na(Int.Age)))%>%
  dplyr::filter(Nclips>0)

Biological = Biological%>%
  dplyr::mutate(Clip = ifelse((Clip %in% c("UNK","NOREC")&!(interaction(Lk_yr,Species)%in%interaction(Clipsrel$Lk_yr,Clipsrel$Species))),NA,Clip))


#Join potential stocking events to Biological to check for natural recruits in stocked species
#This joins by age, so ageing errors can lead to false possible Natural Recruit
NR = dplyr::left_join(Biological,Link_rel, by = c(names(Link_rel)[1:5], "Lk_yr"))%>%
  #dplyr::filter(Species%in%Spp_code_group_LU$species_code[Spp_code_group_LU$Stocked_species])%>%
  dplyr::mutate(Poss_NR = dplyr::case_when(
    is.na(Clip)&is.na(sby_rel)~1,
    is.na(Clip)&AF&Species!="KO"&Sex=="M"~1,
    is.na(Clip)&Sterile&Species!="KO"&Maturity%in%c("M","MT","SP","SB","MR","R")~1,
    is.na(Clip)&Sterile&Sex == "F"&Species=="KO"&Maturity%in%c("M","MT","SP","SB","MR","R")~1,
    TRUE~NA_real_
  ))  

#Create a NRT probability for each lake-species. Weight most recent assessment more heavily.
#Find Lake-years where at least 20 stocked species were sampled
SampleN = Biological%>%
  dplyr::filter(Species%in%c("CT","EB","KO","RB", "WCT"))%>%
  dplyr::count(Lk_yr)%>%dplyr::filter(n>=20)

#Search for natural recruits in stocked lakes with stocked species by year
NR_sum = NR%>%
  dplyr::filter(Lk_yr%in%SampleN$Lk_yr, Species%in%c("CT","EB","KO","RB", "WCT"))%>%
  dplyr::group_by(Region, Waterbody_Name, WBID,Year,Species)%>%
  dplyr::summarise(N = dplyr::n(), Nnr = sum(Poss_NR,na.rm = T), p = round(Nnr/N, 2), MeanFL = round(mean(Length_mm, na.rm = T)), MaxFL = max(Length_mm, na.rm = T))

#Specific lake-species combos where at least 30% of the fish could potentially be natural recruits
NR_lakes = NR_sum%>%
  dplyr::group_by(Region, Waterbody_Name, WBID,Species)%>%
  dplyr::summarise(pNR = round((mean(p)+p[Year == max(Year)])/2,2), N = sum(N))%>%
  dplyr::filter(pNR>0.29)%>%
  dplyr::ungroup()

#Now that established which lakes have a reasonable proportion of potential NR fish species, all non-clipped fish from those lake-species groups need to be treated as suspect.
Biological = dplyr::left_join(Biological,NR[,c("Biological_Data_ID","Poss_NR")], by = "Biological_Data_ID")%>%
  dplyr::mutate(Poss_NR = ifelse((is.na(Clip)&(interaction(WBID,Species)%in%interaction(NR_lakes$WBID,NR_lakes$Species))),1,Poss_NR))


##DIFFERENT strategy, first link everything without using age
#Perform 'Stocked_age' test to see if the entered age is within possible released ages.
Link_rel_noage = Link_rel%>%dplyr::filter(is.na(Int.Age))%>%dplyr::select(-c(Int.Age))
Biopossible <- dplyr::left_join(Biological,Link_rel_noage, by = c("WBID","Species","Year","Lk_yr","Clip"))%>%
  dplyr::rowwise()%>%
  dplyr::mutate(Stocked_age = Int.Age%in%as.integer(strsplit(Poss_Age, ",")[[1]]))%>%
  dplyr::ungroup()


#If no, then leave the possibilities as is (probably an ageing error or natural recruit or not stocked).
Bioambig = Biopossible[!Biopossible$Stocked_age,]

#If yes, then re-link releases to biological using age or brood year as a linking variable. 
Bioaged = Biopossible[Biopossible$Stocked_age,]%>%dplyr::select(-c(sby_rel:Stable_yrs))
Bioaged <- dplyr::left_join(Bioaged,Link_rel[!is.na(Link_rel$Int.Age),], by = c("WBID","Species","Year","Lk_yr","Clip","Int.Age"))

#Bring back together and remove temporary files.
Biological = rbind(Bioambig,Bioaged)

rm(Clipsrel, Stock, NR, NR_lakes, Biopossible, Bioambig, Bioaged,  Link_rel_noage)

#Replace un-observable values associated with stocking events and based on clips.
replace_uni = function(var,uni, Poss_NR){
  class = class(var)
  var = as.character(var)
  var = dplyr::case_when(!is.na(Poss_NR)|is.na(uni) ~ var,
                         grepl(",",uni) ~ var,
                         TRUE ~ uni)
  class(var)<-class#in case replacing a numeric variable
  return(var)
}

Biological = Biological%>%
  dplyr::mutate(
    Strain = replace_uni(Strain, Strain_rel, Poss_NR),
    Genotype = replace_uni(Genotype, Geno_rel, Poss_NR),
    sby_code = replace_uni(sby_code, sby_rel, Poss_NR),
    Int.Age = sby2age(Species, sby_code, Year),
    Dec.Age = round(.data$Int.Age+(lubridate::decimal_date(.data$Date) - lubridate::year(.data$Date)),2)
  )


Biological<<-Biological
NR_sum<<-NR_sum



#Not sur eif it is necessary to reduce to sampled only anymore....
#if(Sampled_only==TRUE){
#Assessments<<- Assessments[Assessments$Assessment_Key%in%Biological$Assessment_Key,]
#Lakes<<-Lakes[Lakes$WBID%in%Biological$WBID,]
#Nets<<-Nets[Nets$Assessment_Key%in%Biological$Assessment_Key,]
#Releases<<-Rel_sampled
#}

#if(Sampled_only==FALSE){
#  Assessments<<- Assessments
#  Lakes<<-Lakes
#  Nets<<-Nets
#  Releases<<-Releases
#}


}
