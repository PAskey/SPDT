#' A function to load, clean and standardize SLD data for SPDT analysis. 
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#' This is the first step in doing and SPDT analyses, and is the rawest form of data. 
#' Loads, cleans all formats data tables for SPDT type analysis.
#' Make sure your VPN is running, so that the database can be accessed by the function.
#' The data sets returned to the RStudio Environment are: Assessments, Lakes, Biological, Nets, Releases.
#' The data sets have minor filtering at this stage.
#' Assessments are filtered to only fishery related assessments, none of: ("GC","UNK","UP","WQ")
#' Lakes are filtered to lakes that appear in this list of assessments (have had some sort of fishery type assessment at some point in their history).
#' Biological is filtered to records that have known year and species.
#' Releases are filtered to releases into lakes (not streams).
#' Lookup tables for integer ages and strain codes are included as part of package and can by called as Ages, Strain_code_LU.
#' Ultimately, as upload filters and cleaning are improved in the main database, this function will become obsolete.
#'
#' @title SLD2R
#' @name SLD2R
#' @keywords SPDT
#' @export
#' @examples
#' SLD2R()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
SLD2R <- function(){
ch <- RODBC::odbcDriverConnect('driver={SQL Server};server=FFSBCSQL06;
                        DSN=SMALL_LAKES-TEST;DATABASE=SMALL_LAKES-TEST;
                        Trusted_Connection=TRUE')

Assessments<-RODBC::sqlFetch(ch, "ffsbc.vw_Assessment_Summary")

Nets <-RODBC::sqlFetch(ch,"ffsbc.vw_Net_Summary")

Lakes<-RODBC::sqlFetch(ch,"ffsbc.vw_Lakes")

Lake_dim <-RODBC::sqlFetch(ch, "ffsbc.vw_Lake_Dimensions")

Biological <- RODBC::sqlFetch(ch, "ffsbc.vw_Biological_Data")

Releases <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_releases")

close(ch)


#_______________________________________________________________________________
#Date re-formatting

#Nets dates have been uploaded as factors with some clear errors
#(e.g. end_date in year 1899)
Nets$Start_Date<-as.POSIXct(Nets$Start_Date, format="%Y-%m-%d")
Nets$End_Date<-as.POSIXct(Nets$End_Date, format="%Y-%m-%d")

#Lakes and Lake_dim no date data, other than date added.

#Biological dates have been uploaded as factors with some possible errors (e.g. year 1905)
Biological$Date<-as.POSIXct(Biological$Date, format="%Y-%m-%d")

#Releases dates have been uploaded as factors without obvious errors
#I believe very old records are accurate bass and lake whitefish stockings.
Releases$rel_Date<-as.POSIXct(Releases$rel_Date, format="%Y-%m-%d")

####################################################################################################################
#Recode Genotypes and Strain

#For naming of ploidy and sex together I believe we agreed that "Genotype" is the best term, so change both datasets to that. Then for all Releases columns we will make consistent with Biological
Biological<-Biological%>%dplyr::rename(Genotype = Ploidy, Strain = Strain_Species)


Releases <- Releases%>%dplyr::rename(WBID = loc_msrm_waterbody_identifier,
                                     Genotype = stock_gtype_code,
                                     Species = sp_code,
                                     Clip = rel_fm_code)

########################################################################################################################
###CLEANING TASKS IN THIS SECTION SHOULD BE REMOVED EVENTUALLY

#Data entry error for Duffy and Harper in Biological fix for now
Biological$Capture_Method[Biological$Capture_Method == "CAM"] = "GN"

#Replace 0s with NA
Biological$Length_mm[Biological$Length_mm==0]<-NA
Biological$Weight_g[Biological$Weight_g==0]<-NA


#Change to consistent capitalization
Biological <- Biological%>%dplyr::mutate(Sex = toupper(Sex))

#To fix alternate codes for same thing change all Biological to "AF" because Releases uses "AF"
Biological$Genotype[Biological$Genotype == "AF2n"] = "AF"
#Match case in Releases
Releases$Genotype = dplyr::recode(Releases$Genotype, '2N'='2n','3N'='3n', 'AF3N' = 'AF3n', 'MT3N'='MT3n')

#Also seems to be an error with redside shiners being coded incorrectly as RSS
Biological$Species[Biological$Species == "RSS"] = "RSC"
Nets$species_caught[Nets$species_caught == "RSS"] = "RSC"
#Buchanan has NSC incorrectly coded as NP
Biological$Species[Biological$Species == "NP" & Biological$WBID == "01492TWAC"] = "NSC"
Nets$species_caught[Nets$species_caught == "NP" & Nets$WBID == "01492TWAC"] = "NSC"

#########################################################################################################################
#_______________________________________________________________________________
#RELEASES
#Several minor adjustments

#Lake names were not exactly matching between releases and biological data, which required this code to use Biological Waterbody names
Releases$Waterbody_Name = Lakes$Waterbody_Name[match(Releases$WBID, Lakes$WBID)]
#Add region
Releases$Region = Lakes$Region[match(Releases$WBID, Lakes$WBID)]

#Lookup strain codes and insert into releases to make consistent with Biological strain codes
Releases<-Releases%>%
            dplyr::mutate(Strain = as.character(plyr::mapvalues(stock_strain_loc_name,
                                                                from=as.character(SPDT::Strain_code_LU$stock_strain_loc_name),
                                                                to=as.character(SPDT::Strain_code_LU$Strain), warn_missing = FALSE)))



#Remove stream releases as generally do not apply to SPDT type analyses
Releases <- Releases%>%dplyr::filter(!(grepl("00000",.data$WBID))&.data$WBID!="")%>%droplevels()
#Create column for release year
Releases$Year<-as.integer(format(as.Date(Releases$rel_Date, format = "%Y-%m-%d"), "%Y"))
#Add in lake brood year and lake stocking year grouping variables that can match Biological
Releases <- Releases%>%dplyr::mutate(Lk_sby = paste(.data$WBID,"_",.data$sby_code, sep = ""), 
                                     Lk_sry = paste(.data$WBID,"_",.data$Year, sep = "")
)


#_______________________________________________________________________________
#NETS
#Similar minor adjustments


#Create column for sample year.Sometimes start date or end date is missing.

Nets <- Nets %>%
  dplyr::mutate(
    Year = pmax(
      as.integer(format(as.Date(Start_Date, format = "%Y-%m-%d"), "%Y")),
      as.integer(format(as.Date(End_Date, format = "%Y-%m-%d"), "%Y")),
      na.rm = TRUE
    )
  )

#Add in lake-year that can match Biological and Releases
Nets <- Nets%>%dplyr::mutate(Lk_yr = paste(.data$WBID,"_",.data$Year, sep = ""))

#This is necessary to link to Bio data - 3 names for method among tables should be fixed
Nets = merge(Nets, Assessments[,c("Assessment_Key", "Method")], by = "Assessment_Key")%>%
  dplyr::rename(Capture_Method = Method)


#_______________________________________________________________________________
#filter down to assessed fishery lakes and merge lake dimension info


#Gets rid of assessments that may have nothing to do with fish
Assessments <- Assessments%>%dplyr::filter(!(.data$Method%in%c("GC","UNK","UP","WQ")))%>%
                              droplevels()

#Add lake year for cross-referencing although not ideal, there are a few cases than span years or have no dates (hence suppresswarnings())
Assessments = Assessments%>%
  #dplyr::rowwise()%>%
  dplyr::mutate(
    Year = pmax(Start_Year,lubridate::year(End_Date), na.rm = TRUE),
    Lk_yr = paste0(WBID, "_", Year))%>%
  #suppressWarnings()%>%
  dplyr::ungroup()


#Find the unique list of WBID that have been assessed or stocked (i.e. known fisheries)
Fishery_WBID <- unique(c(Assessments$WBID, Releases$WBID))

#Now filter the Lakes dataframe down to those WBIDs, and add a column to state whether stockd or not
Lakes<-Lakes%>%
  dplyr::filter(.data$WBID%in%Fishery_WBID & .data$Waterbody_Type == "Lake" & !is.na(.data$WBID))%>%
  dplyr::mutate(Stocked = .data$WBID%in%Releases$WBID)%>%
  droplevels()

#Track year of last stocking into lake
lastYRs = Releases%>%dplyr::group_by(WBID)%>%dplyr::summarize(Last_release = max(Year))

Lakes = dplyr::left_join(Lakes,lastYRs, by = 'WBID')
rm(lastYRs)

#dplyr::filter lake dimensions data frame to same lakes
Lake_dim <- Lake_dim%>%dplyr::filter(.data$WBID %in% Lakes$WBID)%>%droplevels()


#Average and round all the lake dimension stas with multiple entries
Lake_dim <- Lake_dim%>%
  dplyr::group_by(.data$WBID, .data$Waterbody_Name, .data$Region)%>%
      dplyr::summarize(
              Area = round(mean(c(.data$Area_Surface_ha, .data$Area_Surface_2_ha), na.rm = TRUE),1),
              Perimeter = round(mean(c(.data$Perimeter_m,.data$Perimeter_2_m),na.rm = TRUE),1),
              Max_Depth = round(mean(.data$Depth_Max_m,na.rm = TRUE),1),
              Mean_Depth = round(mean(.data$Depth_Max_m, na.rm = TRUE),1),
              Area_Littoral = round(mean(.data$Area_Littoral_ha, na.rm = TRUE),1),
              Elevation = round(mean(.data$Elevation_m,na.rm = TRUE)),
              Inlets = max(.data$No_Of_Inlets_Permanent),
              Outlets = max(.data$No_Of_Outlets))%>%
  dplyr::ungroup()

####################################################################################
#Manual entry of a couple important stocked lakes not having lake area
Lake_dim$Area[Lake_dim$WBID == '01497OKAN']<-32
Lake_dim$Area[Lake_dim$WBID == 'FFSBC3802']<-13.9


#Select fields, can use dput(names(Lakes)) to get full list and reduce from there
Lakes = Lakes%>%dplyr::select(c("Waterbody_Name", "Alias", "WBID", "Region", "Region_Name", "UTM_Easting", 
  "UTM_Northing", "UTM_Zone", "Latitude", "Longitude", "Stocked", "Last_release"))

Lake_dim = Lake_dim%>%dplyr::select(c("WBID", "Area", "Perimeter", "Max_Depth", "Mean_Depth", "Area_Littoral", "Elevation", "Inlets", "Outlets"))

#Join lake dimension summary stats to lakes data frame
Lakes<-dplyr::left_join(Lakes, Lake_dim, by = "WBID")

#We can now remove the Lake_dim data frame as we have incorporated the data into Lakes.

##________________________________________________________________________________________
#Add in species composition as a lake characteristic
#Species are sometimes not recorded in Biological but in Net summary (and vice-versa)
BioSpp = Biological%>%dplyr::group_by(WBID,Year,Species)%>%dplyr::summarize(Nb = dplyr::n())
NetSpp = Nets%>%dplyr::group_by(WBID,Year,species_caught)%>%dplyr::summarize(Nn = sum(no_fish))

#All species captured summary
Lake_Spp  = dplyr::full_join(BioSpp,NetSpp, by = c("WBID", "Year", "Species" = "species_caught"))%>%
  #rowwise()%>%
  dplyr::filter(!is.na(Species),
                !Species %in% c("NFC", "NFP", "UNK", "SP"), 
                rowSums(cbind(Nb,Nn), na.rm = TRUE)>0)%>%
  dplyr::mutate(N = pmax(Nb,Nn, na.rm = T),
                Lk_yr = paste0(WBID,"_",Year),
                Subfamily = Spp_code_group_LU$Subfamily[match(.data$Species,Spp_code_group_LU$species_code)])%>%
  dplyr::group_by(WBID)%>% 
  dplyr::mutate(All_spp = paste(sort(unique(Species)), collapse = ','),
                Spp_class = paste(sort(unique(Subfamily)), collapse = ','),
                Non_salm = paste(sort(unique(Species[.data$Subfamily!="Salmoninae"])), collapse = ','))%>%
  dplyr::group_by(WBID, All_spp, Spp_class, Non_salm, Year, Lk_yr)%>%
  dplyr::summarize(
    Spp_caught = paste(sort(unique(Species)), collapse = ','),
    Dominant_spp = Species[which.max(N)], 
    Dominant_spp_p = max(N)/sum(N),
    .groups = "drop")
  

Lakes = dplyr::left_join(Lakes,unique(Lake_Spp[,c("WBID","All_spp")]), by = "WBID")

rm(Lake_dim,BioSpp,NetSpp)

#_______________________________________________________________________________
#filter biological data to gillnet information, add age interpretation and delete or flag data errors

#Remove all spaces from age descriptions to reduce potential lookup options for converting to integers.
Biological$Age <- gsub('\\s+', '', Biological$Age)



#dplyr::filter out records without year or species and create a Lake_WBID and Region_Name column
Biological <- Biological%>%
                dplyr::filter(!is.na(.data$Year),!is.na(.data$Species))%>%
                dplyr::mutate(#Lake_WBID = paste(.data$Waterbody_Name,"_",.data$WBID, sep = ""),
                              Region_Name = plyr::mapvalues(Region, from=Lakes$Region,
                                                                    to=as.character(Lakes$Region_Name),
                                                                    warn_missing = FALSE),
                              Int.Age = plyr::mapvalues(Age, from=SPDT::Ages$Ages, to=SPDT::Ages$Int.Ages, warn_missing = FALSE),
                              K = round(100000*.data$Weight_g/.data$Length_mm^3,2)
                              )%>%
  suppressWarnings()#for NAs introduced by coercion



#mapvalues() leaves int.ages as factor class, so convert to integers data type
Biological$Int.Age = as.integer(as.character(Biological$Int.Age))%>%suppressWarnings()

#assign Species to spawning period to calculate brood year
spring_spwn = as.character(expression(ACT, BS, CRS, CT, GR, MG, RB, ST, TR, WCT, WP, WSG))#expression adds quotations to each element
fall_spwn = as.character(expression(AS, BL, BT, DV, EB, GB, KO, LT, LW))


#Add in a lake year and a brood year column for filtering and matching to releases
Biological<- Biological%>%
              dplyr::mutate(
                      Lk_yr = paste(.data$WBID,"_",.data$Year, sep = ""),
                      sby_code = dplyr::case_when(
                        .data$Species %in% spring_spwn & .data$Strain == "FV"& .data$Year < 2013 ~ .data$Year - .data$Int.Age + 1L,
                        .data$Species %in% fall_spwn ~ .data$Year - .data$Int.Age - 1L,#1L is integer type
                          TRUE ~ .data$Year - .data$Int.Age),
                      Lk_sby = paste(WBID,"_",sby_code, sep = "")
                            )


#IDENTIFY OUTLIERS
Biological <- Biological%>%
  dplyr::mutate(Outlier = ifelse(is.na(.data$Length_mm)|.data$Length_mm>900|.data$Species%in%c("UNK","NFC","NFP"),1,
                          ifelse(!is.na(.data$Weight_g)&.data$Species %in% c('ACT','CT','WCT','CRS','RBCT','RB','KO','EB','DV','BT','GB','TR','ST') & 
                                   (!(.data$Length_mm %in% c(60:1000)) | 0.7 > .data$K | 2.2 < .data$K),1,0)
  ))


#Last, let's add an expansion factor for gillnet selectivity
##THis is not longer working, so need to fix. Giving different answers for same sized fish!!Uhg
Biological <- Biological%>%
                dplyr::mutate(NetX = ifelse(
                  .data$Capture_Method == "GN"&.data$Length_mm>75&.data$Length_mm<650&.data$Species == "RB",
                  1/SPDT::RICselect(FLengths_mm = .data$Length_mm),1))%>%
                tidyr::replace_na(list(NetX = 1))


#_______________________________________________________________________________
#Initial data download complete
Assessments <<- Assessments
Lakes<<-Lakes
Nets<<-Nets
Biological<<-Biological
Releases<<-Releases
Lake_Spp<<-Lake_Spp

}
