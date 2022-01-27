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

#_______________________________________________________________________________
#Spatial data re-formatting

#Convert net locations, by fist ignoring NA locations and then converting the rest.
Nets<-suppressWarnings(Nets%>%dplyr::filter_at(dplyr::vars(.data$Shore_UTM_Zone:.data$Lake_UTM_Northing),dplyr::all_vars(!is.na(.)))%>%
  dplyr::group_by(.data$Lake_UTM_Zone)%>%
    dplyr::mutate(
            Shore_Lat = UTM_to_latlong(.data$Shore_UTM_Easting, .data$Shore_UTM_Northing, .data$Shore_UTM_Zone)$y,
            Shore_Long = UTM_to_latlong(.data$Shore_UTM_Easting, .data$Shore_UTM_Northing, .data$Shore_UTM_Zone)$x,
            Lake_Lat = UTM_to_latlong(.data$Lake_UTM_Easting, .data$Lake_UTM_Northing, .data$Lake_UTM_Zone)$y,
            Lake_Long = UTM_to_latlong(.data$Lake_UTM_Easting, .data$Lake_UTM_Northing, .data$Lake_UTM_Zone)$x)%>%
  dplyr::ungroup())

Lakes<-suppressWarnings(Lakes%>%dplyr::filter(!is.na(.data$UTM_Zone), !is.na(.data$UTM_Easting))%>%droplevels()%>%
  dplyr::group_by(.data$UTM_Zone)%>%
  dplyr::mutate(Long = UTM_to_latlong(.data$UTM_Easting, .data$UTM_Northing, .data$UTM_Zone)$x,
                Lat = UTM_to_latlong(.data$UTM_Easting, .data$UTM_Northing, .data$UTM_Zone)$y))

#_______________________________________________________________________________
#Reformat to consistent column naming and data coding
#Column names are generally consistent among SLD Tables, but Paris (Releases) is different, so we will make those match

#For naming of ploidy and sex together I believe we agreed that "Genotype" is the best term, so change both dataset to that. Then for all Releases columns we will make consistent with Biological
Biological<-Biological%>%dplyr::rename(Genotype = .data$Ploidy, Strain = .data$Strain_Species)

Releases <- Releases%>%dplyr::rename(WBID = .data$loc_msrm_waterbody_identifier,
                                     Genotype = .data$stock_gtype_code,
                                     Species = .data$sp_code,
                                     Clip = .data$rel_fm_code)

#Lake names were not exactly matching between releases and biological data, which required this code to use Biological Waterbody names
Releases$Waterbody_Name = Biological$Waterbody_Name[match(Releases$WBID, Biological$WBID)]

#Data entry error for Duffy and Harper in Biological fix for now
Biological$Capture_Method[Biological$Capture_Method == "CAM"] = "GN"

#To fix alternate codes for same thing change all Biological to "AF" because Releases uses "AF"
Biological$Genotype[Biological$Genotype == "AF2N"] = "AF"

#Also seems to be an error with redside shiners being coded incorrectly as RSS
Biological$Species[Biological$Species == "RSS"] = "RSC"
Nets$species_caught[Nets$species_caught == "RSS"] = "RSC"

#Lookup strain codes and insert into releases to make consistent with Biological strain codes

Releases<-Releases%>%
            dplyr::mutate(Strain = as.character(plyr::mapvalues(stock_strain_loc_name,
                                                                from=as.character(Strain_code_LU$stock_strain_loc_name),
                                                                to=as.character(Strain_code_LU$Strain))))

#_______________________________________________________________________________
#filter down to assessed fishery lakes and merge lake dimension info


#Gets rid of assessments that may have nothing to do with fish
Assessments <- Assessments%>%dplyr::filter(!(.data$Method%in%c("GC","UNK","UP","WQ")))%>%droplevels()
#Add lake year for cross-referencing although not ideal, there are a few cases than span years.
Assessments = Assessments%>%
  dplyr::rowwise()%>%
  dplyr::mutate(Lk_yr = paste0(.data$WBID, "_", max(.data$Start_Year, lubridate::year(.data$End_Date), na.rm = TRUE)))


#Find the unique list of WBID that have been assessed or stocked (i.e. known fisheries)
Fishery_WBID <- unique(Assessments$WBID, Releases$WBID)

#Now filter the Lakes dataframe down to those WBIDs, and add a column to state whether stockd or not
Lakes<-Lakes%>%
  dplyr::filter(.data$WBID%in%Fishery_WBID & .data$Waterbody_Type == "Lake" & !is.na(.data$WBID))%>%
  dplyr::mutate(Stocked = .data$WBID%in%Releases$WBID)%>%droplevels()

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

#Join lake dimension summary stats to lakes data frame
Lakes<-dplyr::left_join(Lakes[,c(2,3,5:7,10:12,15,16)], Lake_dim[,c(1,4:11)], by = "WBID")

#We can now remove the Lake_dim data frame as we have incorporated the data into Lakes.
rm(Lake_dim)

#_______________________________________________________________________________
#filter biological data to gillnet information, add age interpretation and delete or flag data errors

#Remove all spaces from age descriptions to reduce potential lookup options for converting to integers.
Biological$Age <- gsub('\\s+', '', Biological$Age)



#dplyr::filter out records without year or species and create a Lake_WBID and Region_Name column
Biological <- Biological%>%
                dplyr::filter(!is.na(.data$Year),!is.na(.data$Species))%>%
                dplyr::mutate(Lake_WBID = paste(.data$Waterbody_Name,"_",.data$WBID, sep = ""),
                              Region_Name = plyr::mapvalues(Region, from=Lakes$Region,
                                                                    to=as.character(Lakes$Region_Name),
                                                                    warn_missing = FALSE),
                              Int.Age = plyr::mapvalues(Age, from=Ages$Ages, to=Ages$Int.Ages, warn_missing = FALSE),
                              K = 100000*.data$Weight_g/.data$Length_mm^3
                              )


#mapvalues() leaves int.ages as factor class, so convert to integers data type
Biological$Int.Age = as.integer(as.character(Biological$Int.Age))

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



#Flag extremely odd sized or shaped fish as outliers
Biological <- Biological%>%
                dplyr::mutate(
                          Outlier = ifelse(
                              .data$Species %in% c('ACT','CT','WCT','CRS','RBCT','RB','KO','EB','DV','BT','GB','TR','ST')&
                              (!(.data$Length_mm %in% c(60:1000))|
                              #(.data$Int.Age < 1 & .data$Length_mm>=100)|#Kokanee can beat this
                              #(.data$Int.Age <= 1 & .data$Length_mm>=440)|
                              0.7 > .data$K | 2.2 < .data$K),1,0)
                              )

#Last, let's add an expansion factor for gillnet selectivity
Biological <- Biological%>%
                dplyr::mutate(NetX = ifelse(
                  .data$Capture_Method == "GN",
                  1/RICselect(.data$Length_mm),1))%>%
                tidyr::replace_na(list(NetX = 1))
#_______________________________________________________________________________
#RELEASES
#Couple minor adjustments

#Remove stream releases as generally do not apply to SPDT type analyses
Releases <- Releases%>%dplyr::filter(!(grepl("00000",.data$WBID))&.data$WBID!="")%>%droplevels()
#Create column for release year
Releases$rel_Year<-as.integer(format(as.Date(Releases$rel_Date, format = "%Y-%m-%d"), "%Y"))
#Add in lake brood year and lake stocking year grouping variables that can match Biological
Releases <- Releases%>%dplyr::mutate(Lk_sby = paste(.data$WBID,"_",.data$sby_code, sep = ""), 
                                      Lk_sry = paste(.data$WBID,"_",.data$rel_Year, sep = "")
                                      )

#_______________________________________________________________________________
#NETS
#Similar minor adjustments


#Create column for sampleyear
Nets$Year<-as.integer(format(as.Date(Nets$End_Date, format = "%Y-%m-%d"), "%Y"))
#Add in lake-year that can match Biological and Releases
Nets <- Nets%>%dplyr::mutate(Lk_yr = paste(.data$WBID,"_",.data$Year, sep = ""))

#_______________________________________________________________________________
#Use droplevels() to make sure not retaining any factor levels that are no longer in use after filtering steps above
Assessments = droplevels(Assessments)
Lakes = droplevels(Lakes)
Nets = droplevels(Nets)
Biological = droplevels(Biological)
Releases = droplevels(Releases)

#_______________________________________________________________________________
#Initial data download complete
Assessments <<- Assessments
Lakes<<-Lakes
Nets<<-Nets
Biological<<-Biological
Releases<<-Releases

}
