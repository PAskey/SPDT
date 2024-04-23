#' A function to summarize population structure. 
#'
#'
#' This is a non-exported funciton to simplify code using the releases table to understand all possible stocked groups that could be in a lake at any given time.
#' In cases where clips are unique, then fields for age, strain, genotype are updated.
#' This function was previously withing lines of exported functions: SLD2R() and linkClips().But to make code more tractable, it has been put into it's own function.
#' Ultimately, as upload filters and cleaning are improved in the main database, this function could become obsolete.
#'
#' @title SPDTstructure
#' @name SPDTstructure
#' @keywords SPDT; releases; stocked; clips
#' 
#' @examples
#' #' Must be connected to VPN if working remotely
#' 
#' SPDTreleases()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 
#' 
SPDTstructure <- function(Data_source = FALSE){
  #Pretty much need to do linkClips firs tto get a filtered biological and the the Link_rel df
  if(Data_source == TRUE){SLD2R()}
  if(!exists("Biological")|!exists("Releases")){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}
  

  ##Population structure and growth provincial summary POP_STR file____________________________________________________________________________________________________________________________________________________________________
category = c("Sub-stock", "Stock", "Quality", "Trophy")
FL_cutoff = c(0, 200, 400, 550, 1000)
RegionalorFamily = c(NA, 0.8, NA, NA)
Quality = c(NA, 0.8, .2, NA)
Trophy = c(NA, 0.8, .2, .1)
X = data.frame(category, FL_cutoff[1:4], RegionalorFamily, Quality, Trophy)
colnames(X)<-c("Size classification", "Minimum Length_mm cutoff", "Regional or Family", "Quality", "Trophy")

#This drops all non-salmonid fish from this point forward!!
#___________________________________________________________________________________________________________
Biological<-Biological%>%
  dplyr::filter(Species%in%Spp_code_group_LU$species_code[Spp_code_group_LU$Subfamily == "Salmoninae"])%>%
  dplyr::mutate(size_cat = cut(Length_mm, breaks = FL_cutoff, labels = category))

#Frequency of management size categories
Size_freq <- Biological%>%
  dplyr::group_by(WBID, Waterbody_Name, Year, Species, size_cat)%>%
  dplyr::summarize(observations = dplyr::n())%>%
  dplyr::ungroup()

#Convert to wide format
Size_freq = tidyr::spread(Size_freq, size_cat, observations)%>%dplyr::rename(UNK_FL = "<NA>")

#Summarize key stats and counts
stats <- Biological %>% dplyr::group_by(Region, WBID, Waterbody_Name, Year, Capture_Method, Species)%>%
  dplyr::summarize(meanFL = round(mean(Length_mm, na.rm = TRUE),0), 
                   maxFL = max(Length_mm, na.rm = TRUE),
                   Condition = round(mean(K[Outlier == 0 & (Maturity == "IM"|Length_mm < 200)], na.rm = TRUE),2), 
                   total_N = dplyr::n(),
                   aged_N = sum(!is.na(Int.Age)),
                   Maturity_N = sum(Maturity %in% c("IM","M","MT","MR", "SP", "SB","R")&Length_mm>199),
                   p_mature = round(sum(Maturity %in% c("M","MT","MR", "SP", "SB","R")&Length_mm>199)/Maturity_N,2),
                   p_male = round(sum(Sex == "M", na.rm = T)/sum(Sex %in% c("M","F"), na.rm = T),2),
                   clipped_N = sum(!is.na(Clip)))%>%
  dplyr::filter(!is.na(Species), maxFL>1, total_N>1)%>%
  dplyr::ungroup()

stats = dplyr::left_join(stats, Size_freq)

#Convert size category tallies to proprotions
stats = stats%>%dplyr::mutate_at(dplyr::vars('Sub-stock':'UNK_FL'), ~tidyr::replace_na(.,0))%>%
                dplyr::mutate(Stock = round(Stock/total_N, 2), 
                              Quality = round(Quality/total_N, 2), 
                              Trophy = round(Trophy/total_N, 2), 
                              `Sub-stock` = round(`Sub-stock`/total_N, 2))


#_______________________________________________________________________________________________
#Next stage is age assignements

# Idon't think I need these anymore, becasue filtered below anyways
#min.obs = 20 ##Set a minimum sample size standard

#Nlakes<-stats%>%dplyr::filter(total_N >= min.obs, maxFL >= 200)

###FILTER TO Lake-species-year data that has enough information to infer ages on unaged fish.
#At least 30 samples of at least 3 age classes per species in a given year
Bio<-Biological%>%
  dplyr::filter(Outlier == 0)%>%#have to remove outliers for age assignment, so fish at >1400
  dplyr::group_by(Lk_yr_spp)%>%
  dplyr::filter(sum(!is.na(Int.Age))>29, length(unique(na.omit(Int.Age)))>2)%>%
  dplyr::ungroup()%>%droplevels()

FLbin = 25
#Set boundaries to sequence lengths
minFL <- floor(min(Bio$Length_mm[Bio$Outlier ==0], na.rm = TRUE)/FLbin)*FLbin
maxFL <- floor(max(Bio$Length_mm[Bio$Outlier ==0], na.rm = TRUE)/FLbin)*FLbin
lens <- seq(minFL,maxFL,FLbin)#THis vector is used for predicted length-age key

#Note that length categores set within SLD2R() but could change if needed for age assignments
#Biological$lcat=FSA::lencat(Biological$Length_mm,w=FLbin)

#Filter stats to lakes with sufficient sample sizes
Nlakes <- stats%>%dplyr::filter(WBID%in%unique(Bio$WBID))%>%droplevels()

#Age Length Key code adapted from "Introduction to Fisheries Analaysis with R"

age<-vector(mode = "numeric", length = 0)
Bio2 <- cbind.data.frame(as.data.frame(Bio[0,]), age)

# i = Bio$Lk_yr_spp[1]

for(i in unique(Bio$Lk_yr_spp)) {# used to be by WBID
  
  Target_aged <- Bio %>% dplyr::filter(!is.na(Int.Age), Lk_yr_spp == i)
  Target_unaged <- Bio %>% dplyr::filter(is.na(Int.Age)&!is.na(Length_mm), Lk_yr_spp == i)
  
  #Multinomial smoothing fit of age-length key
  Target.mlr <- nnet::multinom(Int.Age~lcat,data=Target_aged,maxit=500)
  
 #Resulting Age-Length-Key
  alk.sm <- predict(Target.mlr,data.frame(lcat=lens),type="probs")
  row.names(alk.sm) <- lens # for clarity
  
  ##Assign ages to individuals
  if(nrow(Target_unaged)>0){
    Target_unaged <- FSA::alkIndivAge(alk.sm, ~Length_mm, data = Target_unaged)
  }
  Target_aged$age <- Target_aged$Int.Age
  Bio2 <- rbind.data.frame(Target_unaged, Target_aged, Bio2)
}


 tmp <- Bio2%>%dplyr::mutate(age = ifelse(age > 7, "8old", age)) %>%
   dplyr::group_by(WBID, Waterbody_Name, Year, Species, age)%>%
   dplyr::summarize(observations = dplyr::n(), mFL = round(mean(Length_mm)))%>%
   dplyr::ungroup()
 
 #Create a wide tally of numbers by age
 tmpn = tmp %>%dplyr::select(-mFL)%>%dplyr::mutate(age=paste('n', age, sep=".")) %>% tidyr::spread(age, observations)
 #Create a wide summary of mean size by age
 tmpFL = tmp %>%dplyr::select(-observations)%>%dplyr::mutate(age=paste('FL', age, sep=".")) %>% tidyr::spread(age, mFL)
 
 #Create a wide summary of numbers and size and insert 0s for NA observations
 tmp <- dplyr::left_join(tmpn,tmpFL)%>%
        dplyr::mutate_at(dplyr::vars('n.0':'n.8old'), ~tidyr::replace_na(.,0))
 
 #last add some descriptive statisitics related to mortality
 tmp <- tmp %>%dplyr::mutate(Ratio_2_to_older = round(n.2/dplyr::select(., n.2:n.8old)%>%
                                                        apply(1, sum, na.rm=TRUE),2), 
                             M = round(.9*Ratio_2_to_older+.05,2))
 
 #Join back in with Overall lake stats
 tmp = dplyr::left_join(Nlakes, tmp)
 
 #Filter to populations with some observations of fish between age 2 to 5.
 POP_STR<-tmp%>%dplyr::rowwise()%>%dplyr::filter(0<(sum(n.2,n.3,n.4,n.5, na.rm = TRUE)))

POP_STR <<- POP_STR
}