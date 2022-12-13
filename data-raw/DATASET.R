## code to prepare `DATASET` dataset goes here

#Example to load csv
#Strain_code_LU = read.csv("C:/Users/paul.askey/OneDrive - Freshwater Fisheries Society of B.C/FFSBC work docs/Size and Strain Team/SPDT/data-raw/Strain_code_LU.csv",
#                              fileEncoding="UTF-8-BOM")

#Then load to data folder.
#usethis::use_data(Strain_code_LU, overwrite = TRUE)

                              
                              #strsppLU <-RODBC::sqlFetch(ch,"ffsbc.lookup_strain_species")
                              #strLU <-RODBC::sqlFetch(ch,"shiny.lookup_strain_code")
                              #sqlTables(ch)
                              #Releases <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_releases")
                              #Releases2 <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_releases_2")
                              #Releases3 <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_releases_BU")
                              #Requests <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_requests")
                              
#strsppLU <-Releases%>%group_by(Species, stock_strain_loc_name, Strain)%>%summarize(N = n())
#write.csv(strsppLU,"strsppLU.csv", row.names = F)
