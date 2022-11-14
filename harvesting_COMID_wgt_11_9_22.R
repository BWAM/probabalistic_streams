#Keleigh Reynolds
#11/9/2022
#grab EPA files for COMID and weight

library("dplyr")												
library("plyr")												
library("readr")
library("readxl")

# prob_data <- list.files(path = "C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Random_Probabalistic_Assignment/Possible Relevant Historical Data/epa_draw_all",	
#                        pattern = "*.xlsx",
#                        full.names = TRUE) #this isn;t working, but itcould

prob_1<-readxl::read_excel(path=here::here("data/NY_Basin_Design_2018_SummaryNotesFromSiteSelection.xlsx"),
                           sheet = 1)
prob_2<-readxl::read_excel(path=here::here("data/NY_Basin_Design_2013_SummaryNotesFromSiteSelection.xlsx"),
                           sheet = 1)
prob_3<-readxl::read_excel(path=here::here("data/NY_Basin_2008_gis_export.xlsx"),
                           sheet = 1)


#select columns from the dataframes
prob_1_short<-prob_1 %>% 
  select(COMID,wgt) %>% 
  mutate(draw_year="2018_2022")

prob_2_short<-prob_2 %>% 
  select(COMID,wgt)%>% 
  mutate(draw_year="2013_2018")

prob_3_short<-prob_3 %>% 
  select(COMID,wgt)%>% 
  mutate(draw_year="2008_2013")


#bind them together
prob_all<-rbind(prob_1_short,prob_2_short,prob_3_short)

prob_all<-prob_all %>% 
  distinct() #take out any that are duplicated-although there are, we may not want
#to do the draw year?
#look at the duplicates
 
dups <-prob_all[duplicated(prob_all$COMID)|duplicated(prob_all$COMID, fromLast=TRUE),]
dups
#so i thinnk we'll have to bind by draw year AND COMID, since they seem duplicated. 


write.csv(prob_all,"outputs/comid_wgt_all_years.csv")

