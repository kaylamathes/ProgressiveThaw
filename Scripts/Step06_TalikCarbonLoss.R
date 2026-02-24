#### Assigning a percent of carbon loss from talik carbon mass 
## Using the Gerrevink carbon loss model 

library(tidyverse)
library(sf)
library(terra)
options(scipen = 999)

##upland dataframes from Step 05 

TalikCarbon_High <- read_csv("Output/TalikCarbonMass/TalikCarbon_high.csv")%>%
  mutate(Scenario = "High")
TalikCarbon_Medium <- read_csv("Output/TalikCarbonMass/TalikCarbon_medium.csv")%>%
  mutate(Scenario = "Medium")
TalikCarbon_Low <- read_csv("Output/TalikCarbonMass/TalikCarbon_Low.csv")%>%
  mutate(Scenario = "Low")

TalikCarbon_Total <- rbind(TalikCarbon_High,TalikCarbon_Medium,TalikCarbon_Low)

##upload the gerrevink Carbon loss percentages (Still need to assign carbon % type to assume either high carbon or mineral soils)

percentcarbonloss <- read.csv("Data/CarbonDensity/Gerrevink_percentcarbonloss.csv")%>%
  rename(YST = YearsSinceFire_or_ThawInitation)

##Upload 
###Remove the perimeters with no talik area 
###Find a thaw initation year to match with the gerrevink timeframe 
##separate by cold,intermediate and warm models 
#Add up to 50 years (keeping the talik persistent)

############################################################First 1m depth ####################
######Cold Model Fires
TalikCarbon_Total_1m_cold <- TalikCarbon_Total%>%
  filter(TalikModel == "Cold")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_1m, TalikModel, Scenario)

TalikCarbon_Total_1m_cold_50 <- TalikCarbon_Total_1m_cold%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_1m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_1m_cold_50_YST <- TalikCarbon_Total_1m_cold_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  mutate(YST = -20:30)

TalikCarbon_Total_1m_cold_50_percent <- left_join(TalikCarbon_Total_1m_cold_50_YST, percentcarbonloss, by = c("YST"))

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_1m_cold_50_percent <- TalikCarbon_Total_1m_cold_50_percent%>%
  mutate(Carbonloss_kg_1m = (CarbonStorage_kg_1m*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_1m =0))
  
  
TalikCarbon_Total_1m_cold_50_sum <- TalikCarbon_Total_1m_cold_50_percent%>%
 group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_1m = sum(Carbonloss_kg_1m))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_1m_cold_check <- TalikCarbon_Total_1m_cold_50_percent%>%
  filter(YST == 30)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_1m)

TalikCarbon_Total_1m_cold_50_sum_withtotal <- left_join(TalikCarbon_Total_1m_cold_check,TalikCarbon_Total_1m_cold_50_sum, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal = sum_Carbonloss_kg_1m/CarbonStorage_kg_1m)

####
###############Intermediate Model Fires (1m depth)

TalikCarbon_Total_1m_Intermediate <- TalikCarbon_Total%>%
  filter(TalikModel == "Intermediate")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_1m, TalikModel, Scenario)

TalikCarbon_Total_1m_Intermediate_50 <- TalikCarbon_Total_1m_Intermediate%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_1m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw Iitation to align up with the carbon loss model 
TalikCarbon_Total_1m_Intermediate_50_YST <- TalikCarbon_Total_1m_Intermediate_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  mutate(YST = -11:39)

TalikCarbon_Total_1m_Intermediate_50_percent <- left_join(TalikCarbon_Total_1m_Intermediate_50_YST, percentcarbonloss, by = c("YST"))

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_1m_Intermediate_50_percent <- TalikCarbon_Total_1m_Intermediate_50_percent%>%
  mutate(Carbonloss_kg_1m = (CarbonStorage_kg_1m*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_1m =0))


TalikCarbon_Total_1m_Intermediate_50_sum <- TalikCarbon_Total_1m_Intermediate_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_1m = sum(Carbonloss_kg_1m))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_1m_Intermediate_check <- TalikCarbon_Total_1m_Intermediate_50_percent%>%
  filter(YST == 30)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_1m)

TalikCarbon_Total_1m_Intermediate_50_sum_withtotal <- left_join(TalikCarbon_Total_1m_Intermediate_check,TalikCarbon_Total_1m_Intermediate_50_sum, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal = sum_Carbonloss_kg_1m/CarbonStorage_kg_1m)

 
####
############### Warm Model Fires (1m Depth)

TalikCarbon_Total_1m_Warm <- TalikCarbon_Total%>%
  filter(TalikModel == "Warm")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_1m, TalikModel, Scenario)

TalikCarbon_Total_1m_Warm_50 <- TalikCarbon_Total_1m_Warm%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_1m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_1m_Warm_50_YST <- TalikCarbon_Total_1m_Warm_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  mutate(YST = -6:44)

TalikCarbon_Total_1m_Warm_50_percent <- left_join(TalikCarbon_Total_1m_Warm_50_YST, percentcarbonloss, by = c("YST"))

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_1m_Warm_50_percent <- TalikCarbon_Total_1m_Warm_50_percent%>%
  mutate(Carbonloss_kg_1m = (CarbonStorage_kg_1m*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_1m =0))


TalikCarbon_Total_1m_Warm_50_sum <- TalikCarbon_Total_1m_Warm_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_1m = sum(Carbonloss_kg_1m))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_1m_Warm_check <- TalikCarbon_Total_1m_Warm_50_percent%>%
  filter(YST == 30)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_1m)

TalikCarbon_Total_1m_Warm_50_sum_withtotal <- left_join(TalikCarbon_Total_1m_Warm_check,TalikCarbon_Total_1m_Warm_50_sum, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal = sum_Carbonloss_kg_1m/CarbonStorage_kg_1m)

##################### 2 m Depth ################################
######Cold Model Fires 2m Depth
TalikCarbon_Total_2m_cold <- TalikCarbon_Total%>%
  filter(TalikModel == "Cold")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_2m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_2m_cold_50 <- TalikCarbon_Total_2m_cold%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_2m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_2m_cold_50_YST <- TalikCarbon_Total_2m_cold_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -20:30)

TalikCarbon_Total_2m_cold_50_percent <- left_join(TalikCarbon_Total_2m_cold_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_2m_cold_50_percent_extra <- TalikCarbon_Total_2m_cold_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(
    CarbonStorage_kg_2m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 29 ~
      sum(CarbonStorage_kg_2m[YST == 1]) - sum(CarbonStorage_kg_2m[YST == 0])))%>%
  mutate(CarbonStorage_kg_2m_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
    sum(CarbonStorage_kg_2m[YST == 0])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_2m_cold_50_percent_extra <- TalikCarbon_Total_2m_cold_50_percent_extra%>%
  mutate(Carbonloss_kg_2m_step1 = (CarbonStorage_kg_2m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_2m_step2 = (CarbonStorage_kg_2m_step2*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_2m_step1 =0,Carbonloss_kg_2m_step2 =0))


TalikCarbon_Total_2m_cold_50_sum_extra <- TalikCarbon_Total_2m_cold_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_2m_step1 = sum(Carbonloss_kg_2m_step1),sum_Carbonloss_kg_2m_step2 = sum(Carbonloss_kg_2m_step2))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_2m_cold_check_extra <- TalikCarbon_Total_2m_cold_50_percent_extra%>%
  filter(YST == 29)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_2m_step1, CarbonStorage_kg_2m_step2)

TalikCarbon_Total_2m_cold_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_2m_cold_check_extra,TalikCarbon_Total_2m_cold_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_2m_step1/CarbonStorage_kg_2m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_2m_step2/CarbonStorage_kg_2m_step2)




######Intermediate Model Fires 2m Depth
TalikCarbon_Total_2m_Intermediate <- TalikCarbon_Total%>%
  filter(TalikModel == "Intermediate")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_2m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_2m_Intermediate_50 <- TalikCarbon_Total_2m_Intermediate%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_2m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_2m_Intermediate_50_YST <- TalikCarbon_Total_2m_Intermediate_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -11:39)

TalikCarbon_Total_2m_Intermediate_50_percent <- left_join(TalikCarbon_Total_2m_Intermediate_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_2m_Intermediate_50_percent_extra <- TalikCarbon_Total_2m_Intermediate_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(
    CarbonStorage_kg_2m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 38 ~
                                            sum(CarbonStorage_kg_2m[YST == 1]) - sum(CarbonStorage_kg_2m[YST == 0])))%>%
  mutate(CarbonStorage_kg_2m_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                 sum(CarbonStorage_kg_2m[YST == 0])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_2m_Intermediate_50_percent_extra <- TalikCarbon_Total_2m_Intermediate_50_percent_extra%>%
  mutate(Carbonloss_kg_2m_step1 = (CarbonStorage_kg_2m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_2m_step2 = (CarbonStorage_kg_2m_step2*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_2m_step1 =0,Carbonloss_kg_2m_step2 =0))


TalikCarbon_Total_2m_Intermediate_50_sum_extra <- TalikCarbon_Total_2m_Intermediate_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_2m_step1 = sum(Carbonloss_kg_2m_step1),sum_Carbonloss_kg_2m_step2 = sum(Carbonloss_kg_2m_step2))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_2m_Intermediate_check_extra <- TalikCarbon_Total_2m_Intermediate_50_percent_extra%>%
  filter(YST == 38)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_2m_step1, CarbonStorage_kg_2m_step2)

TalikCarbon_Total_2m_Intermediate_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_2m_Intermediate_check_extra,TalikCarbon_Total_2m_Intermediate_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_2m_step1/CarbonStorage_kg_2m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_2m_step2/CarbonStorage_kg_2m_step2)




######Warm Model Fires 2m Depth
TalikCarbon_Total_2m_Warm <- TalikCarbon_Total%>%
  filter(TalikModel == "Warm")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_2m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_2m_Warm_50 <- TalikCarbon_Total_2m_Warm%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_2m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_2m_Warm_50_YST <- TalikCarbon_Total_2m_Warm_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -6:44)

TalikCarbon_Total_2m_Warm_50_percent <- left_join(TalikCarbon_Total_2m_Warm_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again Substract the YST from each subsequent step for the length of time within 50 years post fire that carbon has been thawed 
TalikCarbon_Total_2m_Warm_50_percent_extra <- TalikCarbon_Total_2m_Warm_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(
    CarbonStorage_kg_2m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 43~
                                            sum(CarbonStorage_kg_2m[YST == 1]) - sum(CarbonStorage_kg_2m[YST == 0])))%>%
  mutate(CarbonStorage_kg_2m_step1 = case_when(talik_area_acres > 0 & YST >= 0  ~
                                                 sum(CarbonStorage_kg_2m[YST == 0])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_2m_Warm_50_percent_extra <- TalikCarbon_Total_2m_Warm_50_percent_extra%>%
  mutate(Carbonloss_kg_2m_step1 = (CarbonStorage_kg_2m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_2m_step2 = (CarbonStorage_kg_2m_step2*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_2m_step1 =0,Carbonloss_kg_2m_step2 =0))


TalikCarbon_Total_2m_Warm_50_sum_extra <- TalikCarbon_Total_2m_Warm_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_2m_step1 = sum(Carbonloss_kg_2m_step1),sum_Carbonloss_kg_2m_step2 = sum(Carbonloss_kg_2m_step2))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_2m_Warm_check_extra <- TalikCarbon_Total_2m_Warm_50_percent_extra%>%
  filter(YST == 43)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_2m_step1, CarbonStorage_kg_2m_step2)

TalikCarbon_Total_2m_Warm_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_2m_Warm_check_extra,TalikCarbon_Total_2m_Warm_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_2m_step1/CarbonStorage_kg_2m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_2m_step2/CarbonStorage_kg_2m_step2)




##################### 3 m Depth ################################
######Cold Model Fires 3m Depth
TalikCarbon_Total_3m_cold <- TalikCarbon_Total%>%
  filter(TalikModel == "Cold")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_3m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_3m_cold_50 <- TalikCarbon_Total_3m_cold%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_3m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_3m_cold_50_YST <- TalikCarbon_Total_3m_cold_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -21:29)

TalikCarbon_Total_3m_cold_50_percent <- left_join(TalikCarbon_Total_3m_cold_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_3m_cold_50_percent_extra <- TalikCarbon_Total_3m_cold_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(CarbonStorage_kg_3m_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                 sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 28 ~
                                            sum(CarbonStorage_kg_3m[YST == 1]) - sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step3 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 27 ~
                                                 sum(CarbonStorage_kg_3m[YST == 2]) - sum(CarbonStorage_kg_3m[YST == 1])))%>%
  mutate(CarbonStorage_kg_3m_step4 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 26~
                                                 sum(CarbonStorage_kg_3m[YST == 3]) - sum(CarbonStorage_kg_3m[YST == 2])))%>%
  mutate(CarbonStorage_kg_3m_step5 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 24~
                                                 sum(CarbonStorage_kg_3m[YST == 4]) - sum(CarbonStorage_kg_3m[YST == 3])))%>%
  mutate(CarbonStorage_kg_3m_step6 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 23 ~
                                                 sum(CarbonStorage_kg_3m[YST == 5]) - sum(CarbonStorage_kg_3m[YST == 4])))%>%
  mutate(CarbonStorage_kg_3m_step7 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 22 ~
                                                 sum(CarbonStorage_kg_3m[YST == 6]) - sum(CarbonStorage_kg_3m[YST == 5])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_3m_cold_50_percent_extra <- TalikCarbon_Total_3m_cold_50_percent_extra%>%
  mutate(Carbonloss_kg_3m_step1 = (CarbonStorage_kg_3m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step2 = (CarbonStorage_kg_3m_step2*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step3 = (CarbonStorage_kg_3m_step3*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step4 = (CarbonStorage_kg_3m_step4*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step5 = (CarbonStorage_kg_3m_step5*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step6 = (CarbonStorage_kg_3m_step6*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step7 = (CarbonStorage_kg_3m_step7*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_3m_step1 =0,Carbonloss_kg_3m_step2 =0,Carbonloss_kg_3m_step3 =0,
                  Carbonloss_kg_3m_step4 =0,Carbonloss_kg_3m_step5 =0,Carbonloss_kg_3m_step6 =0,
                  Carbonloss_kg_3m_step7 =0))


TalikCarbon_Total_3m_cold_50_sum_extra <- TalikCarbon_Total_3m_cold_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_3m_step1 = sum(Carbonloss_kg_3m_step1),sum_Carbonloss_kg_3m_step2 = sum(Carbonloss_kg_3m_step2),
            sum_Carbonloss_kg_3m_step3 = sum(Carbonloss_kg_3m_step3),sum_Carbonloss_kg_3m_step4 = sum(Carbonloss_kg_3m_step4),
            sum_Carbonloss_kg_3m_step5 = sum(Carbonloss_kg_3m_step5),sum_Carbonloss_kg_3m_step6 = sum(Carbonloss_kg_3m_step6),
            sum_Carbonloss_kg_3m_step7 = sum(Carbonloss_kg_3m_step7))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_3m_cold_check_extra <- TalikCarbon_Total_3m_cold_50_percent_extra%>%
  filter(YST == 22)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_3m_step1, CarbonStorage_kg_3m_step2,
                CarbonStorage_kg_3m_step3,CarbonStorage_kg_3m_step4,CarbonStorage_kg_3m_step5,CarbonStorage_kg_3m_step6,
                CarbonStorage_kg_3m_step7)

TalikCarbon_Total_3m_cold_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_3m_cold_check_extra,TalikCarbon_Total_3m_cold_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_3m_step1/CarbonStorage_kg_3m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_3m_step2/CarbonStorage_kg_3m_step2)%>%
  mutate(percentTotal_step3 = sum_Carbonloss_kg_3m_step3/CarbonStorage_kg_3m_step3)%>%
  mutate(percentTotal_step4 = sum_Carbonloss_kg_3m_step4/CarbonStorage_kg_3m_step4)%>%
  mutate(percentTotal_step5 = sum_Carbonloss_kg_3m_step5/CarbonStorage_kg_3m_step5)%>%
  mutate(percentTotal_step6 = sum_Carbonloss_kg_3m_step6/CarbonStorage_kg_3m_step6)%>%
  mutate(percentTotal_step7 = sum_Carbonloss_kg_3m_step7/CarbonStorage_kg_3m_step7)



######Intermediate Model Fires 3m Depth
TalikCarbon_Total_3m_Intermediate <- TalikCarbon_Total%>%
  filter(TalikModel == "Intermediate")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_3m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_3m_Intermediate_50 <- TalikCarbon_Total_3m_Intermediate%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_3m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_3m_Intermediate_50_YST <- TalikCarbon_Total_3m_Intermediate_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -12:38)

TalikCarbon_Total_3m_Intermediate_50_percent <- left_join(TalikCarbon_Total_3m_Intermediate_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_3m_Intermediate_50_percent_extra <- TalikCarbon_Total_3m_Intermediate_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(CarbonStorage_kg_3m_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                 sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 37 ~
                                                 sum(CarbonStorage_kg_3m[YST == 1]) - sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step3 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 36 ~
                                                 sum(CarbonStorage_kg_3m[YST == 2]) - sum(CarbonStorage_kg_3m[YST == 1])))%>%
  mutate(CarbonStorage_kg_3m_step4 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 35~
                                                 sum(CarbonStorage_kg_3m[YST == 3]) - sum(CarbonStorage_kg_3m[YST == 2])))%>%
  mutate(CarbonStorage_kg_3m_step5 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 34~
                                                 sum(CarbonStorage_kg_3m[YST == 4]) - sum(CarbonStorage_kg_3m[YST == 3])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_3m_Intermediate_50_percent_extra <- TalikCarbon_Total_3m_Intermediate_50_percent_extra%>%
  mutate(Carbonloss_kg_3m_step1 = (CarbonStorage_kg_3m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step2 = (CarbonStorage_kg_3m_step2*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step3 = (CarbonStorage_kg_3m_step3*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step4 = (CarbonStorage_kg_3m_step4*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step5 = (CarbonStorage_kg_3m_step5*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_3m_step1 =0,Carbonloss_kg_3m_step2 =0,Carbonloss_kg_3m_step3 =0,
                  Carbonloss_kg_3m_step4 =0,Carbonloss_kg_3m_step5 =0))


TalikCarbon_Total_3m_Intermediate_50_sum_extra <- TalikCarbon_Total_3m_Intermediate_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_3m_step1 = sum(Carbonloss_kg_3m_step1),sum_Carbonloss_kg_3m_step2 = sum(Carbonloss_kg_3m_step2),
            sum_Carbonloss_kg_3m_step3 = sum(Carbonloss_kg_3m_step3),sum_Carbonloss_kg_3m_step4 = sum(Carbonloss_kg_3m_step4),
            sum_Carbonloss_kg_3m_step5 = sum(Carbonloss_kg_3m_step5))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_3m_Intermediate_check_extra <- TalikCarbon_Total_3m_Intermediate_50_percent_extra%>%
  filter(YST == 34)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_3m_step1, CarbonStorage_kg_3m_step2,
                CarbonStorage_kg_3m_step3,CarbonStorage_kg_3m_step4,CarbonStorage_kg_3m_step5)

TalikCarbon_Total_3m_Intermediate_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_3m_Intermediate_check_extra,TalikCarbon_Total_3m_Intermediate_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_3m_step1/CarbonStorage_kg_3m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_3m_step2/CarbonStorage_kg_3m_step2)%>%
  mutate(percentTotal_step3 = sum_Carbonloss_kg_3m_step3/CarbonStorage_kg_3m_step3)%>%
  mutate(percentTotal_step4 = sum_Carbonloss_kg_3m_step4/CarbonStorage_kg_3m_step4)%>%
  mutate(percentTotal_step5 = sum_Carbonloss_kg_3m_step5/CarbonStorage_kg_3m_step5)




######Warm Model Fires 3m Depth
TalikCarbon_Total_3m_Warm <- TalikCarbon_Total%>%
  filter(TalikModel == "Warm")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_3m, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_3m_Warm_50 <- TalikCarbon_Total_3m_Warm%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_3m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_3m_Warm_50_YST <- TalikCarbon_Total_3m_Warm_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -7:43)

TalikCarbon_Total_3m_Warm_50_percent <- left_join(TalikCarbon_Total_3m_Warm_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_3m_Warm_50_percent_extra <- TalikCarbon_Total_3m_Warm_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(CarbonStorage_kg_3m_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                 sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 42 ~
                                                 sum(CarbonStorage_kg_3m[YST == 1]) - sum(CarbonStorage_kg_3m[YST == 0])))%>%
  mutate(CarbonStorage_kg_3m_step3 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 41 ~
                                                 sum(CarbonStorage_kg_3m[YST == 2]) - sum(CarbonStorage_kg_3m[YST == 1])))%>%
  mutate(CarbonStorage_kg_3m_step4 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 40~
                                                 sum(CarbonStorage_kg_3m[YST == 3]) - sum(CarbonStorage_kg_3m[YST == 2])))%>%
  mutate(CarbonStorage_kg_3m_step5 = case_when(talik_area_acres > 0 & YST >= 0  & YST <= 39~
                                                 sum(CarbonStorage_kg_3m[YST == 4]) - sum(CarbonStorage_kg_3m[YST == 3])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_3m_Warm_50_percent_extra <- TalikCarbon_Total_3m_Warm_50_percent_extra%>%
  mutate(Carbonloss_kg_3m_step1 = (CarbonStorage_kg_3m_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step2 = (CarbonStorage_kg_3m_step2*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step3 = (CarbonStorage_kg_3m_step3*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step4 = (CarbonStorage_kg_3m_step4*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_3m_step5 = (CarbonStorage_kg_3m_step5*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_3m_step1 =0,Carbonloss_kg_3m_step2 =0,Carbonloss_kg_3m_step3 =0,
                  Carbonloss_kg_3m_step4 =0,Carbonloss_kg_3m_step5 =0))


TalikCarbon_Total_3m_Warm_50_sum_extra <- TalikCarbon_Total_3m_Warm_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_3m_step1 = sum(Carbonloss_kg_3m_step1),sum_Carbonloss_kg_3m_step2 = sum(Carbonloss_kg_3m_step2),
            sum_Carbonloss_kg_3m_step3 = sum(Carbonloss_kg_3m_step3),sum_Carbonloss_kg_3m_step4 = sum(Carbonloss_kg_3m_step4),
            sum_Carbonloss_kg_3m_step5 = sum(Carbonloss_kg_3m_step5))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_3m_Warm_check_extra <- TalikCarbon_Total_3m_Warm_50_percent_extra%>%
  filter(YST == 39)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_3m_step1, CarbonStorage_kg_3m_step2,
                CarbonStorage_kg_3m_step3,CarbonStorage_kg_3m_step4,CarbonStorage_kg_3m_step5)

TalikCarbon_Total_3m_Warm_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_3m_Warm_check_extra,TalikCarbon_Total_3m_Warm_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_3m_step1/CarbonStorage_kg_3m_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_3m_step2/CarbonStorage_kg_3m_step2)%>%
  mutate(percentTotal_step3 = sum_Carbonloss_kg_3m_step3/CarbonStorage_kg_3m_step3)%>%
  mutate(percentTotal_step4 = sum_Carbonloss_kg_3m_step4/CarbonStorage_kg_3m_step4)%>%
  mutate(percentTotal_step5 = sum_Carbonloss_kg_3m_step5/CarbonStorage_kg_3m_step5)



##################### greater than 3 m Depth ################################
######Cold Model Fires > 3m Depth
TalikCarbon_Total_deep_cold <- TalikCarbon_Total%>%
  filter(TalikModel == "Cold")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_deep, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_deep_cold_50 <- TalikCarbon_Total_deep_cold%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_deep, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_deep_cold_50_YST <- TalikCarbon_Total_deep_cold_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -28:22)

TalikCarbon_Total_deep_cold_50_percent <- left_join(TalikCarbon_Total_deep_cold_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_deep_cold_50_percent_extra <- TalikCarbon_Total_deep_cold_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(CarbonStorage_kg_deep_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                 sum(CarbonStorage_kg_deep[YST == 0])))%>%
  mutate(CarbonStorage_kg_deep_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 21 ~
                                                 sum(CarbonStorage_kg_deep[YST == 1]) - sum(CarbonStorage_kg_deep[YST == 0])))%>%
  mutate(CarbonStorage_kg_deep_step3 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 20 ~
                                                 sum(CarbonStorage_kg_deep[YST == 2]) - sum(CarbonStorage_kg_deep[YST == 1])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_deep_cold_50_percent_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  mutate(Carbonloss_kg_deep_step1 = (CarbonStorage_kg_deep_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_deep_step2 = (CarbonStorage_kg_deep_step2*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_deep_step3 = (CarbonStorage_kg_deep_step3*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_deep_step1 =0,Carbonloss_kg_deep_step2 =0,Carbonloss_kg_deep_step3 =0))


TalikCarbon_Total_deep_cold_50_sum_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_deep_step1 = sum(Carbonloss_kg_deep_step1),sum_Carbonloss_kg_deep_step2 = sum(Carbonloss_kg_deep_step2),
            sum_Carbonloss_kg_deep_step3 = sum(Carbonloss_kg_deep_step3))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_deep_cold_check_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  filter(YST == 20)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_deep_step1, CarbonStorage_kg_deep_step2,
                CarbonStorage_kg_deep_step3)

TalikCarbon_Total_deep_cold_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_deep_cold_check_extra,TalikCarbon_Total_deep_cold_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_deep_step1/CarbonStorage_kg_deep_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_deep_step2/CarbonStorage_kg_deep_step2)%>%
  mutate(percentTotal_step3 = sum_Carbonloss_kg_deep_step3/CarbonStorage_kg_deep_step3)



######Intermediate Model Fires > 3m Depth

TalikCarbon_Total_deep_Intermediate <- TalikCarbon_Total%>%
  filter(TalikModel == "Intermediate")%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_deep, TalikModel, Scenario, talik_area_acres)

TalikCarbon_Total_deep_Intermediate_50 <- TalikCarbon_Total_deep_Intermediate%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel, talik_area_acres)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_deep, .direction = "down") %>%
  fill(Talik_m, .direction = "down") %>%
  ungroup()

###Need to create a "Time since thaw INitation to align up with the carbon loss model 
TalikCarbon_Total_deep_Intermediate_50_YST <- TalikCarbon_Total_deep_Intermediate_50%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres)%>%
  mutate(YST = -17:33)


TalikCarbon_Total_deep_Intermediate_50_percent <- left_join(TalikCarbon_Total_deep_Intermediate_50_YST, percentcarbonloss, by = c("YST"))

##Adding an extra column for the additions to the thaw depth so they can start the carbon loss progression over again 
TalikCarbon_Total_deep_Intermediate_50_percent_extra <- TalikCarbon_Total_deep_Intermediate_50_percent%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel,talik_area_acres) %>%
  mutate(CarbonStorage_kg_deep_step1 = case_when(talik_area_acres > 0 & YST >= 0 ~
                                                   sum(CarbonStorage_kg_deep[YST == 0])))%>%
  mutate(CarbonStorage_kg_deep_step2 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 21 ~
                                                   sum(CarbonStorage_kg_deep[YST == 1]) - sum(CarbonStorage_kg_deep[YST == 0])))%>%
  mutate(CarbonStorage_kg_deep_step3 = case_when(talik_area_acres > 0 & YST >= 0 & YST <= 20 ~
                                                   sum(CarbonStorage_kg_deep[YST == 2]) - sum(CarbonStorage_kg_deep[YST == 1])))%>%
  ungroup()

##find Carbon loss (Turn NAs in zero for Carbon loss)
TalikCarbon_Total_deep_cold_50_percent_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  mutate(Carbonloss_kg_deep_step1 = (CarbonStorage_kg_deep_step1*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_deep_step2 = (CarbonStorage_kg_deep_step2*percentCarbonLoss_organic)/100)%>%
  mutate(Carbonloss_kg_deep_step3 = (CarbonStorage_kg_deep_step3*percentCarbonLoss_organic)/100)%>%
  ungroup()%>%
  replace_na(list(Carbonloss_kg_deep_step1 =0,Carbonloss_kg_deep_step2 =0,Carbonloss_kg_deep_step3 =0))


TalikCarbon_Total_deep_cold_50_sum_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_deep_step1 = sum(Carbonloss_kg_deep_step1),sum_Carbonloss_kg_deep_step2 = sum(Carbonloss_kg_deep_step2),
            sum_Carbonloss_kg_deep_step3 = sum(Carbonloss_kg_deep_step3))%>%
  ungroup()

##check to see the percent of total loss over 50 years as compared to total carbon storage within the depth 
TalikCarbon_Total_deep_cold_check_extra <- TalikCarbon_Total_deep_cold_50_percent_extra%>%
  filter(YST == 20)%>%
  dplyr::select(FIRENUMBER, FireName, Scenario, TalikModel, CarbonStorage_kg_deep_step1, CarbonStorage_kg_deep_step2,
                CarbonStorage_kg_deep_step3)

TalikCarbon_Total_deep_cold_50_sum_withtotal_extra <- left_join(TalikCarbon_Total_deep_cold_check_extra,TalikCarbon_Total_deep_cold_50_sum_extra, by = c("FIRENUMBER", "FireName", "Scenario", "TalikModel"))%>%
  mutate(percentTotal_step1 = sum_Carbonloss_kg_deep_step1/CarbonStorage_kg_deep_step1)%>%
  mutate(percentTotal_step2 = sum_Carbonloss_kg_deep_step2/CarbonStorage_kg_deep_step2)%>%
  mutate(percentTotal_step3 = sum_Carbonloss_kg_deep_step3/CarbonStorage_kg_deep_step3)

