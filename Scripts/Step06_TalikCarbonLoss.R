#### Assigning a percent of carbon loss from talik carbon mass 
## Using the Gerrevink carbon loss model 

library(tidyverse)
library(sf)
library(terra)

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
  rename(YSF = YearsSinceFire_or_ThawInitation)

##Upload 
###Remove the perimeters with no talik area 
###Find a thaw initation year to match with the gerrevink timeframe 
##separate by cold,intermediate and warm models 
#Add up to 50 years (keeping the talik persistent)

###Cold Model Fires
TalikCarbon_Total_OnlyTalik_1m_cold <- TalikCarbon_Total%>%
  filter(TalikModel == "Cold")%>%
  filter(talik_area_acres != 0)%>%
  select(FIRENUMBER, FireName, YSF, Talik_m, CarbonStorage_kg_1m, TalikModel, Scenario)

TalikCarbon_Total_OnlyTalik_1m_cold_50 <- TalikCarbon_Total_OnlyTalik_1m_cold%>%
  group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  complete(YSF = 0:50)%>%
  fill(CarbonStorage_kg_1m, .direction = "down") %>%
  fill(Talik_m, .direction = "down") 
  
TalikCarbon_Total_OnlyTalik_1m_cold_50_percent <- left_join(TalikCarbon_Total_OnlyTalik_1m_cold_50, percentcarbonloss, by = c("YSF"))

##find Carbon loss 
TalikCarbon_Total_OnlyTalik_1m_cold_50_percent <- TalikCarbon_Total_OnlyTalik_1m_cold_50_percent%>%
  mutate(Carbonloss_kg_1m = (CarbonStorage_kg_1m*percentCarbonLoss_organic)/100)%>%
  ungroup()

TalikCarbon_Total_OnlyTalik_1m_cold_50_sum <- TalikCarbon_Total_OnlyTalik_1m_cold_50_percent%>%
 group_by(FIRENUMBER, FireName, Scenario, TalikModel)%>%
  summarize(sum_Carbonloss_kg_1m = sum(Carbonloss_kg_1m), CarbonStorage_kg_1m = CarbonStorage_kg_1m)%>%
  ungroup()



            