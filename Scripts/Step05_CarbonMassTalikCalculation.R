####Find the carbon mass of the talik formation (from thawed permafrost pool)

##library
library(sf)
library(tidyverse)
library(ggplot2)

options(scipen = 999)

##Bring in the thawed permafrost from talik volume datasets for each scenario 
##Low Scenario 
Volume_low_cold <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_low_cold_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Cold")

Volume_low_Intermediate <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_low_Intermediate_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Intermediate")

Volume_low_warm <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_low_warm_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Warm")

Volume_low_total <- rbind(Volume_low_cold,Volume_low_Intermediate,Volume_low_warm)

##Medium Scenario 
Volume_medium_cold <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_cold_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Cold")

Volume_medium_Intermediate <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_Intermediate_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Intermediate")

Volume_medium_warm <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_warm_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Warm")

Volume_medium_total <- rbind(Volume_medium_cold,Volume_medium_Intermediate,Volume_medium_warm)

##High Scenario 
Volume_high_cold <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_high_cold_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Cold")

Volume_high_Intermediate <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_high_Intermediate_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Intermediate")

Volume_high_warm <- read.csv("Output/Counterfactual_TalikThawedPermafrost/AllFire_high_warm_depth.csv")%>%
  select(!X.1)%>%select(!X)%>%select(!Valid)%>%select(!Volume_check)%>%select(!Volume_check2)%>%
  mutate(TalikModel = "Warm")

Volume_high_total <- rbind(Volume_high_cold,Volume_high_Intermediate,Volume_high_warm)
  

###Carbon Density Data upload



BurmanLake <- read.csv("Output/Counterfactual_CarbonDensity/BurmanLake_CarbonDensityIntersectionTotal.csv")
ChloyaLakes <- read.csv("Output/Counterfactual_CarbonDensity/ChloyaLakes_CarbonDensityIntersectionTotal.csv")%>%
  dplyr::select(!FireName)%>%
  mutate(FireName = "Chloya Lake")
BrooksCreek <- read.csv("Output/Counterfactual_CarbonDensity/BrooksCreek_CarbonDensityIntersectionTotal.csv")
ChenaDyke <- read.csv("Output/Counterfactual_CarbonDensity/ChenaDyke_CarbonDensityIntersectionTotal.csv")
DiscoveryCreek <- read.csv("Output/Counterfactual_CarbonDensity/DiscoveryCreek_CarbonDensityIntersectionTotal.csv")
Cottonwood <- read.csv("Output/Counterfactual_CarbonDensity/Cottonwood_CarbonDensityIntersectionTotal.csv")
TowahminaLake <- read.csv("Output/Counterfactual_CarbonDensity/TowahminaLake_CarbonDensityIntersectionTotal.csv")
SwiftFork <- read.csv("Output/Counterfactual_CarbonDensity/SwiftFork_CarbonDensityIntersectionTotal.csv")
SuckerCreekNorth <- read.csv("Output/Counterfactual_CarbonDensity/SuckerCreekNorth_CarbonDensityIntersectionTotal.csv")
VunleLake <- read.csv("Output/Counterfactual_CarbonDensity/VunleLake_CarbonDensityIntersectionTotal.csv")
MartenCreek <- read.csv("Output/Counterfactual_CarbonDensity/MartenCreek_CarbonDensityIntersectionTotal.csv")
QwikRiver <- read.csv("Output/Counterfactual_CarbonDensity/QwikRiver_CarbonDensityIntersectionTotal.csv")
LittleMosquito <- read.csv("Output/Counterfactual_CarbonDensity/LittleMosquito_CarbonDensityIntersectionTotal.csv")
TsedolalindinLake <- read.csv("Output/Counterfactual_CarbonDensity/TsedolalindinLake_CarbonDensityIntersectionTotal.csv")
TreatIsland <- read.csv("Output/Counterfactual_CarbonDensity/TreatIsland_CarbonDensityIntersectionTotal.csv")
TwomileLake <- read.csv("Output/Counterfactual_CarbonDensity/TwomileLake_CarbonDensityIntersectionTotal.csv")
VaultCreek <- read.csv("Output/Counterfactual_CarbonDensity/VaultCreek_CarbonDensityIntersectionTotal.csv")
BigCreek <- read.csv("Output/Counterfactual_CarbonDensity/BigCreek_CarbonDensityIntersectionTotal.csv")
BigCreek2 <- read.csv("Output/Counterfactual_CarbonDensity/BigCreek2_CarbonDensityIntersectionTotal.csv")%>%
  dplyr::select(!FireName)%>%
  mutate(FireName = "Big Creek 2")
NorthWhakatna <- read.csv("Output/Counterfactual_CarbonDensity/NorthWhakatna_CarbonDensityIntersectionTotal.csv")
Huslia1 <- read.csv("Output/Counterfactual_CarbonDensity/Huslia1_CarbonDensityIntersectionTotal.csv")
CrowLake <- read.csv("Output/Counterfactual_CarbonDensity/CrowLake_CarbonDensityIntersectionTotal.csv")
BillyHawkCreek <- read.csv("Output/Counterfactual_CarbonDensity/BillyHawkCreek_CarbonDensityIntersectionTotal.csv")
GoldstreamCreek <- read.csv("Output/Counterfactual_CarbonDensity/GoldstreamCreek_CarbonDensityIntersectionTotal.csv")
OlnesPond <- read.csv("Output/Counterfactual_CarbonDensity/OlnesPond_CarbonDensityIntersectionTotal.csv")
TenOClock <- read.csv("Output/Counterfactual_CarbonDensity/TenOClock_CarbonDensityIntersectionTotal.csv")
GobletCreek <- read.csv("Output/Counterfactual_CarbonDensity/GobletCreek_CarbonDensityIntersectionTotal.csv")
BoulderCreek <- read.csv("Output/Counterfactual_CarbonDensity/BoulderCreek_CarbonDensityIntersectionTotal.csv")
Kuranakh <- read.csv("Output/Counterfactual_CarbonDensity/Kuranakh_CarbonDensityIntersectionTotal.csv")
GeorgeLake <- read.csv("Output/Counterfactual_CarbonDensity/GeorgeLake_CarbonDensityIntersectionTotal.csv")

Total_CarbonDensity <- rbind(BurmanLake,ChloyaLakes,BrooksCreek,ChenaDyke,DiscoveryCreek,
                             Cottonwood,TowahminaLake,SwiftFork,SuckerCreekNorth,
                             VunleLake,MartenCreek,QwikRiver,LittleMosquito,
                             TsedolalindinLake,TreatIsland,TwomileLake,VaultCreek,
                             BigCreek,BigCreek2, NorthWhakatna,Huslia1,
                             CrowLake,BillyHawkCreek,GoldstreamCreek,OlnesPond,
                             TenOClock,GobletCreek,BoulderCreek,Kuranakh,GeorgeLake)

###Combine the volume and carbon density dataframes 

Volume_low_total_carbon <- left_join(Volume_low_total, Total_CarbonDensity, by = c("FireName", "FIRENUMBER", "SIZE_ACRES"))%>%
  mutate(CarbonStorage_kg_1m = Talik_Volume_m3_1m*carbon_100)%>%
  mutate(CarbonStorage_kg_2m = Talik_Volume_m3_2m*carbon_200)%>%
  mutate(CarbonStorage_kg_3m = Talik_Volume_m3_3m*carbon_300)%>%
  mutate(CarbonStorage_kg_deep = Talik_Volume_m3_deep*DeepCarbon_3plus)%>%
  mutate(CarbonStorage_Total_kg = CarbonStorage_kg_1m+CarbonStorage_kg_2m+CarbonStorage_kg_3m+CarbonStorage_kg_deep)

Volume_medium_total_carbon <- left_join(Volume_medium_total, Total_CarbonDensity, by = c("FireName", "FIRENUMBER", "SIZE_ACRES"))%>%
  mutate(CarbonStorage_kg_1m = Talik_Volume_m3_1m*carbon_100)%>%
  mutate(CarbonStorage_kg_2m = Talik_Volume_m3_2m*carbon_200)%>%
  mutate(CarbonStorage_kg_3m = Talik_Volume_m3_3m*carbon_300)%>%
  mutate(CarbonStorage_kg_deep = Talik_Volume_m3_deep*DeepCarbon_3plus)%>%
  mutate(CarbonStorage_Total_kg = CarbonStorage_kg_1m+CarbonStorage_kg_2m+CarbonStorage_kg_3m+CarbonStorage_kg_deep)

Volume_high_total_carbon <- left_join(Volume_high_total, Total_CarbonDensity, by = c("FireName", "FIRENUMBER", "SIZE_ACRES"))%>%
  mutate(CarbonStorage_kg_1m = Talik_Volume_m3_1m*carbon_100)%>%
  mutate(CarbonStorage_kg_2m = Talik_Volume_m3_2m*carbon_200)%>%
  mutate(CarbonStorage_kg_3m = Talik_Volume_m3_3m*carbon_300)%>%
  mutate(CarbonStorage_kg_deep = Talik_Volume_m3_deep*DeepCarbon_3plus)%>%
  mutate(CarbonStorage_Total_kg = CarbonStorage_kg_1m+CarbonStorage_kg_2m+CarbonStorage_kg_3m+CarbonStorage_kg_deep)


####Write the volume plus carbon mass dataframes 
write.csv(Volume_low_total_carbon, "Output/TalikCarbonMass/TalikCarbon_low.csv")
write.csv(Volume_medium_total_carbon, "Output/TalikCarbonMass/TalikCarbon_medium.csv")
write.csv(Volume_high_total_carbon, "Output/TalikCarbonMass/TalikCarbon_high.csv")


























###Remove the no talik perimeters and assess the carbon storage of the perimeters with taliks 

Volume_high_total_carbon_onlytalik_summary <- Volume_high_total_carbon%>%
  filter(talik_area_acres != 0)%>%
  group_by(FireName, YSF)%>%
  summarize(CarbonStorage_Total_kg_median = median(CarbonStorage_Total_kg), Talik_Volume_m3_median = median(Talik_Volume_m3))


ggplot(Volume_high_total_carbon_onlytalik_summary, aes(x = YSF, y = CarbonStorage_Total_kg_median, group = YSF))+
  geom_boxplot()






