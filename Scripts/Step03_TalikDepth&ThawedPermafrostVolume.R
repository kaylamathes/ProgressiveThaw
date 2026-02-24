###Talik depth Determination 

library(sf)
library(tidyverse)
options(scipen = 999)

##Upload Talik Depth Model 
cold_model <- read.csv("Data/NFactor/ColdModel.csv")%>%
  select(!Year_Model)
intermediate_model <- read.csv("Data/NFactor/IntermediateModel.csv")%>%
  select(!Year_Model)
warm_model <- read.csv("Data/NFactor/WarmModel.csv")%>%
  select(!Year_Model)

###########################################################################################################################
######################################## LOW ################################################################################
#####################################################################################################################################################
##Intersection Upload 
BigCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BigCreek_medium.csv")%>%
  mutate(FireName = "Big Creek")
GeorgeLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GeorgeLake_medium.csv")%>%
  mutate(FireName = "George Lake")
GobletCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GobletCreek_medium.csv")%>%
  mutate(FireName = "Goblet Creek")
GoldstreamCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GoldstreamCreek_medium.csv")%>%
  mutate(FireName = "Goldstream Creek")
BigCreek2_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BigCreek2_medium.csv")%>%
  mutate(FireName = "Big Creek 2")
CrowLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_CrowLake_medium.csv")%>%
  mutate(FireName = "Crow Lake")
ChenaDyke_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_ChenaDyke_medium.csv")%>%
  mutate(FireName = "Chena Dyke")
Kuranakh_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Kuranakh_medium.csv")%>%
  mutate(FireName = "Kuranakh")%>%
  select(!Valid.x)%>%
  rename(Valid = Valid.y)
NorthWhakatna_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_NorthWhakatna_medium.csv")%>%
  mutate(FireName = "North Whakatna")
TenOClock_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TenOClock_medium.csv")%>%
  mutate(FireName = "Ten O Clock")
Cottonwood_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Cottonwood_medium.csv")%>%
  mutate(FireName = "Cottonwood")
TreatIsland_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TreatIsland_medium.csv")%>%
  mutate(FireName = "Treat Island")
QwikRiver_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_QwikRiver_medium.csv")%>%
  mutate(FireName = "Qwik River")
Huslia1_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Huslia1_medium.csv")%>%
  mutate(FireName = "Huslia 1")
MartenCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_MartenCreek_medium.csv")%>%
  mutate(FireName = "Marten Creek")
ChloyaLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_ChloyaLakes_medium.csv")%>%
  mutate(FireName = "Chloya Lake")
BillyHawkCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BillyHawkCreek_medium.csv")%>%
  mutate(FireName = "Billy Hawk Creek")
VunleLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_VunleLake_medium.csv")%>%
  mutate(FireName = "Vunle Lake")%>%
  select(!"...1")
OlnesPond_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_OlnesPond_medium.csv")%>%
  mutate(FireName = "Olnes Pond")
BoulderCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BoulderCreek_medium.csv")%>%
  mutate(FireName = "Boulder Creek")
TwomileLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TwomileLake_medium.csv")%>%
  mutate(FireName = "Twomile Lake")
VaultCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_VaultCreek_medium.csv")%>%
  mutate(FireName = "Vault Creek")
SuckerCreekNorth_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_SuckerCreekNorth_medium.csv")%>%
  mutate(FireName = "Sucker Creek North")
BrooksCreek_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BrooksCreek_medium.csv")%>%
  mutate(FireName = "Brooks Creek")
SwiftFork_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_SwiftFork_medium.csv")%>%
  mutate(FireName = "Swift Fork")
TowahminaLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TowahminaLake_medium.csv")%>%
  mutate(FireName = "Towahmina Lake")
TsedolalindinLake_low <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TsedolalindinLake_medium.csv")%>%
  mutate(FireName = "Tsedolalindin Lake")

AllFire_low_warm <- rbind(TowahminaLake_low,SwiftFork_low,BrooksCreek_low,
                     VaultCreek_low,BoulderCreek_low,BillyHawkCreek_low,
                     Kuranakh_low,BigCreek2_low,GobletCreek_low,
                     GeorgeLake_low,BigCreek_low)

AllFire_low_Intermediate <- rbind(SuckerCreekNorth_low,TwomileLake_low,OlnesPond_low,
                                  ChloyaLake_low,MartenCreek_low,Huslia1_low,QwikRiver_low,
                                  TreatIsland_low,Cottonwood_low,TenOClock_low,ChenaDyke_low,
                                  CrowLake_low,GoldstreamCreek_low, TsedolalindinLake_low)

AllFire_low_cold <- rbind(VunleLake_low,NorthWhakatna_low)

#####################################################################################################################################################
######################################### Medium #########################################
#####################################################################################################################################################

##Intersection Upload 
BigCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BigCreek.csv")%>%
  mutate(FireName = "Big Creek")
GeorgeLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GeorgeLake.csv")%>%
  mutate(FireName = "George Lake")
GobletCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GobletCreek.csv")%>%
  mutate(FireName = "Goblet Creek")
GoldstreamCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GoldstreamCreek.csv")%>%
  mutate(FireName = "Goldstream Creek")
BigCreek2_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BigCreek2.csv")%>%
  mutate(FireName = "Big Creek 2")
CrowLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_CrowLake.csv")%>%
  mutate(FireName = "Crow Lake")
ChenaDyke_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_ChenaDyke.csv")%>%
  mutate(FireName = "Chena Dyke")
Kuranakh_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Kuranakh.csv")%>%
  mutate(FireName = "Kuranakh")
NorthWhakatna_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_NorthWhakatna.csv")%>%
  mutate(FireName = "North Whakatna")
TenOClock_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TenOClock.csv")%>%
  mutate(FireName = "Ten O Clock")
Cottonwood_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Cottonwood.csv")%>%
  mutate(FireName = "Cottonwood")
TreatIsland_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TreatIsland.csv")%>%
  mutate(FireName = "Treat Island")
QwikRiver_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_QwikRiver.csv")%>%
  mutate(FireName = "Qwik River")
Huslia1_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Huslia1.csv")%>%
  mutate(FireName = "Huslia 1")
MartenCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_MartenCreek.csv")%>%
  mutate(FireName = "Marten Creek")
ChloyaLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_ChloyaLakes.csv")%>%
  mutate(FireName = "Chloya Lake")
BillyHawkCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BillyHawkCreek2.csv")%>%
  mutate(FireName = "Billy Hawk Creek")
VunleLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_VunleLake.csv")%>%
  mutate(FireName = "Vunle Lake")
OlnesPond_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_OlnesPond.csv")%>%
  mutate(FireName = "Olnes Pond")%>%
  select(!"...1")
BoulderCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BoulderCreek.csv")%>%
  mutate(FireName = "Boulder Creek")
TwomileLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TwomileLake.csv")%>%
  mutate(FireName = "Twomile Lake")
VaultCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_VaultCreek.csv")%>%
  mutate(FireName = "Vault Creek")
SuckerCreekNorth_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_SuckerCreekNorth.csv")%>%
  mutate(FireName = "Sucker Creek North")
BrooksCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BrooksCreek.csv")%>%
  mutate(FireName = "Brooks Creek")
SwiftFork_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_SwiftFork.csv")%>%
  mutate(FireName = "Swift Fork")
TowahminaLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TowahminaLake.csv")%>%
  mutate(FireName = "Towahmina Lake")
BurmanLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BurmanLake.csv")%>%
  mutate(FireName = "Burman Lake")
DiscoveryCreek_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_DiscoveryCreek.csv")%>%
  mutate(FireName = "Discovery Creek")
LittleMosquito_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_LittleMosquito.csv")%>%
  mutate(FireName = "Little Mosquito")
TsedolalindinLake_medium <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TsedolalindinLake.csv")%>%
  mutate(FireName = "Tsedolalindin Lake")

AllFire_medium_warm <- rbind(TowahminaLake_medium,SwiftFork_medium,BrooksCreek_medium,
                          VaultCreek_medium,BoulderCreek_medium,BillyHawkCreek_medium,
                          Kuranakh_medium,BigCreek2_medium,GobletCreek_medium,
                          GeorgeLake_medium,BigCreek_medium, LittleMosquito_medium,DiscoveryCreek_medium)

AllFire_medium_Intermediate <- rbind(SuckerCreekNorth_medium,TwomileLake_medium,OlnesPond_medium,
                                  ChloyaLake_medium,MartenCreek_medium,Huslia1_medium,QwikRiver_medium,
                                  TreatIsland_medium,Cottonwood_medium,TenOClock_medium,ChenaDyke_medium,
                                  CrowLake_medium,GoldstreamCreek_medium, BurmanLake_medium, TsedolalindinLake_medium)

AllFire_medium_cold <- rbind(VunleLake_medium,NorthWhakatna_medium)

###########################################################################################################################
########################################### HIGH ##########################################################################
###########################################################################################################################

##Intersection Upload 

##Intersection Upload 
BigCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BigCreek_VeryHigh.csv")%>%
  mutate(FireName = "Big Creek")%>%
  select(!"...1")
GeorgeLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GeorgeLake_VeryHigh.csv")%>%
  mutate(FireName = "George Lake")%>%
  select(!"...1")
GobletCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GobletCreek_VeryHigh.csv")%>%
  mutate(FireName = "Goblet Creek")%>%
  select(!"...1")
GoldstreamCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GoldstreamCreek_VeryHigh.csv")%>%
  mutate(FireName = "Goldstream Creek")%>%
  select(!"...1")
BigCreek2_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BigCreek2_VeryHigh.csv")%>%
  mutate(FireName = "Big Creek 2")%>%
  select(!"...1")
CrowLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_CrowLake_VeryHigh.csv")%>%
  mutate(FireName = "Crow Lake")%>%
  select(!"...1")
ChenaDyke_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_ChenaDyke_VeryHigh.csv")%>%
  mutate(FireName = "Chena Dyke")%>%
  select(!"...1")
Kuranakh_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Kuranakh_VeryHigh.csv")%>%
  mutate(FireName = "Kuranakh")%>%
  select(!"...1")
NorthWhakatna_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_NorthWhakatna_VeryHigh.csv")%>%
  mutate(FireName = "North Whakatna")%>%
  select(!"...1")
TenOClock_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TenOClock_VeryHigh.csv")%>%
  mutate(FireName = "Ten O Clock")%>%
  select(!"...1")
Cottonwood_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Cottonwood_VeryHigh.csv")%>%
  mutate(FireName = "Cottonwood")%>%
  select(!"...1")
TreatIsland_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TreatIsland_VeryHigh.csv")%>%
  mutate(FireName = "Treat Island")%>%
  select(!"...1")
QwikRiver_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_QwikRiver_VeryHigh.csv")%>%
  mutate(FireName = "Qwik River")
Huslia1_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Huslia1_VeryHigh.csv")%>%
  mutate(FireName = "Huslia 1")%>%
  select(!"...1")
MartenCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_MartenCreek_VeryHigh.csv")%>%
  mutate(FireName = "Marten Creek")%>%
  select(!"...1")
ChloyaLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_ChloyaLakes_VeryHigh.csv")%>%
  mutate(FireName = "Chloya Lake")%>%
  select(!"...1")
BillyHawkCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BillyHawkCreek_VeryHigh.csv")%>%
  mutate(FireName = "Billy Hawk Creek")%>%
  select(!"...1")
VunleLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_VunleLake_VeryHigh.csv")%>%
  mutate(FireName = "Vunle Lake")%>%
  select(!"...1")
OlnesPond_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_OlnesPond_VeryHigh.csv")%>%
  mutate(FireName = "Olnes Pond")%>%
  select(!"...1")
BoulderCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BoulderCreek_VeryHigh.csv")%>%
  mutate(FireName = "Boulder Creek")%>%
  select(!"...1")
TwomileLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TwomileLake_VeryHigh.csv")%>%
  mutate(FireName = "Twomile Lake")%>%
  select(!"...1")
VaultCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_VaultCreek_VeryHigh.csv")%>%
  mutate(FireName = "Vault Creek")%>%
  select(!"...1")
SuckerCreekNorth_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_SuckerCreekNorth_VeryHigh.csv")%>%
  mutate(FireName = "Sucker Creek North")%>%
  select(!"...1")
BrooksCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BrooksCreek_VeryHigh.csv")%>%
  mutate(FireName = "Brooks Creek")%>%
  select(!"...1")
SwiftFork_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_SwiftFork_VeryHigh.csv")%>%
  mutate(FireName = "Swift Fork")%>%
  select(!"...1")
TowahminaLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TowahminaLake_VeryHigh.csv")%>%
  mutate(FireName = "Towahmina Lake")%>%
  select(!"...1")
BurmanLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BurmanLake_VeryHigh.csv")%>%
  mutate(FireName = "Burman Lake")%>%
  select(!"...1")
DiscoveryCreek_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_DiscoveryCreek_VeryHigh.csv")%>%
  mutate(FireName = "Discovery Creek")%>%
  select(!"...1")
LittleMosquito_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_LittleMosquito_VeryHigh.csv")%>%
  mutate(FireName = "Little Mosquito")
TsedolalindinLake_high <- read.csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TsedolalindinLake_VeryHigh.csv")%>%
  mutate(FireName = "Tsedolalindin Lake")%>%
  select(!"...1")

AllFire_high_warm <- rbind(TowahminaLake_high,SwiftFork_high,BrooksCreek_high,
                             VaultCreek_high,BoulderCreek_high,BillyHawkCreek_high,
                             Kuranakh_high,BigCreek2_high,GobletCreek_high,
                             GeorgeLake_high,BigCreek_high, LittleMosquito_high,DiscoveryCreek_high)

AllFire_high_Intermediate <- rbind(SuckerCreekNorth_high,TwomileLake_high,OlnesPond_high,
                                     ChloyaLake_high,MartenCreek_high,Huslia1_high,QwikRiver_high,
                                     TreatIsland_high,Cottonwood_high,TenOClock_high,ChenaDyke_high,
                                     CrowLake_high,GoldstreamCreek_high, BurmanLake_high, TsedolalindinLake_high)

AllFire_high_cold <- rbind(VunleLake_high,NorthWhakatna_high)





##Integrate the talik depth over the areas for each fire 
## Low Scenario
AllFire_low_cold_depth <- AllFire_low_cold%>%
  group_by(FireName)%>%
  cross_join(cold_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_low_Intermediate_depth <- AllFire_low_Intermediate%>%
  group_by(FireName)%>%
  cross_join(intermediate_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_low_warm_depth <- AllFire_low_warm%>%
  group_by(FireName)%>%
  cross_join(warm_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

## Medium Scenario
AllFire_medium_cold_depth <- AllFire_medium_cold%>%
  group_by(FireName)%>%
  cross_join(cold_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_medium_Intermediate_depth <- AllFire_medium_Intermediate%>%
  group_by(FireName)%>%
  cross_join(intermediate_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_medium_warm_depth <- AllFire_medium_warm%>%
  group_by(FireName)%>%
  cross_join(warm_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

## High Scenario
AllFire_high_cold_depth <- AllFire_high_cold%>%
  group_by(FireName)%>%
  cross_join(cold_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_high_Intermediate_depth <- AllFire_high_Intermediate%>%
  group_by(FireName)%>%
  cross_join(intermediate_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

AllFire_high_warm_depth <- AllFire_high_warm%>%
  group_by(FireName)%>%
  cross_join(warm_model)%>%
  mutate(Talik_Volume_m3 = (talik_area_m2 * Talik_m))%>%
  ungroup()

####Plot the Rey et al. 2020 models 
warm_model <- warm_model%>%
  mutate(Model = "warm")

intermediate_model <- intermediate_model%>%
  mutate(Model = "intermediate")

cold_model <- cold_model%>%
  mutate(Model = "cold")

AllModels <- rbind(warm_model,intermediate_model,cold_model)%>%
  mutate(Talik_m_depth = Talik_m*-1)

ggplot(AllModels, aes(x = YSF, y = Talik_m_depth, color = Model, fill = Model)) +
         geom_area(position = "identity", alpha = 0.5)+
  geom_line()+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40))+
  scale_fill_manual(values = c("#17436eff", "#5f97cdff", "#c5daeeff"))+
  scale_color_manual(values = c("#17436eff", "#5f97cdff", "#c5daeeff"))+
  theme_classic() + ylab("Talik depth (m)") + xlab("Year Since Fire")

ggsave("Data/NFactor/TalikDepthModels.png", width = 8, height = 5)


######
####Calculate the Talik Volume for difference depths: These should all add up to the total Talik Volume!!! 

#### Low Scenario 
##cold
AllFire_low_cold_depth <- AllFire_low_cold_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                        Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up

##Intermediate 
AllFire_low_Intermediate_depth <- AllFire_low_Intermediate_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up


##Warm
AllFire_low_warm_depth <- AllFire_low_warm_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up

#############################################
#### Medium Scenario 
##cold
AllFire_medium_cold_depth <- AllFire_medium_cold_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up

##Intermediate 
AllFire_medium_Intermediate_depth <- AllFire_medium_Intermediate_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up


##Warm
AllFire_medium_warm_depth <- AllFire_medium_warm_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up



#############################################
#### High Scenario 
##cold
AllFire_high_cold_depth <- AllFire_high_cold_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up

##Intermediate 
AllFire_high_Intermediate_depth <- AllFire_high_Intermediate_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up


##Warm
AllFire_high_warm_depth <- AllFire_high_warm_depth%>%
  mutate(Talik_Volume_m3_1m = case_when(Talik_m == 0 ~ 0, 
                                        Talik_m >= 1 ~ talik_area_m2*1, 
                                        Talik_m <1 ~ talik_area_m2*Talik_m))%>%
  mutate(Talik_Volume_m3_2m = case_when(Talik_m <= 1  ~ 0, 
                                        Talik_m >= 2 ~ talik_area_m2*1,
                                        Talik_m <2 & Talik_m >1 ~ talik_area_m2*(Talik_m - 1)))%>%
  mutate(Talik_Volume_m3_3m = case_when(Talik_m <=2 ~ 0, 
                                        Talik_m >= 3 ~ talik_area_m2*1,
                                        Talik_m <3 & Talik_m > 2 ~ talik_area_m2*(Talik_m - 2)))%>%
  mutate(Talik_Volume_m3_deep = case_when(Talik_m <=3 ~ 0, 
                                          Talik_m > 3 ~ talik_area_m2*(Talik_m - 3)))%>%
  mutate(Volume_check = Talik_Volume_m3_1m+Talik_Volume_m3_2m+Talik_Volume_m3_3m+Talik_Volume_m3_deep)%>%
  mutate(Volume_check2 = Volume_check - Talik_Volume_m3) ###Check to make sure the total volume adds up



###Write the Talik thawed permafrost volume dataframes 
write.csv(AllFire_high_cold_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_high_cold_depth.csv")
write.csv(AllFire_high_Intermediate_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_high_Intermediate_depth.csv")
write.csv(AllFire_high_warm_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_high_warm_depth.csv")

write.csv(AllFire_medium_cold_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_cold_depth.csv")
write.csv(AllFire_medium_Intermediate_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_Intermediate_depth.csv")
write.csv(AllFire_medium_warm_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_medium_warm_depth.csv")

write.csv(AllFire_low_cold_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_low_cold_depth.csv")
write.csv(AllFire_low_Intermediate_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_low_Intermediate_depth.csv")
write.csv(AllFire_low_warm_depth, "Output/Counterfactual_TalikThawedPermafrost/AllFire_low_warm_depth.csv")
