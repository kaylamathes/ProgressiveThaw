###library 
library(tidyverse)
library(ggplot2)
library(ggridges)

##Load Fires Individually 
##High
ChenaDyke_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_ChenaDyke_VeryHigh.csv")%>%
  mutate(FireName = "Chena Dyke") %>% select(!Valid) %>% select(!"...4")
ChloyaLakes_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_ChloyaLakes_VeryHigh.csv")%>%
  mutate(FireName = "Chloya Lakes") %>% select(!Valid)%>% select(!"...4")
CrowLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_CrowLake_VeryHigh.csv")%>%
  mutate(FireName = "Crow Lake") %>% select(!Valid)%>% select(!"...4")
DiscoveryCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_DiscoveryCreek_VeryHigh.csv")%>%
  mutate(FireName = "Discovery Creek") %>% select(!Valid)%>% select(!"...4")
GeorgeLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GeorgeLake_VeryHigh.csv")%>%
  mutate(FireName = "George Lake") %>% select(!Valid)%>% select(!"...4")
Huslia1_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Huslia1_VeryHigh.csv")%>%
  mutate(FireName = "Huslia 1") %>% select(!Valid)%>% select(!"...4")
Kuranakh_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Kuranakh_VeryHigh.csv")%>%
  mutate(FireName = "Kuranakh") %>% select(!Valid)%>% select(!"...4")
LittleMosquito_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_LittleMosquito_VeryHigh.csv")%>%
  mutate(FireName = "Little Mosquito") %>% select(!Valid)
MartenCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_MartenCreek_VeryHigh.csv")%>%
  mutate(FireName = "Marten Creek") %>% select(!Valid)%>% select(!"...4")
QwikRiver_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_QwikRiver_VeryHigh.csv")%>%
  mutate(FireName = "Qwik River") %>% select(!Valid)
TreatIsland_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TreatIsland_VeryHigh.csv")%>%
  mutate(FireName = "Treat Island") %>% select(!Valid)%>% select(!"...4")
TsedolalindinLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TsedolalindinLake_VeryHigh.csv")%>%
  mutate(FireName = "Tsedolalindin Lake") %>% select(!Valid)%>% select(!"...4")
Cottonwood_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_Cottonwood_VeryHigh.csv")%>%
  mutate(FireName = "Cottonwood") %>% select(!Valid)%>% select(!"...4")
BillyHawkCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BillyHawkCreek_VeryHigh.csv")%>%
  mutate(FireName = "Billy Hawk Creek") %>% select(!Valid)%>% select(!"...4")
GoldstreamCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GoldstreamCreek_VeryHigh.csv")%>%
  mutate(FireName = "Goldstream Creek") %>% select(!Valid)%>% select(!"...4")
VunleLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_VunleLake_VeryHigh.csv")%>%
  mutate(FireName = "Vunle Lake") %>% select(!Valid)%>% select(!"...4")
TenOClock_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TenOClock_VeryHigh.csv")%>%
  mutate(FireName = "Ten O Clock") %>% select(!Valid)%>% select(!"...4")
BrooksCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BrooksCreek_VeryHigh.csv")%>%
  mutate(FireName = "Brooks Creek") %>% select(!Valid)%>% select(!"...4")
SuckerCreekNorth_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_SuckerCreekNorth_VeryHigh.csv")%>%
  mutate(FireName = "Sucker Creek North") %>% select(!Valid)%>% select(!"...4")
NorthWhakatna_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_NorthWhakatna_VeryHigh.csv")%>%
  mutate(FireName = "North Whakatna") %>% select(!Valid)%>% select(!"...4")
VaultCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_VaultCreek_VeryHigh.csv")%>%
  mutate(FireName = "Vault Creek") %>% select(!Valid)%>% select(!"...4")
TwomileLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TwomileLake_VeryHigh.csv")%>%
  mutate(FireName = "Twomile Lake") %>% select(!Valid)%>% select(!"...4")
BoulderCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BoulderCreek_VeryHigh.csv")%>%
  mutate(FireName = "Boulder Creek") %>% select(!Valid)%>% select(!"...4")
OlnesPond_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_OlnesPond_VeryHigh.csv")%>%
  mutate(FireName = "Olnes Pond") %>% select(!Valid)%>% select(!"...4")
BigCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh//Talik_perimeter_v2_BigCreek_VeryHigh.csv")%>%
  mutate(FireName = "Big Creek") %>% select(!Valid)%>% select(!"...4")
BigCreek2_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BigCreek2_VeryHigh.csv")%>%
  mutate(FireName = "Big Creek 2") %>% select(!Valid)%>% select(!"...4")
BurmanLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_BurmanLake_VeryHigh.csv")%>%
  mutate(FireName = "Burman Lake") %>% select(!Valid)%>% select(!"...4")
GobletCreek_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_GobletCreek_VeryHigh.csv")%>%
  mutate(FireName = "Goblet Creek") %>% select(!Valid)%>% select(!"...4")
SwiftFork_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_SwiftFork_VeryHigh.csv")%>%
  mutate(FireName = "Swift Fork") %>% select(!Valid)%>% select(!"...4")
TowahminaLake_High <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/VeryHigh/Talik_perimeter_v2_TowahminaLake_VeryHigh.csv")%>%
  mutate(FireName = "Towahmina Lake") %>% select(!Valid)%>% select(!"...4")


AllFire_High <- rbind(SwiftFork_High,GobletCreek_High,BurmanLake_High,
                      BigCreek2_High,BigCreek_High,OlnesPond_High,
                      BoulderCreek_High,TwomileLake_High,VaultCreek_High,
                      NorthWhakatna_High,SuckerCreekNorth_High,BrooksCreek_High,
                      TenOClock_High,VunleLake_High,GoldstreamCreek_High,
                      BillyHawkCreek_High,Cottonwood_High,TsedolalindinLake_High,
                      TreatIsland_High,QwikRiver_High,MartenCreek_High,
                      LittleMosquito_High,Kuranakh_High,Huslia1_High,
                      GeorgeLake_High,DiscoveryCreek_High,CrowLake_High,
                      ChloyaLakes_High,ChenaDyke_High,TowahminaLake_High)%>%
  mutate(Scenario = "3-High")
                      
  


##Medium 
ChenaDyke_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_ChenaDyke.csv")%>%
  mutate(FireName = "Chena Dyke") %>% select(!Valid)
ChloyaLakes_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_ChloyaLakes.csv")%>%
  mutate(FireName = "Chloya Lakes") %>% select(!Valid)
CrowLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_CrowLake.csv")%>%
  mutate(FireName = "Crow Lake") %>% select(!Valid)
DiscoveryCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_DiscoveryCreek.csv")%>%
  mutate(FireName = "Discovery Creek") %>% select(!Valid)
GeorgeLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GeorgeLake.csv")%>%
  mutate(FireName = "George Lake") %>% select(!Valid)
Huslia1_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Huslia1.csv")%>%
  mutate(FireName = "Huslia 1") %>% select(!Valid)
Kuranakh_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Kuranakh.csv")%>%
  mutate(FireName = "Kuranakh") %>% select(!Valid)
LittleMosquito_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_LittleMosquito.csv")%>%
  mutate(FireName = "Little Mosquito") %>% select(!Valid)
MartenCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_MartenCreek.csv")%>%
  mutate(FireName = "Marten Creek") %>% select(!Valid)
QwikRiver_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_QwikRiver.csv")%>%
  mutate(FireName = "Qwik River") %>% select(!Valid)
TreatIsland_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TreatIsland.csv")%>%
  mutate(FireName = "Treat Island") %>% select(!Valid)
TsedolalindinLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TsedolalindinLake.csv")%>%
  mutate(FireName = "Tsedolalindin Lake") %>% select(!Valid)
Cottonwood_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_Cottonwood.csv")%>%
  mutate(FireName = "Cottonwood") %>% select(!Valid)
BillyHawkCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BillyHawkCreek2.csv")%>%
  mutate(FireName = "Billy Hawk Creek") %>% select(!Valid)
GoldstreamCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GoldstreamCreek.csv")%>%
  mutate(FireName = "Goldstream Creek") %>% select(!Valid)
VunleLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_VunleLake.csv")%>%
  mutate(FireName = "Vunle Lake") %>% select(!Valid)
TenOClock_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TenOClock.csv")%>%
  mutate(FireName = "Ten O Clock") %>% select(!Valid)
BrooksCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BrooksCreek.csv")%>%
  mutate(FireName = "Brooks Creek") %>% select(!Valid)
SuckerCreekNorth_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_SuckerCreekNorth.csv")%>%
  mutate(FireName = "Sucker Creek North") %>% select(!Valid)
NorthWhakatna_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_NorthWhakatna.csv")%>%
  mutate(FireName = "North Whakatna") %>% select(!Valid)
VaultCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_VaultCreek.csv")%>%
  mutate(FireName = "Vault Creek") %>% select(!Valid)
TwomileLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TwomileLake.csv")%>%
  mutate(FireName = "Twomile Lake") %>% select(!Valid)
BoulderCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BoulderCreek.csv")%>%
  mutate(FireName = "Boulder Creek") %>% select(!Valid)
OlnesPond_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_OlnesPond.csv")%>%
  mutate(FireName = "Olnes Pond") %>% select(!Valid)%>% select(!"...4")
NorthWhakatna_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_NorthWhakatna.csv")%>%
  mutate(FireName = "North Whakatna") %>% select(!Valid)
BigCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BigCreek.csv")%>%
  mutate(FireName = "Big Creek") %>% select(!Valid)
BigCreek2_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BigCreek2.csv")%>%
  mutate(FireName = "Big Creek 2") %>% select(!Valid)
BurmanLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_BurmanLake.csv")%>%
  mutate(FireName = "Burman Lake") %>% select(!Valid)
GobletCreek_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_GobletCreek.csv")%>%
  mutate(FireName = "Goblet Creek") %>% select(!Valid)
SwiftFork_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_SwiftFork.csv")%>%
  mutate(FireName = "Swift Fork") %>% select(!Valid)
TowahminaLake_Medium <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TowahminaLake.csv")%>%
  mutate(FireName = "Towahmina Lake") %>% select(!Valid)



AllFire_Medium <- rbind(SwiftFork_Medium,GobletCreek_Medium,BurmanLake_Medium,
                      BigCreek2_Medium,BigCreek_Medium,OlnesPond_Medium,
                      BoulderCreek_Medium,TwomileLake_Medium,VaultCreek_Medium,
                      NorthWhakatna_Medium,SuckerCreekNorth_Medium,BrooksCreek_Medium,
                      TenOClock_Medium,VunleLake_Medium,GoldstreamCreek_Medium,
                      BillyHawkCreek_Medium,Cottonwood_Medium,TsedolalindinLake_Medium,
                      TreatIsland_Medium,QwikRiver_Medium,MartenCreek_Medium,
                      LittleMosquito_Medium,Kuranakh_Medium,Huslia1_Medium,
                      GeorgeLake_Medium,DiscoveryCreek_Medium,CrowLake_Medium,
                      ChloyaLakes_Medium,ChenaDyke_Medium,TowahminaLake_Medium)%>%
  mutate(Scenario = "2-Medium")


###Low
ChenaDyke_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_ChenaDyke_medium.csv")%>%
  mutate(FireName = "Chena Dyke") %>% select(!Valid)
ChloyaLakes_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_ChloyaLakes_medium.csv")%>%
  mutate(FireName = "Chloya Lakes")%>% select(!Valid)
CrowLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_CrowLake_medium.csv")%>%
  mutate(FireName = "Crow Lake")%>% select(!Valid)
GeorgeLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GeorgeLake_medium.csv")%>%
  mutate(FireName = "George Lake")%>% select(!Valid)
Huslia1_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Huslia1_medium.csv")%>%
  mutate(FireName = "Huslia 1")%>% select(!Valid)
Kuranakh_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Kuranakh_medium.csv")%>%
  mutate(FireName = "Kuranakh")%>% select(!Valid.x) %>% select(!Valid.y)
MartenCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_MartenCreek_medium.csv")%>%
  mutate(FireName = "Marten Creek")%>% select(!Valid)
QwikRiver_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_QwikRiver_medium.csv")%>%
  mutate(FireName = "Qwik River")%>% select(!Valid)
TreatIsland_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TreatIsland_medium.csv")%>%
  mutate(FireName = "Treat Island")%>% select(!Valid)
BigCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BigCreek_medium.csv")%>%
  mutate(FireName = "Big Creek")%>% select(!Valid)
BigCreek2_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BigCreek2_medium.csv")%>%
  mutate(FireName = "Big Creek 2")%>% select(!Valid)
BillyHawkCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BillyHawkCreek_medium.csv")%>%
  mutate(FireName = "Billy Hawk Creek")%>% select(!Valid)
BoulderCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BoulderCreek_medium.csv")%>%
  mutate(FireName = "Boulder Creek")%>% select(!Valid)
BrooksCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_BrooksCreek_medium.csv")%>%
  mutate(FireName = "Brooks Creek")%>% select(!Valid)
Cottonwood_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_Cottonwood_medium.csv")%>%
  mutate(FireName = "Cottonwood")%>% select(!Valid)
GobletCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GobletCreek_medium.csv")%>%
  mutate(FireName = "Goblet Creek")%>% select(!Valid)
GoldstreamCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_GoldstreamCreek_medium.csv")%>%
  mutate(FireName = "Goldstream Creek")%>% select(!Valid)
NorthWhakatna_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_NorthWhakatna_medium.csv")%>%
  mutate(FireName = "North Whakatna")%>% select(!Valid)
OlnesPond_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_OlnesPond_medium.csv")%>%
  mutate(FireName = "Olnes Pond")%>% select(!Valid)
SuckerCreekNorth_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_SuckerCreekNorth_medium.csv")%>%
  mutate(FireName = "Sucker Creek North")%>% select(!Valid)
SwiftFork_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_SwiftFork_medium.csv")%>%
  mutate(FireName = "Swift Fork")%>% select(!Valid)
TenOClock_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TenOClock_medium.csv")%>%
  mutate(FireName = "Ten O Clock")%>% select(!Valid)
TowahminaLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TowahminaLake_medium.csv")%>%
  mutate(FireName = "Towahmina Lake")%>% select(!Valid)
TsedolalindinLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TsedolalindinLake_medium.csv")%>%
  mutate(FireName = "Tsedolalindin Lake")%>% select(!Valid)
TwomileLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_TwomileLake_medium.csv")%>%
  mutate(FireName = "Twomile Lake")%>% select(!Valid)
VaultCreek_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_VaultCreek_medium.csv")%>%
  mutate(FireName = "Vault Creek")%>% select(!Valid)
VunleLake_Low <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Intersections/Medium/Talik_perimeter_v2_VunleLake_medium.csv")%>%
  mutate(FireName = "Vunle Lake")%>% select(!Valid)%>%select(!"...4")


AllFire_Low <- rbind(TreatIsland_Low,QwikRiver_Low,MartenCreek_Low,
                      Kuranakh_Low,Huslia1_Low,GeorgeLake_Low,
                      CrowLake_Low,ChloyaLakes_Low,ChenaDyke_Low,
                      Cottonwood_Low,BrooksCreek_Low,BoulderCreek_Low,
                      BillyHawkCreek_Low,BigCreek2_Low,BigCreek_Low,
                      GobletCreek_Low,GoldstreamCreek_Low,NorthWhakatna_Low,
                      OlnesPond_Low,SuckerCreekNorth_Low,SwiftFork_Low,
                      TenOClock_Low,TowahminaLake_Low,TsedolalindinLake_Low,
                      TwomileLake_Low,VaultCreek_Low,VunleLake_Low)%>%
  mutate(Scenario = "1-Low")

  

AllFire_Total <- rbind(AllFire_Low, AllFire_Medium,AllFire_High)

AllFire_Total <- AllFire_Total%>%
  mutate(percent_talik = (talik_area_acres/SIZE_ACRES)*100)

All_Fire_Total_summary <- AllFire_Total%>%
  group_by(Scenario, FireName)%>%
  summarize(percent_talik_median = median(percent_talik),
            percent_talik_max = max(percent_talik),
            percent_talik_min = min(percent_talik))


##Boxplot 
ggplot(All_Fire_Total_summary, aes(x = Scenario, y = percent_talik_median, fill = Scenario))+
  geom_boxplot()+
  scale_fill_manual(values = c("1-Low" = "#FFE180",  
                                "2-Medium" = "#EBA059", 
                               "3-High" = "#BB3C76"))+
  theme_classic() + ylab("Percent of Counterfactual Perimeter (%)")+
  theme(axis.text.y = element_text(size = 15),axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20), legend.position = "none")


ggsave("Output/Talik_area.png", height = 10, width = 10)


##Density plot
ggplot(AllFire_Total)+
geom_density_ridges(aes(x = percent_talik, y = FireName, group = interaction(FireName,Scenario),fill = Scenario), alpha = 0.6)+
  scale_fill_manual(values = c("1-Low" = "#FFE180",  
                               "2-Medium" = "#EBA059",
                               "3-High" = "#BB3C76"))+
  theme_classic() + xlab("Percent of Counterfactual perimeter (%)")+
  theme(axis.text.y = element_text(size = 10),axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15))

ggsave("Output/Talik_areaDensity.png", height = 10, width = 10)  





##Other plot Ideas
ggplot(AllFire_Total, aes(x = percent_talik, y = FireName,  fill = Scenario, alpha = Scenario)) +
  geom_density_ridges_gradient() +
  scale_fill_manual(values = c("Medium" = "#89b4a9ff",  
                               "High" = "#5e9294ff"))+
  scale_alpha_manual(values = c("Medium" = 0.5,  
                                "High" = 0.5))+
  theme_classic() + xlab("Percent of Counterfactual perimeter (%)")

ggplot(AllFire_Total, aes(x = percent_talik, fill = Scenario)) +
  geom_histogram(aes(y = ..density..)) +
  stat_bin(bins = 30)+
  scale_fill_manual(values = c("Medium" = "#89b4a9ff",  
                               "High" = "#5e9294ff"))+
  theme_classic() 

