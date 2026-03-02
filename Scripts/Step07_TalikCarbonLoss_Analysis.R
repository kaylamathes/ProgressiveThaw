#####Talik Carbon Loss Data Analysis 

##Library 
library(tidyverse)
library(ggplot2)
library(ggpubr)


##Uploadggpubr##Upload the csvs from Step 06 
cold_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_cold_50_sum_withtotal_mineral.csv")
intermediate_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_Intermediate_50_sum_withtotal_mineral.csv")
warm_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_Warm_50_sum_withtotal_mineral.csv")

cold_2m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_2m_cold_50_sum_withtotal_extra_mineral.csv")
intermediate_2m <- read.csv( "Output/TalikCarbonLoss/TalikCarbon_Total_2m_Intermediate_50_sum_withtotal_extra_mineral.csv")
warm_2m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_2m_Warm_50_sum_withtotal_extra_mineral.csv")

cold_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_cold_50_sum_withtotal_extra_mineral.csv")
intermediate_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_Intermediate_50_sum_withtotal_extra_mineral.csv")
warm_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_Warm_50_sum_withtotal_extra_mineral.csv")

cold_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_cold_50_sum_withtotal_extra_mineral.csv")
intermediate_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_Intermediate_50_sum_withtotal_extra_mineral.csv")
warm_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_Warm_50_sum_withtotal_extra_mineral.csv")

####summarize by depth steps 
cold_2m_sum <- cold_2m%>%
  mutate(sum_Carbonloss_kg_2m_total = sum_Carbonloss_kg_2m_step1+sum_Carbonloss_kg_2m_step2)%>%
  mutate(sum_CarbonStorage_kg_2m_total = CarbonStorage_kg_2m_step1 + CarbonStorage_kg_2m_step2)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_2m_total, sum_CarbonStorage_kg_2m_total)

intermediate_2m_sum <- intermediate_2m%>%
  mutate(sum_Carbonloss_kg_2m_total = sum_Carbonloss_kg_2m_step1+sum_Carbonloss_kg_2m_step2)%>%
  mutate(sum_CarbonStorage_kg_2m_total = CarbonStorage_kg_2m_step1 + CarbonStorage_kg_2m_step2)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_2m_total, sum_CarbonStorage_kg_2m_total)

warm_2m_sum <- warm_2m%>%
  mutate(sum_Carbonloss_kg_2m_total = sum_Carbonloss_kg_2m_step1+sum_Carbonloss_kg_2m_step2)%>%
  mutate(sum_CarbonStorage_kg_2m_total = CarbonStorage_kg_2m_step1 + CarbonStorage_kg_2m_step2)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_2m_total, sum_CarbonStorage_kg_2m_total)


cold_3m_sum <- cold_3m%>%
  mutate(sum_Carbonloss_kg_3m_total = sum_Carbonloss_kg_3m_step1+sum_Carbonloss_kg_3m_step2+sum_Carbonloss_kg_3m_step3+sum_Carbonloss_kg_3m_step4+sum_Carbonloss_kg_3m_step5+sum_Carbonloss_kg_3m_step6+sum_Carbonloss_kg_3m_step7)%>%
  mutate(sum_CarbonStorage_kg_3m_total = CarbonStorage_kg_3m_step1 + CarbonStorage_kg_3m_step2+ CarbonStorage_kg_3m_step3+ CarbonStorage_kg_3m_step4+ CarbonStorage_kg_3m_step5+ CarbonStorage_kg_3m_step6+ CarbonStorage_kg_3m_step7)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_3m_total, sum_CarbonStorage_kg_3m_total)

intermediate_3m_sum <- intermediate_3m%>%
  mutate(sum_Carbonloss_kg_3m_total = sum_Carbonloss_kg_3m_step1+sum_Carbonloss_kg_3m_step2+sum_Carbonloss_kg_3m_step3+sum_Carbonloss_kg_3m_step4+sum_Carbonloss_kg_3m_step5)%>%
  mutate(sum_CarbonStorage_kg_3m_total = CarbonStorage_kg_3m_step1 + CarbonStorage_kg_3m_step2+ CarbonStorage_kg_3m_step3+ CarbonStorage_kg_3m_step4+ CarbonStorage_kg_3m_step5)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_3m_total, sum_CarbonStorage_kg_3m_total)

warm_3m_sum <- warm_3m%>%
  mutate(sum_Carbonloss_kg_3m_total = sum_Carbonloss_kg_3m_step1+sum_Carbonloss_kg_3m_step2+sum_Carbonloss_kg_3m_step3+sum_Carbonloss_kg_3m_step4+sum_Carbonloss_kg_3m_step5)%>%
  mutate(sum_CarbonStorage_kg_3m_total = CarbonStorage_kg_3m_step1 + CarbonStorage_kg_3m_step2+ CarbonStorage_kg_3m_step3+ CarbonStorage_kg_3m_step4+ CarbonStorage_kg_3m_step5)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_3m_total, sum_CarbonStorage_kg_3m_total)


cold_deep_sum <- cold_deep%>%
  mutate(sum_Carbonloss_kg_deep_total = sum_Carbonloss_kg_deep_step1+sum_Carbonloss_kg_deep_step2+sum_Carbonloss_kg_deep_step3)%>%
  mutate(sum_CarbonStorage_kg_deep_total = CarbonStorage_kg_deep_step1 + CarbonStorage_kg_deep_step2+ CarbonStorage_kg_deep_step3)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_deep_total, sum_CarbonStorage_kg_deep_total)

intermediate_deep_sum <- intermediate_deep%>%
  mutate(sum_Carbonloss_kg_deep_total = sum_Carbonloss_kg_deep_step1 + sum_Carbonloss_kg_deep_step2 + sum_Carbonloss_kg_deep_step3 + sum_Carbonloss_kg_deep_step4 + sum_Carbonloss_kg_deep_step5 + sum_Carbonloss_kg_deep_step6 + sum_Carbonloss_kg_deep_step7 + sum_Carbonloss_kg_deep_step8 + sum_Carbonloss_kg_deep_step9 + sum_Carbonloss_kg_deep_step10 + sum_Carbonloss_kg_deep_step11 +sum_Carbonloss_kg_deep_step12 +sum_Carbonloss_kg_deep_step13 + sum_Carbonloss_kg_deep_step14)%>%
  mutate(sum_CarbonStorage_kg_deep_total = CarbonStorage_kg_deep_step1 + CarbonStorage_kg_deep_step2 + CarbonStorage_kg_deep_step3 + CarbonStorage_kg_deep_step4 + CarbonStorage_kg_deep_step5 + CarbonStorage_kg_deep_step6 + CarbonStorage_kg_deep_step7 + CarbonStorage_kg_deep_step8 + CarbonStorage_kg_deep_step9 + CarbonStorage_kg_deep_step10 + CarbonStorage_kg_deep_step11 + CarbonStorage_kg_deep_step12 + CarbonStorage_kg_deep_step13 + CarbonStorage_kg_deep_step14)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_deep_total, sum_CarbonStorage_kg_deep_total)

warm_deep_sum <- warm_deep%>%
  mutate(sum_Carbonloss_kg_deep_total = sum_Carbonloss_kg_deep_step1 + sum_Carbonloss_kg_deep_step2 + sum_Carbonloss_kg_deep_step3 + sum_Carbonloss_kg_deep_step4 + sum_Carbonloss_kg_deep_step5 + sum_Carbonloss_kg_deep_step6 + sum_Carbonloss_kg_deep_step7 + sum_Carbonloss_kg_deep_step8 + sum_Carbonloss_kg_deep_step9 + sum_Carbonloss_kg_deep_step10 + sum_Carbonloss_kg_deep_step11 +sum_Carbonloss_kg_deep_step12 +sum_Carbonloss_kg_deep_step13 + sum_Carbonloss_kg_deep_step14 + sum_Carbonloss_kg_deep_step15 + sum_Carbonloss_kg_deep_step16 + sum_Carbonloss_kg_deep_step17 + sum_Carbonloss_kg_deep_step18 + sum_Carbonloss_kg_deep_step19 + sum_Carbonloss_kg_deep_step20)%>%
  mutate(sum_CarbonStorage_kg_deep_total = CarbonStorage_kg_deep_step1 + CarbonStorage_kg_deep_step2 + CarbonStorage_kg_deep_step3 + CarbonStorage_kg_deep_step4 + CarbonStorage_kg_deep_step5 + CarbonStorage_kg_deep_step6 + CarbonStorage_kg_deep_step7 + CarbonStorage_kg_deep_step8 + CarbonStorage_kg_deep_step9 + CarbonStorage_kg_deep_step10 + CarbonStorage_kg_deep_step11 + CarbonStorage_kg_deep_step12 + CarbonStorage_kg_deep_step13 + CarbonStorage_kg_deep_step14 + CarbonStorage_kg_deep_step15 + CarbonStorage_kg_deep_step16 + CarbonStorage_kg_deep_step17 + CarbonStorage_kg_deep_step18 + CarbonStorage_kg_deep_step19 + CarbonStorage_kg_deep_step20)%>%
  dplyr::select(FIRENUMBER, FireName,TalikModel, Scenario, sum_Carbonloss_kg_deep_total, sum_CarbonStorage_kg_deep_total)


##combine the depths together 

cold_sum_total <- merge(cold_deep_sum,cold_3m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
cold_sum_total <- merge(cold_sum_total,cold_2m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
cold_sum_total <- merge(cold_sum_total,cold_1m, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))

cold_sum_total <- cold_sum_total%>%
  mutate(sum_Carbonloss_kg_total = sum_Carbonloss_kg_deep_total +sum_Carbonloss_kg_3m_total +sum_Carbonloss_kg_2m_total + sum_Carbonloss_kg_1m)%>%
  mutate(sum_CarbonStorage_kg_total = sum_CarbonStorage_kg_deep_total + sum_CarbonStorage_kg_3m_total + sum_CarbonStorage_kg_2m_total + CarbonStorage_kg_1m)%>%
  mutate(percent_loss = sum_Carbonloss_kg_total/sum_CarbonStorage_kg_total)


intermediate_sum_total <- merge(intermediate_deep_sum,intermediate_3m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
intermediate_sum_total <- merge(intermediate_sum_total,intermediate_2m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
intermediate_sum_total <- merge(intermediate_sum_total,intermediate_1m, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))

intermediate_sum_total <- intermediate_sum_total%>%
  mutate(sum_Carbonloss_kg_total = sum_Carbonloss_kg_deep_total +sum_Carbonloss_kg_3m_total +sum_Carbonloss_kg_2m_total + sum_Carbonloss_kg_1m)%>%
  mutate(sum_CarbonStorage_kg_total = sum_CarbonStorage_kg_deep_total + sum_CarbonStorage_kg_3m_total + sum_CarbonStorage_kg_2m_total + CarbonStorage_kg_1m)%>%
  mutate(percent_loss = sum_Carbonloss_kg_total/sum_CarbonStorage_kg_total)


warm_sum_total <- merge(warm_deep_sum,warm_3m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
warm_sum_total <- merge(warm_sum_total,warm_2m_sum, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))
warm_sum_total <- merge(warm_sum_total,warm_1m, by = c("FireName", "FIRENUMBER", "Scenario", "TalikModel"))

warm_sum_total <- warm_sum_total%>%
  mutate(sum_Carbonloss_kg_total = sum_Carbonloss_kg_deep_total +sum_Carbonloss_kg_3m_total +sum_Carbonloss_kg_2m_total + sum_Carbonloss_kg_1m)%>%
  mutate(sum_CarbonStorage_kg_total = sum_CarbonStorage_kg_deep_total + sum_CarbonStorage_kg_3m_total + sum_CarbonStorage_kg_2m_total + CarbonStorage_kg_1m)%>%
  mutate(percent_loss = sum_Carbonloss_kg_total/sum_CarbonStorage_kg_total)



Allfire_sum_total <- rbind(cold_sum_total, intermediate_sum_total, warm_sum_total)%>%
  mutate(sum_Carbonloss_kilotonnes_total = sum_Carbonloss_kg_total/1000000)

##Create a summary data frame and convert to kilotonnes 
Allfire_sum_summary <- Allfire_sum_total%>%
  group_by(FireName, Scenario)%>%
  summarize(median_sum_Carbonloss_kilotonnes_total = median(sum_Carbonloss_kilotonnes_total), 
            mean_sum_Carbonloss_kilotonnes_total = mean(sum_Carbonloss_kilotonnes_total),
            max_sum_Carbonloss_kilotonnes_total = max(sum_Carbonloss_kilotonnes_total),
            min_sum_Carbonloss_kilotonnes_total = min(sum_Carbonloss_kilotonnes_total))

##Adding the fires without any talik in the low scenarios

lowfires <- data.frame(FireName = c("Discovery Creek", "Burman Lake", "Little Mosquito"), 
                       Scenario = c("Low","Low","Low" ), 
                       median_sum_Carbonloss_kilotonnes_total = c(0,0,0),
                       mean_sum_Carbonloss_kilotonnes_total = c(0,0,0), 
                       max_sum_Carbonloss_kilotonnes_total = c(0,0,0), 
                       min_sum_Carbonloss_kilotonnes_total = c(0,0,0))

Allfire_sum_summary <- rbind(Allfire_sum_summary, lowfires)

Allfire_sum_summary <- Allfire_sum_summary%>%
  mutate(Scenario_ordered = case_when(Scenario == "Low" ~ "1-Low",
                                      Scenario == "Medium" ~ "2-Medium", 
                                      Scenario == "High" ~ "3-High"))

Allfire_sum_total <- Allfire_sum_total%>%
  mutate(Scenario_ordered = case_when(Scenario == "Low" ~ "1-Low",
                                      Scenario == "Medium" ~ "2-Medium", 
                                      Scenario == "High" ~ "3-High"))

##Boxplot by scenario and median value 
ggplot(Allfire_sum_summary, aes(x = Scenario_ordered, y = median_sum_Carbonloss_kilotonnes_total, fill = Scenario_ordered))+
  geom_boxplot() + 
  xlab("Scenario") + ylab("Talik carbon loss (kilotonnes C)")+
  scale_fill_manual(values = c("1-Low" = "#FFE180",  
                               "2-Medium" = "#EBA059",
                               "3-High" = "#BB3C76"))+
  theme_classic() + theme(legend.position = "none")

ggsave("Output/Talik_CarbonLoss.png", height = 10, width = 10)


Allfire_sum_total$Scenario_ordered <- factor(Allfire_sum_total$Scenario_ordered, levels = c("3-High", "2-Medium", "1-Low"))

##hisogram by scenario with the total 
gghistogram(
  Allfire_sum_total, x = "sum_Carbonloss_kilotonnes_total", 
  add = "median", rug = TRUE,
  fill = "Scenario_ordered", palette = c( "#BB3C76","#EBA059", "#FFE180"), 
  xlab = "Talik carbon loss (kilotonnes C)")

ggsave("Output/Talik_CarbonLoss_histogram.png", height = 10, width = 10)



ggplot(Allfire_sum_total, aes(x = FireName, y = sum_Carbonloss_kilotonnes_total,  fill = Scenario_ordered))+
  theme_classic()+
  geom_boxplot(outlier.size=0.1, outlier.alpha = 0.5)+theme(legend.position = "none", axis.text.x = element_text(angle = 90) )+
  scale_fill_manual(values = c("1-Low" = "#FFE180",  
                               "2-Medium" = "#EBA059",
                               "3-High" = "#BB3C76")) + ylab("Talik carbon loss (kilotonnes C)")
         
ggsave("Output/Talik_CarbonLoss_byfire.png", height = 10, width = 10)

Allfire_sum_summary_Low <- Allfire_sum_summary%>%
  filter(Scenario == "Low")

Allfire_sum_summary_Medium <- Allfire_sum_summary%>%
  filter(Scenario == "Medium")

Allfire_sum_summary_High <- Allfire_sum_summary%>%
  filter(Scenario == "High")

median(Allfire_sum_summary_Low$median_sum_Carbonloss_kilotonnes_total)
median(Allfire_sum_summary_Medium$median_sum_Carbonloss_kilotonnes_total)
median(Allfire_sum_summary_High$median_sum_Carbonloss_kilotonnes_total)

#######

Allfire_sum_total_High <- Allfire_sum_total%>%
  filter(Scenario == "High")%>%
  select(FireName, FIRENUMBER, sum_Carbonloss_kg_total)

Allfire_sum_total_Medium <- Allfire_sum_total%>%
  filter(Scenario == "Medium")%>%
  select(FireName, FIRENUMBER, sum_Carbonloss_kg_total)


Allfire_sum_total_Low <- Allfire_sum_total%>%
  filter(Scenario == "Low")%>%
  select(FireName, FIRENUMBER, sum_Carbonloss_kg_total)



###this is assuming we would look at the full fire perimeter and depth to the deepest talik
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
BigCreek2 <- read.csv("Output/Counterfactual_CarbonDensity/BigCreek2_CarbonDensityIntersectionTotal.csv")

Total_CarbonDensity <- rbind(BurmanLake,ChloyaLakes,BrooksCreek,ChenaDyke,DiscoveryCreek,
                             Cottonwood,TowahminaLake,SwiftFork,SuckerCreekNorth,
                             VunleLake,MartenCreek,QwikRiver,LittleMosquito,
                             TsedolalindinLake,TreatIsland,TwomileLake,VaultCreek,
                             BigCreek,BigCreek2,
                             NorthWhakatna,Huslia1,
                             CrowLake,BillyHawkCreek,GoldstreamCreek,OlnesPond,
                             TenOClock,GobletCreek,BoulderCreek,Kuranakh,GeorgeLake)


##Use the depth from the deepest Talik scenarios

Total_CarbonDensity <- Total_CarbonDensity%>%
  mutate(Fire_perimeter_m2 = SIZE_ACRES*4046.86)%>%
  mutate(Fire_perimeter_volume_m3_m1 = Fire_perimeter_m2*1)%>%
  mutate(Fire_perimeter_volume_m3_m2 = Fire_perimeter_m2*1)%>%
  mutate(Fire_perimeter_volume_m3_m3 = Fire_perimeter_m2*1)%>%
  mutate(Fire_perimeter_volume_m3_Deep = Fire_perimeter_m2*2)%>%
  mutate(Fire_perimeter_carbon_kg_m1 = carbon_100*Fire_perimeter_volume_m3_m1)%>%
  mutate(Fire_perimeter_carbon_kg_m2 = carbon_200*Fire_perimeter_volume_m3_m2)%>%
  mutate(Fire_perimeter_carbon_kg_m3 = carbon_300*Fire_perimeter_volume_m3_m3)%>%
  mutate(Fire_perimeter_carbon_kg_Deep = DeepCarbon_3plus*Fire_perimeter_volume_m3_Deep)%>%
  mutate(Fire_perimeter_carbon_kg_Total = Fire_perimeter_carbon_kg_Deep+Fire_perimeter_carbon_kg_m3+Fire_perimeter_carbon_kg_m2+Fire_perimeter_carbon_kg_m1)

Total_CarbonDensity_sub <- Total_CarbonDensity%>%
  select(FireName, FIRENUMBER, Fire_perimeter_carbon_kg_Total)

options(scipen = 999)

Total_CarbonDensity_TalikCarbonLoss_low <- merge(Total_CarbonDensity_sub, Allfire_sum_total_Low, by = c("FireName", "FIRENUMBER"))%>%
  mutate(percentTotalCarbon = sum_Carbonloss_kg_total/Fire_perimeter_carbon_kg_Total)


###Here we are going to only use the talik area but assume the carbon density and depth of yedoma 

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



####Now add a TOTAL talik depth, the deepest part of the talik to each of these dataframes


####Find the total talik Volume 

AllFire_totalTalik_Low <- rbind(AllFire_low_cold,AllFire_low_Intermediate,AllFire_low_warm)
AllFire_totalTalik_Medium <- rbind(AllFire_medium_cold,AllFire_medium_Intermediate,AllFire_medium_warm)
AllFire_totalTalik_High <- rbind(AllFire_high_cold,AllFire_high_Intermediate,AllFire_high_warm)

AllFire_totalTalik_Low <- AllFire_totalTalik_Low%>%
  mutate(TalikDepth_m = 4.92)

TalikCarbon_High <- read_csv("Output/TalikCarbonMass/TalikCarbon_high.csv")%>%
  mutate(Scenario = "High")
TalikCarbon_Medium <- read_csv("Output/TalikCarbonMass/TalikCarbon_medium.csv")%>%
  mutate(Scenario = "Medium")
TalikCarbon_Low <- read_csv("Output/TalikCarbonMass/TalikCarbon_Low.csv")%>%
  mutate(Scenario = "Low")


TalikCarbon_High <- TalikCarbon_High%>%
  filter(talik_area_m2 != 0)%>%
  filter(YSF == 30)%>%
  select(FireName, FIRENUMBER, CarbonStorage_Total_kg)

Allfire_sum_total_High <- Allfire_sum_total_High%>%
  filter(sum_Carbonloss_kg_total != 0)

TalikCarbon_High_withloss <- merge(Allfire_sum_total_High,TalikCarbon_High, by = c("FireName", "FIRENUMBER"))%>%
  mutate(percentLoss = (sum_Carbonloss_kg_total/CarbonStorage_Total_kg)*100)


library(sf)

Alaska_soils <- st_read("Data/NCSCD_Alaska.shp")




