#####Talik Carbon Loss Data Analysis 

##Library 
library(tidyverse)
library(ggplot2)


##Upload the csvs from Step 06 
cold_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_cold_50_sum_withtotal.csv")
intermediate_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_Intermediate_50_sum_withtotal.csv")
warm_1m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_1m_Warm_50_sum_withtotal.csv")

cold_2m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_2m_cold_50_sum_withtotal_extra.csv")
intermediate_2m <- read.csv( "Output/TalikCarbonLoss/TalikCarbon_Total_2m_Intermediate_50_sum_withtotal_extra.csv")
warm_2m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_2m_Warm_50_sum_withtotal_extra.csv")

cold_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_cold_50_sum_withtotal_extra.csv")
intermediate_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_Intermediate_50_sum_withtotal_extra.csv")
warm_3m <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_3m_Warm_50_sum_withtotal_extra.csv")

cold_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_cold_50_sum_withtotal_extra.csv")
intermediate_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_Intermediate_50_sum_withtotal_extra.csv")
warm_deep <- read.csv("Output/TalikCarbonLoss/TalikCarbon_Total_deep_Warm_50_sum_withtotal_extra.csv")

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
                               "3-High" = "#BB3C76")) + ylab("Talik carbon loss (kilotonnes C)")+
  ylim(0, 15000)
         
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

#####

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


