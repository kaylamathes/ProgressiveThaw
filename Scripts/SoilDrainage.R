###Soil Texture Figure 

#Library 
library(tidyverse)
library(ggplot2)

#Data Upload 

VunleLakes <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_VunleLakes.csv")
VaultCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_VaultCreek.csv")
TwomileLake <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TwomileLake.csv")
GobletCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_GobletCreek.csv")
TsedolalindinLake <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TsedolalindinLake.csv")
TreatIsland <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TreatIsland.csv")
TenOClock <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TenOClock.csv")
SuckerCreekNorth <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_SuckerCreekNorth.csv")
NorthWhakatna <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_NorthWhakatna.csv")
QwikRiver <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_QwikRiver.csv")
OlnesPond <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_OlnesPond.csv")
Kuranakh <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_Kuranakh.csv")
LittleMosquito <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_LittleMosquito.csv")
GoldstreamCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_GoldstreamCreek.csv")
Huslia1 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_Huslia1.csv")
Cottonwood <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_Cottonwood.csv")
GeorgeLake <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_GeorgeLake.csv")
BurmanLake <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_BurmanLake.csv")
ChloyaLakes <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_ChloyaLakes.csv")
CrowLake <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_CrowLake.csv")
ChenaDyke <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_ChenaDyke.csv")
DiscoveryCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_DiscoveryCreek.csv")
BrooksCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_BrooksCreek.csv")
BoulderCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_BoulderCreek.csv")
BillyHawkCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_BillyHawkCreek.csv")
BigCreek <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_BigCreek.csv")

SwiftFork1 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_SwiftFork1.csv")
SwiftFork2 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_SwiftFork2.csv")
SwiftFork_Total <- rbind(SwiftFork1,SwiftFork2)

MartenCreek1 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_MartenCreek1.csv")
MartenCreek2 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_MartenCreek2.csv")
MartenCreek_Total <- rbind(MartenCreek1,MartenCreek2)

TowahminaLake1 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TowahminaLake1.csv")%>%
  select(!RID_old)
TowahminaLake2 <- read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_TowahminaLake2.csv")%>%
  select(!RID_old)

TowahminaLake_Total <- rbind(TowahminaLake1,TowahminaLake2)

AllFires <- rbind(TowahminaLake_Total,MartenCreek_Total,SwiftFork_Total,BigCreek,BillyHawkCreek,BoulderCreek,
                  BrooksCreek,DiscoveryCreek,ChenaDyke,CrowLake,ChloyaLakes,BurmanLake,GeorgeLake,Cottonwood,
                  Huslia1,GoldstreamCreek,LittleMosquito,Kuranakh,OlnesPond,QwikRiver,NorthWhakatna,
                  SuckerCreekNorth,TenOClock,TreatIsland,TsedolalindinLake,GobletCreek,TwomileLake,
                  VaultCreek,VunleLakes)


# 1. Calculate percentages
AllFires_summary <- AllFires %>%
  group_by(ID, soils) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(percent = (count / sum(count)))

AllFires_summary <- AllFires_summary %>%
  mutate(ID = fct_reorder(ID, 
                                 ifelse(soils == 1, percent, 0), 
                                 .fun = sum, 
                                 .desc = TRUE))

AllFires_summary$soils <- as.factor(AllFires_summary$soils)

AllFires_summary <- AllFires_summary%>%
  mutate(SoilDrainageType = case_when(soils == "1" ~ "1-Somewhat Excessively Drained", 
                                      soils == "2" ~ "2-Well Drained", 
                                      soils == "3" ~ "3-Moderately Well Drained", 
                                      soils == "4" ~ "4-Somewhat Poorly Drained", 
                                      soils == "5" ~ "5-Poorly Drained", 
                                      soils == "6"  ~"6-Very Poorly Drained"))

# 2. Create the plot
ggplot(AllFires_summary, aes(y = ID, x = percent, fill = SoilDrainageType)) +
  geom_bar(stat = "identity") +
  #scale_y_continuous(labels = scales::percent) +
  labs(
       y = "Fire Name",
       x = "Proportion",
       fill = "Soil Type") +
  theme_minimal()+
  scale_fill_manual(values = c("1-Somewhat Excessively Drained" = "#C3FF00", 
                               "2-Well Drained" = "#82FC2D", 
                               "3-Moderately Well Drained" =   "#16CB9D",
                               "4-Somewhat Poorly Drained" =  "#0A87D9", 
                               "5-Poorly Drained" = "#224DD7", 
                               "6-Very Poorly Drained" = "#0700B5"
                               ))
ggsave("ProgressiveThaw/Output/drainage_individualfires.png", height = 5, width = 8)

ggplot(AllFires_summary, aes(x = SoilDrainageType, y = percent, fill = SoilDrainageType))+
  theme_classic()+
  geom_boxplot()+
  scale_fill_manual(values = c("1-Somewhat Excessively Drained" = "#C3FF00", 
                               "2-Well Drained" = "#82FC2D", 
                               "3-Moderately Well Drained" =   "#16CB9D",
                               "4-Somewhat Poorly Drained" =  "#0A87D9", 
                               "5-Poorly Drained" = "#224DD7", 
                               "6-Very Poorly Drained" = "#0700B5"))     +
  xlab("Soil Drainage Class") +ylab("proportion across fires") +
theme(legend.position = "none", axis.text = element_text(size =10)) 

ggsave("ProgressiveThaw/Output/drainage.png", height = 5, width = 11)


