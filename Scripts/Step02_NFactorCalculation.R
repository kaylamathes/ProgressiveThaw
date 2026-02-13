############################################################################
################N-Factor calcuation for counterfactual fire perimeters 
############################################################################

##Library
library(tidyverse)
library(sf)

###Air Temperature

perimeter_Air <- read.csv("ProgressiveThaw/Data/NFactor/AirTemp_perimeters/ERA5DailyMeanT2M_SwiftFork.csv")%>%
  pivot_longer(., cols = starts_with("X"),
names_to = "Date",
values_to = "AirTempC")%>%
  dplyr::select(!FID)%>%
  dplyr::select(!.geo)%>%
  dplyr::select(!system.index)%>%
  mutate(across(Date, ~ . |> str_extract("\\d+") |> as.character())) %>%
  mutate(Date=ymd(Date))%>%
  group_by(Date)%>%
  summarize(meanAirTempC = mean(AirTempC),maxAirTempC = max(AirTempC),minAirTempC = min(AirTempC))


#####Ground Temperature 

perimeter_Ground <- read.csv("ProgressiveThaw/Data/NFactor/SoilTemperature_perimeters/ERA5DailyMean_soilTLevel1_SwiftFork.csv")%>%
  pivot_longer(., cols = starts_with("X"),
               names_to = "Date",
               values_to = "SoilTempC")%>%
  dplyr::select(!FID)%>%
  dplyr::select(!.geo)%>%
  dplyr::select(!system.index)%>%
  mutate(across(Date, ~ . |> str_extract("\\d+") |> as.character())) %>%
  mutate(Date=ymd(Date))%>%
  group_by(Date)%>%
  summarize(meanSoilTempC = mean(SoilTempC),maxSoilTempC = max(SoilTempC),minSoilTempC = min(SoilTempC))

###Combine Soil and air temp columns per perimeter 

perimeter_merge <- merge(perimeter_Air, perimeter_Ground, by = "Date")

##Create Separate Freezing and Thawing seasons based off of mean air temperature 

perimeter_freezing <- perimeter_merge%>%
  filter(meanAirTempC <=0)

perimeter_Thawing <- perimeter_merge%>%
  filter(meanAirTempC > 0)

###Freezing 
###Calculate the Freezing Degree Days for air and soil 
##Mean 
DDAf_mean <- sum(perimeter_freezing$meanAirTempC)
DDSf_mean <- sum(perimeter_freezing$meanSoilTempC)

##Calculate the Freezing N-Factor 

Nf_mean <- (0-DDSf_mean)/ (0-DDAf_mean)

##Max
DDAf_max <- sum(perimeter_freezing$maxAirTempC)
DDSf_max <- sum(perimeter_freezing$maxSoilTempC)

##Calculate the Freezing N-Factor 

Nf_max <- (0-DDSf_max)/ (0-DDAf_max)

##Min
DDAf_min <- sum(perimeter_freezing$minAirTempC)
DDSf_min <- sum(perimeter_freezing$minSoilTempC)

##Calculate the Freezing N-Factor 
Nf_min <- (0-DDSf_min)/ (0-DDAf_min)

##Thawing 
###Calculate the Thawing Degree Days for air and soil 

#mean
DDAu_mean <- sum(perimeter_Thawing$meanAirTempC)
DDSu_mean <- sum(perimeter_Thawing$meanSoilTempC)

##Calculate the Thawing N-Factor 
uF_mean <- (DDSu_mean)/ (DDAu_mean)

#max
DDAu_max <- sum(perimeter_Thawing$maxAirTempC)
DDSu_max <- sum(perimeter_Thawing$maxSoilTempC)

##Calculate the Thawing N-Factor 
uF_max <- (DDSu_max)/ (DDAu_max)

#min
DDAu_min <- sum(perimeter_Thawing$minAirTempC)
DDSu_min <- sum(perimeter_Thawing$minSoilTempC)

##Calculate the Thawing N-Factor 
uF_min <- (DDSu_min)/ (DDAu_min)


### Create New Column 

FireName <- "Swift Fork"

NFactor_new <- data.frame(FireName, Nf_mean, uF_mean, Nf_max, Nf_min, uF_max,uF_min)


##read 
NFactor <- read_csv("ProgressiveThaw/Data/NFactor/NFactor_Results.csv")

NFactor_total <- rbind(NFactor_new,NFactor)

write_csv(NFactor_total, "ProgressiveThaw/Data/NFactor/NFactor_Results.csv", append = FALSE)

###STOP### 


#### START######
################################################################################################################
################################### Assign a model from the Rey et al. 2020 post-fire talik deveopment model 
##Library
library(tidyverse)
library(sf)
library(ggplot2)
library(gt)
library(gtExtras)

###Reference numbers: 

##Rey et al. 2020 and Jorgenson et al. 2010 say that interior Alaska N-factors range from: 

# nF (Freezing): 0.12 - 0.44 

# uF (Thawing): 0.5 - 0.7 

###parameters for the inital conditions of the Rey et al. 2020 Talik development Model 

## Cold Model:         nf = 0.4 uf = 0.6 
## Intermediate Model: nf = 0.35, uf = 0.5
## Warm model:         nf = 0.2, uf = 0.6

#### Read in NFactor calculations 
NFactor <- read_csv("ProgressiveThaw/Data/NFactor/NFactor_Results.csv")

NFactor_modelselection <- NFactor%>%
  mutate(ReyModel_mean = case_when(Nf_mean <= 0.2 ~ "Warm", 
                                   Nf_mean > 0.2 & Nf_mean <= 0.35 ~ "Intermediate", 
                                   Nf_mean > 0.35 ~ "Cold"))%>%
  mutate(ReyModel_max = case_when(Nf_max <= 0.2 ~ "Warm", 
                                   Nf_max > 0.2 & Nf_max <= 0.35 ~ "Intermediate", 
                                   Nf_max > 0.35 ~ "Cold"))%>%
  mutate(ReyModel_min = case_when(Nf_min <= 0.2 ~ "Warm", 
                                  Nf_min > 0.2 & Nf_min <= 0.35 ~ "Intermediate", 
                                  Nf_min > 0.35 ~ "Cold"))%>%
  dplyr::select(FireName, ReyModel_mean, ReyModel_max, ReyModel_min)%>%
  arrange(desc(ReyModel_mean))



categories <- c("Warm", "Intermediate", "Cold")

# 2. Build the color-coded table
styled_table <- NFactor_modelselection%>%
  gt()%>%
  # Clean up formatting
tab_options(
  table.width = pct(80),
  column_labels.font.weight = "bold"
)%>%
  # Apply color coding to the categorical columns
  data_color(
    columns = c(ReyModel_mean, ReyModel_max, ReyModel_min),
    fn = scales::col_factor(
      palette = c("Warm" = "#c5daeeff", "Intermediate" = "#5f97cdff", "Cold" = "#17436eff"),
      domain = categories
    )
  ) %>%
  # Clean up formatting
  tab_options(
    table.width = pct(80),
    column_labels.font.weight = "bold"
  ) %>%
  # Optional: Add a subtle theme
  cols_label(
    FireName = "Fire",
    ReyModel_mean = "N-Factor (mean)",
    ReyModel_max = "N-Factor (max)",
    ReyModel_min = "N-Factor (min)"
  )

# Display the table
styled_table
  
gtsave(styled_table, "ProgressiveThaw/Data/NFactor/N-factor_ReyModel.png")


