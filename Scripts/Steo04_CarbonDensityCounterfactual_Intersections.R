### Carbon Density to volume calcuations 

library(tidyverse)
library(sf)
library(parallel)
library(tidyverse)
library(data.table)


##Data upload 
CarbonDensity <- read.csv("Data/CarbonDensity/Palmtag_CarbonStorage_FullCounterfactualPerimeter_Step06_CarbonStorage_BurmanLake.csv")
##convert to sf 
CarbonDensity_sf <- st_as_sf(CarbonDensity, coords = c("lon", "lat"), crs = 4326)
points <- st_transform(CarbonDensity_sf, crs = 3338)


#Perimeter Data
polygons_notvalid = st_read("/Users/kmathes/Desktop/FSPro_Runs/2021_Burman Lake/2021_Burman_Lake_FINAL_032525_FSPro_EnsemblePerimeters/2021_Burman_Lake_FINAL_032525_FSPro_EnsemblePerimeters.shp")%>%
  st_transform(crs = 3338)

##Find the NA valid fires 
polygons_notvalid <- polygons_notvalid%>%
  mutate(Valid = as.character(st_is_valid(.)))

##Delete the NA valid fires 
polygons <- polygons_notvalid%>%
  filter(Valid == "TRUE")

####Split up the batches 
batch_size <- 100000
batches <- split(points, ceiling(seq_len(nrow(points)) / batch_size))

#500
polygons_clip_500 <- polygons%>%
  filter(FIRENUMBER < 500)

results_500 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_500, join = st_intersects)
})

final_result_500 <- rbindlist(results_500)

summary_500 <- final_result_500 %>%
  st_drop_geometry() %>%  # Drop geometry for faster summary
  group_by(SIZE_ACRES, FIRENUMBER) %>%
  summarize(
    carbon_100_mean = mean(carbon_100, na.rm = TRUE),
    carbon_200_mean = mean(carbon_200, na.rm = TRUE),
    carbon_300_mean = mean(carbon_300, na.rm = TRUE),
    carbon_100_max = max(carbon_100, na.rm = TRUE),
    carbon_200_max = max(carbon_200, na.rm = TRUE),
    carbon_300_max = max(carbon_300, na.rm = TRUE),
    carbon_100_min = min(carbon_100, na.rm = TRUE),
    carbon_200_min = min(carbon_200, na.rm = TRUE),
    carbon_300_min = min(carbon_300, na.rm = TRUE),
    .groups = 'drop'
  )%>%
  drop_na()

write.csv(summary_500, "Output/Couterfactual_CarbonDensity/Partial/500.csv", row.names = FALSE)












####Once all run, upload to bind the files together 

CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")



burndepth_total <- rbind(burndepth1,burndepth2,burndepth3,burndepth4,
                         burndepth5,burndepth6,burndepth7,burndepth8,
                         burndepth9,burndepth10)


write.csv(burndepth_total, "Counterfactual/Output/2019_BigCreek/BigCreek_Burndepth_Total.csv", row.names = FALSE)


