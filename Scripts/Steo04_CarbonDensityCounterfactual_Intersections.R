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

write.csv(summary_500, "Output/Counterfactual_CarbonDensity/Partial/500.csv", row.names = FALSE)



#1000
polygons_clip_1000 <- polygons%>%
  filter(FIRENUMBER >= 500 & FIRENUMBER < 1000)

results_1000 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_1000, join = st_intersects)
})

final_result_1000 <- rbindlist(results_1000)

summary_1000 <- final_result_1000 %>%
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

write.csv(summary_1000, "Output/Counterfactual_CarbonDensity/Partial/1000.csv", row.names = FALSE)

#1500
polygons_clip_1500 <- polygons%>%
  filter(FIRENUMBER >= 100 & FIRENUMBER < 1500)

results_1500 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_1500, join = st_intersects)
})

final_result_1500 <- rbindlist(results_1500)

summary_1500 <- final_result_1500 %>%
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

write.csv(summary_1500, "Output/Counterfactual_CarbonDensity/Partial/1500.csv", row.names = FALSE)

#2000
polygons_clip_2000 <- polygons%>%
  filter(FIRENUMBER >= 1500 & FIRENUMBER < 2000)

results_2000 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_2000, join = st_intersects)
})

final_result_2000 <- rbindlist(results_2000)

summary_2000 <- final_result_2000%>%
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

write.csv(summary_2000, "Output/Counterfactual_CarbonDensity/Partial/2000.csv", row.names = FALSE)

#2500
polygons_clip_2500 <- polygons%>%
  filter(FIRENUMBER >= 2000 & FIRENUMBER < 2500)

results_2500 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_2500, join = st_intersects)
})

final_result_2500 <- rbindlist(results_2500)

summary_2500 <- final_result_2500%>%
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

write.csv(summary_2500, "Output/Counterfactual_CarbonDensity/Partial/2500.csv", row.names = FALSE)

#3000
polygons_clip_3000 <- polygons%>%
  filter(FIRENUMBER >= 2500 & FIRENUMBER < 3000)

results_3000 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_3000, join = st_intersects)
})

final_result_3000 <- rbindlist(results_3000)

summary_3000 <- final_result_3000%>%
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

write.csv(summary_3000, "Output/Counterfactual_CarbonDensity/Partial/3000.csv", row.names = FALSE)


#3500
polygons_clip_3500 <- polygons%>%
  filter(FIRENUMBER >= 3000 & FIRENUMBER < 3500)

results_3500 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_3500, join = st_intersects)
})

final_result_3500 <- rbindlist(results_3500)

summary_3500 <- final_result_3500%>%
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

write.csv(summary_3500, "Output/Counterfactual_CarbonDensity/Partial/3500.csv", row.names = FALSE)

#4000
polygons_clip_4000 <- polygons%>%
  filter(FIRENUMBER >= 3500 & FIRENUMBER < 4000)

results_4000 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_4000, join = st_intersects)
})

final_result_4000 <- rbindlist(results_4000)

summary_4000 <- final_result_4000%>%
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

write.csv(summary_4000, "Output/Counterfactual_CarbonDensity/Partial/4000.csv", row.names = FALSE)

#4500
polygons_clip_4500 <- polygons%>%
  filter(FIRENUMBER >= 4000 & FIRENUMBER < 4500)

results_4500 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_4500, join = st_intersects)
})

final_result_4500 <- rbindlist(results_4500)

summary_4500 <- final_result_4500%>%
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

write.csv(summary_4500, "Output/Counterfactual_CarbonDensity/Partial/4500.csv", row.names = FALSE)

#5000
polygons_clip_5000 <- polygons%>%
  filter(FIRENUMBER >= 4500 & FIRENUMBER < 5000)

results_5000 <- lapply(batches, function(batch) {
  st_join(batch, polygons_clip_5000, join = st_intersects)
})

final_result_5000 <- rbindlist(results_5000)

summary_5000 <- final_result_5000%>%
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

write.csv(summary_5000, "Output/Counterfactual_CarbonDensity/Partial/5000.csv", row.names = FALSE)


####Once all run, upload to bind the files together 

CarbonDensity_500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/500.csv")
CarbonDensity_1000  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/1000.csv")
CarbonDensity_1500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/1500.csv")
CarbonDensity_2000  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/2000.csv")
CarbonDensity_2500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/2500.csv")
CarbonDensity_3000  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/3000.csv")
CarbonDensity_3500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/3500.csv")
CarbonDensity_4000  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/4000.csv")
CarbonDensity_4500  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/4500.csv")
CarbonDensity_5000  <- read.csv("Output/Couterfactual_CarbonDensity/Partial/5000.csv")



CarbonDensity_total <- rbind(CarbonDensity_500,CarbonDensity_1000,CarbonDensity_1500,
                             CarbonDensity_2000,CarbonDensity_2500,CarbonDensity_3000,
                             CarbonDensity_3500,CarbonDensity_4000,CarbonDensity_4500,
                             CarbonDensity_5000)


write.csv(CarbonDensity_total, "Output/Couterfactual_CarbonDensity/BurmanLake_CarbonDensityIntersection.csv", row.names = FALSE)


