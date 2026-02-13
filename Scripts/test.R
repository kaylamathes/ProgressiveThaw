
# Load libraries
library(sf)
library(parallel)
library(tidyverse)
library(data.table)
library(ggplot2)

options(scipen=999)

##Upload the Area determination perimeters 

perimeters_1000 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_1000.shp")
perimeters_2000 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_2000.shp")
perimeters_3000 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_3000.shp")
perimeters_4000 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_4000.shp")
perimeters_5000 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_5000.shp")

perimeter_total <- rbind()
################## Burn Depth Intersection ###############
##Burn Depth 
Burn_depth  <- read.csv("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2022_Kuranakh/Kuranakh_BurnDepthRetrained.csv")%>%
  dplyr::select(lat, lon, prediction, ID)%>%
  rename(depth_prediction=prediction)

Burn_depth_criteria <- Burn_depth%>%
  filter(depth_prediction>=15)

##convert to sf 
Burn_depth_criteria_sf <- st_as_sf(Burn_depth_criteria , coords = c("lon", "lat"), crs = 4326)
points <- st_transform(Burn_depth_criteria_sf, crs = 3338)


# --- Create Clipping Polygon (Buffer) ---
# Define a Projected CRS (e.g., UTM zone 15N, EPSG:32615) for accurate metric buffer
# We must project before buffering!
target_crs <- 32615

# Project both datasets to the metric CRS
points_proj <- st_transform(points, target_crs)


# Create a 30-meter buffer around the points and dissolve/union them.
# The 'dist = 30' is now correctly interpreted as 30 meters.
points_buffer <- st_buffer(points_proj, dist = 30)%>%
  st_union() %>%
  st_as_sf()

polygons_proj <- st_transform(perimeters_2000, target_crs)


# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj <- st_intersection(polygons_proj, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84 <- st_transform(intersection_proj, 4326)

###Calculating area
intersection_proj_wgs84<- intersection_proj_wgs84%>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)


