#############################################################################################################
############################ Talik Area Determination ###########################################
#############################################################################################################


#############################################################################################################
############################ Burn Point Criteria Determination ##############################
#############################################################################################################

# Load libraries
library(sf)
library(parallel)
library(tidyverse)
library(data.table)
library(ggplot2)

options(scipen=999)


#######Create burn point criteria for talik area determination 
#######Burn Depth, Soil texture and upland burn points 

##Burn Depth 
Burn_depth  <- read.csv("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2020_SwiftFork/SwiftFork_BurnDepthMeanImputation2.csv")%>%
  rename(depth_prediction=Burn_depth)

Burn_depth_sf <- st_as_sf(Burn_depth , coords = c("lon", "lat"), crs = 4326)
Burn_depth_sf <- st_transform(Burn_depth_sf, crs = 3338)


##Upland 
Upland <- read.csv("TransientThaw/Data/Counterfactual_landform_classification/SwiftFork_Upland.csv")
Upland_sf <- st_as_sf(Upland, coords = c("lon", "lat"), crs = 4326)
Upland_sf <- st_transform(Upland_sf, crs = 3338)


#Soil Texture
SoilTexture <-read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_SwiftFork1.csv")

SoilTexture_sf <- st_as_sf(SoilTexture, coords = c("lon", "lat"), crs = 4326)
SoilTexture_sf <- st_transform(SoilTexture_sf, crs = 3338)


SoilTexture2 <-read.csv("ProgressiveThaw/Data/SoilDrainageClass/DrainageClass_SwiftFork2.csv")

SoilTexture_sf2 <- st_as_sf(SoilTexture2, coords = c("lon", "lat"), crs = 4326)
SoilTexture_sf2 <- st_transform(SoilTexture_sf2, crs = 3338)

SoilTexture_Total <- rbind(SoilTexture,SoilTexture2)



##Merge into single dataframe  
Combined <- merge(Upland,SoilTexture_Total, by = c("lat", "lon"))
Combined <- merge(Combined, Burn_depth, by = c("lat", "lon"))

Combined_sf <- st_as_sf(Combined, coords = c("lon", "lat"), crs = 4326)
Combined_sf <- st_transform(Combined_sf, crs = 3338)

#########Filtering for Criteria: Very High: High severity (>=12 cm burn depth : coarse textured soils: upland)
#########Filtering for Criteria: High: High severity (>=15 cm burn depth : coarse textured soils: upland)
#########Filtering for Criteria: Medium: High severity (>18 cm burn depth : very coarse textured soils: upland)
Combined_criteria <- Combined%>%
  filter(depth_prediction > 12)%>%
  filter(soils == "1" | soils == "2" | soils == "3")%>%
  dplyr::select(ID, soils, depth_prediction, landform, lat, lon, .geo)


##convert to sf 
Combined_criteria_sf <- st_as_sf(Combined_criteria, coords = c("lon", "lat"), crs = 4326)
points <- st_transform(Combined_criteria_sf, crs = 3338)

##CHANGE THE NAME!!!!
st_write(points, "/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Counterfactual_Talik_perimeters_V2/Talik_criteria_v2_SwiftFork_VeryHigh.shp", driver = "ESRI Shapefile", append = FALSE)




#############################################################################################################
############################ Create Intersections with the ensemble perimeters ##############################
#############################################################################################################
# Load libraries
library(sf)
library(parallel)
library(tidyverse)
library(data.table)
library(ggplot2)

options(scipen=999)

##Load ensemble perimeters 

#Counterfactual Perimeter Data
polygons_notvalid = st_read("/Users/kmathes/Desktop/FSPro_Runs/2018_Twomile Lake/2018_Twomile_Lake_NEW_5000f_Final_Rare_Clip_040725_FSPro_EnsemblePerimeters/2018_Twomile_Lake_NEW_5000f_Final_Rare_Clip_040725_FSPro_EnsemblePerimeters.shp")%>%
  st_transform(crs = 3338)

##Find the NA valid fires 
polygons_notvalid <- polygons_notvalid%>%
  mutate(Valid = as.character(st_is_valid(.)))

##Delete the NA valid fires 
polygons <- polygons_notvalid%>%
  filter(Valid == "TRUE")

####Read in criteria points: Changing the soils criteria. Will keep the consistent 1-3 textures throughout the scenarios and just change the burn depth 
points <- st_read("Output/Counterfactual_Talik_perimeters_V2/Talik_criteria_v2_TwomileLake_VeryHigh.shp")


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

polygons_proj <- st_transform(polygons, target_crs)

##500 
polygons_proj_500  <- polygons_proj%>%
  filter(FIRENUMBER <= 500)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_500 <- st_intersection(polygons_proj_500, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_500 <- st_transform(intersection_proj_500, 4326)

###Calculating area
intersection_proj_wgs84_500<- intersection_proj_wgs84_500%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_500, "Output/Counterfactual_Talik_perimeters_V2/Partial/500.csv")


##1000 
polygons_proj_1000  <- polygons_proj%>%
  filter(FIRENUMBER > 500 & FIRENUMBER <= 1000)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_1000 <- st_intersection(polygons_proj_1000, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_1000 <- st_transform(intersection_proj_1000, 4326)

###Calculating area
intersection_proj_wgs84_1000<- intersection_proj_wgs84_1000%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_1000, "Output/Counterfactual_Talik_perimeters_V2/Partial/1000.csv")


##1500 
polygons_proj_1500  <- polygons_proj%>%
  filter(FIRENUMBER > 1000 & FIRENUMBER <= 1500)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_1500 <- st_intersection(polygons_proj_1500, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_1500 <- st_transform(intersection_proj_1500, 4326)

###Calculating area
intersection_proj_wgs84_1500<- intersection_proj_wgs84_1500%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_1500, "Output/Counterfactual_Talik_perimeters_V2/Partial/1500.csv")


##2000
polygons_proj_2000  <- polygons_proj%>%
  filter(FIRENUMBER > 1500 & FIRENUMBER <= 2000)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_2000 <- st_intersection(polygons_proj_2000, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_2000 <- st_transform(intersection_proj_2000, 4326)

###Calculating area
intersection_proj_wgs84_2000 <- intersection_proj_wgs84_2000%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_2000, "Output/Counterfactual_Talik_perimeters_V2/Partial/2000.csv")


##2500
polygons_proj_2500  <- polygons_proj%>%
  filter(FIRENUMBER > 2000 & FIRENUMBER <= 2500)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_2500 <- st_intersection(polygons_proj_2500, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_2500 <- st_transform(intersection_proj_2500, 4326)

###Calculating area
intersection_proj_wgs84_2500 <- intersection_proj_wgs84_2500%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_2500, "Output/Counterfactual_Talik_perimeters_V2/Partial/2500.csv")


##3000
polygons_proj_3000  <- polygons_proj%>%
  filter(FIRENUMBER > 2500 & FIRENUMBER <= 3000)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_3000 <- st_intersection(polygons_proj_3000, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_3000 <- st_transform(intersection_proj_3000, 4326)

###Calculating area
intersection_proj_wgs84_3000 <- intersection_proj_wgs84_3000%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_3000, "Output/Counterfactual_Talik_perimeters_V2/Partial/3000.csv")


##3500
polygons_proj_3500  <- polygons_proj%>%
  filter(FIRENUMBER > 3000 & FIRENUMBER <= 3500)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_3500 <- st_intersection(polygons_proj_3500, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_3500 <- st_transform(intersection_proj_3500, 4326)

###Calculating area
intersection_proj_wgs84_3500 <- intersection_proj_wgs84_3500%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_3500, "Output/Counterfactual_Talik_perimeters_V2/Partial/3500.csv")


##4000
polygons_proj_4000  <- polygons_proj%>%
  filter(FIRENUMBER > 3500 & FIRENUMBER <= 4000)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_4000 <- st_intersection(polygons_proj_4000, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_4000 <- st_transform(intersection_proj_4000, 4326)

###Calculating area
intersection_proj_wgs84_4000 <- intersection_proj_wgs84_4000%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_4000, "Output/Counterfactual_Talik_perimeters_V2/Partial/4000.csv")


##4500
polygons_proj_4500  <- polygons_proj%>%
  filter(FIRENUMBER > 4000 & FIRENUMBER <= 4500)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_4500 <- st_intersection(polygons_proj_4500, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_4500 <- st_transform(intersection_proj_4500, 4326)

###Calculating area
intersection_proj_wgs84_4500 <- intersection_proj_wgs84_4500%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_4500, "Output/Counterfactual_Talik_perimeters_V2/Partial/4500.csv")


##5000
polygons_proj_5000  <- polygons_proj%>%
  filter(FIRENUMBER > 4500 & FIRENUMBER <= 5000)

# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj_5000 <- st_intersection(polygons_proj_5000, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84_5000 <- st_transform(intersection_proj_5000, 4326)

###Calculating area
intersection_proj_wgs84_5000 <- intersection_proj_wgs84_5000%>%
  st_make_valid() %>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)%>%
  st_drop_geometry()

write.csv(intersection_proj_wgs84_5000, "Output/Counterfactual_Talik_perimeters_V2/Partial/5000.csv")



intersection_proj_wgs84_500 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/500.csv")
intersection_proj_wgs84_1000 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/1000.csv")
intersection_proj_wgs84_1500 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/1500.csv")
intersection_proj_wgs84_2000 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/2000.csv")
intersection_proj_wgs84_2500 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/2500.csv")
intersection_proj_wgs84_3000 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/3000.csv")
intersection_proj_wgs84_3500 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/3500.csv")
intersection_proj_wgs84_4000 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/4000.csv")
intersection_proj_wgs84_4500 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/4500.csv")
intersection_proj_wgs84_5000 <- read_csv("Output/Counterfactual_Talik_perimeters_V2/Partial/5000.csv")

intersection_proj_wgs84_total <- rbind(intersection_proj_wgs84_1000, intersection_proj_wgs84_2000,
                                       intersection_proj_wgs84_3000,intersection_proj_wgs84_4000,
                                       intersection_proj_wgs84_5000,intersection_proj_wgs84_500,
                                       intersection_proj_wgs84_1500,intersection_proj_wgs84_2500,
                                       intersection_proj_wgs84_3500,intersection_proj_wgs84_4500)

#####Join the full polygon Dataframe with the talik Area to include all the ensemble perimeters with zero talik area 
polygons_csv <- polygons%>%
  st_drop_geometry()%>%
  dplyr::select(!Valid)

#intersection_proj_wgs84_csv <- intersection_proj_wgs84_total%>%
 # st_drop_geometry()

full_polygons <- left_join(polygons_csv, intersection_proj_wgs84_total, by = c("FIRENUMBER", "SIZE_ACRES"))
  
full_polygons$talik_area_m2 <- as.numeric(full_polygons$talik_area_m2)
full_polygons$talik_area_acres <- as.numeric(full_polygons$talik_area_acres)

full_polygons <- full_polygons%>%
  replace_na(list(talik_area_m2 = 0, talik_area_acres = 0))

###Write Files
write.csv(full_polygons,"/Users/kmathes/Desktop/ProgressiveThaw/Output/Counterfactual_Talik_perimeters_V2/Intersections/High/Talik_perimeter_v2_TwomileLake_VeryHigh.csv")

##st_write(intersection_proj_wgs84_total, "/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Counterfactual_Talik_perimeters_V2/Talik_perimeters_v2_BigCreek.shp", driver = "ESRI Shapefile", append = FALSE)






############################ THE END ##########################################################

####Graphing 
intersection_proj_wgs84_total$talik_area_m2 <- as.numeric(intersection_proj_wgs84_total$talik_area_m2)


ggplot() +
  geom_sf(data = polygons, color = "black")+
  geom_sf(data = SoilTexture_sf, aes(color = soils))


  geom_sf(data = intersection_proj_wgs84_total, fill = "darkred",color = "darkred")







############################################ Trial 2 ############################################

#### Soil perimeter
Soil_perimeter <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Soil_Perimeters/soils_Kuranakh.shp")%>%
  st_transform(crs = 3338)%>%
  rename(soils = label)%>%
  filter(soils == "2" | soils == "3" | soils == "5" | soils == "6")

##Landform perimeters 

Landform_perimeter_1 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N66W147.shp")%>%
  st_transform(crs = 3338)%>%
  rename(landform = label)%>%
  filter(landform == 0)%>%
  mutate(Valid = as.character(st_is_valid(.)))

Landform_perimeter_1_valid <- Landform_perimeter_1%>%
  filter(Valid == "TRUE")

Landform_perimeter_2 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N65W145.shp")%>%
  st_transform(crs = 3338) %>%
  rename(landform = label)%>%
  filter(landform == 0) %>%
  mutate(Valid = as.character(st_is_valid(.)))

Landform_perimeter_2_valid <- Landform_perimeter_2%>%
  filter(Valid == "TRUE")

Landform_perimeter_3 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N65W147.shp")%>%
  st_transform(crs = 3338) %>%
  rename(landform = label)%>%
  filter(landform == 0)%>%
mutate(Valid = as.character(st_is_valid(.)))

Landform_perimeter_3_valid <- Landform_perimeter_3%>%
  filter(Valid == "TRUE")

Landform_perimeter_4 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N66W145.shp")%>%
  st_transform(crs = 3338) %>%
  rename(landform = label)%>%
  filter(landform == 0)
  st_make_valid(.)

  Landform_perimeter_4_valid <- Landform_perimeter_4%>%
  filter(Valid == "TRUE")

Landform_perimeter_5 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N65W146.shp")%>%
  st_transform(crs = 3338) %>%
  rename(landform = label)%>%
  filter(landform == 0)%>%
  mutate(Valid = as.character(st_is_valid(.)))%>%
  st_make_valid(.)
 

Landform_perimeter_6 <- st_read("/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Data/Landform_perimeters/landform_Kuranakh_N66W146.shp")%>%
  st_transform(crs = 3338) %>%
  rename(landform = label)%>%
  filter(landform == 0)%>%
  mutate(Valid = as.character(st_is_valid(.)))

Landform_perimeter_6_valid <- Landform_perimeter_6%>%
  filter(Valid == "TRUE")

Landform_perimeter_total <- rbind(Landform_perimeter_1_valid,Landform_perimeter_2_valid,Landform_perimeter_3_valid,Landform_perimeter_4_valid,Landform_perimeter_5,Landform_perimeter_6_valid)



####Load the ensemble perimeters
ensembleperimeter <- st_read("/Users/kmathes/Desktop/FSPro_Runs/EnsemblePerimeters_Kuranakh.shp")%>%
  st_transform(crs = 3338)

soil_landform_intersection <- st_intersection(Soil_perimeter, Landform_perimeter_total)

ggplot()+
  geom_sf(data = Soil_perimeter, fill = "blue")+
  geom_sf(data = Landform_perimeter_total, fill = "green")+
  geom_sf(data = soil_landform_intersection)


##Intersection:
ensembleperimeter_sub <- ensembleperimeter%>%
  filter(FIRENUMBER >= 4000 & FIRENUMBER <= 5000)

ensemble_intersection_5000 <- st_intersection(ensembleperimeter_sub, soil_landform_intersection)%>%
  group_by(FIRENUMBER, SIZE_ACRES)%>%
  summarise(geometry = st_union(geometry))

st_write(ensemble_intersection_5000, "/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_5000.shp")

ggplot() +
  geom_sf(data = ensemble_intersection_3000)


ensemble_intersection_total <- rbind(ensemble_intersection,ensemble_intersection_2000,
                                     ensemble_intersection_3000,ensemble_intersection_4000,
                                     ensemble_intersection_5000)

st_write(ensemble_intersection_total, "/Users/kmathes/Desktop/PermafrostThaw/ProgressiveThaw/Output/Partial_landform_soil_perimeters/Kuranakh_total.shp")


ggplot() +
  geom_sf(data = ensembleperimeter_total )+
  geom_sf(data = Landform_intersection_soil, fill = "blue")



################## Burn Depth Intersection ###############
##Burn Depth 
Burn_depth  <- read.csv("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2022_Kuranakh/Kuranakh_BurnDepthRetrained.csv")%>%
  dplyr::select(lat, lon, prediction, ID)%>%
  rename(depth_prediction=prediction)

Burn_depth_criteria <- burn_depth%>%
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

polygons_proj <- st_transform(Landform_intersection_soil_total, target_crs)


# --- Clip the Target Polygon ---
# Perform the intersection using the projected layers
intersection_proj <- st_intersection(polygons_proj_sub, points_buffer)

#  Re-project back to WGS84 for mapping or storage
intersection_proj_wgs84 <- st_transform(intersection_proj, 4326)

###Calculating area
intersection_proj_wgs84<- intersection_proj_wgs84%>%
  mutate(talik_area_m2 = st_area(.))%>%
  mutate(talik_area_acres = talik_area_m2*0.000247105)




