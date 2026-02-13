###Figure out the area that might undergo progressive thaw based on Burn depth and upland/lowland area


###### Topography classification 

##Start with the EPA ecoregion 

##Load Data 

##library 
library(sf)
library(tidyverse)

#Perimeter Data
perimeter <- st_read("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2011_Cottonwood/Perimeter_Cottonwood.shp")%>%
st_transform(.,crs = 3338)%>%
  mutate(FireName = "Cottonwood")

##Boreal forest ecoregion 
boreal_eco_region <- st_read("/Users/kmathes/Desktop/DATA/ak_eco_l3/ak_eco_l3.shp")



##Run an intersection to fin the ecoregions of the fire perimeters

###We are looking for "Uplands": So select only the areas with: 
## 3.1.1: Boreal Forested Uplands and Lowlands
## 6.1.1 Interior Highlands and Klondike PLateau 
## 6.1.2 Alaska Range 

##Remove areas with: 

## 3.1.2 Interior Bottomlands 
## 3.1.3 Yukon Flats 
polygon_ecoregion <- st_intersection(perimeter,boreal_eco_region)

boreal_eco_region_nobottomlands <- polygon_ecoregion%>%
  filter(US_L3NAME != "Interior Bottomlands")%>%
  filter(US_L3NAME != "Yukon Flats")


ggplot()+
  geom_sf(data = perimeter)+
  geom_sf(data = boreal_eco_region_nobottomlands, aes(fill = US_L3NAME))


##For perimeters that have forested Interior Bottomlands and Uplands, 
##we need to determine whether or not they are bottomlands or uplands

##Bring in a slope dataset
slope <- read.csv("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2011_Cottonwood/Cottonwood_CombustionModel.csv")%>%
  dplyr::select(ID, lat, lon,elevation, slope)%>%
  drop_na()

##convert to sf 
slope_sf <- st_as_sf(slope, coords = c("lon", "lat"), crs = 4326)
slope_sf <- st_transform(slope_sf, crs = 3338)


##Find the slope values from just the Interior Boreal Region to find the high and lowlands 
boreal_eco_region_boreal <- boreal_eco_region_nobottomlands%>%
  filter(US_L3NAME == "Interior Forested Lowlands and Uplands")


slope_boreal_eco_region_boreal <- st_intersection(slope_sf, boreal_eco_region_boreal)

###Assign some kind of elevation criteria to what constitutes uplands vs lowlands 
##Maybe start with any area greater than the average elevation 

slope_boreal_eco_region_boreal 


###Save the total perimeters for area upland area

#st_write(boreal_eco_region_nobottomlands, "ProgressiveThaw/Output/UplandTotalPerimeter/ChloyaLakes_Upland.shp", driver  = "ESRI Shapefile", append = FALSE)


