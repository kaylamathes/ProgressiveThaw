#Code and comments taken from:https://vt-bioinformatics.github.io/rgeoraster.html
library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(terra)

##Fire Perimeters
DEM_tiles <-  st_read("/Users/kmathes/Desktop/Combustion Model/FABDEM_v1-2_tiles.geojson")%>%
  st_transform(crs = 3338)

counterfactual_perimeter = st_read("/Users/kmathes/Desktop/Combustion Model/Counterfactual/Output/2010_TsedolalindinLake/Perimeter_TsedolalindinLake.shp")%>%
  st_transform(crs = 3338)



##Find overlaping DEM tiles with the fire points datafile 
Tile_intersection <- st_intersection(counterfactual_perimeter,DEM_tiles )

unique(Tile_intersection$tile_name)


###Active layer thickness Talucci Dataset
ALD <- read.csv("TransientThaw/Data/PermafrostFireDatacubeFinal_20240718.csv")


###Filtering the datasets for only boreal interior and separating dataset out by disturbed/undisturbed. then turn some columns into numeric 
ALD_sub <- ALD %>%
  filter(biome == "boreal")%>%
  filter(cntryId != "RU")

unique_combinations_ALD_sub  <- ALD_sub  %>%
  distinct(lat, lon)
  

ALD_sub_sf <- st_as_sf(ALD_sub, coords = c("lon", "lat"), crs = 4326)
ALD_sub_sf <- st_transform(ALD_sub_sf, crs = 4326)

st_write(ALD_sub_sf, "TransientThaw/Data/Talucci_NA_boreal_sf",driver="ESRI Shapefile", append = FALSE)


##Find overlaping DEM tiles with the fire points datafile 
Tile_intersection <- st_intersection(unique_combinations_ALD_sub_sf,DEM_tiles )

cat(paste0(sprintf("'%s'", length(unique(Tile_intersection$tile_name)))))

##Set some themes 
#theme_set(theme_classic())
tmap_mode("view")

##download tile looking at arcGIS tile map, finding the tile(s) that overlap with individual fire perimeters. 
#https://woodwell.maps.arcgis.com/apps/mapviewer/index.html?webmap=b6e979647429461c992c789226b5622b
##load in DEM raster



############################ TPI Calculation ###################

dem <- raster("/Users/kmathes/Desktop/DATA/FABDEM/N67W147_FABDEM_V1-2.tif", crs = '+init=EPSG:4326')

writeRaster(dem, "TPI_Calculations/Output/dem_N67W147.tif", overwrite = TRUE) 

###TPI 
wbt_diff_from_mean_elev(
  dem = "TPI_Calculations/Output/dem_N67W147.tif",
  output = "TPI_Calculations/Output/TPI_N67W147.tif",
  filterx = 100,
  filtery =100,
  wd = NULL,
  verbose_mode = FALSE,
  compress_rasters = FALSE
)




tpi_raster <- rast("TPI_Calculations/Output/TPI_N67W147.tif")
plot(tpi_raster, main = "Topographic Position Index (DiffFromMeanElev)")



###Slope

wbt_slope(
  dem = "TPI_Calculations/Output/dem_N67W147.tif",
  output = "TPI_Calculations/Output/Slope_N67W147.tif",
  zfactor = 1,
  units = "degrees"
)

slope_raster <- rast("TPI_Calculations/Output/Slope_N67W147.tif")
plot(slope_raster)


tpi_r <- rast("TPI_Calculations/Output/TPI_N67W147.tif")
slope_r <- rast("TPI_Calculations/Output/Slope_N67W147.tif")


##Sanity Check: Print the raster objects to ensure they loaded correctly
# and show a numeric data type (e.g., 'float', 'INT4U', 'double').
print("Slope Raster Info:")
print(slope_r)
print("TPI Raster Info:")
print(tpi_r)


# --- 2. Define Classification Thresholds ---

# Lowland criteria:
# 1. Slope < 5 degrees
# 2. TPI >= -5
# 3. TPI <= 5

slope_threshold_max <- 5
tpi_threshold_min <- -5
tpi_threshold_max <- 5


# --- 3. Create the Binary Classification Raster ---

# STACK the rasters first. This is crucial for applying a function across both layers.
# The order is important: the function below expects c(slope, tpi)
input_stack <- c(slope_r, tpi_r)

# Define the classification function
classify_landform <- function(x) {
  # x is a vector where x[1] is slope and x[2] is TPI
  slope <- x[1]
  tpi <- x[2]
  
  # Apply the combined condition using vectorized R logic
  is_lowland <- slope < slope_threshold_max & 
    tpi >= tpi_threshold_min & 
    tpi <= tpi_threshold_max
  
  # Return 1 for Lowland (TRUE) or 0 for Upland (FALSE)
  return(ifelse(is_lowland, 1, 0))
}

# Apply the function to the stacked rasters.
# 'app' applies a function to cells, processing both layers in the stack together.
upland_lowland_r <- app(input_stack, classify_landform)


# --- 4. Set Categories and Visualize ---

# Set category names for clarity in the output file metadata
upland_lowland_r <- classify(upland_lowland_r, 
                             rcl = data.frame(from = c(0, 1), 
                                              to = c(0, 1), 
                                              becomes = c(0, 1), 
                                              is_class = c("Upland", "Lowland")))

# Check the new raster's summary and categories
print(upland_lowland_r)
cats(upland_lowland_r)

# Plot the new raster for visualization
plot(upland_lowland_r, 
     main = "Upland (0) vs. Lowland (1) Classification",
     col = c("brown", "green"),
     legend = TRUE, 
     plg = list(legend = c("Upland (0)", "Lowland (1)"), 
                fill = c("brown", "green")))


# --- 5. Export the New Raster ---

# Export the final raster as a GeoTIFF
writeRaster(upland_lowland_r, 
            filename = "upland_lowland_classification_updated.tif", 
            overwrite = TRUE, 
            datatype = "INT1U") # Saves as a small 8-bit integer file


landform <- rast("/Users/kmathes/Desktop/imageToDriveExample_transform.tif")
plot(landform)
