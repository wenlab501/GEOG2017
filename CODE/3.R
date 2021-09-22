#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 3: Using R as a GIS (Intersections)           #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Source: Brunsdon and Comber(2018), Chapter 5          #
#                                                       #
#########################################################

rm(list = ls())

library(sf)
library(tmap)

setwd("D:/1092SA/Data")
load("Sample3.RData")

# Step 0: Loading tracts_sf
head(tracts_sf)
tm_shape(tracts_sf) + 
  tm_polygons("HSE_UNITS", palette = "Greens", style = "jenks", title = "No. of houses") +
  tm_layout(frame = F, legend.position = c(1,0.5))
#locator()

# Step 1: Buidlding fishnet
grid <- st_make_grid(tracts_sf, 5000,
                     crs = st_crs(tracts_sf),
                     what = "polygons", square = TRUE)

n <- length(lengths(grid))

grid_sf <- st_sf(index = 1:n, grid)
head(grid_sf)
names(grid_sf) <- c("grd_id","grid")

grd_bg <- tm_shape(grid_sf) + tm_polygons("grey90")
tracts <- tm_shape(tracts_sf) + tm_borders(col = "red")
grd_bg + tracts

# Step 2: Intersection
new_sf <- st_intersection(grid_sf, tracts_sf)
new_lyr <- tm_shape(new_sf) + tm_polygons("grey90")
new_lyr

# Step 3: Field calculation
head(new_sf)
new_sf$new_area<-st_area(new_sf)
new_sf$houses<- (new_sf$new_area / new_sf$AREA) * new_sf$HSE_UNITS

# Step 4: Grouping data
library(tidyverse)
new_sf <- summarise(group_by(new_sf, grd_id), count = sum(houses))
plot(new_sf)

head(new_sf)
grid_sf$houses <- 0
grid_sf$houses[new_sf$grd_id] <- new_sf$count  # using [grd_id] as the index

# Step 5: Mapping grid_sf
tm_shape(grid_sf) + 
  tm_polygons("houses", palette = "Greens", style = "jenks", title = "No. of houses") +
  tm_layout(frame = F, legend.position = c(1,0.5)) +
  tm_shape(tracts_sf) + tm_borders(col = "red")





