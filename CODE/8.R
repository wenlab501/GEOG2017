#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 8: Kernel Density Estimation                  #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
#########################################################

rm(list = ls())
library(sf)
library(tmap)
library(SpatialKDE)

#######################
#
# 0. Data Preparation
#
#######################

setwd("D:/1092SA/Data/")
schools_sf <- st_read("Schools.shp")
county_sf <- st_read("TaiwanCounty.shp")
index<- county_sf$COUNTY_ID == "67000" # Tainan city
TN_BND<- county_sf[index,]
TN_BND_lyr <- tm_shape(TN_BND) + tm_borders((col="blue"))
schools_lyr <- tm_shape(schools_sf) + tm_dots(col = "red") + tm_layout(frame = F)

###############################
#
# 1. Kernek Density Estimation
#
###############################

### 1.1 Polygon Grid
  
grid1 <- create_grid_rectangular(TN_BND, cell_size = 500)
tm_shape(grid1) + tm_polygons(col = "grey90") + TN_BND_lyr
  

TN_KDE <- kde(schools_sf, band_width = 4000, grid = grid1)
TN_KDE_lyr <- tm_shape(TN_KDE) + 
              tm_polygons(col = "kde_value", palette = "Greens", border.alpha=0, title = "school density") 

TN_KDE_lyr + TN_BND_lyr + schools_lyr 


# Optimal Bandwidth Setting: Bowman and Azzalini's rule
#
# https://en.wikipedia.org/wiki/Kernel_density_estimation
# https://doi.org/10.1016/j.csda.2005.06.019
# optimal bandwidth = sd *{2/(3*n)}^(1/(6))

X <- st_coordinates(schools_sf)
sigma <- mean(sd(X[,1]), sd(X[,2])) * (2 / (3 * nrow(X))) ^ (1/6)
sigma


### 1.2 Raster Grid
raster_grid <- create_raster(TN_BND, cell_size = 500)
TN_KDE2 <- kde(schools_sf, band_width = 4000, grid = raster_grid)
head(TN_KDE2)

TN_KDE2_lyr<- tm_shape(TN_KDE2) + 
              tm_raster(palette = "Greens", title = "KDE Estimate")

TN_KDE2_lyr + TN_BND_lyr + schools_lyr 

library(raster)
ncell(TN_KDE2)
freq(TN_KDE2)
mean(TN_KDE2)

HKDE<- TN_KDE2[TN_KDE2 > 10] 
hist(HKDE)


#######################
#
# 2. Raster Algebra
#
#######################

### TPE City Boundary
index<- county_sf$COUNTY_ID == "63000" # TPE City
TPE_BND<- county_sf[index,]
st_crs(TPE_BND) <- st_crs(county_sf)
TPE_BND_lyr <- tm_shape(TPE_BND) + tm_borders((col="grey")) +tm_layout(frame = F)

### Reading points 1 and 2
data1 <- read.table("point1.csv", header=TRUE, sep=",")
data2 <- read.table("point2.csv", header=TRUE, sep=",")

xcoor <- data1[,1]
ycoor <- data1[,2]
pts1_df = data.frame(x=xcoor, y=ycoor)
pts1_sf = st_as_sf(pts1_df, coords=c("x","y"))
st_crs(pts1_sf) <- st_crs(county_sf)
pts1_lyr <- tm_shape(pts1_sf)+tm_dots(col="red", size= 0.1)

xcoor <- data2[,1]
ycoor <- data2[,2]
pts2_df = data.frame(x=xcoor, y=ycoor)
pts2_sf = st_as_sf(pts2_df, coords=c("x","y"))
st_crs(pts2_sf) <- st_crs(county_sf)
pts2_lyr <- tm_shape(pts2_sf)+tm_dots(col="blue", size= 0.1)

TPE_BND_lyr + pts2_lyr + pts1_lyr 


### KDE
raster_grid <- create_raster(TPE_BND, cell_size = 100)
TPE_Pts1 <- kde(pts1_sf, band_width = 3000, grid = raster_grid)
TPE_KDE1_lyr<- tm_shape(TPE_Pts1) + tm_raster(palette = "Greens", title = "KDE of Pts 1")
TPE_KDE1_lyr + TPE_BND_lyr

TPE_Pts2 <- kde(pts2_sf, band_width = 3000, grid = raster_grid)
TPE_KDE2_lyr<- tm_shape(TPE_Pts2) + tm_raster(palette = "Blues", title = "KDE of Pts 2")
TPE_KDE2_lyr + TPE_BND_lyr

### Raster Algebra
TPE_Diff<- TPE_Pts2 - 10 * TPE_Pts1
TPE_Diff_lyr<- tm_shape(TPE_Diff) + tm_raster(palette = "OrRd", title = "Diff")
TPE_Diff_lyr + TPE_BND_lyr


