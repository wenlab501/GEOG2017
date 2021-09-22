#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 5: Quadrat Analysis in R                      #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
#########################################################

rm(list = ls())
library(sf)
library(tmap)

setwd("D:/1092SA/Data/")
schools_sf<- st_read("Schools.shp")
head(schools_sf)

# Step 1: Buidlding fishnet
grid <- st_make_grid(schools_sf, 5000,
                     crs = st_crs(schools_sf),
                     what = "polygons", square = TRUE)

n <- length(lengths(grid))

grid_sf <- st_sf(index = 1:n, grid)
head(grid_sf)
names(grid_sf) <- c("grd_id","grid")

grd_bg <- tm_shape(grid_sf) + tm_polygons("grey90")
schools_lyr <- tm_shape(schools_sf) + tm_dots(col = "red") + tm_layout(frame = F)
grd_bg + schools_lyr

# Step 2: Quadrat counting
new_sf <- st_intersection(grid_sf, schools_sf)
head(new_sf)

library(tidyverse)
quad_sf <- summarise(group_by(new_sf, grd_id), count = length(grd_id))

grid_sf$count <- 0
grid_sf$count[quad_sf$grd_id] <- quad_sf$count  # using [grd_id] as the index

grid_lyr <- tm_shape(grid_sf) + 
            tm_polygons("count", palette = "Greens", style = "jenks", n=7, title = "No. of points")
grid_lyr + schools_lyr

# Step 3: VMR test
mean <- mean(grid_sf$count)
var<- var(grid_sf$count)

vmr <- var/mean
se<- sqrt((2/(n-1)))

t <- (vmr-1)/se
pvalue <- pt(t,df=n-1,lower.tail = F)

######################################
#
# ?ɥR(?i????)?GUsing quadrat.test()
#
######################################

library(spatstat)
bnd<-st_bbox(schools_sf)
x.coor<-schools_sf$X_coor
y.coor<-schools_sf$Y_coor
x.range<-c(bnd[1],bnd[3]) 
y.range<-c(bnd[2],bnd[4])
schools_pp1 <- ppp(x.coor,y.coor,x.range,y.range)

# Chi-squared test
z=quadrat.test(schools_pp1, nx=11, ny=11,
             alternative=c("two.sided"),
             method="Chisq")


z$observed%>%sort
grid_sf$count%>%sort

# Monte Carlo test of CSR
quadrat.test(schools_pp1, nx=11, ny=11,
             alternative=c("two.sided"),
             method="MonteCarlo", nsim=999)

((z$observed-z$expected)^2/z$expected)%>%sum


a=c(rep(0,13),rep(1,5),2,2,3,4,4,6,6)
b=mean(a)

sum((a-b)^2/b)

