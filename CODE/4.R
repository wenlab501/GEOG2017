#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 4: Describing Spatial Point Patterns          #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
#                                                       #
#########################################################

# Using R package:aspace

# Measures of Centrality
#1.(Weighted) Mean center 
#2.Median center
#3.Central feature

# Measures of Dispersion
#1.Standard Distance
#2.Weighted Std. Distance
#3.Standard Deviational Ellipse

rm(list = ls())
library(sf)
library(tmap)

setwd("D:/1092SA/Data")
schools_sf<- st_read("Schools.shp")
head(schools_sf)
schools_lyr <- tm_shape(schools_sf)+tm_dots(col="red", size= 0.1) + tm_layout(frame = F)

##########################
#
# 0. Data Preparation
#
##########################


# Generating no. of student in each school
schools_sf$Students<-as.integer(runif(424,100,1000))

# Generating school type: cluster vs. isolation
for (i in 1: 424) {
  
  if (schools_sf$NEAR_DIST[i]< 500) {
    schools_sf$type[i]<- "Cluster"
  } else schools_sf$type[i]<- "Isolation"
  
}

index<- schools_sf$type == "Cluster"
school_cluster <- schools_sf[index,]

length(school_cluster)
schools.c_lyr <- tm_shape(school_cluster)+tm_dots(col="red", size= 0.1) + tm_layout(frame = F)

School_df <- data.frame(x=schools_sf$X_coor, y=schools_sf$Y_coor, 
                       type=schools_sf$type, students=schools_sf$Students)


##########################
#
# 1. Mean Center
#
##########################


#install.packages("aspace")
library(aspace)

Mean.Center<-mean_centre(id=1, weighted=FALSE, weights=NULL, points=School_df[,1:2])
W.Mean.Center<-mean_centre(id=1, weighted=TRUE, weights=School_df$students, points=School_df[,1:2])

Mean.Center_sfg = st_point(c(Mean.Center[,2],Mean.Center[,3]))
class(Mean.Center_sfg)
Mean.Center_sfc = st_sfc(Mean.Center_sfg)
Mean.Center_sf <- st_sf(Mean.Center_sfc)
head(Mean.Center_sf) 
st_crs(Mean.Center_sf) <- st_crs(schools_sf)
Mean.Center_lyr <- tm_shape(Mean.Center_sf)+tm_dots(col="blue", size= 0.5)

W.Mean.Center_sfg = st_point(c(W.Mean.Center[,2],W.Mean.Center[,3]))
W.Mean.Center_sfc = st_sfc(W.Mean.Center_sfg)
W.Mean.Center_sf <- st_sf(W.Mean.Center_sfc)
st_crs(W.Mean.Center_sf) <- st_crs(schools_sf)
W.Mean.Center_lyr <- tm_shape(W.Mean.Center_sf)+tm_dots(col="green", size= 0.5)

schools_lyr + Mean.Center_lyr + W.Mean.Center_lyr


##########################
#
# 2. Median Center
#
##########################

Median.Center<-median_centre(id=1,points=School_df[,1:2])
Median.Center_sfg = st_point(c(Median.Center[,2],Median.Center[,3]))
Median.Center_sfc = st_sfc(Median.Center_sfg)
Median.Center_sf <- st_sf(Median.Center_sfc)
st_crs(Median.Center_sf) <- st_crs(schools_sf)
Median.Center_lyr <- tm_shape(Median.Center_sf)+tm_dots(col="green", size= 0.5)

### Compare Mean vs. Median
schools_lyr + Mean.Center_lyr + Median.Center_lyr


############################
#
# 3. SDD: Standard Distance
#
############################

school.SDD<- calc_sdd(id=1, points=School_df[,1:2])

center.x<- school.SDD$CENTRE.x
center.y<- school.SDD$CENTRE.y

center_sfg <- st_point(x = c(center.x, center.y))
center_sfc <- st_sfc(center_sfg)
center_sf<- st_sf(center_sfc)
st_crs(center_sf) <-  st_crs(schools_sf)
rad<- school.SDD$SDD.radius
SD_sf<- st_buffer(center_sf, rad)
SD_lyr <- tm_shape(SD_sf) + tm_borders(col = "blue")+tm_layout(frame = F)
Center_lyr <-  tm_shape(center_sf) + tm_dots(col="blue", size= 0.5)
schools_lyr+ Center_lyr+ SD_lyr 


school.SDD2<- calc_sdd(id=1, points = School_df[,1:2], weighted = TRUE, weights=School_df$students)


## SDD to shapefile example
shp <- convert.to.shapefile(sddloc, sddatt,"id",5) # ESRI Shape type 1=point, 3=polyLine, 5=polygon
write.shapefile(shp, "SDD_Shape", arcgis=T) # Replace "." with "\_" in column names for ArcGIS

SDD_Shape_sf<- st_read("SDD_Shape.shp")
st_crs(SDD_Shape_sf) <-  st_crs(schools_sf)
schools_lyr+ tm_shape(SDD_Shape_sf) + tm_borders(col = "blue")+tm_layout(frame = F)

########################################
#
# 4. SDE:Standard Deviational Ellipse
#
########################################

## SDE Center: sdeatt
school.SDE<- calc_sde(id=1, points=School_df[,1:2])
head(sdeatt)
SDE.center.x<- sdeatt$CENTRE.x
SDE.center.y<- sdeatt$CENTRE.y
SDE.center_sfg <- st_point(x = c(SDE.center.x, SDE.center.y))
SDE.center_sfc <- st_sfc(SDE.center_sfg)
SDE.center_sf<- st_sf(SDE.center_sfc)
st_crs(SDE.center_sf) <-  st_crs(schools_sf)
SDE.center_lyr <- tm_shape(SDE.center_sf) + tm_dots(col = "blue", size=0.5)+tm_layout(frame = F)

## SDD to shapefile 
shp2 <- convert.to.shapefile(sdeloc, sdeatt,"id",5) # ESRI Shape type 1=point, 3=polyLine, 5=polygon
write.shapefile(shp2, "SDE_Shape", arcgis=T) # Replace "." with "\_" in column names for ArcGIS

SDE_Shape_sf<- st_read("SDE_Shape.shp")
st_crs(SDE_Shape_sf) <-  st_crs(schools_sf)

schools_lyr+ SDE.center_lyr + 
  tm_shape(SDE_Shape_sf) + tm_borders(col = "blue")+tm_layout(frame = F)

## SDE Polygon: using sdeloc
xcoor <- sdeloc$x
ycoor <- sdeloc$y
xy = data.frame(x=xcoor, y=ycoor)
xys = st_as_sf(xy, coords=c("x","y"))
st_crs(xys)<-st_crs(schools_sf)

SDE_sfc<- st_cast(st_combine(xys),"POLYGON")
SDE_sf<-st_sf(poly_sfc)
SDE_lyr <- tm_shape(SDE_sf) + tm_borders(col = "blue") +tm_layout(frame = F)

schools_lyr+ SDE.center_lyr+ SDE_lyr


########################################
#
# 5. Central Feature
#
########################################


school.CF<- CF(id=1, points=School_df[,1:2])

CF.x<- school.CF$CF.x
CF.y<- school.CF$CF.y
CF_sfg <- st_point(x = c(CF.x, CF.y))
CF_sfc <- st_sfc(CF_sfg)
CF_sf<- st_sf(CF_sfc)
st_crs(CF_sf) <-  st_crs(schools_sf)
CF_lyr <- tm_shape(CF_sf) + tm_dots(col = "blue", size= 0.5) +tm_layout(frame = F)
schools_lyr + CF_lyr


###########################################
#
# 6. Mean Centers by Grouping Attribubtes
#
###########################################

plot(schools_sf["type"], pch=16)

type<-schools_sf$type
newid<- unique(type)

#Students<-schools_sf$Students
#new<-aggregate(Students~type, schools_sf, sum)
#newid<-new[,1]

xx<-vector(); yy<-vector(); ctype<-vector()

for (i in 1:2){
  index<-(type == newid[i])
  newschool<-schools_sf[index,]
  xcoor<-newschool$X_coor
  ycoor<-newschool$Y_coor
  newschool.mc <- mean_centre(id=1, points=cbind(xcoor, ycoor))
  xx[i]<-newschool.mc$CENTRE.x
  yy[i]<-newschool.mc$CENTRE.y
  ctype[i]<-newid[i]
}

newcenterxy <- data.frame(xx,yy, ctype)
New_sf <- st_as_sf(newcenterxy , coords=c("xx","yy"))
st_crs(New_sf) <-  st_crs(schools_sf)

New_lyr <- tm_shape(New_sf) + tm_dots(col="ctype", palette=c(Cluster='blue', Isolation='cyan'), size= 0.5) + tm_layout(frame = F) 
schools_lyr2 <- tm_shape(schools_sf)+tm_dots("type",palette=c(Cluster='red', Isolation='orange'), size= 0.1) + tm_layout(frame = F)
schools_lyr2 + New_lyr 

