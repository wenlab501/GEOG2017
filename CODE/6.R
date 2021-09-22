#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 6: Nearest Neighbor Analysis                  #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
#########################################################

# Topics:
# 1. Introducing PPP format
# 2. Nearest neighbor disntance
# 3. K-order Nearest Neighbor Distance
# 4. G(d) function
# 5. G(d) function compared with a random pattern
# 6. PPP format: setting the boudary coordinates of a polygon
# 7. Generating Random Points


rm(list = ls())
library(sf)
library(tmap)
library(spatstat)

setwd("Data")
schools_sf <- st_read("Schools.shp")
county_sf <- st_read("Taiwan_county.shp")
head(county_sf)

# 1. Introducing PPP format

# method-1
bnd<-st_bbox(schools)
x.coor<-schools_sf$X_coor
y.coor<-schools_sf$Y_coor
x.range<-c(bnd[1],bnd[3]) 
y.range<-c(bnd[2],bnd[4])
schools_pp1 <- ppp(x.coor,y.coor,x.range,y.range)
plot(schools_pp1)

# method-2
schools_pp2 <- as.ppp(X=st_coordinates(schools_sf), W=as.owin(bnd) )


# 2. Nearest neighbor disntance

# calculating the area
x<-x.range[2]-x.range[1]
y<-y.range[2]-y.range[1]
sqr.area<- x*y

nnd<-nndist(schools_pp1, k=1)
d1<-mean(nnd) # Tainan School
rd<- 0.5/sqrt(424/sqr.area)  # theoretical random pattern
r.scale <- d1/rd

# 3. K-order Nearest Neighbor Distance

mean(nndist(schools_pp1, k=2))

ANN <- apply(nndist(schools_pp1, k=1:20),2,FUN=mean) # margin=2: columns
xValue= 1:20
yValue <- ANN
df1 <- data.frame(xValue,yValue)
ggplot(data=df1,aes(x=xValue, y=yValue))+ geom_line() + geom_point() + 
      xlab("K-order NN") +  ylab("distance") +
      ggtitle("Average K-order NN Distance")

# 4. G(d) function

G = ecdf(nnd) 
plot(G, main="G function", xlim=c(0,5000))

# 5. G(d) function compared with a random pattern

TN.Windows<-owin(xrange=x.range, yrange=y.range)
nn1<-rpoint(424, win=TN.Windows)
plot(nn1, pch=16)

nnd1<-nndist(nn1, k=1)
G1 = ecdf(nnd1) 
plot(G, main="G function", xlim=c(0,5000))
lines(G1,col='blue')

# 6. PPP format: setting the boundary coordinates of a polygon

index<- county_sf$COUNTY_ID == "67000" #"?x?n??"
TN_BND<- county_sf[index,]
plot(TN_BND)
xy <- st_coordinates(TN_BND)

x1<-rev(xy[,1]) # reverse the vector of X coord
y1<-rev(xy[,2]) # reverse the vector of Y coord
newxy<-cbind(x1, y1)
# building WINDOW file
PTS_bnd <- owin(poly=newxy) # owin: The vertices must be listed anticlockwise.
# ppp(x.coordinates, y.coordinates, window)
school.pp3<-ppp(x.coor,y.coor, window = PTS_bnd) 
plot(school.pp3)

# 7. Generating Random Points

nn2<- rpoint(415, win = PTS_bnd)
plot(nn2, pch=16) 


