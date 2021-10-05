##############################################
#
# Spatial Analysis (Geog 5069 (2018); NTU Geography)
# Lecture 6: Space-Time Interaction and Clustering
#
# Instructor: Dr. Tzai-Hung Wen 
#
##############################################


rm(list=ls())
setwd("C:/Users/user/Desktop/SA")

library(sp);library(rgdal)
library (splancs)  

source("ST_functions.R")
PT3 <- readOGR(dsn = "ST_Data", layer = "point3", encoding="big5")
head(PT3@data)

xcoord<-PT3@data$X
ycoord<-PT3@data$Y
time<-PT3@data$DAYS
Pts_Loc <- as.points(xcoord, ycoord) 

library(GISTools)
shades <- auto.shading(time, n=6)
choropleth(PT3,time, shades, pch=20, main="Sample")


################################
#
# Test of Space-time Interaction
#
################################

#KnoxM.test=function(x,y,time,del1,del2,Nrep)
out1<-KnoxM.test(xcoord,ycoord,time,8.8 ,8.6,999)
out1
hist(out1$Freq, freq=F)
lines(x=out1$Knox.T, y=0.2, col="red", type="h")

out2<-KnoxA.test(xcoord,ycoord,time,8.8 ,8.6)

#Mantel.test
out3<-Mantel.test(xcoord,ycoord,time,0.1,0.1,99)
out3$Mantel.T
out3$Simulated.p.value
hist(out3$Freq, freq=F)


#Jacquez.test
out4<-Jacquez.test(xcoord,ycoord,time,1,99)

Jstat<-Jacquez.test(xcoord,ycoord,time,2,99)



################################
#
# Space-time K function
#
################################


source("ST_functions.R")

ptdata <- read.table("ST_Data/Patients.csv", header=TRUE, sep=",")
Pts_Loc <- as.points(ptdata[,2], ptdata[,3]) 
Pts_time <- ptdata[,4] 
ptbnd <- read.table("ST_Data/Paitents_BND.csv", header=TRUE, sep=",")
Pts_BND <- as.points(ptbnd[,2], ptbnd[,3]) 
polymap(Pts_BND)
pointmap(Pts_Loc, add=T)

## Plotting D0(s,t)
kap1<- stkhat(Pts_Loc, Pts_time, Pts_BND, c(1955, 1980),seq(1,5),seq(0,4))
g1<- matrix(kap1$ks)
g2<- matrix(kap1$kt)
g1g2<- g1 %*% t(g2)

turD<- kap1$kst - g1g2

persp(kap1$s, kap1$t, turD, theta=-30, phi = 15, expand = 0.5, xlim=c(0,5), ylim=c(0,4), xlab="spatial distance", ylab="temporal distance", zlab="D", ticktype ="detailed" )

turD0<- kap1$kst/g1g2-1.0

persp(kap1$s, kap1$t, turD0, theta=-30,phi = 15, expand = 0.5, xlim=c(0,5), ylim=c(0,4), xlab="spatial distance", ylab="temporal distance", zlab="D0", ticktype ="detailed" )

# plotting standardized residuals R(s,t)
se<- stsecal(Pts_Loc, Pts_time, Pts_BND,c(1955, 1980),seq(1,5),seq(0,4))
Res<- turD / se
plot( g1g2, Res ,ylim=c(-3, 6), xlab="K1(s)K2(t)", ylab=" standardized residuals R(s,t)")
abline(h=c(-2,2), lty=2)

