#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 12: Variogram Analysis                        #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
# Date: 2020-06-08                                      #
#                                                       #
#########################################################

library(rgdal)

rm(list=ls())
setwd("D:/1082SA/Data")
EPA_STN <- readOGR("EPA_STN1.shp", encoding="utf-8")
plot(EPA_STN); head(EPA_STN)

data= EPA_STN@data
PMdata=EPA_STN@data["PM"]

bubble(EPA_STN, "PM", col="red", fill=FALSE, maxsize = 1.5, main = "PM concentrations")


# Exploring Distance vs. PM concentration

x= coordinates(EPA_STN)[,1]
y= coordinates(EPA_STN)[,2]

STNDF = cbind(x,y)
dis_STN= dist(STNDF)

pm= EPA_STN@data[,12]

PMDF= cbind(pm,pm)
dis_PM = dist(PMDF)

plot(dis_PM~sqrt(dis_STN))
abline(lm(dis_PM~sqrt(dis_STN)), lwd=3, col='red')


library(gstat)
pm.vgm = variogram(PM~1, EPA_STN,cutoff=200000, width=2000)
pm.fit = fit.variogram(pm.vgm, model = vgm(500, "Exp",30000,5) )
plot(pm.vgm,pm.fit)

##################
grid <- makegrid(EPA_STN, cellsize = 5000)
grid <- SpatialPoints(grid, proj4string = EPA_STN@proj4string)
pm.kriged = krige(PM~1, EPA_STN, grid, model = pm.fit)
spplot(pm.kriged["var1.pred"],col.regions=lm.palette(20)) 
lm.palette=colorRampPalette(c( "white","orange","red"), space = "rgb")
points(EPA_STN,pch=20)

plot(dis_PM^2/2~dis_STN)
abline(lm(dis_PM~sqrt(dis_STN)), lwd=3, col='red')

TW=readOGR("Taiwan_county.shp")
plot(TW,add=T)
points(EPA_STN,col=brewer.pal(5,"Blues")[cut(EPA_STN$PM,5)],pch=20,cex=.8)
plot(pm.kriged,col=brewer.pal(5,"Reds")[cut(pm.kriged$var1.pred,5)],pch=15)
##################

SP=SpatialPointsDataFrame(cbind(runif(10),runif(10)),data.frame(z=runif(10,0,10)))
d=dist(SP@coords)
z=dist(SP$z)^2
plot(z~d)
vgm =variogram(SP$z~1, SP,cutoff=.9, width=0.1)
plot(vgm)

x=seq(0.1,0.9,0.1)
v.d=v.z=c()
for(i in 1:length(x)){
  judge=d>x[i]-0.1 & d<x[i]
  v.d[i]=mean(d[judge])
  v.z[i]=mean(z[judge])/2
}
plot(v.d,v.z)

x=seq(0,0.9,0.1)
v.d=v.z=c()
for(i in 1:(length(x)-1)){
  judge = d>x[i] & d<x[i+1]
  v.d[i]=mean(d[judge])
  v.z[i]=mean(z[judge])/2
}
