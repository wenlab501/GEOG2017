##############################################
#
# Spatial Analysis (Geog 5069 (2018); NTU Geography)
#
# Lecture 9: Geostatistics
#
# Instructor: Dr. Tzai-Hung Wen 
#
##############################################


library(rgdal)

rm(list=ls())
setwd("C:/Users/user/Desktop/SA")

EPA_STN <- readOGR(dsn = "Data", layer = "EPA_STN2", encoding="utf-8")
plot(EPA_STN); head(EPA_STN)

data= EPA_STN@data
pm=EPA_STN@data["PM"]

bubble(EPA_STN, "PM", col="red", fill=FALSE, maxsize = 1.5, main = "PM concentrations")


# Exploring Distance vs. PM concentration

x= coordinates(EPA_STN)[,1]
y= coordinates(EPA_STN)[,2]

STNDF = cbind(x,y)
dis_STN= dist(STNDF)

PMDF= cbind(pm,pm)
dis_PM = dist(PMDF)

plot(dis_PM~sqrt(dis_STN))
abline(lm(dis_PM~sqrt(dis_STN)), lwd=3, col='red')

# generating grid
proj4string(EPA_STN)
grid <- makegrid(EPA_STN, cellsize = 5000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(EPA_STN)))

plot(EPA_STN)
plot(grid, pch = ".", add = T)


library(gstat)
pm.vgm = variogram(PM~1, EPA_STN,cutoff=80000, width=2000)
pm.fit = fit.variogram(pm.vgm, model = vgm(1000, "Exp",20000,1) )
plot(pm.vgm,pm.fit)

pm.kriged = krige(PM~1, EPA_STN, grid, model = pm.fit)

ntu_pts <-SpatialPoints(cbind(304023,2767886), proj4string = CRS(proj4string(EPA_STN)))
krige(PM~1, EPA_STN, ntu_pts, model = pm.fit)

spplot(pm.kriged["var1.pred"]) 
spplot(pm.kriged["var1.var"])

pm.condsim = krige(PM~1, EPA_STN, grid, model = pm.fit, nmax = 20, nsim = 10)
spplot(pm.condsim)


# IDW
pm.idw = idw(PM~1, EPA_STN, grid, idp=3)
spplot(pm.idw)

#TWPOP <- readOGR(dsn = "Data", layer = "Popn_TWN", encoding="utf-8")
#library(latticeExtra) # for layer
#spplot(pm.kriged["var1.pred"]) + layer(sp.polygons(TWPOP, alpha=0.5))

