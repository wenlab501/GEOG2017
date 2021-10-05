##############################################
#
# Spatial Analysis (Geog 5069 (2018); NTU Geography)
#
# Lecture 10: Spatial Regression
#
# Instructor: Dr. Tzai-Hung Wen 
#
##############################################


rm(list=ls())
setwd("C:/Users/user/Desktop/SA")

library(rgdal)
library (spdep)  

#**********************************************************************

data(columbus)

# Compariing col.moran1 and col.moran2
# lm.morantest() vs. resid() + moran.test()
col.listw <- nb2listw(col.gal.nb)

columbus.lm0<- lm(CRIME ~ 1, data=columbus); summary(columbus.lm0)
col.moran1 <-  lm.morantest(columbus.lm0,col.listw); col.moran1

col.e <- resid(columbus.lm0)
col.moran2 <- moran.test(col.e,col.listw, randomisation=FALSE); col.moran2


# OLS Regression
columbus.lm<- lm(CRIME ~ INC + HOVAL, data=columbus)
summary(columbus.lm)

# Setting Neighbors
col.listw <- nb2listw(col.gal.nb)

# Checking the regression residuals
# Global Moran's I for LM regression residuals
col.moran <- lm.morantest(columbus.lm, col.listw); col.moran
col.moran2 <-  lm.morantest(columbus.lm,col.listw,alternative="two.sided"); col.moran2

# What Not To Do
col.e <- resid(columbus.lm)
col.morane <- moran.test(col.e,col.listw, randomisation=FALSE,alternative="two.sided")

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation
columbus.lagrange <- lm.LMtests(columbus.lm,col.listw,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
columbus.lagrange

# Maximum Likelihood Estimation of the Spatial Lag Model
columbus.lag <- lagsarlm(CRIME ~ INC + HOVAL,data=columbus, col.listw); summary(columbus.lag)

# What Not To Do: OLS estimation
attach(columbus)
lagCRIME <- lag.listw(col.listw,CRIME)
wrong.lag <- lm(CRIME ~ lagCRIME + INC + HOVAL); summary(wrong.lag)
detach(columbus)

# Maximum Likelihood Estimation of the Spatial Erroe Model
columbus.err <- errorsarlm(CRIME ~ INC + HOVAL,data=columbus,col.listw); summary(columbus.err)

# Spatial Durbin Model
columbus.durbin <- lagsarlm(CRIME ~ INC+HOVAL,data=columbus, col.listw, type="mixed"); summary(columbus.durbin)

#***********************************************************************


NY8 <- readOGR("NY_Data", "NY8_utm18")

head(NY8@data)

plot(NY8)

nylm <- lm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8)
summary(nylm)
NY8$lmresid <- residuals(nylm)
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(NY8, zcol="lmresid", col.regions=lm.palette(20), main="Resid")

NY_nb <- read.gal("NY_Data/NY_nb.gal", region.id=row.names(NY8))
NYlistw<-nb2listw(NY_nb, style = "B")
lm.morantest(nylm, NYlistw)

# Simultaneous Autoregressive Model (SAR)
nysar<-spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistw)
summary(nysar)

# Mapping Trend vs. Stochastic 
NY8$sar_trend <- nysar$fit$signal_trend
NY8$sar_stochastic <- nysar$fit$signal_stochastic
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(NY8, zcol="sar_trend", col.regions=lm.palette(20), main="sar_Trend")
spplot(NY8, zcol="sar_stochastic", col.regions=lm.palette(20), main="sar_Stochastic")

# Spatial Lag Model 
NYlistwW <- nb2listw(NY_nb, style = "W")
nylag <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nylag)

# Spatial Error Model
nyerr <- errorsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW)
summary(nyerr)

# Spatial Durbin Model (SDM)
nymix <- lagsarlm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, data=NY8, listw=NYlistwW, type="mixed")
summary(nymix)

# Comparing models
anova(nymix, nylag)

# Langrage Multiplier (LM) tests
NYlistwW <- nb2listw(NY_nb, style = "W")
res <- lm.LMtests(nylm, listw=NYlistwW, test="all")
summary(res)
