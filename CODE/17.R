##############################################
#
# Spatial Analysis (Geog 5069 (2018); NTU Geography)
#
# Lecture 11: Spatial Equilibrium Effects
#
# Instructor: Dr. Tzai-Hung Wen 
#
##############################################


rm(list=ls())
setwd("C:/Users/user/Desktop/SA")
library(rgdal)
library (spdep)  

# Load Shapefiles
TWN.TB <- readOGR(dsn = "Data", layer = "Taiwan_TB", encoding="utf-8")

head(TWN.TB@data)
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(TWN.TB, zcol="TBINCI", col.regions=lm.palette(20), main="TB Incidence")

# Neighbors: Construct neighbors list
TWN_nbq<-poly2nb(TWN.TB) #QUEEN = TRUE
summary(TWN_nbq)

# Neighborhood Matrix
TWN_nbq_w.mat <-nb2mat(TWN_nbq, style="W", zero.policy=T) # row-standardized
TWN_nbq_w2.mat <-nb2mat(TWN_nbq, style="B", zero.policy=T) # binary

# Row-standardized weights matrix (list)
TWN_nbq_w<- nb2listw(TWN_nbq, zero.policy=T)
# Binary matrix (list)
TWN_nbq_wb2<-nb2listw(TWN_nbq, style="B", zero.policy=T)

# OLS Regression
TBData<-TWN.TB@data
TBINCI<-TWN.TB@data$TBINCI # TB發生率
ABOR_P<-TWN.TB@data$ABOR_P # 原住民比例
INCOME<-TWN.TB@data$INCOME # 平均家戶收入  

TB.lm<- lm(TBINCI ~ ABOR_P + INCOME, data=TBData); summary(TB.lm)

# Global Moran's I for LM regression residuals
TB.moran0 <-  lm.morantest(TB.lm, TWN_nbq_w, zero.policy=T); TB.moran0 

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation
TB.lagrange <- lm.LMtests(TB.lm,TWN_nbq_w,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"), zero.policy=T)
summary(TB.lagrange)

# MLE of the Spatial Lag Model
TB.lag <- lagsarlm(TBINCI ~ ABOR_P + INCOME, data=TBData, TWN_nbq_w, zero.policy=T); summary(TB.lag)

# Modeling Spatial Equilibrium Effect
TB.lag2 <- lagsarlm(TBINCI ~ ABOR_P, data=TBData, TWN_nbq_w, zero.policy=T); summary(TB.lag2)
rho<-coef(TB.lag2)[1]  
beta<-coef(TB.lag2)[3]

# example of impact on other townships (observation No.301)
cvec <- rep(0,dim(TBData)[1])
cvec[301] <- 0.1 # 花蓮縣秀林鄉

# Store estimates for impact of change in one town in rus.est
eye <- matrix(0,nrow=dim(TBData)[1],ncol=dim(TBData)[1])
diag(eye) <- 1
rus.est <- solve(eye - rho * TWN_nbq_w.mat) %*% cvec * beta


# Find ten highest values of rus.est vector
rus.est <- round(rus.est,6)
rus.est <- data.frame(TBData$FULLNAME,rus.est)
rus.est[rev(order(rus.est$rus.est)),][1:10,]

TWN.TB$rus.est <-rus.est[,2]
spplot(TWN.TB, zcol="rus.est", col.regions=lm.palette(20), main="TB Spillover Effects")

