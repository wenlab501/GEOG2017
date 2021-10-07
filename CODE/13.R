##############################################
#
# Spatial Analysis (Geog 5069 (2018); NTU Geography)
# Lecture 4: Spatial Point Clustering-- 
#            Density-based methods
#
# Instructor: Dr. Tzai-Hung Wen 
#
##############################################

#########################################
#
# Detecting Clusters: DBSCAN
#
#########################################

install.packages("dbscan")
library("dbscan")

data <- read.table("Data/tpedata.csv", header=TRUE, sep=",")
Pts0 <- cbind(data[,2], data[,3])

# Using k-nn distance as the eps
kNNdistplot(Pts0, k = 3)
abline(h=1500, col = "red", lty=2)

( res <- dbscan(Pts0, eps = 1500, minPts = 3) )

polymap(Pts_bnd, col="lightgray")
pointmap(Pts0, col = res$cluster +1 , pch = res$cluster +1 , add=T)

hullplot(Pts0, res, asp=1)
