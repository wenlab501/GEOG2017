#######################
#
# Bivariate K function 
#
#######################
install.packages("splancs")
library("splancs")

data1 <- read.table("Data/point1.csv", header=TRUE, sep=",")
data2 <- read.table("Data/point2.csv", header=TRUE, sep=",")
Pts1 <- as.points(data1[,1], data1[,2]) 
Pts2 <- as.points(data2[,1], data2[,2]) 

s <- seq(0, 5000 ,50)
tpe.k12 <-k12hat(Pts1, Pts2, Pts_bnd, s)
plot(s, tpe.k12, type="l",xlab="distance", ylab="K(d)", main="Bivariate K function")
env12<-Kenv.tor(Pts1, Pts2, Pts_bnd, nsim=49, s)
lines(s, env12$upper, col="red", lty=3)
lines(s, env12$lower, col="blue",lty=3)

plot(s, sqrt(tpe.k12/pi)-s, type="l", ylim=c(-4000,4000),xlab="distance", ylab="D(d)", main="D function")
lines(s, sqrt(env12$upper/pi)-s, col="red", lty=3)
lines(s, sqrt(env12$lower/pi)-s, col="blue",lty=3)

