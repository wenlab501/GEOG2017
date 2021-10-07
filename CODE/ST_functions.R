#########################################################################
#
# Source: Tango T. Statistical Methods for Disease Clustering, Springer, 2010.
#
# KnoxA.test: Knox・s test for space-time interaction with approximated p-values
# KnoxM.test: Knox・s test for space-time interaction with Monte Carlo simulated p-values
# Mantel.test: Mantel・s test for space-time interaction
# Jacquez.test: Jacquez・s k nearest neighbors test for space-time interaction
# DiggleETAL.test: Diggle et al.・s test for space-time interaction
#
#########################################################################


KnoxM.test<-function(x,y,time,del1,del2,Nrep){
  # -----------------------------------------------------------------
  #  An example of execution
  #
  #     dat<-scan("d:/demo/McHardy.dat",list(x=0,y=0,time=0))
  #     del1<-2; del2<-0; Nrep<-999
  #     out<-KnoxM.test(dat$x,dat$y,dat$time,del1,del2,Nrep)
  #
  # ---------------------------------------------------------------------
  #  ARGUEMENTS
  #     x    : a vector of x-coordinates of case
  #     y    : a vector of y-coordinates of case
  #     time : a vector of observed times
  #     del1 : a measure of closeness in space
  #     del2 : a measure of closeness in time
  #     Nrep : The number of Monte Carlo replications, e.g., 999, 9999
  #
  #  VALUES
  #     Knox.T            : test statistic
  #     Freq              : a vector of simulated test statistics under the null
  #     Simulated.p.value : simulated p-value
  #
  #-----------------------------------------------------------------------
  n<-length(x)
  ktmon<-1:(Nrep+1)
  sdis<-matrix(0,n,n)
  tdis<-matrix(0,n,n)
  as<-sdis
  at<-tdis
  for (i in 1:n){
    for (j in 1:n){
      sdis[i,j]<-sqrt( (x[i]-x[j])^2 + (y[i]-y[j])^2 )
      tdis[i,j]<- abs( time[i] - time[j])
    }
  }
  for(i in 1:n){
    for (j in 1:n){
      as[i,j] <- ifelse(sdis[i,j]<= del1, 1, 0)
      at[i,j] <- ifelse(tdis[i,j]<= del2, 1, 0)
    }
    as[i,i]<-0
    at[i,i]<-0
  }
  s1<-0
  for(i in 1:n){
    for (j in 1:n){
      s1<-s1+as[i,j] * at[i,j]
    }
  }
  obst <- s1/2
  #
  # Monte Carlo Hypothesis testing 
  #
  for(k in 1:Nrep){
    timeR<-sample(time)
    
    for (i in 1:n){
      for (j in 1:n){
        tdis[i,j]<- abs( timeR[i] - timeR[j])
      }
    }
    for(i in 1:n){
      for (j in 1:n){
        at[i,j] <- ifelse(tdis[i,j]<= del2, 1, 0)
      }
      at[i,i]<-0
    }
    s1<-0
    for(i in 1:n){
      for (j in 1:n){
        s1<-s1+as[i,j] * at[i,j]
      }
    }
    ktmon[k] <- s1/2
  }
  ktmon[Nrep+1]<-obst
  r<-length(ktmon[ktmon>=obst])
  p<-r/(Nrep+1)
  list(Knox.T=obst , Freq=ktmon, Simulated.p.value=p)
}

#########################################################################
#########################################################################


KnoxA.test<-function(x,y,time,del1,del2){
  # -----------------------------------------------------------------
  #  An example of execution
  #
  #     dat<-scan("d:/demo/McHardy.dat",list(x=0,y=0,time=0))
  #     del1<-2; del2<-0
  #     out<-KnoxA.test(dat$x,dat$y,dat$time,del1,del2)
  #
  # ---------------------------------------------------------------------
  #  ARGUEMENTS
  #     x    : a vector of x-coordinates of case
  #     y    : a vector of y-coordinates of case
  #     time : a vector of observed times
  #     del1 : a measure of closeness in space
  #     del2 : a measure of closeness in time
  #
  #  VALUES
  #     Knox.T              : test statistic
  #     Expected            : expected number of cases
  #     Var                 : variance
  #     Standardized        : standardized test statistic
  #     Poisson.p.value     : p-value by Poisson approximation
  #     Poisson.Mid.p.value : mid-p-value by Poisson approximation 
  #     Normal.p.value      : p-value by Normal approximation
  #
  #-----------------------------------------------------------------------
  
  n<-length(x)
  #
  sdis<-matrix(0,n,n)
  tdis<-matrix(0,n,n)
  as<-sdis
  at<-tdis
  for (i in 1:n){
    for (j in 1:n){
      sdis[i,j]<-sqrt( (x[i]-x[j])^2 + (y[i]-y[j])^2 )
      tdis[i,j]<- abs( time[i] - time[j])
    }
  }
  #
  for(i in 1:n){
    for (j in 1:n){
      as[i,j] <- ifelse(sdis[i,j]<= del1, 1, 0)
      at[i,j] <- ifelse(tdis[i,j]<= del2, 1, 0)
    }
    as[i,i]<-0
    at[i,i]<-0
  }
  #
  s1<-0; s2<-0; s3<-0
  for(i in 1:n){
    for (j in 1:n){
      s1<-s1+as[i,j] * at[i,j]
      s2<-s2+as[i,j]
      s3<-s3+at[i,j]  
    }
  }
  obsT <- s1/2
  Ms <- s2/2
  Mt <- s3/2
  #
  s1<-0; s2<-0
  for(i in 1:n){
    for (j in 1:n){
      for (k in 1:n){
        if(k!=j){ 
          s1<-s1+as[i,j] * as[i,k]
          s2<-s2+at[i,j] * at[i,k]
        }
      }
    }
  }
  #
  Ms2 <- s1/2
  Mt2 <- s2/2
  ee  <- Ms*Mt/(n*(n-1)/2)
  vv  <- ee - ee*ee + 4*Ms*Mt/n/(n-1)/(n-2)
  vv  <- vv + 4*(Ms*(Ms-1)-Ms2) * (Mt*(Mt-1)-Mt2)/n/(n-1)/(n-2)/(n-3)
  sdk <- sqrt(vv)
  zz  <-(obsT - ee)/sdk
  pk  <- 1-ppois(obsT-1,ee)
  mid <- 1-ppois(obsT,ee)+dpois(obsT,ee)/2
  pkn <- 1-pnorm(zz)
  list(Knox.T=obsT, Expected=ee, Var=vv, Standardized=zz, Poisson.p.value=pk, 
       Poisson.Mid.p.value=mid, Normal.p.value=pkn)
}


#########################################################################
#########################################################################


Mantel.test<-function(x,y,time,c1,c2,Nrep){
  # -----------------------------------------------------------------
  #  An example of execution
  #
  #     dat<-scan("d:/demo/McHardy.dat",list(x=0,y=0,time=0))
  #     c1<-2; c2<-0; Nrep<-999
  #     out<-KnoxM.test(dat$x,dat$y,dat$time,c1,c2,Nrep)
  #
  # ---------------------------------------------------------------------
  #  ARGUEMENTS
  #     x    : a vector of x-coordinates of case
  #     y    : a vector of y-coordinates of case
  #     time : a vector of observed times
  #     c1   : a constant for Mantel's measure of closeness in space
  #     c2   : a constant for Mantel's measure of closeness in time
  #     Nrep : The number of Monte Carlo replications, e.g., 999, 9999
  #
  #  VALUES
  #     Mantel.T          : test statistic
  #     Expected          : expected value of test statistic
  #     Simulated.p.value : simulated p-value
  #     Freq              : a vector of simulated test statistics under the null
  #
  #-----------------------------------------------------------------------
  
  n<-length(x)
  ktmon<-1:(Nrep+1)
  sdis<-matrix(0,n,n)
  tdis<-matrix(0,n,n)
  as<-sdis
  at<-tdis
  for (i in 1:n){
    for (j in 1:n){
      sdis[i,j]<-sqrt( (x[i]-x[j])^2 + (y[i]-y[j])^2 )
      tdis[i,j]<- abs( time[i] - time[j])
    }
  }
  for(i in 1:n){
    for (j in 1:n){
      as[i,j] <- 1/(sdis[i,j]+c1)
      at[i,j] <- 1/(tdis[i,j]+c2)
    }
    as[i,i]<-0
    at[i,i]<-0
  }
  s1<-0; s2<-0; s3<-0
  for(i in 1:n){
    for (j in 1:n){
      s1<-s1+as[i,j] * at[i,j]
      s2<-s2+as[i,j]
      s3<-s3+at[i,j]  
    }
  }
  obst <- s1/2
  Ms <- s2/2
  Mt <- s3/2
  ee  <- Ms*Mt/(n*(n-1)/2)
  #
  # Monte Carlo Hypothesis testing 
  #
  for(k in 1:Nrep){
    timeR<-sample(time)
    
    for (i in 1:n){
      for (j in 1:n){
        tdis[i,j]<- abs( timeR[i] - timeR[j])
      }
    }
    for(i in 1:n){
      for (j in 1:n){
        at[i,j] <- 1/(tdis[i,j]+c2)
      }
      at[i,i]<-0
    }
    s1<-0
    for(i in 1:n){
      for (j in 1:n){
        s1<-s1+as[i,j] * at[i,j]
      }
    }
    ktmon[k] <- s1/2
  }
  ktmon[Nrep+1]<-obst
  r<-length(ktmon[ktmon>=obst])
  p<-r/(Nrep+1)
  list(Mantel.T=obst, Expected=ee, Simulated.p.value=p,Freq=ktmon)
}

#########################################################################
#########################################################################



Jacquez.test<-function(x,y,time,k,Nrep){
  # -----------------------------------------------------------------
  #  An example of execution
  #
  #     dat<-scan("d:/demo/McHardy.dat",list(x=0,y=0,time=0))
  #     k<-1; Nrep<-999
  #     out<-Jacquez.test(dat$x,dat$y,dat$t,k,Nrep)
  #
  # ---------------------------------------------------------------------
  #  ARGUEMENTS
  #     x    : a vector of x-coordinates of case
  #     y    : a vector of y-coordinates of case
  #     time : a vector of observed times
  #     k    : k of k nearest neighbors
  #     Nrep : The number of Monte Carlo replications, e.g., 999, 9999
  #
  #  VALUES
  #     Jacquez.T         : test statistic
  #     Expected          : expected value of test statistic
  #     Simulated.p.value : simulated p-value
  #     Freq              : a vector of simulated test statistics under the null
  #
  #-----------------------------------------------------------------------
  n<-length(x)
  time<-time+runif(n)/100
  ktmon<-1:(Nrep+1)
  sdis<-matrix(0,n,n)
  tdis<-matrix(0,n,n)
  as<-sdis
  at<-tdis
  for (i in 1:n){
    for (j in 1:n){
      sdis[i,j]<-sqrt( (x[i]-x[j])^2 + (y[i]-y[j])^2 )
      tdis[i,j]<- abs( time[i] - time[j])
    }
  }
  for(i in 1:n){
    ff<-order(sdis[i,])
    gg<-order(tdis[i,])
    for(v in 1:k){
      m1<-ff[v+1]
      m2<-gg[v+1]
      as[i,m1]<-1 
      at[i,m2]<-1 
    }
  }
  s1<-0; s2<-0; s3<-0
  for(i in 1:n){
    for (j in 1:n){
      s1<-s1+as[i,j] * at[i,j]
      s2<-s2+as[i,j]
      s3<-s3+at[i,j]  
    }
  }
  obst <- s1/2
  Ms <- s2/2
  Mt <- s3/2
  ee  <- Ms*Mt/(n*(n-1)/2)
  #
  # Monte Carlo Hypothesis testing 
  #
  for(ij in 1:Nrep){
    timeR<-sample(time)
    
    for (i in 1:n){
      for (j in 1:n){
        tdis[i,j]<- abs( timeR[i] - timeR[j])
      }
    }
    at[,]<-0
    for(i in 1:n){
      gg<-order(tdis[i,])
      for(v in 1:k){
        m2<-gg[v+1]
        at[i,m2]<-1 
      }
    }
    s1<-0
    for(i in 1:n){
      for (j in 1:n){
        s1<-s1+as[i,j] * at[i,j]
      }
    }
    ktmon[ij] <- s1/2
  }
  ktmon[Nrep+1]<-obst
  r<-length(ktmon[ktmon>=obst])
  p<-r/(Nrep+1)
  list(Jacquez.T=obst, Expected=ee, Simulated.p.value=p, Freq=ktmon)
}

#########################################################################
#########################################################################


DiggleETAL.test<-function(pts,time,polygon,range,s,t,Nrep){
  # -----------------------------------------------------------------
  #  An example of execution
  #
  #     dat<-scan("d:/demo/McHardy.dat",list(x=0,y=0,time=0))
  #     pts<- as.points(dat$x, dat$y)
  #     kap.poly<-spoints(scan("d:/book/DiseaseCluster/kappoly.txt"))
  #      (The file "kappoly.txt" contains a set of points for a polygon)
  #     range<-c(1955, 1980); s<- seq(1,5); t<- seq(0, 4))
  #     Nrep<-999
  #     out<-DiggleETAL.test(pts,dat$t,polygon,range,s,t,Nrep)
  #
  # ---------------------------------------------------------------------
  #  ARGUEMENTS
  #     pts     : a sets of points (x, y) as defined in Splancs
  #     time    : a vector of times, the same length of as the number of points in pts
  #     polygon : a polygon enclosing the points
  #     range   : a vector of length 2 specifying the lower and 
  #                 upper temporal domain
  #     s       : a vector of equally spaced spatial distances for the analysis
  #     t       : a vector of equally spaced temporal distances for the analysis
  #     Nrep    : The number of Monte Carlo replications, e.g., 999, 9999
  #
  #  VALUES
  #     Sum.p.value      : simulated p-value for the test statistic Usum
  #     Max.p.value      : simulated p-value for the test statistic Umax
  #     Freq.Usum        : a vector of simulated test statistics Usum under the null
  #     Freq.Umax        : a vector of simulated test statistics Umax under the null
  #
  #-----------------------------------------------------------------------
  kap1 <- stkhat(pts, time, polygon, range, s, t)
  se   <- stsecal(pts, time, polygon, range, s, t)
  Res <- turD/se 
  D1<- sum( Res )
  D2<- max( Res )
  
  for (i in 2:Nrep+1){
    Stime <-sample(time)
    Skap1 <- stkhat(pts, Stime, polygon, range,s,t)
    f1<-matrix(Skap1$ks)
    f2<-matrix(Skap1$kt)
    f1f2 <- f1 %*% t(f2)
    SturD<-Skap1$kst-f1f2
    Sse <- stsecal(pts, Stime, polygon, range, s, t)
    SRes <- SturD/Sse 
    sim1<- sum( SRes )
    sim2<- max( SRes )
    D1<-c(D1,sim1)
    D2<-c(D2,sim2)
  }
  
  p1<-rank(-D1)[1]/(Nrep+1)
  p2<-rank(-D2)[1]/(Nrep+1)
  
  list(Sum.p.value=p1, Max.p.value=p2, Freq.Usum=D1, Freq.Umax=D2)
}












