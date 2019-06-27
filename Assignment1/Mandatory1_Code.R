rm(list=ls())
##############################################################################################
###Problem1

1 - ppois(3, lambda=3)  # probability that at least four visitors arrive during one minute
pgamma(5, shape=2, scale=3, lower.tail = FALSE) # probability that a visitor spends more than 5 minutes on the website

#Expected total visitor time for visitors arriving during one minute
N <- rpois(1, lambda=3)
x <- rgamma(N, shape=2, scale=3)
total = sum(x)

#Expected total visitor time for visitors arriving during one hour
N2 = rpois(60, lambda=3) 
x2 <- rgamma(sum(N2), shape=2, scale=3)
total2 = sum(x2)

#Estimation of the probability that the total visitor time exceeds 1000 minutes
Nrep = 10000
count = 0
for (i in 1:Nrep) {
  N3 = rpois(60, lambda=3)
  x3 <- rgamma(sum(N3), shape=2, scale=3)
  total3 = sum(x3)
  if (total3>1000){
    count = count + 1
  }
}
count/Nrep  #Estimated probability


#############################################################################################
###Problem 2
#b)
genweibulldistr <- function(Nsim,alpha,beta){
  U <- runif(Nsim)
  X <- (-log(1-U)/alpha)^(1/beta)  
  return(X)
}

Nsim <- 10000
alpha <- 1.81
beta = 2
x <- genweibulldistr(Nsim = Nsim,alpha = alpha, beta = beta)

#histogram of the data
hist(x, prob = TRUE,breaks=seq(0,ceiling(max(x)*1.1),length.out=max(10,sqrt(Nsim))),
     main="Histogram of the simulated data")

# Expectation
alpha^(-1/beta)*gamma(1+1/2)
# Mean of the data
mean(x)
# we can notice that the average of the data does not differ mutch from the
# expectation calculated from the formula of the expecttion


# beta = 0.5 
Nsim <- 10000
beta2 = 0.5
x2 <- genweibulldistr(Nsim = Nsim,alpha = alpha, beta = beta2)
hist(x2, prob = TRUE,breaks=seq(0,ceiling(max(x2)),length.out=max(100)), main="Histogram of the simulated data")
mean(x2)

# beta = 1
Nsim <- 10000
beta3 = 1
x3 <- genweibulldistr(Nsim = Nsim,alpha = alpha, beta = beta3)
hist(x3, prob = TRUE, breaks=seq(0,ceiling(max(x3)),length.out=max(50)), main="Histogram of the simulated data")
mean(x3)

#####################################################################################
##d)

#distribution of the time until failure in months for certain pumps

alpha2 = 0.08
beta4 = 1
x4 <- genweibulldistr(Nsim = Nsim, alpha = alpha2, beta = beta4)

monthsCount = 0
for (i in x4){
  if (i<12){
    monthsCount = monthsCount + 1
  }
}
pumpFailProb = monthsCount/Nsim # prob that the pump will fail before 1 year
pumpWorkProb = 1 - pumpFailProb # prob that the pump will work after 1 years


p = c(rep(pumpFailProb,3))

#The system fails when all pumps have failed
R1 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[1],p[1])) # Creating a sample of values 1 and 0
R2 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[2],p[2])) # where 1 mens the failure of a pump before
R3 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[3],p[3])) # 1 year. 
OldPumps = sum(pmin(R1,R2,R3))/Nsim 

#The system fails when at least 2 of the pumps have failed
Rmatrix <- matrix(nrow=Nsim,ncol=3)
for(i in 1:3){
  Rmatrix[,i] <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[i],p[i]))
}
Nfailing <- rowSums(Rmatrix) # Calculate number of failings for each row
OldPumps1 = sum(Nfailing>=2)/Nsim

#The system failes when the first pump fail
OldPumps2 = sum(pmax(R1,R2,R3))/Nsim  



#Calculating Expected time until failure
#Generation the failure times for every pump
pump1 <- genweibulldistr(Nsim = Nsim, alpha = alpha2, beta = beta4)
pump2 <- genweibulldistr(Nsim = Nsim, alpha = alpha2, beta = beta4)
pump3 <- genweibulldistr(Nsim = Nsim, alpha = alpha2, beta = beta4)


#Expected time in model where the system fails when all pumps have failed
total = c()
for (i in 1:Nsim){
  theFirstTobreak = pmin(pump1[i], pump2[i], pump3[i])
  total[i] = theFirstTobreak
}
mean(total)

## Expected time in model where the system failes when the first pump fail
total1 = c()
for (i in 1:Nsim){
  theFirstTobreak1 = pmax(pump1[i], pump2[i], pump3[i])
  total1[i] = theFirstTobreak1
}
mean(total1)

##Expected time utill failure in model if at least two pumps fail then the system fails
total2 = c()
alltimes = c()
for (i in 1:Nsim){
  alltimes = c(pump1[i], pump2[i], pump3[i])
  alltimes2 = sort(alltimes, decreasing = FALSE)
  theFirstTobreak2 = alltimes2[2]
  total2[i] = theFirstTobreak2
}
mean(total2)


#################################################################################
####f)
  
rtriang <- function(Nsim, a, b, c){ 
 if(!(a<c & c<b))
    stop("Error, check a, b and c.") 
  k <- 0 # counter for accepted
  x <- numeric(Nsim) # vector for accepted
  while(k<Nsim){
    u <- runif(1)
    y <- runif(1,min=a,max=b) # proposal distribution
    if(y<c){  #different pdf before and after c
      if(u<((y-a)/(c-a))){ # Then we accept
        k <- k+1
        x[k] <- y
      }
    }
    else{ 
      if(u<((b-y)/(b-c))){ # Then we accept
        k <- k+1
        x[k] <- y
      }
    }
  }
  return(x)
}

genweibulldistr <- function(Nsim,alpha,beta){
  U <- runif(Nsim)
  X <- (-log(1-U)/alpha)^(1/beta)
  return(X)
}

Nsim <- 10000
x.tri <- rtriang(Nsim=Nsim,a=10,b=50,c=20)

a=10
b=50
beta = 1

hist(x.tri, prob = TRUE,breaks=seq(a,b,length.out=max(10,sqrt(Nsim))),
     main="Histogram of data and true density")

#Expectation
#expectation = alpha^(-1/beta)*gamma(1+1/beta)
#expectation = 1/alpha
#alpha = 1/expectation


#calculating values of alphas
alphasVec = c()
for (i in 1:Nsim){
  alphasVec[i] = 1/x.tri[i]
}

# This function calculates the probability for a pump to fail within 1 year for a given alpha
ProbSimulation = function(alpha,Nsim){
  for (i in 1:Nsim){
    times <- genweibulldistr(Nsim = Nsim, alpha = alpha, beta = 1)
    yearCount = 0
    for (i in times){
      if (i<12){
        yearCount = yearCount + 1
      }
    }
    return(yearCount/Nsim)
  }
}

# model where the system fails when all pumps have failed 
probToFailInOneY = c()
FinalProb = c()
for (j in 1:Nsim){
  probToFailInOneY[j] = ProbSimulation(alphasVec[j], 10000)
  prbV = c(rep(probToFailInOneY[j],3))
  
  Set1 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV[1],prbV[1]))  
  Set2 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV[2],prbV[2]))  
  Set3 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV[3],prbV[3]))
  FinalProb[j] = sum(pmin(Set1,Set2,Set3))/Nsim 
}
hist(FinalProb, prob = TRUE,breaks=seq(0,1,length.out=max(200)),
     main="Histogram of data and true density")
Qdata <- quantile(FinalProb,probs = seq(0,1,by=0.1))


# model where if all pump fail then the system fails 
probToFailInOneY1 = c()
FinalProb1 = c()
for (k in 1:Nsim){
  probToFailInOneY1[k] = ProbSimulation(alphasVec[k], 10000)
  prbV1 = c(rep(probToFailInOneY1[k],3))
  
  Set4 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV1[1],prbV1[1]))  
  Set5 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV1[2],prbV1[2]))  
  Set6 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV1[3],prbV1[3]))
  FinalProb1[k] = sum(pmax(Set4,Set5,Set6))/Nsim 
}
hist(FinalProb1, prob = TRUE,breaks=seq(0,1,length.out=max(350)),
     main="Histogram of data and true density")
Qdata1 <- quantile(FinalProb1,probs = seq(0,1,by=0.1))


# model where at least 2 pumps have to fail for the system to fail
probToFailInOneY2 = c()
FinalProb2 = c()
for (z in 1:Nsim){
  probToFailInOneY2[z] = ProbSimulation(alphasVec[z], 10000)
  prbV2 = c(rep(probToFailInOneY2[z],3))
  
  Zmatrix <- matrix(nrow=Nsim,ncol=3)
  for(i in 1:3){
    Zmatrix[,i] <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-prbV2[i],prbV2[i]))
  }
  Nfailing2 <- rowSums(Zmatrix) # Calculate number of failings for each row
  FinalProb2[z] = sum(Nfailing2>=2)/Nsim
}
hist(FinalProb2, prob = TRUE,breaks=seq(0,1,length.out=max(350)),
     main="Histogram of data and true density")
Qdata2 <- quantile(FinalProb2,probs = seq(0,1,by=0.1))
rbind(Qdata,Qdata1,Qdata2)

#Comparing the new pumps to the old
#old pumps
OldPumps   # all pumps to fail for the system to fail
OldPumps1  # one pump to fail for the system to fail
OldPumps2  # at last two pump to fail for the system to fail


#new pumps
par(mfrow=c(2,2))
hist(FinalProb, prob = TRUE,breaks=seq(0,1,length.out=max(100)),
     main="Histogram of data and true density")
hist(FinalProb1, prob = TRUE,breaks=seq(0,1,length.out=max(100)),
     main="Histogram of data and true density")
hist(FinalProb2, prob = TRUE,breaks=seq(0,1,length.out=max(100)),
     main="Histogram of data and true density")

# Comparing the probabilitys of the failure for the new pumps to the old pumps we can 
# state that the new pumps have a lower probability of failure for each of the three scenarios  


############################################################################################
#problem 3

Yatzy <- function(){
  upperSecScore = 0
  for (i in 1:6){
    dices = 5
    for(j in 1:3){
      singleThrow = sample(1:6, size=dices, replace=T)
      for (k in singleThrow){
        if (k==i){
          upperSecScore = upperSecScore + k
          dices = dices - 1
        }
      }
    }
  }
  return(upperSecScore)
}


#plot of the probability distribution
observations = c()
Nsim = 10000
for (i in 1:Nsim){
  observations[i] = Yatzy()
}
hist(observations, prob = TRUE,breaks=seq(0,max(observations)*1.2),
     main="Histogram of data")
plot(observations)



#probablility of an upper section score of at least 42 points based on Nsim
Nsim = 10000
totalCount = 0
for (i in 1:Nsim){
  x = Yatzy()
  if (x>=42){
    totalCount = totalCount + 1 #Count the amount of outcomes that areat least 42
  }
}
totalCount/10000 #Estimated probability










  
  
  
  
  




