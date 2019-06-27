rm(list=ls()) # clear all variables
par(mfrow=c(1,1)) # display figures on a 3x2 grid. you can adjust this as appropriate later
cat("\014") # clear console window


###################################################################
##################### Problem 1b ###################################
###################################################################
cat("Problem 1b \n")


Trinagular = function(Nsim,a,b,c){
  Result = numeric(Nsim)
  X = runif(Nsim,0,1)
  for (i in 1:Nsim){
    if (X[i] <= ((c-a)/(b-a))){
      Result[i] = a + sqrt((b-a)*(c-a)*X[i])
    }
    else{
      Result[i] = b - sqrt((b-a)*(b-c)*(1-X[i]))
    }
  }
  return(Result)
}

Nsim = 100000
a = 0.7
b = 2
c = 1.5

triangle = Trinagular(Nsim,a,b,c)
par(mfrow=c(1,2))
hist(triangle, prob = TRUE, breaks=80, main="ICDF")



rtriang <- function(Nsim, a, b, c){ 
  if(!(a<=c & c<=b))
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

triangle2 = rtriang(Nsim,a,b,c)
hist(triangle2, prob = TRUE, breaks=80, main="PDF")
par(mfrow=c(1,1))




cat("\n")

###################################################################
##################### Problem 2b ###################################
###################################################################
cat("Problem 2b \n")

library(mvtnorm)

# Specify expectation and covariance matrix
mu <- c(75,46,18)  # Expectation
Nsim <- 100000

#i)
sigma1 <- matrix(c(625,-187.5,0,-187.5,100,0,0,0,25),nrow=3) # Covariance matrix 
sigma1 
Xdata1 <- rmvnorm(Nsim, mean = mu, sigma = sigma1) 
# P(X1>80, X2>50 and X3>20)
P1 = mean(Xdata1[,1]>80 & Xdata1[,2]>50 & Xdata1[,3]>20)

#ii)
sigma2 <- matrix(c(625,187.5,0,187.5,100,0,0,0,25),nrow=3) # Covariance matrix
sigma2  
Xdata2 <- rmvnorm(Nsim, mean = mu, sigma = sigma2) 
# P(X1>80, X2>50 and X3>20)
P2 = mean(Xdata2[,1]>80 & Xdata2[,2]>50 & Xdata2[,3]>20)

#iii)
sigma3 <- matrix(c(625,-187.5,50,-187.5,100,-25,50,-25,25),nrow=3) # Covariance matrix
sigma3  
Xdata3 <- rmvnorm(Nsim, mean = mu, sigma = sigma3) 
# P(X1>80, X2>50 and X3>20)
P3 = mean(Xdata3[,1]>80 & Xdata3[,2]>50 & Xdata3[,3]>20)
rbind(P1,P2,P3)

##############################################################


# Probability of X1+X2+X3>150 for scenarion i
P21 = mean(Xdata1[,1]+Xdata1[,2]+Xdata1[,3]>150)

# Probability of X1+X2+X3>150 for scenarion ii
P22 = mean(Xdata2[,1]+Xdata2[,2]+Xdata2[,3]>150)

# Probability of X1+X2+X3>150 for scenarion iii
P23 = mean(Xdata3[,1]+Xdata3[,2]+Xdata3[,3]>150)

rbind(P21,P22,P23)


cat("The probability that all 3 values(X1,X2,X3) in total will be bigger than 150 is higher than ","\n")
cat("if we take the values seperately. This means that there is a higher chance to get a Top tier ","\n")
cat("if we dont require 1 value to be very high and let the other be very low, but if we concider ","\n")
cat("the sum of the values as a requirement for a Top tier. ","\n")

cat("\n")

###################################################################
##################### Problem 3b ###################################
###################################################################
cat("Problem 3b \n")


# The intensity function 
IntensityFunc <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3) 

# Plot the intensity function
curve(IntensityFunc,0,24)

# Approximate the integral 0 to 24 of IntensityFunc
# First estimate:
x = runif(10000,0,24)
intMC1 = 24*mean(IntensityFunc(x))
intMC1                   

# Estimate of sd
sdg = sd(IntensityFunc(x))

#Required number of simulations
nsim = 1.96^2*24^2*sdg^2/10^2
cat("Required number of simulations: ",nsim,"\n")

# Final estimate
Final = 24*mean(IntensityFunc(runif(ceiling(nsim),0,24)))
cat("Final estimate: ",Final,"\n")


cat("\n")
###################################################################
##################### Problem 3d ###################################
###################################################################
cat("Problem 3d \n")

# The intensity function 
IntensityFunc <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3)

a = 0
b = 24
Nsim = 100000
maxXD = optimize(IntensityFunc, interval = c(a, b), maximum = TRUE) #Maximum values
c = maxXD[1]$maximum # Time of max density 
MaxDen = maxXD[2]$objective #Maximum density

plot(IntensityFunc, a, b)
f = function (x)(2*x)/(b*c) #Function for the first half of the triangle distribution
maxValues = optimize(f, interval = c(a, c), maximum = TRUE)
#plot the curves of the triangle distribution to verufy the correctness
scale = MaxDen/maxValues[2]$objective
curve(scale*((2*x)/(b*c)), col = "red", add = TRUE, xlim=c(a, c))
curve(scale*((2*(b-x))/(b*(b-c))), col = "red", add = TRUE, xlim=c(c,b))

## Importance Sampling implementation

dtriang <- function(x, a, b, c){ #function for triangle density
  if(!(a<=c & c<=b))
    stop("Error, check a, b and c.")
  fx <- numeric(length(x))
  fx[x<c] <- 2*(x[x<c]-a)/((b-a)*(c-a))
  fx[x>=c] <- 2*(b-x[x>=c])/((b-a)*(b-c))
  fx
}

Xtriang <- Trinagular(Nsim, a, b, c) #simulate vector of values from triangle distribution
ISexp <- mean(IntensityFunc(Xtriang)/dtriang(Xtriang, a, b, c))

Nsim <- 200
Result <- numeric(Nsim)
for(i in 1:Nsim){ 
  x <- Trinagular(5000, a=a, b=b, c=c)
  Result[i] <- mean(IntensityFunc(x)/dtriang(x, a, b, c)) # importance
}
# standard deviation with Imortance sampling
sdIS <- sd(Result)
cat("Standard deviation with Imortance sampling:",sdIS,"\n")

#calculating SD with ordinary Monte Carlo integration.
X <- runif(Nsim, a, b)
MCexp <- (b-a)*mean(IntensityFunc(X))
# standard deviation of CMC
sdCMC <- sd(IntensityFunc(X))
cat("Standard deviation with ordinary Monte Carlo integration:",sdCMC,"\n")

# Compare SD of ordinary MC with SD of Importance sampling
Matrix <- matrix(c("type","Ordinary MC","Importance","EXP",MCexp,ISexp,"SD",sdCMC,sdIS),nrow=3,byrow = TRUE)
Matrix


cat("We can see that the SD with Importance Sampling is much lower than with","\n")
cat("Ordinary MC aproach. That means that","\n")
cat("the data wtih IS is closely clustered around the mean,","\n")
cat("which means that the estimation with Is is more precise.","\n")

cat("\n")

###################################################################
##################### Problem 4b ###################################
###################################################################
cat("Problem 4b \n")

# Data from condition 1
ntimesC1=c(12585,726,939,8101,7107,573,21246,306,6360,3684,
           4350,26970,918,18630,4659,300,13464,1494,7743,2070,13008,921)
lambdafunc <- function(dataset){
  length(dataset)/sum(dataset) # 1/mean(dataset)
}

lambdafunc(ntimesC1)


# Bootstrap estimate for standard deviation and bias
B = 20000
lambdaVec = numeric(B)
for(i in 1:B){
  lambdaVec[i] = lambdafunc(sample(ntimesC1, size=length(ntimesC1), replace=TRUE))
}
hist( lambdaVec, prob=TRUE, nclass=sqrt(B)/2, density = 40)  
abline( v=lambdafunc(ntimesC1), col="red", lwd=2 )



# Bias of the estimate
bias = round(mean(lambdaVec) - lambdafunc(ntimesC1), 7)

# Bias adjusted estimate
estimate = round(lambdafunc(ntimesC1), 7)
bAdjEst = estimate - bias 
abline(v=bAdjEst, col="yellow", lwd=2)

# Standard deviation of the estimate
sd = round(sd(lambdaVec), 7)

# Basic
lowerB = round(2*lambdafunc(ntimesC1)-quantile(lambdaVec,0.975), 7)
upperB = round(2*lambdafunc(ntimesC1)-quantile(lambdaVec,0.025), 7)
# Percentile
P1 = round(quantile(lambdaVec,0.05), 7)
P2 = round(quantile(lambdaVec,0.95), 7)
estimator = c("LambdaEst")
rbind(estimator, estimate, bias, bAdjEst, sd, P1, P2)


# Using the boot function
library(boot)
thetaest1func <- function(data,i)   
  lambdafunc(data[i])
boot.obj <- boot(data=ntimesC1,statistic = thetaest1func,R=20000)
boot.ci(boot.obj,type=c("bca"))


cat("\n")


###################################################################
##################### Problem 4d ###################################
###################################################################
cat("Problem 4d \n")

lambdafunc <- function(dataset){
  length(dataset)/sum(dataset) # 1/mean(dataset)
}
# Data from condition 2
ntimesC1=c(12585,726,939,8101,7107,573,21246,306,6360,3684,
           4350,26970,918,18630,4659,300,13464,1494,7743,2070,13008,921)
# Data from condition 2
ntimesC2=c(2220,13329,231,2181,3366,387,565,846,3612,2907,
           1026,1617,5304,2070,144,882,1212,78,1590,270,48,2457)

lambdafunc(ntimesC2)


Len1 = length(ntimesC1)
Len2 = length(ntimesC2)

totalLen = (Len1+Len2)
CombinedData = c(ntimesC1, ntimesC2)
P = 20000

meandifference = mean(ntimesC1) - mean(ntimesC2)  # mean difference original data
meandiffVec = numeric(P)
estdifference = lambdafunc(ntimesC1) - lambdafunc(ntimesC2)  # difference between estimator original data
estdiffVec = numeric(P)

for(i in 1:P){
  X1 = sample(c(CombinedData), size=totalLen, replace=FALSE)  
  X2 = sample(c(CombinedData), size=totalLen, replace=FALSE) 
  meandiffVec[i] = mean(X1[1:Len1]) - mean(X1[(Len1+1):totalLen]) # Compute mean diff for permuted sample
  estdiffVec[i] = lambdafunc(X2[1:Len1]) - lambdafunc(X2[(Len1+1):totalLen]) # Compute estimator diff for permuted sample
}

par(mfrow=c(1,2))
hist( meandiffVec, prob=TRUE, nclass=sqrt(P)/2, density = 40 )  
abline( v=meandifference, col="blue", lwd=2 )
hist( estdiffVec, prob=TRUE, nclass=sqrt(P)/2, density = 40)  
abline( v=estdifference, col="blue", lwd=2 )


Res1 = mean(abs(meandiffVec)>abs(meandifference)) 
Res2 = mean(abs(estdiffVec)>abs(estdifference)) 
type = c("Mean", "Estimator")
probs = round(c(Res1, Res2), 6)

rbind(type, probs)


cat("Conclusion of the test: ","\n")
cat("So having that the p-value is smaller in each test than a=0.05 ","\n")
cat("we can reject H0 and state that mean of X is not equal to mean of Y and est of X ","\n")
cat("is not equal to est of Y. ","\n")
par(mfrow=c(1,1))
###################################################################
##################### Problem 4f ###################################
###################################################################
cat("Problem 4f \n")


# Data from condition 3 
ntimesC3=c(18722,54,141,471,867,552,1110,6066,108,1146,13734,3855,
           6183,54,153,111,327,195,270,888,13563,7035,12562,5391,
           660,474,1275,126,93,252,330,48,9798,132,93,4245,1599,39,1527,60)

lambdafunc <- function(dataset){
  length(dataset)/sum(dataset) 
}

lambdafunc(ntimesC3)

B = 200000
lambda = 0.0003
lambdaestVec = numeric(B)
diffLambda = lambdafunc(ntimesC3) - lambda
for(i in 1:B){
  newExp = rexp(n = length(ntimesC3), rate = lambda)
  lambdaestVec[i] = lambdafunc(newExp) - lambda
}
hist( lambdaestVec, prob=TRUE, nclass=sqrt(B)/2, density = 40)  
abline( v=diffLambda, col="blue", lwd=2 ) 

Pvalue = mean(lambdaestVec>diffLambda) # The p-value for one-sided test obtained by permutation
Pvalue
cat("p-value: ",Pvalue,"\n")

cat("Conclusion of the test: ","\n")
cat("The p-value appears to be much larger than a=0.05 so we can ","\n")
cat("not reject the H0 hypothesis. It also appears that the dataset","\n")
cat("is not large enough. ","\n")


cat("\n")


###################################################################
##################### Problem 5b ###################################
###################################################################
cat("Problem 5b \n")

#Function to calculate the total production G(s) at any time s for any  
# valid value of t0, ts, tp, beta, gamma.
TotalProduction <- function(s, t0, ts, tp, beta, gamma) {
  if (s >= 0 && s <= t0  )
    return(0)
  if (s > t0 && s <= t0 + ts)
    return((beta/(2*ts)) * ((s - t0)^2))
  if (s > t0 + ts && s <= t0 + ts + tp)
    return((beta/2)*ts + beta*(s - (ts + t0)))
  if (s > t0 + ts + tp)
    return((beta/2)*ts + beta*tp + (beta/gamma)*(1 - exp(-gamma*(s-(t0+ts+tp)))))
}
scenario1 = TotalProduction(5, 1, 1.5, 6, 8, 0.15)
scenario2 = TotalProduction(10, 1, 1.5, 6, 8, 0.15)
scenario3 = TotalProduction(15, 1, 1.5, 6, 8, 0.15)
scenario4 = TotalProduction(5, 0.75, 1, 4, 8.5, 0.25)
scenario5 = TotalProduction(10, 0.75, 1, 4, 8.5, 0.25)
scenario6 = TotalProduction(15, 0.75, 1, 4, 8.5, 0.25)

rbind(scenario1,scenario2,scenario3,scenario4,scenario5,scenario6)

cat("Total production up to time s for the given scenarious:","\n")
cat("scenario 1: ",scenario1,"\n")
cat("scenario 2: ",scenario2,"\n")
cat("scenario 3: ",scenario3,"\n")
cat("scenario 4: ",scenario4,"\n")
cat("scenario 5: ",scenario5,"\n")
cat("scenario 6: ",scenario6,"\n")


cat("\n")


###################################################################
##################### Problem 5d ###################################
###################################################################
cat("Problem 5d \n")

Nsim = 3000



simulateTotalProduction <- function(s, Nsim) {
  Vec = numeric(Nsim)
  t0 = Trinagular(Nsim = Nsim, a = 0.85, b = 1.5, c = 1.1)
  ts = Trinagular(Nsim = Nsim, a = 0.7, b = 1.7, c = 1)
  tp = Trinagular(Nsim = Nsim, a = 4, b = 7, c = 5)
  beta = Trinagular(Nsim= Nsim, a = 7.5, b = 8.5, c = 8)
  gamma = Trinagular(Nsim= Nsim, a = 0.15, b = 0.3, c = 0.25)
  for (x in 1:Nsim) {
    Vec[x] = TotalProduction(s, t0[x], ts[x], tp[x], beta[x], gamma[x])
  }
  return(Vec)
}

cat("Distribution of the 5,10,15 years total production,","\n")
cat("with relevant plots and summary statistics:","\n")

s5 = simulateTotalProduction(5,Nsim)
hist(s5, prob=TRUE, nclass=sqrt(Nsim)/2, density=40 )  
lines(density(s5), col='red', lwd=2 )
summary(s5)

s10 = simulateTotalProduction(10,Nsim)
hist(s10, prob=TRUE, nclass=sqrt(Nsim)/2, density=40 )  
lines(density(s10), col='red', lwd=2 )
summary(s10)

s15 = simulateTotalProduction(15,Nsim)
hist(s15, prob=TRUE, nclass=sqrt(Nsim)/2, density=40 )  
lines(density(s15), col='red', lwd=2 )
summary(s15)

cat("\n")



#Minimum required number of simulations
err = 0.02
MinNumOfSim = round(1/(err^2), 0) + 100
cat("Minimum required number of simulations:",MinNumOfSim,"\n")
cat("\n")

# probability that the 15 years total production exceeds 80 with the required precision.
sim15Years = simulateTotalProduction(s = 15, Nsim = MinNumOfSim)
Prob = length(sim15Years[sim15Years > 80])/MinNumOfSim 
cat("Probability that the 15 years total production exceeds 80 with the required precision:",Prob,"\n")
cat("\n")


###################################################################
##################### Problem 5f ###################################
###################################################################
cat("Problem 5f \n")

Treshold <- function(t0, ts, tp, beta, gamma){
  t0 + ts + tp - ((1/gamma)*log(1/beta))
}

ThresholdAndVolume <- function(Nsim=10000) {
  t0 = Trinagular(Nsim = Nsim, a = 0.85, b = 1.5, c = 1.1)
  ts = Trinagular(Nsim = Nsim, a = 0.7, b = 1.7, c = 1)
  tp = Trinagular(Nsim = Nsim, a = 4, b = 7, c = 5)
  beta = Trinagular(Nsim= Nsim, a = 7.5, b = 8.5, c = 8)
  gamma = Trinagular(Nsim= Nsim, a = 0.15, b = 0.3, c = 0.25)
  thresholdsVec = numeric(Nsim)
  volumesVec = numeric(Nsim)
  for (x in 1:Nsim) {
    thresholdsVec[x] <- Treshold(t0[x], ts[x], tp[x], beta[x], gamma[x])
    volumesVec[x] <- TotalProduction(thresholdsVec[x], t0[x], ts[x], tp[x], beta[x], gamma[x])
  }
  ThresholdsAndVolumesComb <- data.frame(thresholdsVec, volumesVec)
  return(ThresholdsAndVolumesComb) 
}

Nsim1 = 10000

thresholdsAndvolumes <- ThresholdAndVolume()

thresholds <- thresholdsAndvolumes[1][,1]
hist( thresholds, prob=TRUE, nclass=sqrt(Nsim1)/2, density=40 )
lines( density( thresholds ), col='red', lwd=2 )

volumes <- thresholdsAndvolumes[2][,1]
hist( volumes, prob=TRUE, nclass=sqrt(Nsim1)/2, density=40 )   
lines( density( volumes ), col='red', lwd=2 )

#Calculating the required number of simulations for treshold
NumSimForThreshold <- round((4*var(thresholds))/((1/12)^2), 0) + 100
NumSimForThreshold
cat("Required number of simulations for Treshold: ",NumSimForThreshold,"\n")
thresholdsAndvolumes2 <- ThresholdAndVolume(NumSimForThreshold)
thresholds2 <- thresholdsAndvolumes2[1][,1]
hist( thresholds2, prob=TRUE, nclass=sqrt(NumSimForThreshold)/2, density=40 )   
lines( density( thresholds2 ), col='red', lwd=2 )
summary(thresholds2)
quantile(thresholds2, 0.1)
quantile(thresholds2, 0.9)

#Calculating the required number of simulations for volumes
NumSimForVolume <- round((4*var(volumes))/((0.25)^2), 0) + 100
NumSimForVolume
cat("Required number of simulations for Volume: ",NumSimForVolume,"\n")
thresholdsAndvolumes3 <- ThresholdAndVolume(NumSimForVolume)
volumes2 <- thresholdsAndvolumes3[2][,1]
hist( volumes2, prob=TRUE, nclass=sqrt(NumSimForVolume)/2, density=40 )   
lines( density( volumes2 ), col='red', lwd=2 )
summary(volumes2)
quantile(volumes2, 0.1)
quantile(volumes2, 0.9)

cat("A brief practical interpretation of what the quantiles tells us: ","\n")
cat("The 10% quantile and the 90% quantile tells us that the values of threshold and volume","\n")
cat("fall mostly in the range (14.9, 18.7) and (69.2, 87.6) respetively.","\n")


cat("\n")

