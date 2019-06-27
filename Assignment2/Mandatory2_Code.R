rm(list=ls()) # clear all variables
graphics.off() # clear all figures
cat("\014") # clear console window


###################################################################
##################### Problem 1c ###################################
###################################################################
cat("Problem 1c \n")

#Odds for different outcomes
NorKyp = c(1.3, 4.1, 8.1)
TysFra = c(2.35, 3.0, 2.65)
PorKro = c(2.2, 2.9, 2.85)
NedPer = c(1.6, 3.4, 4.45)

#Real probabilities for diferrent outcomes
NorKyp2 = c(0.67, 0.22, 0.11)
TysFra2 = c(0.38, 0.29, 0.33)
PorKro2 = c(0.40, 0.29, 0.31)
NedPer2 = c(0.54, 0.26, 0.20)



ProfitCalculation = function(Game,odd){
  Nsim = 10000
  MoneySpent = 1000000  
  MoneyWon = 0
  GameOutcomes <- sample(1:3,size=Nsim,replace=TRUE,prob=c(Game[1],Game[2],Game[3]))
  for (i in GameOutcomes){
    if (i == 1){
      winnings = 100 * odd
      MoneyWon = MoneyWon + winnings
    }
  }
  profit = MoneyWon - MoneySpent
  return(profit)
}

ProfitRecords = c()
for (j in 1:100){
  RealProfit1 = ProfitCalculation(NorKyp2, NorKyp[1])
  RealProfit2 = ProfitCalculation(TysFra2, TysFra[1])
  RealProfit3 = ProfitCalculation(PorKro2, PorKro[1])
  RealProfit4 = ProfitCalculation(NedPer2, NedPer[1])
  ProfitRecords[j] = RealProfit1 + RealProfit2 + RealProfit3 + RealProfit4
}

cat("The expected profit:",mean(ProfitRecords),"\n")


cat("\n")

###################################################################
##################### Problem 1e ###################################
###################################################################
cat("Problem 1e \n")

#All game outcomes probabilities combined in one vector
CombinedProb = c(NorKyp2, TysFra2, PorKro2, NedPer2) 

ProfitCalculation2 = function(GameProb){
  Nsim = 10000
  Stake = 400
  MoneySpent1 = Stake*Nsim 
  MoneyWon1 = 0
  CombinedOdd = 10.75
  
  GameResults1 <- sample(1:3,size=Nsim,replace=TRUE,prob=c(GameProb[1],GameProb[2],GameProb[3]))
  GameResults2 <- sample(1:3,size=Nsim,replace=TRUE,prob=c(GameProb[4],GameProb[5],GameProb[6]))
  GameResults3 <- sample(1:3,size=Nsim,replace=TRUE,prob=c(GameProb[7],GameProb[8],GameProb[9]))
  GameResults4 <- sample(1:3,size=Nsim,replace=TRUE,prob=c(GameProb[10],GameProb[11],GameProb[12]))
  
  
  for (k in 1:Nsim){
    if (sum(GameResults1[k],GameResults2[k],GameResults3[k],GameResults4[k]) == 4){
      MoneyWon1 = MoneyWon1 + (Stake*CombinedOdd)
    }
  }
  Profit1 = MoneyWon1 - MoneySpent1 
  
}

ProfitRecords1 = c()
for (z in 1:100){
  ProfitRecords1[z] = ProfitCalculation2(CombinedProb)
}

cat("The expected profit:",mean(ProfitRecords1),"\n")


cat("\n")

###################################################################
##################### Problem 2b ###################################
###################################################################
cat("Problem 2b \n")


# Intensity function for the arrival of visitors to the website
IntensityFunc <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3)     

curve(IntensityFunc,0,24) # Expected number of guest arriving at every hour in time 0:24

max(IntensityFunc(seq(0,24,length.out = 100))) # max value in the plot
nsim <- (1.96^2*208^2*24^2*0.25)/10^2

# Hit or miss method
nsim <- 243957
c <- 208
y <- runif(nsim,0,c)
x <- runif(nsim,0,24)
z = y <= IntensityFunc(x) # hit or miss
intHM <- c*24*mean(z) # hit or miss estimate (mean(x) is probability of True in x)
intHM

# More precise calculation of required number of simulations:
phat <- mean(z) # estimated proportion of hit
nsim2 <- 1.96^2*208^2*24^2*phat*(1-phat)/(10^2)
y2 <- runif(nsim2,0,c)
x2 <- runif(nsim2,0,24)
z2 = y2 <= IntensityFunc(x2) # hit or miss
intHM2 <- c*24*mean(z2)


cat("Probability of having more than 1250 visitors during a day:",ppois(intHM, 1250),"\n")






cat("\n")
###################################################################
##################### Problem 2d ###################################
###################################################################
cat("Problem 2d \n")


# Function for simulating arrival times for a NHPP between a and b using thinning
simtNHPP <- function(a,b,lambdamax,lambdafunc){
  
  # Simple check that a not too small lambdamax is set
  if(max(lambdafunc(seq(a,b,length.out = 100)))>lambdamax)
    stop("lambdamax is smaller than max of the lambdafunction")
  
  # First simulate HPP with intensity lambdamax on a to b
  expectednumber <- (b-a)*lambdamax  
  Nsim <- 3*expectednumber  # Simulate more than the expected number to be certain to exceed stoptime
  timesbetween <- rexp(Nsim,lambdamax) # Simulate interarrival times
  timesto <- a+cumsum(timesbetween)   # Calculate arrival times starting at a
  timesto <- timesto[timesto<b] # Dischard the times larger than b
  Nevents <- length(timesto) # Count the number of events
  
  # Next do the thinning. Only keep the times where u<lambda(s)/lambdamax
  U <- runif(Nevents)
  timesto <- timesto[U<lambdafunc(timesto)/lambdamax] 
  return(timesto)  # Return the remaining times
}


# The intensity function 
lambdafunction <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3)
# Plot the intensity function
tvec <- seq(0,24,by=0.01)
plot(tvec,lambdafunction(tvec),type="l",ylim=c(0,220))

# Generate data with the traffic intensity and plot them
NHPPtimesV1 <- simtNHPP(a=0,b=24,lambdamax=210,lambdafunc=lambdafunction)
plot(NHPPtimesV1,1:length(NHPPtimesV1),type="s",xlab = "time", 
     ylab = "Event number",lwd=1.5)
points(NHPPtimesV1,rep(0,length(NHPPtimesV1)),pch=21,bg="red")

########################################################################################
#Verify the proportion of arrival times being deleted in the thinning step

# Function for simulating arrival times for a NHPP between a and b using thinning
Proportion <- function(a,b,lambdamax,lambdafunc){
  # First simulate HPP with intensity lambdamax on a to b
  expectednumber <- (b-a)*lambdamax  
  Nsim <- 3*expectednumber  # Simulate more than the expected number to be certain to exceed stoptime
  timesbetween <- rexp(Nsim,lambdamax) # Simulate interarrival times
  timesto <- a+cumsum(timesbetween)   # Calculate arrival times starting at a
  timesto <- timesto[timesto<b] # Dischard the times larger than b
  Nevents <- length(timesto) # Count the number of events
  
  # Next do the thinning. Only keep the times where u<lambda(s)/lambdamax
  U <- runif(Nevents)
  timesto <- timesto[U<lambdafunc(timesto)/lambdamax] 
  Nevents2 <- length(timesto)
  ProportionOfTimesDel = 1 - Nevents2/Nevents
  return(ProportionOfTimesDel)  
}

Records = numeric(100)
for (z in 1:100){
  Records[z] <- Proportion(a=0,b=24,lambdamax=210,lambdafunc=lambdafunction)
}

cat("Proportion of times being deleted in the thinning step:",mean(Records),"\n")




#############################################################################################
##########################################################################################
### Simulating the number of active visitors over time for one the day

calculatequeue <- function(arrivaltimes, servicetimes){
  Narrivals <- length(arrivaltimes) # Total number of arrival
  departuretimes <- sort(arrivaltimes+servicetimes) # Calculate and sort the departure times
  eventtimes <- 0 # This will be the vector for event times
  numbersinqueue <- 0 # This will be the vector for current active visitors, updated at each event time
  currentnumber <- 0  # Keeps track of the current number of visitors
  acounter <- 1  # Counter for the arrivals vector
  dcounter <- 1  # Counter for the departures vector
  while(acounter<=Narrivals){
    if(arrivaltimes[acounter]<departuretimes[dcounter]){ # If the next event is an arrival
      currentnumber <- currentnumber+1
      numbersinqueue <- c(numbersinqueue,currentnumber)
      eventtimes <- c(eventtimes,arrivaltimes[acounter])
      acounter <- acounter+1
    }
    else{  # If the next event is an departure
      currentnumber <- currentnumber-1
      numbersinqueue <- c(numbersinqueue,currentnumber)
      eventtimes <- c(eventtimes,departuretimes[dcounter])
      dcounter <- dcounter+1
    }
  }
  return(list(numbers=numbersinqueue,times=eventtimes))
}

# The intensity function 
lambdafunction <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3)
# Plot the intensity function
tvec <- seq(0,24,by=0.1)
plot(tvec,lambdafunction(tvec),type="l")

# Generate and plot the number of active visitors at the website a day
arrivaltimes <- simtNHPP(a=0,b=24,lambdamax=210,lambdafunc=lambdafunction)
servicetimes <- rgamma(length(arrivaltimes),shape=2,scale=3)/60  
novertime <- calculatequeue(arrivaltimes,servicetimes)
plot(novertime$times,novertime$numbers,type="s",xlab = "time", 
     ylab = "Number of visitors at the website",lwd=1.5)

#Estimation of the probability that the maximum number of visitors exeeds 30
Nsim <- 100 
maxvalues <- numeric(Nsim)
count = 0
for(i in 1:Nsim){
  arrivaltimes <- simtNHPP(a=0,b=24,lambdamax=210,lambdafunc=lambdafunction)
  servicetimes <- rgamma(length(arrivaltimes),shape=2,scale=3)/60
  novertime <- calculatequeue(arrivaltimes,servicetimes)
  maxvalues[i] <- max(novertime$numbers)  
  if (maxvalues[i]> 30){
    count = count +1
  }
  
}

cat("The probability that the maximum number of visitor exeeds 30:",count/Nsim,"\n")


# calculation of median, 5% and 95% quantiles of the number of visitors at time 12
Nsim <- 100 # This simulation takes some time
maxvalues <- numeric(Nsim)
for(i in 1:Nsim){
  arrivaltimes <- simtNHPP(a=0,b=12,lambdamax=210,lambdafunc=lambdafunction)
  servicetimes <- rgamma(length(arrivaltimes),shape=2,scale=3)/60
  novertime <- calculatequeue(arrivaltimes,servicetimes)
  maxvalues[i] <- max(novertime$numbers)  
}

cat("The median of the number of visitors at time 12:",median(maxvalues),"\n")
cat("The 5% and 95% quantiles of number of visitors at time 12:",quantile(maxvalues,probs = seq(0.05, 0.95, 0.9)),"\n")

summary(maxvalues)

cat("\n")

###################################################################
##################### Problem 2f ###################################
###################################################################
cat("Problem 2f \n")


# The function below simulate guest arrival times over [0,tau] to the website from 
# the intensity function.(Each time denotes a time of an gues arrival to the website)
NHPPsim <- function(tau=24){
  HPPtimes <- rexp(4000) # Simulate 4000 interarrival times on HPP scale
  HPPtimescumsum <- cumsum(HPPtimes)   # Calculate arrival times on HPP scale
  NHPPtimes <- sqrt((HPPtimescumsum/5)+1)-1   # Transform to NHPP scale
  NHPPtimes <- NHPPtimes[NHPPtimes<tau] # Delete the times larger than tau 
  return(NHPPtimes)  
}

# Generate and plot the data
NHPPtimes <- NHPPsim(tau=24)
plot(NHPPtimes,1:length(NHPPtimes),type="s",xlab = "hour", 
     ylab = "Event number",lwd=1.5)
points(NHPPtimes,rep(0,length(NHPPtimes)),pch=21,bg="red")


cat("\n")


###################################################################
##################### Problem 2g ###################################
###################################################################
cat("Problem 2g \n")

par(mfrow=c(1,2)) #split plot erea in 2

# The intensity functions
lambdafunction <- function(t)
  5+50*sin(pi*t/24)^2+190*exp(-(t-20)^2/3)
lambdafunction2 <- function(t)
  10+10*t

# Plot the intensity functions
tvec <- seq(0,24,by=0.01)
plot(tvec,lambdafunction(tvec),type="l")
tvec <- seq(0,24,by=0.01)
plot(tvec,lambdafunction2(tvec),type="l")

par(mfrow=c(1,1)) 

#Implementation of the alternative thinning algorithm
simtNHPP2 <- function(tau,lambdafunc,lambdafunc2){
  HPPtimes <- rexp(4000) # Simulate 4000 interarrival times on HPP scale
  HPPtimescumsum <- cumsum(HPPtimes)   # Calculate arrival times on HPP scale
  NHPPtimes <- sqrt((HPPtimescumsum/5)+1)-1   # Transform to NHPP scale
  NHPPtimes <- NHPPtimes[NHPPtimes<tau] # Delete the times larger than tau 
  Nevents <- length(NHPPtimes) # Count the number of events
  # Next do the thinning. Only keep the times where u<lambda(s)/lambdamax
  U <- runif(Nevents)
  NHPPtimes <- NHPPtimes[U<lambdafunc(NHPPtimes)/lambdafunc2(NHPPtimes)] #lambdasi/lambdamax
  return(NHPPtimes)  # Return the remaining times
}

# Generate data with the traffic intensity and plot them
NHPPtimesV2 <- simtNHPP2(tau=24,lambdafunc=lambdafunction,lambdafunc2=lambdafunction2)
plot(NHPPtimesV2,1:length(NHPPtimesV2),type="s",xlab = "time", 
     ylab = "Event number",lwd=1.5)
points(NHPPtimesV2,rep(0,length(NHPPtimesV2)),pch=21,bg="red")

#####################################################################################################
# One way to verify that the algorithm seems to be correct is to plot thinning methods
# And see if they are identical
cat("One way to verify that the algorithm seems to be correct is to plot thinning methods","\n")
cat("and see if they are identical.","\n")

par(mfrow=c(1,2)) #split plot erea in 2

# Standart thinning method
NHPPtimesV1 <- simtNHPP(a=0,b=24,lambdamax=210,lambdafunc=lambdafunction)
plot(NHPPtimesV1,1:length(NHPPtimesV1),type="s",xlab = "time", 
     ylab = "Event number",lwd=1.5)
points(NHPPtimesV1,rep(0,length(NHPPtimesV1)),pch=21,bg="red")

# Alternative thinning method
NHPPtimesV2 <- simtNHPP2(tau=24,lambdafunc=lambdafunction,lambdafunc2=lambdafunction2)
plot(NHPPtimesV2,1:length(NHPPtimesV2),type="s",xlab = "time", 
     ylab = "Event number",lwd=1.5)
points(NHPPtimesV2,rep(0,length(NHPPtimesV2)),pch=21,bg="red")

par(mfrow=c(1,1))

#####################################################################################################
# Estimation of the proportion of times being deleted in the thinning step for the alternative algorithm
Proportion2 <- function(tau,lambdafunc,lambdafunc2){
  HPPtimes <- rexp(4000) # Simulate 4000 interarrival times on HPP scale
  HPPtimescumsum <- cumsum(HPPtimes)   # Calculate arrival times on HPP scale
  NHPPtimes <- sqrt((HPPtimescumsum/5)+1)-1   # Transform to NHPP scale
  NHPPtimes <- NHPPtimes[NHPPtimes<tau] # Delete the times larger than tau 
  Nevents <- length(NHPPtimes) # Count the number of events
  # Next do the thinning. Only keep the times where u<lambda(s)/lambdamax
  U <- runif(Nevents)
  NHPPtimes <- NHPPtimes[U<lambdafunc(NHPPtimes)/lambdafunc2(NHPPtimes)] 
  Nevents2 <- length(NHPPtimes)
  ProportionOfTimesDel2 = 1 - Nevents2/Nevents
  return(ProportionOfTimesDel2) 
}

Records2 = numeric(100)
for (g in 1:100){
  Records2[g] <- Proportion2(tau=24,lambdafunc=lambdafunction,lambdafunc2=lambdafunction2)
}

cat("Proportion of times being deleted in the thinning step:",mean(Records2),"\n")

cat("The proportion of the deleted times in the alternative thinning algorithm became smaller.","\n")
cat("So we can state that it is not very efficeient to use lambda(max) when it is much higher","\n")
cat("compared to most lambda(t) as most of the proposed arrival times will be rejected.","\n")


cat("\n")


