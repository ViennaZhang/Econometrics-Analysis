#########################################################
#####        Simulation of Omitted Variable Bias    #####
#####                                               #####
#####  In this exercise we will explore the affect  #####
#####  that OVB has on estimating the coefficient   #####
#####  of interest. This code simulates 100 data    #####
#####  sets with E(U|X)=0 and with E(U|X)!=0. One   #####
#####  can run the code as is. The only parameter   #####
#####  you will be asked to change is the rho       #####
#####  parameter---this controls the level of bias  #####
#########################################################


#################################################.
######             PRELIMINARIES           ######
###### No need to edit anything here. Just ######
###### loading libraries                   ######
#################################################
library(MASS)
library(stats)
library(sm)
library(ggplot2)
library(dplyr)

#################################################
####### 				PARAMETERS   		  #######
####### Here is where we will experiment  #######
####### with changing things.             #######
#################################################
N = 100      # sample size
rho = .25    # Correlation (between 0,1)

sigmaX = 1   # Std. deviation of X
sigmaU = 1   # Std. deviation of U if uncorrelated

####################################################
#######            SIMULATION CODE           #######
####### Feel free to read through this, but  #######
####### you do not need to change anything!  #######
####################################################
# Generates the X variable -- You may ignore this code
set.seed(71923)              # Setting a seed
X = rnorm(N,0,sigmaX)	     # Draws X as an N(0,sigma_X^2) variable


# Generates the U variable -- normally we don't observe this (don't worry about the formula)
# If rho is zero these are independent, but if rho>0 these are correlated!
U = rnorm(N,sigmaU/sigmaX*rho*X,(1-rho^2)*sigmaU)

# Generates Y
Y = 1 + 2*X + U


# Creates the data frame that we would "see" in the real world:
data = data.frame(X,Y)


m1 = lm(Y~X, data = data)
print(coef(m1))


# Now let's see how the distribution of beta changes

# First, to have a baseline, let's get the right distribution of beta
S = 500   # number of samples

betaRight = numeric(S)
betaWrong = numeric(S)
for (s in 1:S){
	X = rnorm(N,0,sigmaX)   # Generates the X variable in this sample
	
	# Unbiased Sample
	U = rnorm(N,0,sigmaU)   # Generates the right U
	Y = 1 + 2*X + U
	mTemp = lm(Y~X)
	betaRight[s] = coef(mTemp)[2]
	
	# Biased Sample
	U = rnorm(N,sigmaU/sigmaX*rho*X,(1-rho^2)*sigmaU)   # Generates the biased Us
	Y = 1 + 2*X + U
	mTemp = lm(Y~X)
	betaWrong[s] = coef(mTemp)[2]
}

betaData = data.frame(c(betaRight,betaWrong),c(numeric(S),numeric(S)+1))
colnames(betaData) = c("BETAHAT","BIAS_DUMMY")

##################################################
########        STUDENT ANALYSIS         #########
######## Use some of the tools you have  #########
######## practiced to look at how OVB    #########
######## affects estimates               #########
##################################################

# Useful Information:
# 1. The simulation data is stored in "betadata"
# 2. We have named our estimates BETAHAT
# 3. The variable for whether it's a non-biased or biased estimated is BIAS_DUMMY
# 4. BIAS_DUMMY == 1 means biased, BIAS_DUMMY == 0 means unbiased

# Calculate the mean for BETAHAT when BIAS_DUMMY==1 and when BIAS_DUMMY==0
betaData %>% group_by(BIAS_DUMMY) %>% summarize(Mean = mean(BETAHAT, na.rm=TRUE),Std = sd(BETAHAT, na.rm=TRUE))

# Plot a comparison of the densities here using the sm.density.compare command
pdf("betaDensities_rho25_bigS.pdf")
sm.density.compare(betaData$BETAHAT,betaData$BIAS_DUMMY)
dev.off()




# Optional Problem
# Step 1: Get vector of means and SDs to calculate t's
b = betaData %>% group_by(BIAS_DUMMY) %>% summarize(Mean = mean(BETAHAT, na.rm=TRUE),Std = sd(BETAHAT, na.rm=TRUE))

# Step 2: Calculate the CI for beta = 2. This is the set of all \hat\beta's that would not reject 2.
ciLower = 2-1.96*b[2,3]
ciUpper = 2+1.96*b[2,3]

# Step 3: Count the number of beta's outside the CI
numDNRs = nrow(filter(betaData,BIAS_DUMMY==1 & BETAHAT>as.numeric(ciLower) & BETAHAT<as.numeric(ciUpper)))
numDNRs/500
