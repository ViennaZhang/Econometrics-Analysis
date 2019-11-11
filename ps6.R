### Binary Dependent Variables Examples and Code ### 

# Set working directory
setwd('~/Dropbox/Teaching/NYU/ugMetrics_2018/ps6/')

# Loading Libraries
library(MASS)
library(stats)
library(sm)
library(ggplot2)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)


# Library with lots of example data sets
#library(Ecdat)

load("Cracker.Rda")

# Part a
ma1 = lm(nabiscoDummy ~ price.nabisco + disp.nabisco + feat.nabisco, data=Cracker)
se.ma1 = sqrt(diag( vcovHC(ma1, type = "HC1")))

ma2 = glm(nabiscoDummy ~ price.nabisco + disp.nabisco + feat.nabisco, data=Cracker, family=binomial(link="probit"))
se.ma2 = sqrt(diag( vcovHC(ma2)))

# Part c (you can do this by hand, I am using the dot product to make life easier)
z1 = coef(ma2) %*% c(1,100,0,0)

marg1 = coef(ma2)["price.nabisco"]*dnorm(z1)

z2 = coef(ma2) %*% c(1,100,0,1)
marg2 = coef(ma2)["price.nabisco"]*dnorm(z2)


# Part d
meanP = mean(Cracker$price.nabisco)

marketShareLPM1 = coef(ma1) %*% c(1,meanP,0,1)
marketShareProbit1 = pnorm(coef(ma2) %*% c(1,meanP,0,1))

marketShareLPM2 = coef(ma1) %*% c(1,50,0,1)
marketShareProbit2 = pnorm(coef(ma2) %*% c(1,50,0,1))

marketShareLPM3 = coef(ma1) %*% c(1,150,0,1)
marketShareProbit3 = pnorm(coef(ma2) %*% c(1,150,0,1))

print(c(marketShareLPM1,marketShareLPM2,marketShareLPM3))
print(c(marketShareProbit1,marketShareProbit2,marketShareProbit3))


# Part e
# LPM
deltaPi_LPM = coef(ma1)["feat.nabisco"]*100 - 5

# Probit
deltaPi_Probit = (pnorm(coef(ma2) %*% c(1,100,0,1))-pnorm(coef(ma2) %*% c(1,100,0,0)))*100-5


# Part f
mf1 = lm(nabiscoDummy ~ price.nabisco + disp.nabisco*feat.nabisco, data=Cracker)
se.mf1 = sqrt(diag( vcovHC(mf1, type = "HC1")))

mf2 = glm(nabiscoDummy ~ price.nabisco + disp.nabisco*feat.nabisco, data=Cracker, family=binomial(link="probit"))
se.mf2 = sqrt(diag( vcovHC(mf2)))

stargazer(ma1, mf1, ma2, mf2, type = "latex", 
          title            = "Nabisco Market Share and Marketing Chocies",
          dep.var.caption  = "Market Share",
          omit.stat = c("ser","f"),
          se = list(se.ma1,se.mf1, se.ma2, se.mf2))


#stargazer(mf1, mf2, type = "latex", 
#          title            = "Nabisco Market Share and Marketing Chocies",
#          dep.var.caption  = "Market Share",
#          omit.stat = c("ser","f"),
#          se = list(se.mf1,se.mf2))


# Part g
mg1 = lm(nabiscoDummy ~ price.nabisco + price.keebler + price.sunshine + price.private, data=Cracker)
se.mg1 = sqrt(diag( vcovHC(mg1, type = "HC1")))

mg2 = glm(nabiscoDummy ~ price.nabisco + price.keebler + price.sunshine + price.private, data=Cracker, family=binomial(link="probit"))
se.mg2 = sqrt(diag( vcovHC(mg2)))

stargazer(mg1, mg2, type = "latex", 
          title            = "Nabisco Market Share and Marketing Chocies",
          dep.var.caption  = "Market Share",
          omit.stat = c("ser","f"),
          se = list(se.mg1,se.mg2))


# Part i
a0 = coef(mg1) %*% c(1,0,mean(Cracker$price.keebler),mean(Cracker$price.sunshine),mean(Cracker$price.private))
a1 = coef(mg1)["price.nabisco"]

pStar = -a0/(2*a1)

# Part j
mj = glm(nabiscoDummy ~ feat.nabisco + feat.keebler + feat.sunshine + feat.private, data=Cracker, family=binomial(link="probit"))
se.mj = sqrt(diag( vcovHC(mj)))

stargazer(mj, type = "latex", 
          title            = "Nabisco Market Share and Marketing Chocies",
          dep.var.caption  = "Market Share",
          omit.stat = c("ser","f"),
          se = list(se.mj))

effect1 = pnorm(coef(mj)%*%c(1,1,0,1,1))-pnorm(coef(mj)%*%c(1,0,0,1,1))
effect2 = pnorm(coef(mj)%*%c(1,1,0,0,0))-pnorm(coef(mj)%*%c(1,0,0,0,0))


# Part k
mk = glm(nabiscoDummy ~ price.nabisco + price.keebler + feat.nabisco , data=Cracker, family=binomial(link="probit"))

R1 = pnorm(coef(mk)%*%c(1,90,100,0))*90
R2 = pnorm(coef(mk)%*%c(1,90,100,1))*90
R3 = pnorm(coef(mk)%*%c(1,110,100,0))*110
R4 = pnorm(coef(mk)%*%c(1,110,100,1))*110

Pi1 = R1
Pi2 = R2 - 5
Pi3 = R3
Pi4 = R4 - 5
