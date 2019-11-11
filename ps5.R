# Set Working Directory

setwd("/Users/SharonT/Dropbox/Teaching/NYU/ugMetrics/ps5")

load("birthweight.Rdata")

# Loading Libraries
library(MASS)
library(stats)
library(sm)
library(ggplot2)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(plm)
library(Ecdat)


# Part a. 
prop = sum(birthweight$weight<=2500)/length(birthweight$weight)

# Parts b - d (you can ignore the lines generating the standard errors)
mb = lm(weight~factor(boy), data=birthweight)
mb.se   <- sqrt(diag( vcovHC(mb, type = "HC1")))

mc = lm(weight~factor(boy) + factor(smoke), data=birthweight)
mc.se   <- sqrt(diag( vcovHC(mc, type = "HC1")))

md = lm(weight~factor(boy)*factor(smoke), data=birthweight)
md.se   <- sqrt(diag( vcovHC(md, type = "HC1")))

stargazer(mb, mc, md, type = "latex", 
          title            = "Birthweight and Smoking Habits",
          dep.var.caption  = "Birthweight (grams)",
          omit.stat = c("ser","f"),
          se = list(mb.se, mc.se, md.se))


# Part e
# There are many ways to get the R2. One may use the summary command and do it by hand.
# One may also extract the R2 from the summary using the $r.squared command

mUR =  lm(weight~factor(smoke), data=birthweight)

sUR = summary(mUR)
sd = summary(md)

Fstat = (sd$r.squared-sUR$r.squared)/(1-sd$r.squared)*(sd$df[2]/2)

# To double check, we can compare to the computer calculated F
w = waldtest(md,mUR)


# Parts f-i

mf = lm(weight ~ mom_age,data=birthweight)
mf.se   <- sqrt(diag( vcovHC(mf, type = "HC1")))

mg = lm(weight ~ mom_age + I(mom_age^2),data=birthweight)
mg.se   <- sqrt(diag( vcovHC(mg, type = "HC1")))

waldtest(mg,mf,vcov=vcovHC(mg, type = "HC1"))

sg = summary(mg)
regF = sg$r.squared/(1-sg$r.squared)*sg$df[2]/(sg$df[1]-1)


# Part j: Involves taking the derivative

marg20 = coef(mg)["mom_age"] + 2*coef(mg)["I(mom_age^2)"]*20
marg40 = coef(mg)["mom_age"] + 2*coef(mg)["I(mom_age^2)"]*40


# Part k - l
mk = lm(weight ~  log(mom_age),data=birthweight)
mk.se   <- sqrt(diag( vcovHC(mk, type = "HC1")))

ml = lm(weight ~ smoke + mom_age + I(mom_age^2) + factor(educ) + factor(black) + factor(boy), data=birthweight)
ml.se <- sqrt(diag( vcovHC(ml, type = "HC1")))

stargazer(mf, mg, mk, ml, type = "latex", 
          title            = "Birthweight and Smoking Habits",
          dep.var.caption  = "Birthweight (grams)",
          omit.stat = c("ser","f"),
          se = list(mf.se, mg.se, mk.se, ml.se))

# Part m
birthweight$girl  = 1 - birthweight$boy
lm1 = lm(weight ~ boy + girl, data = birthweight)
lm2 = lm(weight ~-1 + boy + girl, data = birthweight)

stargazer(lm1,lm2, type = "latex", 
          title            = "Birthweight and Gender",
          dep.var.caption  = "Birthweight (grams)",
          omit.stat = c("ser","f"))