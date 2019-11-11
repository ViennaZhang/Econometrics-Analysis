library(AER)
library(lmtest)
library(ggplot2)
library(sandwich)
library(stargazer)
library(plm)



data("Guns",package="AER")


# Part a
m1 <- lm(log(murder)~law,data=Guns)
m2 <- lm(log(murder)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns)
m3 <- plm(log(murder)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="individual")
m4 <- plm(log(murder)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="twoway")

v1 <- lm(log(violent)~law,data=Guns)
v2 <- lm(log(violent)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns)
v3 <- plm(log(violent)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="individual")
v4 <- plm(log(violent)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="twoway")

r1 <- lm(log(robbery)~law,data=Guns)
r2 <- lm(log(robbery)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns)
r3 <- plm(log(robbery)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="individual")
r4 <- plm(log(robbery)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns,index = c("state","year"),model="within",effect="twoway")

mur.se1   <- sqrt(diag( vcovHC(m1, type = "HC1")))
mur.se2   <- sqrt(diag( vcovHC(m2, type = "HC1")))
mur.se3   <- sqrt(diag( vcovHC(m3,  method="white1", type = "HC1")))
mur.se4   <- sqrt(diag( vcovHC(m4, method="white1", type = "HC1")))
mur.se5   <- sqrt(diag( vcovHC(m4, type = "HC1",cluster="group")))

vio.se1   <- sqrt(diag( vcovHC(v1, type = "HC1")))
vio.se2   <- sqrt(diag( vcovHC(v2, type = "HC1")))
vio.se3   <- sqrt(diag( vcovHC(v3, method="white1", type = "HC1")))
vio.se4   <- sqrt(diag( vcovHC(v4, method="white1", type = "HC1")))
vio.se5   <- sqrt(diag( vcovHC(v4, type = "HC1",cluster="group")))

rob.se1   <- sqrt(diag( vcovHC(r1, type = "HC1")))
rob.se2   <- sqrt(diag( vcovHC(r2, type = "HC1")))
rob.se3   <- sqrt(diag( vcovHC(r3,  method="white1",type = "HC1")))
rob.se4   <- sqrt(diag( vcovHC(r4, method="white1", type = "HC1")))
rob.se5   <- sqrt(diag( vcovHC(r4, type = "HC1",cluster="group")))


stargazer(m1, m2,m3,m4,m4, type = "latex", 
          title            = "Murder Rate and Shall Carry Law",
          dep.var.caption  = "Log of Murder Rate",
          omit.stat = c("ser","f"),
          add.lines = list(c("Fixed effects?", "No", "No","State","State+Year","State+Year"),
                           c("Clustering?", "No", "No","No","No","State")),
          se = list(mur.se1,mur.se2,mur.se3,mur.se4,mur.se5 ))

stargazer(v1, v2,v3,v4,v4, type = "latex", 
          title            = "Violent Crime Rate and Shall Carry Law",
          dep.var.caption  = "Log of Violent Crime Rate",
          omit.stat = c("ser","f"),
          add.lines = list(c("Fixed effects?", "No", "No","State","State+Year","State+Year"),
                           c("Clustering?", "Noi", "No","No","No","State")),
          se = list(vio.se1,vio.se2,vio.se3,vio.se4,vio.se5 ))


stargazer(r1, r2,r3,r4,r4, type = "latex", 
          title            = "Robbery Rate and Shall Carry Law",
          dep.var.caption  = "Log of Robbery Rate",
          omit.stat = c("ser","f"),
          add.lines = list(c("Fixed effects?", "No", "No","State","State","State+Year"),
                           c("Clustering?", "No", "No","No","No","State")),
          se = list(rob.se1,rob.se2,rob.se3,rob.se4,rob.se5 ))


# Part h
ssr = sum(m3$residuals^2)
sst = sum((log(Guns$murder)-mean(log(Guns$murder)))^2)

m6 = lm(log(murder)~law+log(density)+log(income)+log(prisoners) + male + cauc+ afam,data=Guns)
s6 = summary(m6)

R2UR = 1-ssr/sst
R2R = s6$r.squared

Fstat = (R2UR-R2R)/(1-R2UR)*(1173 - 51 - 7 - 1)/51

