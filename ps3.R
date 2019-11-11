# Move to my folder
setwd("/Users/sharont/Dropbox/Teaching/NYU/ugMetrics_2018/ps3")

# Load the important libraries
library(ggplot2)
library(stats)
library(MASS)
library(dplyr)
library(lmtest)
library(sandwich)

# Read in the data
dat = read.csv("nyc_schools.csv", header = TRUE)

# a) Scatter Plot
gg = ggplot(data = dat, aes(x = averageclasssize,y=mathscore)) + 
  geom_point(alpha=.4, size=2, color="#880011") +
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("Class Size and Test Scores") +
  labs(x="Average Class Size", y="Math Score")

plot(gg)

# b - c ) Testing with homo- and hetero- scedastic errors 
reg1 = lm(mathscore ~ averageclasssize, data = dat)
summary(reg1)
coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))

# d) Redoing with a filtered data set
dat2 = (dat %>% filter(averageclasssize<40))
gg2 = ggplot(data = dat2, aes(x = averageclasssize,y=mathscore)) + geom_point(alpha=.4, size=2, color="#880011")
plot(gg2)

reg2 = lm(mathscore ~ averageclasssize, data = dat2)
summary(reg2)
coeftest(reg2, vcov = vcovHC(reg1, type="HC1"))
