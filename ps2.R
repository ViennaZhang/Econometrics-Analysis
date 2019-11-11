# Calling libraries
library(ggplot2)
library(dplyr)

# a: Loading the datasets
load('~/Dropbox/Teaching/NYU/ugMetrics_2018/ps2/votes1.Rda')
load('~/Dropbox/Teaching/NYU/ugMetrics_2018/ps2/votes2.Rda')
load('~/Dropbox/Teaching/NYU/ugMetrics_2018/ps2/votes3.Rda')
load('~/Dropbox/Teaching/NYU/ugMetrics_2018/ps2/votes1000.Rda')

# b: Calculate the mean
mean1 = votes1 %>% summarize(mean(candidate_support) ,sd(candidate_support))

# c: First way - do by hand

# c: Second way - using R
test1 = votes1 %>% summarize(mean = mean(candidate_support), sd = sd(candidate_support), N = n())
                     
# d: More means
mean2 = votes2 %>% summarize(mean(candidate_support))
mean3 = votes3 %>% summarize(mean(candidate_support))

# f: t-test 
test1000 = vote1000 %>% group_by(state) %>% summarize(mean(candidate_support),sd(candidate_support), N=n())