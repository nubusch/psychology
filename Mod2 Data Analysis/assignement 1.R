


#assignment 1
#Ting Boezewinkel
#18/11/2022



#import
library(tidyverse)
library(janitor)
library(ggplot2)


y <- c( 81,63,55,73,91,66,48,81,92,73,35,21,66,63)
x = sum(y,rm = FALSE)

n = 14


mu = x / n

variance = ((sum((y-rep(mu,n))**2))/n)

SD = sqrt(variance)

sigy = sqrt(SD/n)


Z  = 1.645    #90% CI
Z2 = 2.58     #99% CI


#90% CI
top90       = mu + Z * (SD/sqrt(n)) 
bottom90    = mu - Z * (SD/sqrt(n))


#99% CI
top99       = mu + Z2 * (SD/sqrt(n))
bottom99    = mu - Z2 * (SD/sqrt(n))

t.test(y, conf.level = 90)
























