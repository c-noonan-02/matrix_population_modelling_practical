# Matrix Population Modelling

rm(list=ls())

# load required packages
library(popbio)
library(ggplot2)

# STEP ONE: Parameterising your Matrix Model (MPM)

# used a Cormack-Jolly-Seber model in package marked to estimate survival rates
# comparing a model with and without time varying detection probabilities, found
# evidence that detection probability varied among years. Used time-varying
# detection probabilities when estimating the stage-specific survival rates:

juv_survival <- 0.463 # (95% CI 0.404-0.524)
yr_survival <- 0.510 # (95% CI 0.445-0.574)
ad_survival <- 0.559 # (95% CI 0.499-0.618)

# enter the values into the data frame
sparrow_survival <- data.frame(stage=factor(c('Juvenile','Yearling','Adult'), levels=c('Juvenile','Yearling','Adult')), estimate=c(juv_survival, yr_survival, ad_survival), lcl=c(0.404, 0.445, 0.499), ucl=c(0.524, 0.574, 0.618))

# plot the data by stage
ggplot(sparrow_survival, aes(stage, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

# Q1 - Which stage has the lowest survival rate? Is this what you would expect?
# juveniles have the lowest survival - this makes sense as they are likely the
# more vulnerable stage, and there are factors such as overproduction etc. 

