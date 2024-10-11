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


# STEP TWO: Per capita reproduction

# need to estimate the the number of female offspring produced by each female between censuses

# load the dataset for Gjeroy for 1998-2012
nest_data <- read.table("./Data/gjeroynest.txt", header = TRUE, sep = '\t')
head(nest_data)

# clutchno indicates whether it was the first, second or third etc. clutch laid 
# in that nest in that breeding season

# hatchingsuc indicates whether any live chicks were found for that clutch (yes - 1, no = 0)

# chickno indicates the number of chicks counted on the final visit prior to fledging

# Q2 - How might you estimate per capita reproduction from these data?
# calculate average hatching success
hatching_success <- mean(nest_data$hatchingsuc)
hatching_success
# and average number of chicks
fledgling_no <- mean(nest_data$chickno)
fledgling_no

# for the number of clutches we create a new dataframe
# one row for each unique nest
# column number of clutches takes maximum value of clutchno for each unique 
# value of nest ID
nests <- data.frame(nestid = sort(unique(nest_data$nestid)), numberofclutches=tapply(nest_data$clutchno, nest_data$nestid, max))
head(nests)
# then take the mean of these values to be the average number of clutches
clutch_no <- mean(nests$numberofclutches)

# calculate expected number of chicks per female over the breeding season
# divide by 2 as only concerned with female segment of the population (assumes equal sex ratio)
sparrow_R <- (clutch_no*hatching_success*fledgling_no)/2


# STEP THREE: Deterministic population model

# get vital rate estimates
sparrow_R
juv_survival
yr_survival
ad_survival

# Juvenile to Juvenile: survival_juv * R
# Yearling to Juvenile: survival_yr * R
# Adult to Juvenile: survival_.ad * R
# Juvenile to Yearling: survival_juv
# Yearling to Yearling: 0 
# Adult to Yearling: 0 
# Juvenile to Adult: 0
# Yearling to Adult: survival_yr
# Adult to Adult: survival_ad

# put the transition probabilities into a vector
sparrow_MPM <- c(juv_survival * sparrow_R, yr_survival * sparrow_R, ad_survival * sparrow_R, juv_survival, 0, 0, 0, yr_survival, ad_survival)

# save vector as a matrix, specifying the number of rows and columns
# use byrow=TRUE argument to tell R that the first the elements of the vector
# correspond to the first row of the matrix
sparrow_MPM <- matrix(sparrow_MPM, nrow=3, ncol=3, byrow=T)
sparrow_MPM

# use popbio package to do some analyses of our deterministic MPM
# look at growth rate, lambda
lambda(sparrow_MPM)

# Q3 - What does this tell us about the dynamics of sparrow population?
# population is growing

