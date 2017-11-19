# Oscar Clarke
# How to perform binary logistic regression in R
# Date created: 18.11.17

# Credits:
# https://rpubs.com/brouwern/logistic

# Looking at the influence of length and number of spots on mortality in an arbitrary fish species

# last edited: 19.11.17

# Clear work environment
rm(list=ls())

## Create data

# vector for number of spots 
SPOTS <- ceiling(runif(50, -1, 10))

# vector for vish length
LENGTH <- seq(10,100,length.out = 50)

# vector for fish mortality (1 = dead, 0 = alive)
MORTALITY <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,0,1,0,1,1,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0)

# Merge spots, length and mortality into a single dataframe
FISH <- data.frame(SPOTS = SPOTS, LENGTH = LENGTH, MORTALITY = MORTALITY)

# Examine data to see it merged correctly
head(FISH)

summary(FISH)

str(FISH)
# Note that for binary regression the reponse variable can be coded as an number or factor, although it is often recommended to have this as a number

## Quality caontrol

# Examine our three variables
hist(FISH$SPOTS)
# data fairly uniformly distributed (note this may be slightly different when you run it due to random numbers generated)

hist(FISH$LENGTH)
# again a relatively uniform distribution

hist(FISH$MORTALITY)
# a similar number of observation for dead and alive fish but slighlty less less for alive fish


# Explanatory plots

# Association between mortality and length
plot(data = FISH, MORTALITY~LENGTH)
# appears to be a minor negative association between mortality and length

# Association between mortality and spots
plot(data= FISH, MORTALITY~SPOTS)
# Appears to be no relationship

# Check for covariance
pairs(FISH)
# none identified

# Model construction
# Perform binary logistic regression analysis
MOD.1 <- glm(data = FISH, MORTALITY~LENGTH+SPOTS, family = 'binomial')

# Install package {boot} for function glm.diag.plots to enable us to examine model diagnostics
install.packages('boot')
library(boot)
# Check model diagnostics
glm.diag.plots(MOD.1)

# Examine model output
summary(MOD.1)
# Spots are non-significant, try removing this variable

MOD.2 <- update(MOD.1,~.-SPOTS)

# Examine MOD.2 diagnistics
glm.diag.plots(MOD.2)

anova(MOD.1,MOD.2,test="Chi")
# no significant difference between models

# Examine MOD.2 output
summary(MOD.2)
# Lower AIC value and length significant
# MOD.2 minimal adequate model

# allows specific points to be identified
glm.diag.plots(MOD.2,iden=TRUE)
par(mfrow=c(1,1))


# Plotting

# Generate prediction values for our regression slope
# New variables for length
summary(FISH$LENGTH)
XLENGTH<-seq(10,100,length=100)

# Model predictions for mortality on new length values
YLENGTH<-predict(MOD.2,list(LENGTH=XLENGTH),type="link",se=TRUE)

# Plot raw data
plot(FISH$LENGTH,FISH$MORTALITY,xlab=expression("Length (cm)"),ylab="Mortality probability")

# Add regression lines with confidence intervals
# library {faraway} needed for logit function
install.packages('faraway')
library(faraway)

# Add regression lines with upper and lower confidence intervals
lines(XLENGTH,ilogit(YLENGTH$fit),lwd=2)
lines(XLENGTH,ilogit(YLENGTH$fit+1.96*YLENGTH$se.fit),lty=3)
lines(XLENGTH,ilogit(YLENGTH$fit-1.96*YLENGTH$se.fit),lty=3)

