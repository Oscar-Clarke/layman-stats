# Oscar Clarke
# How to perform multinomial logistic regression in R 
# Date created: 18.11.17

# Credits:
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

# Interested in determining influence of prior education at private or public school and writing score on students choice of programme (academic, general or vocation).


# Last edited: 18.11.17

# clear work environment
rm(list=ls())

## Import data 
# Contains 13 different variables but we are interested in three of these: prog (programme choice), schtyp (school type), and write (writing score). Because our data is in dta format we need the 'read.dta' function from the foreign library 
install.packages('foreign')
library(foreign)
CHOICE <- read.dta('hsbdemo.dta')


# Examine data to check that it imported correctly and variables are encoded appropriately
head(CHOICE)

summary(CHOICE)

str(CHOICE)
# It is essential that our response variable is coded as a factor for multinomial analysis but this is already the case.


## Data quality control
# Examine our three variables

levels(CHOICE$prog)
summary(CHOICE$prog)
# Three levels for our response variable programme choice: general (45), academic (105), and vocation (50). Clearly academic is the most popular choice overall.

levels(CHOICE$schtyp)
summary(CHOICE$schtyp)
# Two levels: public (168) and private (32). Many more observations from students educated at public schools

hist(CHOICE$write)
# Left skewed bimodal distribution with most grades falling between 50-65 but a large drop-off after 65. No visible outliers


## Explanatory plots and tables

# Choices of programme type from students educated at public and private schools
with(CHOICE, table(schtyp, prog))
# Academic most popular choice for both public and private, however vocation choice seems proportionaly more popular for public over private

# Association between writing score and programme choice
plot(data=CHOICE,write~prog)
# A higher write score is more common in academic choice, followed by general, with vocation associated with the lowest write score

# Instead of looking at overall influence of writing score on programme choice, it may be more beneficial to look at this for both public and private schools
PUBLICPROG <- subset(CHOICE, schtyp == 'public', select = c(schtyp, write, prog))
PRIVATEPROG <- subset(CHOICE, schtyp == 'private', select = c(schtyp, write, prog))

par(mfrow=c(1,2))
boxplot(data = PUBLICPROG, write~prog)
boxplot(data = PRIVATEPROG, write~prog)
par(mfrow=c(1,1))
# similar association between writing score and programme choice for general and academic but vocation writing score greater for private than public 


# Relevel data so academic is the reference level
CHOICE$prog2 <- relevel(CHOICE$prog, ref='academic')
head(CHOICE)
# Multinomial analysis compares all categories against a reference level so the output will result in k-1 lines of coefficients (where k = the number of categories in our response variable). Typically it is recommended to use the non-effect level (e.g. a control or the norm), but as this is not relevant in our dataset we have chosen the one with the most observations.

# Import library nnet to gain the multinom function
install.packages('nnet')
library(nnet)

## Model construction
# Perform multinomial logistic analysis
MULTINOM.1 <- multinom(prog2~schtyp*write,data=CHOICE)
# automatically provides some output. This includes some iteration history the final negative log-likelihood. Multiplying this value by two provides the residual deviance which can be used to compare nested models 

# Remove interaction term from the model
MULTINOM.2 <- update(MULTINOM.1,~.-schtyp:write)

# Compare models against one another to determine minimal adequate model
install.packages('VGAM')
library(VGAM)
lrtest(MULTINOM.1,MULTINOM.2)

summary(MULTINOM.1)
summary(MULTINOM.2)
# Currently lr test not working, however AIC value is lower for MULTINOM.2 so select this as our minimal adequate model

# rexamine output of MULTINOM.2
summary(MULTINOM.2)
# The output contains two rows for comparison of general vs academic and vocation vs academic, and  three columns for our intercept, difference between public and private school and writing score. Coefficients are given in log odds, thus is interpreted as:
# The log odds of being in general over academic decrease by 0.575 if moving from public to private
# The log odds of being in vocation over academic decrease by 1.74 if moving from public to private
# A one unit increase in write is associated with a decrease in the log odds of being in general over academic by 0.065
# A one unit increase in write is associated with a decrease in the log odds of being in vocation over academic by 0.113


# Extract coefficients from the model and exponentiate
exp(coef(MULTINOM.2))
# exponentiating the model coefficients modifies them from log odds to odds. If odds = 1 there is no difference, if odds < 1, odds of being in one category over another decreases, if odds > 1, odds of being in one category over another increases.
# Here all of our odds fall below 1, thus odds of being in general over academic programme decreases as writing score increases, etc... 
# The effect size is greater for school type than writing score, and greatest between vocation vs academic


# Predict enables us to use our model output to predict the response category for each of our observations in our original dataset (here 200 observations).
# This can be outputted as probabilities...
PROB <- predict(MULTINOM.2, CHOICE, type = "prob")
head(PROB)
# ... or classes (the predicted class will be the one with the highest probability)
CLASS <- predict(MULTINOM.2, CHOICE, type = "class")
CLASS

# Confusion matrix

# Significance values

## Plotting model predictions
# Need to import two additional packages for the melt function {reshape2} to create a new dataframe with appropriate values, and for plotting 0functions {ggplot2} 
install.packages('ggplot2')
library(ggplot2)
install.packages('reshape2')
library(reshape2)

# Generates a new x variables for write at each level of schtyp
NEWXVARS <- data.frame(schtyp=rep(c('public','private'), each = 41), write = rep(c(30:70),2))

# Stores the predicted probailities for each value in NEWXVARS
NEWYVARS <- cbind(NEWXVARS, predict(MULTINOM.2, newdata = NEWXVARS, type = 'probs', se = TRUE))

# Melt data so that each new row is a unique value of schooltype and writing score from NEWYVARS
MDATA <- melt(NEWYVARS, id.vars = c('schtyp', 'write'), value.name = 'probability')

# Create plot showing probability of being in academic, general or vocation programme based on school type and writing score
ggplot(MDATA, aes(x = write, y = probability, colour = schtyp)) + 
  geom_line() + 
  facet_grid(variable ~., scales = "free") +
  ylim(0,1) +
  ylab('Probability') +
  xlab('Writing score') +
  scale_colour_discrete(name="School type")
# adding the values of both respective line types (public vs private) at each level of writing score will equate to one as data is presented as probabilities                        
# Plot shows that as writing score increases, the probability of choosing an academic programme increases for both publically and privately educated students, however the probability is always higher for privately educated students. The probability of choosing a vocation programme decreases as writing score increases for both private and public students, however the decline is much greater for public students as probability is greater at low writing scores for public students. Probability of choosing a general programme changes only moderately as writing score increases, exhibiting a slight decline.


# Calculate Z score and p-value for model variables
z.score <- summary(MULTINOM.2)$coefficients/summary(MULTINOM.2)$standard.errors
z.score

p.value <- (1-pnorm(abs(z),0,1))*2
p.value

