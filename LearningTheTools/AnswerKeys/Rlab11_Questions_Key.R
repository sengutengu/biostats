## RLab 11 Practice Questions Key ##

## Question 1: Telomeres ##

# A - Create a scatter plot

teloData <- read.csv("DataForLabs/telomere inheritance.csv",stringsAsFactors = T)
library(ggplot2)
ggplot(teloData,aes(x=father_telomere_length,y=offspring_telomere_length))+
  geom_point()+
  theme_classic()

# B - Is transformation needed?

# Subjectively, it looks like there is a linear pattern between
# X and Y and that there are no outliers. So it doesn't seem that 
# a transformation is needed

# C - Estimate an equestion for father telomere length predicting offspring telomere length

teloReg <- lm(father_telomere_length~offspring_telomere_length,data=teloData)
teloReg
summary(teloReg)

equation: Y = 0.3425*X + 0.3265


## Question #2 -- Number lines ##

# A -- plot 4th graders guesses against the true value

lineData <- read.csv("DataForLabs/numberline.csv",stringsAsFactors = T)
ggplot(lineData,aes(x=true_value,y=fourth_graders_guess))+
  geom_point()+
  theme_classic()

# relationship appears approximately linear

# B -- plot 4th graders guesses against the true value

ggplot(lineData,aes(x=true_value,y=second_graders_guess))+
  geom_point()+
  theme_classic()

# This doesn't look very linear across the full range of X values. 
# At first Y increases steeply, but as X increases the relationship 
# becomes flatter.

# try natural log transformation to both variables
lineData$LNtrue_value <- log(lineData$true_value)
lineData$LNsecond_graders_guess <- log(lineData$second_graders_guess)

# plot log-log transformed data

ggplot(lineData,aes(x=LNtrue_value,y=LNsecond_graders_guess))+
  geom_point()+
  theme_classic()

# relationship looks more linear

# create regression models with original and transformed data
lineReg1 <- lm(second_graders_guess~true_value,data=lineData)
lineReg2 <- lm(LNsecond_graders_guess~LNtrue_value,data=lineData)

# plot residuals of both models
ggplot(lineReg1,aes(x=true_value,y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_classic()

ggplot(lineReg2,aes(x=LNtrue_value,y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_classic()

# Notice that in the residual plot of the original values that there
# is a "funnel" pattern (wide with small X, narrow with large X) that 
# is not present in the residual plot with the transformed values.

# C -- Difference between 2nd and 4th graders?

# Let's make a plot with both...
# Here I plot the 4th graders first and then add a second geom_point()
# for the 2nd graders. Note that for the 2nd graders geom_point that I 
# add aes(y=second_graders_guess) to specify a different Y and col="blue"
# to specify a different color. For an extra visual, I add a line with
# slope=1 and intercept=0, which would represent a perfect match between 
# X and Y

ggplot(lineData,aes(x=true_value,y=fourth_graders_guess))+
  geom_point()+
  geom_point(aes(y=second_graders_guess),col="blue")+
  geom_abline(slope=1,intercept=0)+
  theme_classic()

# Notice that the 4th graders (black points) are more accurate, particularly
# for values < 600


## Question #3 -- Brain and body size ##

# A -- plot brain size against body size

mammalData <- read.csv("DataForLabs/mammals.csv",stringsAsFactors = T)

ggplot(mammalData,aes(x=body_mass_kg,y=brain_mass_g))+
  geom_point()+
  theme_classic()

# it is hard to judge linearity with the two outlier points (really large mammals)

# B -- try transformation

# try natural log for both variables
mammalData$LNbody_mass_kg <- log(mammalData$body_mass_kg)
mammalData$LNbrain_mass_g <- log(mammalData$brain_mass_g)

ggplot(mammalData,aes(x=LNbody_mass_kg,y=LNbrain_mass_g))+
  geom_point()+
  theme_classic()

# looks linear!

# C -- statistical evidence?

mammalReg <- lm(LNbrain_mass_g~LNbody_mass_kg,data=mammalData)
summary(mammalReg)

# Here the P-value for the slope is <2e-16, so you reject the null
# hypothesis that the slope is zero

# D -- prediction line

mammalReg

# Y = 0.7545*X + 2.1272 

# E -- change in Y with 3 units change in X

# if X=0 then Y = 0.7545*0 + 2.1272, or Y = 2.1272
# if X=3 then Y = 0.7545*3 + 2.1272, or Y = 4.3907
# so the difference is 4.3907-2.1272 = 2.2635

# notice that the answer is the same with different
# X values that are 3 units apart. Try X=3 and X=6:

# if X=3 then Y = 0.7545*3 + 2.1272, or Y = 4.3907
# if X=6 then Y = 0.7545*6 + 2.1272, or Y = 6.6542
# so the difference is 6.6542-4.3907 = 2.2635

# F -- residual plot

ggplot(mammalReg,aes(x=LNbody_mass_kg,y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_classic()

# the plot looks like it meets the assumptions
# there cloud is centered on zero without a funnel pattern

# G -- largest residual

# The residuals are the distances between individual data points
# and the best fit line. Positive residuals mean values of Y
# that are larger than predicted. So the mammal with the largest
# brain size relative to body size is the positive point farthest
# above the horizontal line in the plot

# view the residuals, which have the same order as the rows in the 
# original data frame
mammalReg$residuals

# find the largest residual value
max(mammalReg$residuals)

# the function which.max() will give you the index position for the largest value
which.max(mammalReg$residuals)

# The index position is 32, and if you view the mammalData df 
# then you see that human is in the 32nd row