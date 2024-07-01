## R Lab 8: The normal distribution and sample means

## Key for questions at end of lab

#1a vector of random numbers from normal dist with mean=15 and sd=3
normal_vector <- rnorm(n = 10, mean = 15, sd = 3)

#1b plot data with hist()
hist(normal_vector)

#1c QQ plot
qqnorm(normal_vector)

#1d repeat a-c several times

#For this variable the "population" does have a normal distribution 
#with mean=15 and sd=3. However, when you draw only 10 samples from
#this population the sample is susceptible to error. Thus, the histogram
#could potentially not be bell-shaped and QQ plot could potentially
#not be a straight line of points.

#2 repeat questions in #1, but with n=250

#Increasing n gives a better estimation of the underlying population
#and the plots should better match expectations of normality

#3 data set on sparrow measurements
s <- read.csv("DataForLabs/bumpus.csv")

#3a plot frequency distribution of total length
library(ggplot2)
ggplot(s,aes(x=total_length_mm))+
  geom_histogram(bins=15)+
  xlab("Total body length (mm)")+
  ylab("Frequency")+
  theme_classic()

#although there is a bin at the mode that appears undersampled, it 
#looks plausible that the variable is approximately normally distributed

#3b qqnorm of total length
qqnorm(s$total_length_mm)
qqline(s$total_length_mm)

#data points generally fall along line, signaling normality

#3c calculate mean and 95%CI
total_length_mean <- mean(s$total_length_mm)
total_length_mean

#we'll use the approximate 2SE rule for the 95%CI
sem <- sd(s$total_length_mm)/sqrt(length(s$total_length_mm))
lower_mean95CI <- total_length_mean - 2*sem
upper_mean95CI <- total_length_mean + 2*sem
lower_mean95CI
upper_mean95CI

#4 - skip

#5 mammal body mass
m <- read.csv("DataForLabs/mammals.csv")

#5a plot distribution of body masses
ggplot(m,aes(x=body_mass_kg))+
  geom_histogram()+
  xlab("Body mass (kg)")+
  ylab("Frequency")+
  theme_classic()

#Distribution does not look normal. Most species have mass 
#close to 0 kg, with a few species with high mass (positive skew)

#5b log-transform and re-plot
m$LNbody_mass_kg <- log(m$body_mass_kg)
ggplot(m,aes(x=LNbody_mass_kg))+
  geom_histogram()+
  xlab("Body mass (ln kg)")+
  ylab("Frequency")+
  theme_classic()

#Natural log transformed data has an approximately normal distribution
#We'll learn more about this in chapter 13