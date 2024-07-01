#### Practice Questions Rlab 9 ####

library(ggplot2)
library(car)

### Question 1 ###
# Natural selection in house sparrows

## a. Plot histograms for surviving and nonsurviving groups
sparrow <- read.csv("DataForLabs/bumpus.csv")
ggplot(sparrow,aes(x=weight_g))+geom_histogram()+facet_wrap(~survival,ncol=1)

# tough to decipher normality with small sample size, but both samples appear somewhat normal
# variance in the survived group seems lower

## b. Welch's t-test
t.test(sparrow$weight_g~sparrow$survival,var.equal=F)

# P-value of 0.01 is below alpha of 0.05; reject null hypothesis of equal means

## c. 95%CI of difference in mean

# from output of Welch's test: 0.1351376 1.1339597
# does not include zero, thus consistent with different means (weight in survived is lower based on output)

## d. Levene's test
leveneTest(sparrow$weight_g~sparrow$survival,center=mean)

# P-value of 0.37 is above alpha of 0.05; fail to reject equal variance


### Question 2 ###
# ecological footprint of countries

## a. plot histogram of difference in ecological footprint: 2000-2012
c <- read.csv("DataForLabs/countries.csv")
# make new column/vector of differences
c$footprintDiff <- c$ecological_footprint_2000-c$ecological_footprint_2012
# plot with hist()
hist(c$footprintDiff,xlab="Ecological footprint difference (2000-2012)",main="")
# plot with geom_histogram()
ggplot(c,aes(x=footprintDiff))+geom_histogram(binwidth = 1)

## b. paired t-test for change in ecological footprint
# run as paired t-test
t.test(c$ecological_footprint_2000,c$ecological_footprint_2012,paired=T)
# run on vector of differences as one-sample t-test
t.test(c$footprintDiff,mu=0)

# tests give same answer

## c. interpret result

# both get same answer with P-value of 0.008 below alpha of 0.05
# thus reject null of equal footprints in 2000 and 2012
# check to see which group is higher
# paired test syntax t.test(group1,group2,paired=T), which does group1-group2; so positive difference means 2000 > 2012
# when we made vector footprintDiff above we did 2000-2012, so positive mean translates to 2000>2012


### Question 3 ###
# Shaving and hair growth, subjects shaved hair on one leg (test), but not the other (control)

## a. Perform suitable hypothesis

# Measurements for test and control taken from same individual so we know to run a paired t-test
leg <- read.csv("DataForLabs/leg shaving.csv")

# calculate difference in hair thickness of samples
leg$diff <- leg$hair_width_change_control-leg$hair_width_change_test

# run Shapiro-Wilk test to see if data is normally distributed
shapiro.test(leg$diff)

# P-value is above alpha of 0.05; fail to reject null hypothesis that our sample of differences has a normal distribution

# there are two ways to run the paired t-test
t.test(leg$diff,mu=0)
t.test(leg$hair_width_change_control,leg$hair_width_change_test,paired=T)

# tests give same answer with P-value of 0.130; fail to reject null of equal thickness

## b. How big is the difference?

# from test output: 95% confidence interval of mean difference is (-4.023, 21.626)
# consistent with slight decrease, no difference, and moderate increase in thickness


### Question 4 ###
# 2D:4D ratio

# a. Make appropriate graph comparing male and female 2D:4D ratios

# We know to run a two-sample t-test because this cannot be a paired design experiment since individuals cannot be both male and female
finger <- read.csv("DataForLabs/fingerLengths.csv")
# insert a column for 2D:4D ratio
finger$ratio <- finger$digit2/finger$digit4

# use graph to evaluate normality
ggplot(finger,aes(x=ratio)) + geom_histogram(color="black",fill="darkred") + facet_wrap(~sex,ncol=1) + theme_classic()
# both distributions appear to be normally distributed

#$ b. Test for a difference in means between males and females

# formally test for normal distribution for males and females; syntax #1
shapiro.test(filter(finger,sex=="M")$ratio)
shapiro.test(filter(finger,sex=="F")$ratio)

# formally test for normal distribution for males and females; syntax #2
tapply(finger$ratio,finger$sex,shapiro.test)

# P-value is 0.2774 (male) and 0.344 respectively (female),
# so we fail to reject the null hypothesis that ratio is normally distributed in both groups

# use Levene's test to test if male and female groups have equal variance
library(car)
leveneTest(data=finger,ratio~sex,center=mean)

# P-value is 0.73; fail to reject null of equal variances

# run two-sample t-test
t.test(ratio~sex, data=finger, var.equal=T)

# P-value is 0.162, so we fail to reject null hypothesis that there is no difference in means between males and females for 2D:4D ratio

## c. What is the difference in magnitude

# confidence interval is (-0.00434, 0.0256), which includes zero (= no difference)
# magnitude below zero (higher mean in males) and above zero (larger mean in females) are both small
