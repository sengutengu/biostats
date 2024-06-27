# R Labs & Notes Ch. 10

#HW Q1 
mu = 3339
sigma = 573
n = 100

sdm = sigma/sqrt(n)
sdm

qnorm(0.8, mu, sdm, lower.tail=T)

# prob of 2215 or lower birth weight
pnorm(2215, mu, sigma, lower.tail=T)
# birth weight less than 90%
qnorm(0.1, mu, sigma, lower.tail=T)
# NOTE sigma NOT sdm. 
# Use sdm ONLY when it's about the sampling distro


muPhone <- 94.6
sigmaPhone <- 63.5
nPhone <- 125
sigmaPhone
nPhone
sdmPhone <- sigmaPhone/sqrt(nPhone)
sdmPhone

###### Ch 10 Lab & Notes

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors=T)

# reminder that we can make histograms
ggplot(titanicData, aes(x=age)) +
  geom_histogram(binwidth=10)

# we can just as well use hist()
hist(titanicData$age)

# QQ plots c an tell us whether a variable is normal
# should follow straight line
qqnorm(titanicData$age)
qqline(titanicData$age)
# close enough to count as normal!

# rnorm() generates a vector of numbers 
# all drawn randomly from a normal distro
normal_vector <- rnorm(n=100, mean=13, sd=4) # generate 20 numbers
qqnorm(normal_vector)
qqline(normal_vector)

# with non-normal data, we can still transform them
# to work with our tests

# one of the transformations is LOG-transformation
# can only use if all values > 0
# only improve fit if data is right-skewed
log(titanicData$age)
##################################################

###### Lab 8b: Probability under a normal curve 

# population male height w stdev
mu <- 177.6
sigma <- 9.7

# NASA's limits to male height are 157.5cm to 190.5cm
upperTail <- pnorm(190.5, mu, sigma, lower.tail=F)
lowerTail <- pnorm(157.5, mu, sigma, lower.tail=T)

totalProbability <- lowerTail + upperTail
totalProbability

##################################################

###### Lab 8c: Critical values for a normal distro

# 2 times stdev is not actually 2.5%
pnorm(2, 0, 1, lower.tail=F)
pnorm(1.96, 0, 1, lower.tail=F) # but 1.96 is!

# To calculate critical values (backwards),
qnorm(0.025, 0, 1, lower.tail=F)
qnorm(0.975, 0, 1, lower.tail=T)
# Q in qnorm is for quantile. 
# value of st normal distro with 30% to right
qnorm(0.3, 0, 1, lower.tail=F)

# what height is the 90th quartile?
qnorm(0.9, mu, sigma, lower.tail=T)
qnorm(0.1, mu, sigma, lower.tail=F)

# sampling distribution of mean SD given n=50
sdm <- sigma/sqrt(50)
sdm

# from random samples of n=50 from pop, 
# what is the mean height that is greater than 
# (more extreme than) the mean height of 90% of samples?
qnorm(0.9, mu, sdm, lower.tail=T)

# and increasing the sample size
# decreases uncertainty, makes distro narrower
# and the qnorm value decreases.
sdm <- sigma/sqrt(500)
qnorm(0.9, mu, sdm, lower.tail=T)

# Quiz 10

qnorm(0.9, 36.59, 0.43, lower.tail=T)
qnorm(0.1, 36.59, 0.43, lower.tail=F)
pnorm(36, 36.59, 0.43, lower.tail=T)

pnorm(36.3, 36.59, 0.43, lower.tail=F)-pnorm(37, 36.59, 0.43, lower.tail=F)

pnorm(37.5, 36.59, 0.43, lower.tail=F)

pnorm(515, 505.6, 9.3, lower.tail=F)
pnorm(485, 505.6, 9.3, lower.tail=T)

pnorm(500, 505.6, 9.3, lower.tail=F)-pnorm(510, 505.6, 9.3, lower.tail=F)
