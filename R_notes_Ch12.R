# Ch. 12 Labs & Notes

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors=T)
library(ggplot2)
library(car)

summary(titanicData)

# strip chart
ggplot(titanicData, aes(x=survive, y=age)) +
  geom_jitter(position=position_jitter(0.05)) +
  theme_minimal()

# multiple histograms
ggplot(titanicData, aes(x=age)) +
  geom_histogram() +
  facet_wrap(~ survive, ncol=1)
  # facet_wrap breaks up histogram accd to categorical var

# violin plots
ggplot(titanicData, aes(x=survive, y=age, fill=survive)) +
  geom_violin() +
  xlab("Survival") +
  ylab("Age") +
  theme_classic() +
  scale_fill_manual(values=c("gold", "darkred")) +
  stat_summary(fun.y=mean, geom="point", color="black")+
  theme(legend.position = "none") +
  theme(aspect.ratio=1)

####### t test 
# age by survival (are ages different pending on survival)
# var.equal=T tells R that we assume variances for two samples are equal
t.test(age ~ survive, data=titanicData, var.equal=T)
# 95% CI gives 95 CI for difference between means

####### Welch's t test 
# DOES NOT ASSUME EQUAL VARIANCE
# var.equal=F
t.test(age ~ survive, data=titanicData, var.equal=F)

####### Paired t-test
blackbird <- read.csv("DataForLabs/chap12e2BlackbirdTestosterone.csv", stringsAsFactors=T)
str(blackbird)
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant) # WELCH's t-test
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired=T) # PAIRED t-test

####### Levene's test
leveneTest(data=titanicData, age ~ survive, center=mean)
# p=0.04855 so reject the null that survived & died groups have same pop. variances.

##### R Lab 9b
# Paired t-test
# has greater power than regular 2-sample t-test
# Assume: random sampling, normal distro of differences
# (CLT only works for means)

tempData <- read.csv("DataForLabs/chap12q11OstrichTemp.csv", stringsAsFactors=T)
summary(tempData)
str(tempData)
tempData$diff <- tempData$bodyTemp - tempData$brainTemp
tempData$diff

library(ggplot2)

# Histogram
ggplot(tempData, aes(x=diff)) +
  geom_histogram(color="black", fill="darkred") +
  xlab("Body temp (ºC) – brain temp (ºC)") +
  ylab("Frequency") +
  theme_classic()

# box plot
ggplot(tempData, aes(x="", y=diff)) +
  geom_boxplot(width=0.25) +
  xlab("") +
  ylab("Body temp (ºC) – brain temp (ºC)") +
  theme_classic()

qqnorm(tempData$diff, datax=T)
qqline(tempData$diff, datax=T)
# Implies right skew. Let's test.

# Shapiro-Wilk Test
shapiro.test(tempData$diff)
# p=0.2275 so we fail to reject null
# that diff is normal

# Paired t-test
t.test(tempData$diff, mu=0)
# Can also do the following:
t.test(tempData$bodyTemp, tempData$brainTemp, paired=T)
# reversing order yields same output
# but with positive t statistic
t.test(tempData$brainTemp, tempData$bodyTemp, paired=T)

#### Lab 9c: Two-Sample t-test
# Do mosquitos prefer beer drinkers?
mosqData <- read.csv("DataForLabs/chap12q15BeerAndMosquitoes.csv", stringsAsFactors=T)
head(mosqData)
str(mosqData)
summary(mosqData)

# separated histograms
ggplot(mosqData, aes(x=change)) +
  geom_histogram(color="black", fill="darkred") +
  facet_wrap(~drink, ncol=1) +
  theme_classic()

# box plot
ggplot(mosqData, aes(x=drink, y=change))+
  geom_boxplot(width=0.25) +
  xlab("Treatment") +
  ylab("Change in proportion of mosquitos attracted") +
  theme_classic()

# strip plot
ggplot(mosqData, aes(x=drink, y=change)) +
  geom_jitter(width=0.25) +
  xlab("Treatment") +
  ylab("Change in proportion of mosquitos attracted") +
  theme_classic()


# normal quantile plot

library(dplyr)
qqnorm(filter(mosqData, drink=="beer")$change, datax=T)
qqline(filter(mosqData, drink=="beer")$change, datax=T)

qqnorm(filter(mosqData, drink=="water")$change, datax=T)
qqline(filter(mosqData, drink=="water")$change, datax=T)

# shapiro-wilk test of normality
# 2 sample t test requires assumption of normality to be
# satisfied in BOTH samples
shapiro.test(filter(mosqData, drink=="beer")$change)
shapiro.test(filter(mosqData, drink=="water")$change)

# tapply to do both shapiro-wilk tests in one line
tapply(mosqData$change, mosqData$drink, shapiro.test)

# Check for equal VARIANCE
# 1. are sample sizes above 30?
tapply(mosqData$change, mosqData$drink, length)
# 25 and 18 --- so NO!

# 2. then are standard deviations similar enough?
tapply(mosqData$change, mosqData$drink, sd)
# 1.28x diff --- kind of? Let's check.
# use LEVENE's TEST for SD Equivalence

library(car)
leveneTest(data=mosqData, change ~ drink, center="mean")
# same as
leveneTest(mosqData$change ~ mosqData$drink, center="mean")
# P > 0.05 so we're good. 

# Now we can run the t-test.
t.test(data=mosqData, change ~ drink, var.equal=T)
# p = 0.002 so we reject null. 

fishData <- read.csv("DataForLabs/chap12q08Cichlids.csv", stringsAsFactors=T)
str(fishData)

# Strip plot. Looks like diff variances
ggplot(fishData, aes(x=genotype, y=preference)) +
  geom_jitter(width=0.25) +
  xlab("Genotype") +
  ylab("Preference score") +
  theme_classic()

# test for normality
shapiro.test(filter(fishData, genotype=="F1")$preference)
shapiro.test(filter(fishData, genotype=="F2")$preference)
# both pass

# test for equal variance
# length of preferences based on genotype
# one is below 30
tapply(fishData$preference, fishData$genotype, length)
# SD - one is like 3x so probably not equal.
tapply(fishData$preference, fishData$genotype, sd)
# Levene's test to confirm.
leveneTest(data=fishData, preference ~ genotype, center="mean")
# p=0.015 so we cannot run a standard t test.

### INSTEAD run Welch's t-test! var.equal=F
t.test(data=fishData, preference~genotype, var.equal=F)


#### HW 12
pacesData <- read.csv("DataForLabs/PACES.csv", stringsAsFactors=T)
str(pacesData)
pacesData

library(ggplot2)

# visual inspection suggests diff variances
ggplot(pacesData, aes(x=Group, y=Score)) +
  geom_jitter(width=0.25) +
  theme_classic()

# Are the two pops normal? Yes.
tapply(pacesData$Score, pacesData$Group, shapiro.test)

# Are pop variances equal? 
# n=8 <<< 30
tapply(pacesData$Score, pacesData$Group, length)
# SD - one is like 3x so probably not equal.
tapply(pacesData$Score, pacesData$Group, sd)
# Levene's test to confirm. p=0.01105
leveneTest(data=pacesData, Score ~ Group, center="mean")

# So we have to use Welch's T-test
t.test(data=pacesData, Score~Group, var.equal=F)

d = -21.81201
sd = 213.53201
SEd = 38.35148
mu = 0
(d-mu)/SEd

miceData <- read.csv("DataForLabs/mice_nematode.csv", stringsAsFactors = T)
summary(miceData)

# visual suggests diff means, maybe diff var
ggplot(miceData, aes(x=group, y=nematodeBiomass)) +
  geom_jitter(width=0.25) +
  theme_classic()

# Are the two pops normal? yes.
tapply(miceData$nematodeBiomass, miceData$group, shapiro.test)

# Are the two pop variances equal? yes.
leveneTest(data=miceData, nematodeBiomass ~ group, center="mean")

t.test(data=miceData, nematodeBiomass~group, var.equal=T)

lianasData <- read.csv("DataForLabs/lianas.csv", stringsAsFactors = T)
summary(lianasData)
lianasData$diff <- lianasData$LianaAbundance2012-lianasData$LianaAbundanceEarly
summary(lianasData)

t.test(lianasData$diff, mu=0)
t.test(lianasData$LianaAbundance2012, lianasData$LianaAbundanceEarly, paired=T)


# Q1
liverData <- read.csv("DataForLabs/liver_tumor.csv", stringsAsFactors=T)
str(liverData)
liverData$diff <- liverData$tumor - liverData$healthy
str(liverData)

ggplot(liverData, aes(x=diff)) +
  geom_histogram(color="black", fill="darkred") +
  xlab("Body temp (ºC) – brain temp (ºC)") +
  ylab("Frequency") +
  theme_classic()

qqnorm(liverData$diff, datax=T)
qqline(liverData$diff, datax=T)

shapiro.test(liverData$diff)
t.test(liverData$diff, mu=0)
t.test(liverData$tumor, liverData$healthy, paired=T)

#Q2
install.packages("data.table")
library(data.table)
irisData <- read.csv("DataForLabs/iris_sepals.csv", stringsAsFactors = T)
head(irisData)
str(irisData)
irisFrame <- melt(irisData)
irisFrame

# both are normal
shapiro.test(irisData$versicolor)
shapiro.test(irisData$virginica)

# Are pop variances equal? 
# n=8 <<< 30
tapply(irisFrame$value, irisFrame$variable, length)
# SD - one is like 3x so probably not equal.
tapply(irisFrame$value, irisFrame$variable, sd)
# Levene's test to confirm. p=0.01105
leveneTest(data=irisFrame, value ~ variable, center="mean")

t.test(data=irisFrame, value ~ variable, var.equal=F)


beerData <- read.csv("DataForLabs/BeerGlassShape.csv", stringsAsFactors=T)
str(beerData)
head(beerData)

ggplot(beerData, aes(x=glassShape, y=drinkingMinutes)) +
  geom_jitter(width=0.25) +
  theme_classic()

tapply(beerData$drinkingMinutes, beerData$glassShape, shapiro.test)
tapply(beerData$drinkingMinutes, beerData$glassShape, length)
tapply(beerData$drinkingMinutes, beerData$glassShape, sd)
leveneTest(data=beerData, drinkingMinutes ~ glassShape, center="mean")

t.test(data=beerData, drinkingMinutes~glassShape, var.equal=T)

smokerData <- read.csv("DataForLabs/smoker_age.csv", stringsAsFactors=T)
str(smokerData)
summary(smokerData)

ggplot(smokerData, aes(x=smokingStatus, y=ageStart)) +
  geom_jitter(width=0.25) +
  theme_classic()

tapply(smokerData$ageStart, smokerData$smokingStatus, mean)

# both normal
tapply(smokerData$ageStart, smokerData$smokingStatus, shapiro.test)

# lengths
tapply(smokerData$ageStart, smokerData$smokingStatus, length)
# sd
tapply(smokerData$ageStart, smokerData$smokingStatus, sd)

# SDs are different. 
leveneTest(data=smokerData, ageStart ~ smokingStatus, center="mean")

t.test(data=smokerData, ageStart~smokingStatus, var.equal=F)

bacteriaData <- read.csv("DataForLabs/bacteria_ZOI.csv", stringsAsFactors=T)
str(bacteriaData)
summary(bacteriaData)

ggplot(bacteriaData, aes(x=treatment, y=ZOI)) +
  geom_jitter(width=0.25) +
  theme_classic()

tapply(bacteriaData$ZOI, bacteriaData$treatment, mean)

# both normal
tapply(bacteriaData$ZOI, bacteriaData$treatment, shapiro.test)

# lengths
tapply(bacteriaData$ZOI, bacteriaData$treatment, length)

# sd
tapply(bacteriaData$ZOI, bacteriaData$treatment, sd)

# SDs are different. 
leveneTest(data=bacteriaData, ZOI ~ treatment, center="mean")

t.test(data=bacteriaData, ZOI~treatment, var.equal=F)

((9*2) + (14*3))/(9+14)
((14*3)+(18*2))/(14+18)

