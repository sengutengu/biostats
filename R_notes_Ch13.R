# R Notes Ch. 13
# R Lab 9d

# Assumptions of t tests:

# One sample t test:
# - Data are random sample from population
# - Variable is normally distributed in population

# Paired t test:
# - Sampling units are randomly sampled from population
# - Paired differences are normally distributed in population

# Two-sample t test:
# - Both samples are random samples from their populations
# - Variable is normally distributed in each population
# - SD/Variance is the same is both populations

marineData <- read.csv("DataForLabs/chap13e1MarineReserve.csv", stringsAsFactors=T)
str(marineData)
summary(marineData)

library(ggplot2)
# Got a right skew. Doesn't look very normal.
ggplot(marineData, aes(x=biomassRatio)) +
  geom_histogram(color="black", fill="darkred") +
  theme_classic()

library(dplyr)
# Obviously not normal.
qqnorm(marineData$biomassRatio, datax=T)
qqline(marineData$biomassRatio, datax=T)

# Shapiro test gives us p=8.851e-05!
# So it's NOT normal!
shapiro.test(marineData$biomassRatio)

# DATA TRANSFORMATION!
marineData$lnBiomassRatio <- log(marineData$biomassRatio)
ggplot(marineData, aes(x=lnBiomassRatio)) +
  geom_histogram(color="black", fill="darkred") +
  theme_classic()

# much better... But just barely. 
shapiro.test(marineData$lnBiomassRatio)

# Remind ourselves... This is ratio data.
# The null is that the ratio is 1, that there is no diff between the biomasses
# between protected and comparable unprotected areas.
# But the natural log of 1 is 0, so we have to change the null to 0.
t.test(marineData$lnBiomassRatio, mu=0)

# We obtain a significant p value.
# We reject the null hypothesis that the mean ln ratio is 0.

# BACKTRANSFORMATION
t.test(marineData$lnBiomassRatio, mu=0)$conf.int

exp(t.test(marineData$lnBiomassRatio, mu=0)$conf.int)
# 95% CI is consistent with protected areas having 1.4-1.8x the biomass

##### NONPARAMETRIC SIGN TEST
## WHEN ASSUMPTION OF NORMALITY IS VIOLATED

# Insect sexual conflict. Paired design
conflictData <- read.csv("DataForLabs/chap13e4SexualConflict.csv", stringsAsFactors = T)
str(conflictData)
summary(conflictData)

# wowee! a huge outlier. Likely not normal. Let's check with shapiro-wilk.
ggplot(conflictData, aes(x=difference)) +
  geom_histogram(color="black", fill="darkred") +
  theme_classic()

# p=2.911e-10. Very very not normal.
shapiro.test(conflictData$difference)
# log transformation won't help us either 
# since we can't take the log of a negative number.
# so we have to use a sign test. 

# sample size = 25
length(conflictData$difference)

library(dplyr)
positive <- filter(conflictData, difference > 0)
# number of positive values = 18
length(positive)
# we can also do a one liner
length(filter(conflictData, difference > 0)$difference)
# and we have no datum equal to 0. If we had one, we'd remove it
length(filter(conflictData, difference == 0)$difference)

# What is the probability of seeing 18 out of 25 positive?
binom.test(18, 25, 0.5)
# significant result. p=0.04
# Multiple-mated groups have more species.

# We can also try transforming these data.
# first, let's make ratios so we can do natural log.
conflictData$ratio <- conflictData$nSpeciesMultipleMating/conflictData$nSpeciesSingleMating
# still not normal.
shapiro.test(conflictData$ratio)

# still looks pretty bad.
ggplot(data=conflictData, aes(x=ratio)) +
  geom_histogram(color="black", fill="darkred") +
  theme_classic()

# Let's log transform the ratio...
conflictData$lnRatio <- log(conflictData$ratio)

# it's now normal.
shapiro.test(conflictData$lnRatio)
ggplot(data=conflictData, aes(x=lnRatio)) +
  geom_histogram(color="black", fill="darkred") +
  theme_classic()

# again, mu has to be 0 since log(1)=0
t.test(conflictData$lnRatio, mu=0)
# P value here is lower, because t test has more power than sign test!

##### NONPARAMETRIC MANN-WHITNEY U-TEST
### ASSUMPTION OF NORMALITY VIOLATED
### IN A TWO-SAMPLE DESIGN

# starving and fed female crickets. 
# compare time to mating
cricketData <- read.csv("DataForLabs/chap13e5SagebrushCrickets.csv", stringsAsFactors = T)
str(cricketData)
summary(cricketData)

# Does not look normal at all!
ggplot(cricketData, aes(x=timeToMating)) +
  geom_histogram(color="black", fill="darkred") +
  facet_wrap(~feedingStatus, ncol=1) +
  theme_classic()

# shapiro test timeToMating, separatead by feedingStatus
# both are sig, so not normal 
tapply(cricketData$timeToMating, cricketData$feedingStatus, shapiro.test)

# log transform time to mating since no val is negative
cricketData$lnTimeToMating <- log(cricketData$timeToMating)
tapply(cricketData$lnTimeToMating, cricketData$feedingStatus, shapiro.test)
# while the starved group is normal, 
# the fed group STILL is NOT NORMAL!
# === CANNOT RUN t-TEST! DO U-TEST INSTEAD

wilcox.test(cricketData$timeToMating ~ cricketData$feedingStatus)
# fail to reject null. There is no difference between the two samples.


############# HW 

# Q2

sexData <- read.csv("DataForLabs/SportsVsBiology.csv", stringsAsFactors=T)
summary(sexData)

ggplot(sexData, aes(x=numberSexPartners)) +
  geom_histogram(color="black", fill="darkred") +
  facet_wrap(~major, ncol=1) +
  theme_classic()

length(filter(sexData, major=="biology")$major)
wilcox.test(sexData$numberSexPartners ~ sexData$major)

# Q3
hayfever <- read.csv("DataForLabs/hayfever.csv", stringsAsFactors=T)
str(hayfever)
ggplot(hayfever, aes(x=Age)) +
  geom_histogram(color="black", fill="darkred")
shapiro.test(hayfever$Age)

# Q4, 7
finchData <- read.csv("DataForLabs/gouldian_finch.csv", stringsAsFactors=T)
finchData$difference <- finchData$incompatible-finchData$compatible
str(finchData)
summary(finchData)

finchData$difference
qqnorm(finchData$difference, datax=T)
qqline(finchData$difference, datax=T)

library(dplyr)
positive <- filter(finchData, difference > 0)
# number of positive values = 42
length(positive$difference)
# we can also do a one liner
length(filter(finchData, difference > 0)$difference)
# and we have no datum equal to 0. If we had one, we'd remove it
length(filter(finchData, difference == 0)$difference)

# What is the probability of seeing 18 out of 25 positive?
binom.test(42, 43, 0.5)

# Q5

smokerData <- read.csv("DataForLabs/BMI_smokers.csv", stringsAsFactors=T)

ggplot(smokerData, aes(x=BMICalc)) +
  geom_histogram(color="black", fill="darkred")+
  theme_classic()
smokerData$lnBMICalc <- log(smokerData$BMICalc)
summary(smokerData)

shapiro.test(smokerData$lnBMICalc)
# back-transformed 95% CI 
exp(t.test(smokerData$lnBMICalc, mu=25)$conf.int)

# Q6

sexData <- read.csv("DataForLabs/SportsVsBiology.csv", stringsAsFactors = T)
summary(sexData)

#### QUIZ

# Q1
beetleData <- read.csv("DataForLabs/beetle_orientation.csv", stringsAsFactors = T)
beetleData$lnOrientationError <- log(beetleData$OrientationError)
summary(beetleData)
ggplot(beetleData, aes(x=OrientationError)) +
  geom_histogram() +
  facet_wrap(~Condition, ncol=1) +
  theme_classic()

# normal quantile plot
qqnorm(filter(beetleData, Condition=="Equal_Intensity")$OrientationError, datax=T)
qqline(filter(beetleData, Condition=="Equal_Intensity")$OrientationError, datax=T)
  
tapply(beetleData$OrientationError, beetleData$Condition, shapiro.test)

# log values are normal
tapply(beetleData$lnOrientationError, beetleData$Condition, shapiro.test)
ggplot(beetleData, aes(x=lnOrientationError)) +
  geom_histogram() +
  facet_wrap(~Condition, ncol=1) +
  theme_classic()
library(car)

# test for variance equivalence
leveneTest(data=beetleData, lnOrientationError~Condition, center=mean)
sd(filter(beetleData, Condition=="Equal_Intensity")$lnOrientationError)
sd(filter(beetleData, Condition=="Milky_Way")$lnOrientationError)

t.test(data=beetleData, lnOrientationError~Condition, var.equal=F)

# Q3
stressData <- read.csv("DataForLabs/stress.csv", stringsAsFactors=T)
summary(stressData)
stressData$ratio
stressData$difference <- stressData$after-stressData$before
length(stressData$ratio)
ggplot(stressData, aes(x=ratio)) +
  geom_histogram() +
  theme_classic()

# fails shapiro test
shapiro.test(stressData$ratio)

stressData$lnRatio <- log(stressData$ratio)
# fails again. 
shapiro.test(stressData$lnRatio)

# Need to do sign test.
length(stressData$ratio)
length(filter(stressData, ratio > 1)$ratio)
length(filter(stressData, ratio == 1)$ratio)

length(filter(stressData, difference > 0)$difference)
binom.test(26, 40, 0.5)

# Q4

miceData <- read.csv("DataForLabs/GutFloraAutoimmune.csv", stringsAsFactors = T)
summary(miceData)

ggplot(miceData, aes(x=lnPercent)) +
  geom_histogram() +
  facet_wrap(~treatment, ncol=1) +
  theme_classic()

tapply(miceData$percentInterleukin17, miceData$treatment, shapiro.test)
miceData$lnPercent <- log(miceData$percentInterleukin17)
# both are normal now
tapply(miceData$lnPercent, miceData$treatment, shapiro.test)

leveneTest(data=miceData, lnPercent~treatment, center=mean)
t.test(data=miceData, lnPercent~treatment, var.equal=T)

# Q5 

hunterData <- read.csv("DataForLabs/hunter.csv", stringsAsFactors=T)
summary(hunterData)

# two sample design
ggplot(hunterData, aes(x=rotations)) +
  geom_histogram() +
  facet_wrap(~treatment, ncol=1) +
  theme_classic()

tapply(hunterData$rotations, hunterData$treatment, shapiro.test)
hunterData$lnRotations <- log(hunterData$rotations)
# now the other group is nonnormal!
tapply(hunterData$lnRotations, hunterData$treatment, shapiro.test)

wilcox.test(hunterData$rotations~hunterData$treatment)

# Q6
eggData <- read.csv("DataForLabs/goldeneye_eggs.csv", stringsAsFactors=T)

summary(eggData)
shapiro.test(eggData$parasitismFirstIndex)
t.test(eggData$parasitismFirstIndex, mu=0)

length(filter(eggData, parasitismFirstIndex > 0)$parasitismFirstIndex)
length(filter(eggData, parasitismFirstIndex == 0)$parasitismFirstIndex)
length(eggData$parasitismFirstIndex)
ggplot(eggData, aes(x=parasitismFirstIndex)) +
  geom_histogram() +
  theme_classic()
binom.test(1, 13, 0.5)

# Q7

distanceData <- read.csv("DataForLabs/home_distance.csv", stringsAsFactors=T)
summary(distanceData)
distanceData
shapiro.test(distanceData$homeDistance)
distanceData$lnDistance <- log(distanceData$homeDistance)
summary(distanceData)
shapiro.test(distanceData$lnDistance)
exp(t.test(distanceData$lnDistance, mu=0)$conf.int)


# Exam

ataxiaData <- read.csv("DataForLabs/SpinocerebellarAtaxia.csv", stringsAsFactors=T)
summary(ataxiaData)

# two sample design
ggplot(ataxiaData, aes(x=lifespan)) +
  geom_histogram() +
  facet_wrap(~treatment, ncol=1) +
  theme_classic()

tapply(ataxiaData$lifespan, ataxiaData$treatment, shapiro.test)
leveneTest(data=ataxiaData, lifespan~treatment, center=mean)
t.test(data=ataxiaData, lifespan~treatment, var.equal=T)

zebrafishData <- read.csv("DataForLabs/zebrafish_jawjoint.csv", stringsAsFactors = T)
summary(zebrafishData)

# two sample design
ggplot(zebrafishData, aes(x=jawjoint_vol)) +
  geom_histogram() +
  facet_wrap(~group, ncol=1) +
  theme_classic()

# doesn't look so normal.
qqnorm(filter(zebrafishData, group=="hypo")$jawjoint_vol, datax=T)
qqline(filter(zebrafishData, group=="hypo")$jawjoint_vol, datax=T)
qqnorm(filter(zebrafishData, group=="wt")$jawjoint_vol, datax=T)
qqline(filter(zebrafishData, group=="wt")$jawjoint_vol, datax=T)

# both are non-normal.
tapply(zebrafishData$jawjoint_vol, zebrafishData$group, shapiro.test)

zebrafishData$ln_jawjoint_vol <- log(zebrafishData$jawjoint_vol)
# log makes data normal
tapply(zebrafishData$ln_jawjoint_vol, zebrafishData$group, shapiro.test)
# variance is good
leveneTest(data=zebrafishData, ln_jawjoint_vol~group, center=mean)

t.test(data=zebrafishData, ln_jawjoint_vol~group, var.equal=T)

crabData <- read.csv("DataForLabs/crab_switches.csv", stringsAsFactors = T)
summary(crabData)

# two sample design. not so normal
ggplot(crabData, aes(x=switches)) +
  geom_histogram() +
  facet_wrap(~treatment, ncol=1) +
  theme_classic()

qqnorm(filter(crabData, treatment=="group")$switches, datax=T)
qqline(filter(crabData, treatment=="group")$switches, datax=T)

qqnorm(filter(crabData, treatment=="solitary")$switches, datax=T)
qqline(filter(crabData, treatment=="solitary")$switches, datax=T)

tapply(crabData$switches, crabData$treatment, shapiro.test)
# equal variance
leveneTest(data=crabData, switches~treatment, center=mean)

t.test(data=crabData, switches~treatment, var.equal=T)

larvalData <- read.csv("DataForLabs/larval_speed.csv", stringsAsFactors=T)
summary(larvalData)

# seems about equal, kind of normal.
ggplot(larvalData, aes(x=speed)) +
  geom_histogram() +
  facet_wrap(~mutant, ncol=1) +
  theme_classic()

# all normal
tapply(larvalData$speed, larvalData$mutant, shapiro.test)

# variances not equal but ss >30 and sd <10x
leveneTest(data=larvalData, speed~mutant, center=mean)
sd(filter(larvalData, mutant=="bocks")$speed)
sd(filter(larvalData, mutant=="koi")$speed)
sd(filter(larvalData, mutant=="ote")$speed)

larvalModel <- lm(data=larvalData, speed~mutant)
summary(larvalModel)
anova(larvalModel)

indigoMassData <- read.csv("DataForLabs/indigobird_mass.csv", stringsAsFactors=T)
summary(indigoMassData)
tapply(indigoMassData$mass, indigoMassData$species, shapiro.test)
leveneTest(data=indigoMassData, mass~species, center=mean)
indigoMassModel <- lm(data=indigoMassData, mass~species)
anova(indigoMassModel)

TukeyHSD(aov(indigoMassModel))

bodyData <- read.csv("DataForLabs/elevation_bodysize.csv", stringsAsFactors=T)
