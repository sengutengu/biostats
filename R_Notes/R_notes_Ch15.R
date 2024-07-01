# R Lab & Notes Ch. 15

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors=T)

library(ggplot2)
# The data look pretty normal 
# with similar spreads.
ggplot(titanicData, aes(x=age)) +
  geom_histogram() +
  facet_wrap(~passenger_class, ncol=1)

library(dplyr)
# Let's confirm visual inspection
titanic_by_passenger_class <- group_by(titanicData, passenger_class)
# group means
summarise(titanic_by_passenger_class, group_mean=mean(age, na.rm=T))
# add stdev
summarise(titanic_by_passenger_class, group_mean=mean(age, na.rm=T), group_sd=sd(age, na.rm=T))

# ANOVA is not as simple as we hope
# 1. fit ANOVA to data using lm()
# 2. do ANOVE calcs and put in table using anova()

titanicANOVA <- lm(age ~ passenger_class, data=titanicData)
anova(titanicANOVA)

# Tukey-Kramer test allows us to 
# test all pairs of groups
TukeyHSD(aov(titanicANOVA))

# Kruskal-Wallis test is 
# non-parametric analog of 1-way ANOVA.
# DOES NOT ASSUME that the var has normal distro

kruskal.test(age~passenger_class, data=titanicData)

# Q1 Cuckoo
cuckooEggs <- read.csv("DataForLabs/cuckooeggs.csv", stringsAsFactors=T)
str(cuckooEggs)
ggplot(cuckooEggs, aes(x=egg_length)) +
  geom_histogram(color="black", fill="darkred") +
  facet_wrap(~host_species, ncol=1)

cuckoo_by_species <- group_by(cuckooEggs, host_species)
summarize(cuckoo_by_species, group_mean=mean(egg_length), group_sd=sd(egg_length))
# meadow pipit doesn't pass normality
tapply(cuckooEggs$egg_length, cuckooEggs$host_species, shapiro.test)
# log transform
cuckooEggs$lnEggLength <- log(cuckooEggs$egg_length)
# meadow pipit still doesn't pass
tapply(cuckooEggs$lnEggLength, cuckooEggs$host_species, shapiro.test)
library(car)
# homogeneous variance
leveneTest(data=cuckooEggs, egg_length~host_species, center=mean)

# we have to run a kruskal-wallis test. 
kruskal.test(cuckooEggs$egg_length, cuckooEggs$host_species)
library(FSA)
dunnTest(cuckooEggs$egg_length, cuckooEggs$host_species)


# Lab 10B

indigoData <- read.csv("DataForLabs/indigobirds_reflectance.csv", stringsAsFactors=T)
str(indigoData)
# somewhat similar in mean, sd, and n
tapply(indigoData$hue, indigoData$species, mean)
tapply(indigoData$hue, indigoData$species, sd)
tapply(indigoData$hue, indigoData$species, length)

library(ggplot2)
# looks more or less normal
ggplot(indigoData, aes(x=hue)) +
  geom_histogram(color='black', fill='darkred') +
  facet_wrap(~species, ncol=1) +
  xlab("Feather hue") +
  ylab("Frequency") +
  theme_classic()

# all are normal!
tapply(indigoData$hue, indigoData$species, shapiro.test)

# ANOVA is robust to departures from
# equal SD, up to 10x
library(car)

# equal variance
leveneTest(indigoData$hue ~ indigoData$species, center = mean)

# Reject null!
indigoAnova <- lm(data=indigoData, hue~species)
anova(indigoAnova)

# R squared = total variance explained by group differences
summary(indigoAnova)$r.squared

# post hoc comparison
TukeyHSD(aov(indigoAnova))

tapply(indigoData$hue, indigoData$species, max) + 10
# plotting data
ggplot(indigoData, aes(x=species, y=hue)) +
  geom_jitter(pch=1, color="darkred", width=0.25) +
  xlab("Species") +
  ylab("Feather hue (nm)") +
  theme_classic() +
  annotate(geom="text", x=1, y=500, label="a") +
  annotate(geom="text", x=2, y=496, label="a") +
  annotate(geom="text", x=3, y=444, label="b") +
  annotate(geom="text", x=4, y=421, label="c") 


# R Lab 10C NONPARAMETRIC ALTERNATIVE TO ANOVA
# ANOVA assumes that:
# 1. each population has normal distro
# 2. all populations have same variance

PLP1data <- read.csv("DataForLabs/chap15q07DisordersAndGeneExpression.csv", stringsAsFactors=T)
str(PLP1data)
summary(PLP1data)
# refactor to put control first.
PLP1data$group <- factor(PLP1data$group, levels=c("control", "bipolar", "schizo"))
summary(PLP1data)

# check each group normality
# bipolar is not normal
tapply(PLP1data$PLP1Expression, PLP1data$group, shapiro.test)

# transforming to log produces NaNs 
# because you can't take logs of negative vals
PLP1data$lnPLP1Expression <- log(PLP1data$PLP1Expression)

# WE CAN ONLY DO KRUSKAL-WALLIS TEST!
# p is significant. so null is rejected
kruskal.test(PLP1data$PLP1Expression ~ PLP1data$group)

# post hoc with Dunn's test
library(FSA)
dunnTest(PLP1data$PLP1Expression ~ PLP1data$group, method="bonferroni")

ggplot(PLP1data, aes(x=group, y=PLP1Expression)) +
  geom_jitter(width=0.2) +
  theme_classic() +
  xlab("Group") +
  ylab("PLP1 Expression (normalized units)") +
  annotate(geom="text", x=1, y=0.55, label="a") +
  annotate(geom="text", x=2, y=0.15, label="b") +
  annotate(geom="text", x=3, y=0.3, label="b")


# HW 
# Q1

striderData <- read.csv("DataForLabs/water_strider.csv", stringsAsFactors = T)
summary(striderData)
tapply(striderData$yaw, striderData$age, shapiro.test)
tapply(striderData$yaw, striderData$age, sd)
leveneTest(data=striderData, yaw~age, center=mean)

striderModel <- lm(striderData$yaw ~ striderData$age)
anova(striderModel)
TukeyHSD(aov(striderModel))

hispanicData <- read.csv("DataForLabs/Hispanic_Weights.csv", stringsAsFactors=T)

summary(hispanicData)
tapply(hispanicData$Weight, hispanicData$Region, shapiro.test)
hispanicData$lnWeight <- log(hispanicData$Weight)
# still doesn't pass shapiro wilk
tapply(hispanicData$lnWeight, hispanicData$Region, shapiro.test)

kruskal.test(hispanicData$Weight~hispanicData$Region)


10.809912/1.534706


# Quiz 
# Q1 
beetleData <- read.csv("DataForLabs/female_lifespan.csv", stringsAsFactors=T)
summary(beetleData)
tapply(beetleData$total_offspring, beetleData$treatment, shapiro.test)
beetleData$lnTotalOffspring <- log(beetleData$total_offspring)
tapply(beetleData$lnTotalOffspring, beetleData$treatment, shapiro.test)
kruskal.test(beetleData$total_offspring~beetleData$treatment)

# Q2 
crabData <- read.csv("DataForLabs/fiddler_crabs.csv", stringsAsFactors=T)
summary(crabData)
crabModel <- lm(crabData$bodyTemperature~crabData$crabType)
anova(crabModel)
summary(crabModel)$r.squared

# Q7

coneData <- read.csv("DataForLabs/cone_size.csv", stringsAsFactors=T)
coneModel <- lm(coneData$conemass~coneData$habitat)
anova(coneModel)
TukeyHSD(aov(coneModel))

# Exam

# Lab 10B

alcoholData <- read.csv("DataForLabs/alcohol_BMI.csv", stringsAsFactors=T)
alcoholData$alcohol <- factor(alcoholData$alcohol, levels=c("non", "light", "heavy", "very heavy"))
summary(alcoholData)
library(ggplot2)
# looks more or less normal
ggplot(alcoholData, aes(x=BMI)) +
  geom_histogram(color='black', fill='darkred') +
  facet_wrap(~alcohol, ncol=1) +
  theme_classic()

# Reject null!
alcoholRegression <- lm(data=alcoholData, BMI~alcohol)
anova(alcoholRegression)

# post hoc comparison
TukeyHSD(aov(alcoholRegression))

tapply(alcoholData$BMI, alcoholData$alcohol, max) + 2
# plotting data
ggplot(alcoholData, aes(x=alcohol, y=BMI)) +
  geom_jitter(pch=1, color="darkred", width=0.25) +
  xlab("Alcohol Consumption") +
  ylab("BMI") +
  theme_classic() +
  annotate(geom="text", x=1, y=32, label="a") +
  annotate(geom="text", x=2, y=32, label="a") +
  annotate(geom="text", x=3, y=33, label="a") +
  annotate(geom="text", x=4, y=30, label="a") 
