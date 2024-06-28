#R Lab & Notes Ch. 17
guppyData <- read.csv("DataForLabs/chap02e3bGuppyFatherSonAttractiveness.csv", stringsAsFactors=T)

# slope is 0.982, intercept is 0.005
guppyRegression <- lm(data=guppyData, sonAttractiveness ~ fatherOrnamentation)
guppyRegression

# p value for null (slope=0) is 6.78e-05
# super reject
summary(guppyRegression)

ggplot(guppyData, aes(x=fatherOrnamentation, y=sonAttractiveness))+
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness") +
  geom_smooth(method=lm) # line & 95% conf bands

# to remove 95% conf bands, add se=F
# as in geom_smooth(method=lm, se=F)

# Residual plots
residuals(guppyRegression)

# no major deviation from lin reg assumptions
plot(data=guppyData, residuals(guppyRegression) ~ fatherOrnamentation)
abline(h=0) # add horizontal line at 0 for baseline

# R Lab 11b

lionData <- read.csv("DataForLabs/chap17e1LionNoses.csv", stringsAsFactors=T)
str(lionData)

# data exploration
library(ggplot2)

# shows positive correlation
ggplot(lionData, aes(x=proportionBlack, y=ageInYears)) +
  geom_point() +
  xlab("Proportion of black") +
  ylab("Age (years)")+
  theme_classic()

lionRegression <- lm(data=lionData, ageInYears ~ proportionBlack)
lionRegression
# slope of least squares line is 10.647
# for every 0.1 increase in black, 1.0647 year increase in age

# hypothesis test
# first row gives p(a=0). fail to reject
# second row gives p(b=0). reject
# positive correlation!
summary(lionRegression)

# add regression line
ggplot(lionData, aes(x=proportionBlack, y=ageInYears)) +
  geom_point() +
  xlab("Proportion of black") +
  ylab("Age (years)")+
  theme_classic() +
  geom_smooth(method="lm", se=T)

# PREDICT Y VALUES!
predict(lionRegression, data.frame(proportionBlack=0.7))

# Qualitative check of assumptions using residuals plot
ggplot(lionRegression, aes(x=proportionBlack, y=.resid)) +
  geom_point() +
  xlab("Proportion black") +
  ylab("Residuals") +
  geom_hline(yintercept = 0) +
  theme_classic()


# HW

grassData <- read.csv("DataForLabs/grasses.csv", stringsAsFactors=T)
summary(grassData)
grassRegression <- lm(data=grassData, chaparral ~ non_native_grass)
summary(grassRegression)
predict(grassRegression, data.frame(non_native_grass=60))

ankleData <- read.csv("DataForLabs/ankle_motion.csv", stringsAsFactors = T)
summary(ankleData)
ankleRegression <- lm(data=ankleData, footwear ~ barefoot)
summary(ankleRegression)

# Quiz

mammalData <- read.csv("DataForLabs/mammal_bmr.csv", stringsAsFactors=T)
mammalData$lnMass <- log(mammalData$mass)
mammalData$lnBMR <- log(mammalData$bmr)
summary(mammalData)
ggplot(mammalData, aes(x=lnMass, y=lnBMR)) +
  geom_point()

mammalRegression <- lm(data=mammalData, lnBMR ~ lnMass)
summary(mammalRegression)

ggplot(mammalRegression, aes(x=lnBMR, y=.resid)) +
  geom_point() +
  xlab("ln(basal metabolic rate)") +
  ylab("Residuals") +
  geom_hline(yintercept = 0) +
  theme_classic()

popData <- read.csv("DataForLabs/popsize_CVDIR.csv", stringsAsFactors=T)
summary(popData)
ggplot(popData, aes(x=POP, y=CVDIR)) +
  geom_point() +
  xlab("Population (in millions)") +
  ylab("Cardiovascular disease incidence rate (CVDIR)")+
  geom_smooth(method="lm", se=T) +
  theme_classic()
popRegression <- lm(data=popData, CVDIR ~ POP)
summary(popRegression)
predict(popRegression, data.frame(POP=30))

plantData <- read.csv("DataForLabs/nutrients_plants.csv", stringsAsFactors=T)
summary(plantData)
ggplot(plantData, aes(x=nutrients, y=species)) +
  geom_point()

plantRegression <- lm(data=plantData, species ~ nutrients)
summary(plantRegression)
anova(plantRegression)
predict(plantRegression, data.frame(nutrients=3))

# Exam

bodyData <- read.csv("DataForLabs/elevation_bodysize.csv", stringsAsFactors=T)
summary(bodyData)
ggplot(bodyData, aes(x=elevation, y=bodyPC1)) +
  geom_point() +
  geom_smooth(method=lm)
bodyRegression <- lm(data=bodyData, bodyPC1~elevation)
bodyRegression
summary(bodyRegression)
plot(data=bodyData, residuals(bodyRegression) ~ elevation)
abline(h=0) # add horizontal line at 0 for baseline

antData <- read.csv("DataForLabs/ant_policing.csv", stringsAsFactors=T)
summary(antData)
str(antData)
ggplot(antData, aes(x=relatedness, y=policing)) +
  geom_point() +
  xlab("Relative relatedness of workers to queen") +
  ylab("Policing behavior of workers")+
  ggtitle("Kinship to queen and peer policing across ant species")+
  geom_smooth(method=lm) +
  theme_classic()

antRegression <- lm(data=antData, policing~relatedness)
antRegression
summary(antRegression)
cor.test(antData$policing, antData$relatedness)

proteinData <- read.csv("DataForLabs/protein_walking.csv", stringsAsFactors = T)
summary(proteinData)
ggplot(proteinData, aes(x=protein, y=sixMWD)) +
  geom_point() +
  geom_smooth(method=lm)

proteinRegression <- lm(data=proteinData, sixMWD~protein)
proteinRegression
summary(proteinRegression)
