# R Lab & Notes Ch. 16

guppyData <- read.csv("DataForLabs/chap02e3bGuppyFatherSonAttractiveness.csv", stringsAsFactors=T)
# demonstrates moderately strong positive correlation
ggplot(guppyData, aes(x=fatherOrnamentation, y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness")

# Find correlation. doesn't matter the order.
cor(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)
cor(guppyData$sonAttractiveness, guppyData$fatherOrnamentation)

# correlation test
cor.test(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)
# p quite small
# conf int doesn't include 0

birthData <- read.csv("DataForLabs/birthweight_height.csv", stringsAsFactors=T)
summary(birthData)
ggplot(birthData, aes(x=birthWeight, y=currentHeight)) +
  geom_point() +
  theme_minimal() +
  xlab("Birth weight") +
  ylab("Current height")

cor(birthData$birthWeight, birthData$currentHeight)
cor.test(birthData$birthWeight, birthData$currentHeight)

# SE of correlation coefficient.

r <- 0.52
n <- 33
SEr <- sqrt((1-(r^2))/(n-2))
SEr

petalData <- read.csv("DataForLabs/petal_anther.csv", stringsAsFactors = T)
cor.test(petalData$petal_length, petalData$anther_height)

# Quiz

# Q1
sleepData <- read.csv("DataForLabs/sleep.csv", stringsAsFactors=T)
summary(sleepData)
ggplot(sleepData, aes(x=sleep, y=improvement)) +
  geom_point() +
  theme_minimal() +
  xlab("Sleep") +
  ylab("Improvement")
cor.test(sleepData$sleep, sleepData$improvement)
cor(sleepData$sleep, sleepData$improvement)

r <- 0.8633217
n <- 10
SEr <- sqrt((1-(r^2))/(n-2))
SEr


greenData <- read.csv("DataForLabs/GreenSpaceBiodiversity.csv", stringsAsFactors=T)
summary(greenData)
cor.test(greenData$WellBeingScore, greenData$bird)

cocaineData <- read.csv("DataForLabs/CocaineHigh.csv", stringsAsFactors=T)

# Exam

shaqData <- read.csv("DataForLabs/FreeThrow_Height.csv", stringsAsFactors=T)
summary(shaqData)
ggplot(shaqData, aes(x=Height, y=FT)) +
  geom_point() +
  theme_minimal()
# correlation coefficient
cor(shaqData$FT, shaqData$Height)
cor.test(shaqData$FT, shaqData$Height)
