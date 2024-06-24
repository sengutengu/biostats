# stringsAsFactors reads strings as factors rather than as simple strings.
# Q2

caffeineData <- read.csv("DataForLabs/caffeine.csv", stringsAsFactors = TRUE)
summary(caffeineData)

mean(caffeineData$caffeine_mg_16oz)
sd(caffeineData$caffeine_mg_16oz)

# standard error

# the variance of the sampling distro is the variance of the data divided by N
# and the standard error is just the square root of that.
sd(caffeineData$caffeine_mg_16oz)/sqrt(length(caffeineData$caffeine_mg_16oz))
t.test(caffeineData$caffeine_mg_16oz)$conf.int

ggplot(caffeineData, aes(x=caffeine_mg_16oz)) +geom_histogram()

studentSampleData <- read.csv("DataForLabs/studentSampleData.csv", stringsAsFactors=T)
summary(studentSampleData)


ggplot(studentSampleData, aes(x=height_cm))+ geom_histogram()


umassData <- read.csv("DataForLabs/Massachusetts_Payrollv2_2018_UMS.csv", stringsAsFactors=T)
summary(umassData)
ggplot(umassData, aes(x=annual_rate)) + geom_histogram()+ theme_classic()

ggplot(umassData, aes(x=annual_rate)) + geom_boxplot() + theme_classic()
ggplot(umassData, aes(y=annual_rate)) + geom_boxplot() + theme_classic()

# mean is skewed by outliers
# median is much less so

mean(umassData$annual_rate) # 79752.93
median(umassData$annual_rate) # 67000.18

ggplot(umassData, aes(x=annual_rate)) + 
  geom_histogram() + 
  theme_classic() +
  geom_vline(xintercept=median(umassData$annual_rate), col="blue") +
  geom_vline(xintercept=mean(umassData$annual_rate), col="red")



small <- c(33, 23, 28, 36, 25)
mean(small)
print(small-29)

botanist <- c(25, 21, 26, 24, 29, 34, 29, 25, 20, 23)
summary(botanist)

IQR(botanist)

tuberworm <- read.csv("DataForLabs/potato_tuberworm.csv", stringsAsFactors=T)

summary(tuberworm)

ggplot(tuberworm, aes(x=EGGCNT))+geom_histogram()
mean(tuberworm$EGGCNT)
sd(tuberworm$EGGCNT)

test <- c(5, 5, 5, 5, 5, 5, 5)
sd(test)

stitches <- c(10, 13, 9, 50, 16, 13, 22, 18, 20, 14, 19, 12)
median(stitches)

threesome <- c(9, 13, 8)
sd(threesome)^2
# calculating variance by hand
sum((threesome-mean(threesome))^2)/2

sample1 <- c(1.61, 1.82, 1.58, 1.75, 1.55, 1.79, 1.64, 1.63, 1.80, 1.69)
median(sample1)
sample2 <- c(1.61, 1.82, 1.58, 1.75, 1.55, 1.79, 1.64, 1.63, 18.0, 1.69)
median(sample2)


locustSerotonin <- read.csv("DataForLabs/LocustSerotonin.csv", stringsAsFactors=T)
summary(locustSerotonin)
locustLowOnly <-filter(locustSerotonin, treatmentTime == "low")

locustLowOnlySerotonin <- locustLowOnly$serotoninLevel

sum((locustLowOnlySerotonin-mean(locustLowOnlySerotonin))^2)/(length(locustLowOnlySerotonin)-1)

seaUrchins <- read.csv("DataForLabs/sea_urchins.csv", stringsAsFactors=T)
summary(seaUrchins)
seaUrchinsAAOnly <- filter(seaUrchins, populationOfFemale == "AA")
summary(seaUrchinsAAOnly)
median(seaUrchinsAAOnly$percentAAfertilization)
ggplot(seaUrchins, aes(x=populationOfFemale)) +
  geom_bar()

seaUrchinsBBOnly <- filter(seaUrchins, populationOfFemale == "BB")
summary(seaUrchinsBBOnly)
mean(seaUrchinsBBOnly$percentAAfertilization)
sd(seaUrchinsBBOnly$percentAAfertilization)

sparrowLRS <- read.csv("DataForLabs/sparrow_LRS.csv", stringsAsFactors=T)
summary(sparrowLRS)
sparrowLRSMale <- filter(sparrowLRS, sex=="male")
summary(sparrowLRSMale)
mean(sparrowLRSMale$lifetimeRS)
median(sparrowLRSMale$lifetimeRS)

option1 <- c(0, 0, 5, 5)
option2 <- c(0, 2, 3, 5)
sd(option1)
sd(option2)
sd(c(2, 2, 3, 3))

# R lab 5c

geneData <- read.csv("DataForLabs/HumanGeneLengthsLongestTranscript.csv", stringsAsFactors=T)

# empty vector to store 1000 means of sample size n=20
n20 <- c()
n100 <- c()
n500 <- c()

for (i in 1:1000) {
  n20[i] <- mean(sample(geneData$size, 20, replace=F))
  n100[i] <- mean(sample(geneData$size, 100, replace=F))
  n500[i] <- mean(sample(geneData$size, 500, replace=F))
}

# combine into dataframe

sampleSize <- c(rep("n20", 1000), rep("n100", 1000), rep("n500", 1000))
means <- c(n20, n100, n500)
samples <- data.frame(sampleSize, means)
samples$sampleSize <- factor(samples$sampleSize, levels=c("n20", "n100", "n500"))
ggplot(samples, aes(x=means)) + 
  geom_histogram(color="black", fill="darkred") +
  facet_wrap(~sampleSize, ncol=1) +
  xlab("Sample mean length (nucleotides)") +
  ylab("Frequency") +
  theme_classic()


