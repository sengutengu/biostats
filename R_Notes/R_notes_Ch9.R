# import custom functions written by DaCosta
source("relrisk.R")
source("oddsratio.R")

####### Aspirin Example #######

aspirinData <- read.csv("DataForLabs/chap09e2AspirinCancer.csv")
summary(aspirinData) # two columns: aspirinTreatment (y/n) & cancer (y/n)

# set up table as follows:
#                       Treat   Control
# Success (yes cancer)
# Failure (no cancer)

aspirinTable <- table(aspirinData)
aspirinTable # not the right format!


# Order factors in treat-control and success-failure orders
aspirinData$cancer <- factor(aspirinData$cancer, levels=c("Cancer", "No cancer"))
aspirinData$aspirinTreatment <- factor(aspirinData$aspirinTreatment, levels=c("Aspirin", "Placebo"))

# Define y and x axes
aspirinTable <- table(aspirinData$cancer, aspirinData$aspirinTreatment)
aspirinTable

# >>> successful table setup
#           Aspirin Placebo
# Cancer       1438    1427
# No cancer   18496   18515

mosaicplot(aspirinTable) # correct formatting leads to wrong axes
mosaicplot(t(aspirinTable)) # t() to transpose table

# eye candy. Looks like equal probability of getting cancer.
mosaicplot(t(aspirinTable), xlab="Treatment", ylab="Relative frequency",
           main="Title", col=c("darkred", "gold"))

# find relative risk
relrisk(aspirinTable)

# find odds ratio
oddsratio(aspirinTable)

####### Pigeon Feather Example #######
pigeonData <- read.csv("DataForLabs/chap09q05PigeonRumps.csv", stringsAsFactors = T)
head(pigeonData)
summary(pigeonData)
# killed is disease or "success
# white is treatment (does it confer advantage?)
# format into treat-ctrl, success-failure table.
pigeonData$survivalWithFalcon <- factor(pigeonData$survivalWithFalcon, levels=c("killed", "survived"))
pigeonData$rumpColor <- factor(pigeonData$rumpColor, levels=c("white", "blue"))
pigeonTable <- table(pigeonData$survivalWithFalcon, pigeonData$rumpColor)
pigeonTable

relrisk(pigeonTable) #=0.099. 
# white pigeons have 9.9% the risk of predation compared to blue!
# CI far lower than 1 (equal risk)

oddsratio(pigeonTable) # 0.01
# white pigeons have 1/100th the odds of being predated
# compared to blue!
# CI for lower than 1 (equal odds)

# If a variable has more than two groups, 
# not possible to do relrisk or odds ratio.
# must instead do chi-square contingency test. 

fishData <- read.csv("DataForLabs/chap09e4WormGetsBird.csv", stringsAsFactors=T)
head(fishData)
summary(fishData) # three levels of infection so no relrisk/odds ratio
# table format DOES NOT AFFECT RESULT! unlike relrisk/oddsratio.
# to prove this, let's make two tables
fishTable1 <- table(fishData$fate, fishData$infection)
fishTable1
fishTable2 <- t(fishTable1)
fishTable2
fishTest <- chisq.test(fishTable1, correct=F) # do not apply continuity correction
fishTest
chisq.test(fishTable2, correct=F)

# make mosaic plot
mosaicplot(t(fishTable1), xlab="Infection level", ylab="Relative frequency", main="Title", col=c("darkred", "gold"))

install.packages('plyr')
library(plyr)
smokerData <- read.csv("DataForLabs/smokerBMI.csv", stringsAsFactors=T)
summary(smokerData)
smokerData$BMI <- factor(smokerData$BMI, levels=c("high", "healthy", "low"))
smokerData$CurrentSmoker <- factor(smokerData$CurrentSmoker, levels=c("yes", "no"))
smokerData$CurrentSmoker <- revalue(smokerData$CurrentSmoker, c("yes"="smoker", "no"="nonsmoker"))
smokerTable <- table(smokerData$CurrentSmoker, smokerData$BMI)
smokerTable
mosaicplot(smokerTable, xlab="Smoking status", ylab="Relative frequency of BMI levels", main="Relative frequencies of BMI levels in smokers and nonsmokers", col=c("darkred", "darkgreen", "gold"))

fishTest$observed
fishTest$expected
fishTest$observed-fishTest$expected
# sig result driven by surplus of highly infected fish eaten and
# surplus of uninfected that survive


# HW 

# Q2 
2727-65

p <- 65/2727
q <- 1-p 
o <- p/q
o

# Q4

tailMating <- read.csv("DataForLabs/tail_mating.csv", stringsAsFactors=T)
head(tailMating)
summary(tailMating)
tailMating$tail <- factor(tailMating$tail, levels=c("short", "medium", "long"))
tailTable <- table(tailMating$tail, tailMating$matingsuccess)
tailTable
tailTest <- chisq.test(tailTable, correct=F)
tailTest
tailTest$expected
tailTest$observed

# Q6

A <- 12*62/126
A
B <- 12*64/126
B
C <- 114*62/126
C
D <- 114*64/126
D

A+B
C+D

# Q7
22+12+14
13+77+14
104+48
35+89+28
(89*48)/152
Age <- c(rep("elderly", 13), rep("young", 22), rep("elderly", 77), rep("young", 12), rep("elderly", 14), rep("young", 14))
Genotype <- c(rep("MM", 13), rep("MM", 22), rep("MV", 77), rep("MV", 12), rep("VV", 14), rep("VV", 14))
Kuru <- data.frame(cbind(Age, Genotype))
KuruTable <- table(Kuru)
KuruTable
KuruTest <- chisq.test(KuruTable, correct=F)
KuruTest$expected

# Q8
gardasilData <- read.csv("DataForLabs/Gardasil.csv", stringsAsFactors=T)
summary(gardasilData)
gardasilTable <- table(gardasilData$cervicalCancer, gardasilData$treatment)
gardasilTable
oddsratio(gardasilTable)

# Quiz Q2

widowHealth <- read.csv("DataForLabs/widow_health.csv", stringsAsFactors=T)
summary(widowHealth)
head(widowHealth)
widowTable <- table(widowHealth$health_deterioration, widowHealth$widowed)
widowTable
widowTest <- chisq.test(widowTable, correct=F)
widowTest

widowTest$expected
widowTest$observed
widowTest$observed-widowTest$expected

widowTable2 <- t(widowTable)
widowTable2
widowTest2 <- chisq.test(widowTable2, correct=F)
widowTest2

tailMating$tail <- factor(tailMating$tail, levels=c("short", "medium", "long"))
tailTable <- table(tailMating$tail, tailMating$matingsuccess)
tailTable
tailTest <- chisq.test(tailTable, correct=F)
tailTest
tailTest$expected
tailTest$observed

# Q4

plasma <- read.csv("DataForLabs/plasmaTransfusion.csv", stringsAsFactors = T)
summary(plasma)
head(plasma)

plasma$treatment <- factor(plasma$treatment, levels=c("Transfusion", "Control"))
plasmaTable <- table(plasma$response, plasma$treatment)
plasmaTable

relrisk(plasmaTable)

# Q8
yawningData <- read.csv("DataForLabs/yawning.csv", stringsAsFactors=T)
head(yawningData)
summary(yawningData)
length(yawningData$faceShown)
yawningData$yawn <- factor(yawningData$yawn, levels=c("yes", "no"))
levels(yawningData$yawn) <- c("Yawned", "Did not yawn")
levels(yawningData$faceShown) <- c("Covered", "Uncovered")
yawningTable <- table(yawningData$yawn, yawningData$faceShown)
yawningTable

mosaicplot(yawningTable) # correct formatting leads to wrong axes
mosaicplot(t(yawningTable)) # t() to transpose table

# eye candy. Looks like equal probability of getting cancer.
mosaicplot(t(yawningTable), xlab="Eye covering", ylab="Relative frequency", main="", col=c("darkred", "gold"))

allData
allData$socialActivity <- factor(allData$socialActivity, levels=c("yes", "no"))
allTable <- table(allData)
allTable
oddsratio(allTable)
1020/5343
252/895

smokerData
smokerTable <- table(smokerData)
smokerData$BMI <- factor(smokerData$BMI, levels=c("low", "healthy", "high"))
smokerData$CurrentSmoker <- factor(smokerData$CurrentSmoker, levels=c("yes", "no"))
smokerTable <- table(smokerData$BMI, smokerData$CurrentSmoker)
smokerTable
smokerTest <- chisq.test(smokerTable, correct=F)
smokerTest

mosaicplot(t(smokerTable), main="Title", col=c("darkred", "gold"))

tuberculosisData
tuberculosisData$group <- factor(tuberculosisData$group, levels=c("tuberculosis", "no tuberculosis"))
tuberculosisTable <- table(tuberculosisData$group, tuberculosisData$genotype)
tuberculosisTable

tuberculosisTest <- oddsratio(tuberculosisTable)

helmetData
helmetTable <- table(helmetData$result, helmetData$type)
helmetTable

relrisk(helmetTable)
oddsratio(helmetTable)
chisq.test(helmetTable, correct=F)

