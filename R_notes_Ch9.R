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
