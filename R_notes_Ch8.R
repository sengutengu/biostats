#R_notes_Ch8

haircolor <- c("black", "blonde", "brown", "red")
count <- c(137, 150, 419, 294)
hairdata <- data.frame(haircolor, count)

table <- xtabs(count~haircolor, data=hairdata)
table

chisq.test(table)

results <- chisq.test(table)

results$observed
results$expected
results$residuals # which ones deviate the most?

# does hair color match the population prob of 
# 15% black, 15% blonde, 40% brown, and 30% red?
probs <- c(0.15, 0.15, 0.4, 0.3)
chisq.test(table, p=probs) # yes, consistent with H0 

MMlist <- read.csv("DataForLabs/MandMlist.csv", stringsAsFactors = T)
MMtable <- table(MMlist$color) # summarize data into table
MMtable

# probabilities according to the company
expected_proportions <- c(0.24, 0.14, 0.16, 0.20, 0.13, 0.13)
sum(MMtable) * expected_proportions # expected frequencies

chisq.test(MMtable, p=expected_proportions)

# Spirit Bear Example

obsGen <- c(42, 24, 21)
sum(obsGen)
propB <- ((2*42)+24)/(87*2)
propB

propb <- 1-propB
propb

expBB <- propB^2
expBb <- 2*propB*propb
expbb <- propb^2
sum(expBB, expBb, expbb)

bearTest <- chisq.test(obsGen, p=c(expBB, expBb, expbb))
bearTest # reject null hypothesis, observed genotype is sig different.

summary(bearTest)
bearTest$observed
bearTest$expected # deficiency of heterozygotes
bearTest$residuals

# potential deficiency in heterozygosity or bears select for like colors

# differences explicitly
bearTest$observed-bearTest$expected

Genotype <- rep(c("BB", "Bb", "bb"), 2)
Genotype
Type <- c(rep("Observed", 3), rep("Expected", 3))
Type
Count <- c(bearTest$observed, bearTest$expected)
Count
graphData <- data.frame(cbind(Genotype, Type, Count))
str(graphData)
graphData$Count <- as.numeric(graphData$Count) # convert from chr to num
str(graphData)

library(ggplot2)
ggplot(graphData, aes(x=Genotype, y=Count, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic()

births <- read.csv("DataForLabs/chap08e1DayOfBirth2016.csv", stringsAsFactors = T)
summary(births)
str(births) # structure of births. we see that the days are not in order
# reorder factors so we have days of week in order
births$day <- factor(births$day, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
birthsTable <- table(births$day) # create frequency table
birthsTable

# defining expected probabilities. 53 Fridays & Saturdays, 366 days.
expected_probabilities <- c(52, 52, 52, 52, 52, 53, 53)/366
expected_probabilities

chisq.test(birthsTable, p=expected_probabilities) # p = 0.0149

# graphing
expFreq <- expected_probabilities * 180
ObsExp <- data.frame(birthsTable, expFreq)
ObsExp <- setNames(ObsExp, c("day", "obsFreq", "expFreq"))
ObsExp

# combine three two columns into two
library(reshape2)
ObsExp <- melt(ObsExp)
ObsExp

# fill=variable allows split values into diff variable groups
ggplot(ObsExp, aes(x=day, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() +
  xlab("Day of Week") +
  ylab("N") +
  scale_fill_discrete(name="Freequency", labels=c("Observed", "Expected"))


leadingDigits <- read.csv("DataForLabs/leading_digits.csv", stringsAsFactors = T)
summary(leadingDigits)
length(leadingDigits$LeadingDigit)
leadingDigitsTable <- table(leadingDigits)
leadingDigitsTable
expected_probabilities <- c(0.079, 0.921)
chisq.test(leadingDigitsTable, p=expected_probabilities)

binom.test(x=29, n=325, p=0.079)

labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
daysMonths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daysMonths_probabilities <- daysMonths/365
mortalities <- c(83, 86, 111, 97, 94, 63, 80, 74, 71, 83, 72, 86)

daysMonths_expected_mortalities <- 1000*daysMonths_probabilities
daysMonths_expected_mortalities
data.frame(cbind(labels, daysMonths, daysMonths_expected_mortalities, mortalities))

Xsq <- sum(((mortalities-daysMonths_expected_mortalities)^2)/daysMonths_expected_mortalities)
Xsq
chisq.test(mortalities, p=daysMonths_probabilities)

# Quiz

chessData <- read.csv("DataForLabs/chess.csv", stringsAsFactors=T)
chessData$white <- factor(chessData$white, levels=c("win", "draw", "lose"))
chessTable <- table(chessData$white)
chessTable
expected_probabilities <- c(0.375, 0.349, 0.276)
chessTest <- chisq.test(chessTable, p=expected_probabilities)
chessTest$expected
chessTest$observed

chessTest$observed-chessTest$expected


ObservedAndExpected <- data.frame(chessTable, chessTest$expected)
ObservedAndExpected <- setNames(ObservedAndExpected, c("Outcome", "Observed", "Expected"))
ObservedAndExpected
library(reshape2)
ObservedAndExpected <- melt(ObservedAndExpected)
ObservedAndExpected$variable <- factor(ObservedAndExpected$variable, levels=c("Expected", "Observed"))
ObservedAndExpected

library(ggplot2)
ggplot(ObservedAndExpected, aes(x=Outcome, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() +
  ylab("Frequency") +
  scale_fill_discrete(name="Frequency") +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Expected and Observed Frequencies of Chess Outcomes - White Pieces")

# die question
(1/6)
121*6

dieData <- read.csv("DataForLabs/die.csv", stringsAsFactors=T)
dieData$result <- factor(dieData$result, levels=c("one", "two", "three", "four", "five", "six"))
dieTable <- table(dieData$result)
dieTable
expected_probabilities <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
dieTest <- chisq.test(dieTable, p=expected_probabilities)
dieTest
dieTest$expected
dieTest$observed
dieTest$observed-dieTest$expected


# Snapdragon 

observedGenotypes <- c(40, 28, 22)
sum(observedGenotypes)

# get proportions of R and r
frequencyOfR <- ((2*40)+28)/(90*2)
frequencyOfR

frequencyOfr <- 1-frequencyOfR
frequencyOfr

# use hardy weinberg to calculate expected frequencies
expectedRR <- frequencyOfR^2
expectedRr <- 2*frequencyOfR*frequencyOfr
expectedrr <- frequencyOfr^2

sum(expectedRR, expectedRr, expectedrr) # should sum to 1

# run the test!
snapTest <- chisq.test(observedGenotypes, p=c(expectedRR, expectedRr, expectedrr))
snapTest

summary(snapTest)
snapTest$observed
snapTest$expected

snapTest$residuals


# Exam 2

labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
daysMonths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daysMonths_probabilities <- daysMonths/365
daysMonths_probabilities

catData <- read.csv("DataForLabs/falling_cats.csv", stringsAsFactors=T)
length(catData$month)
catData$month <- factor(catData$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
head(catData)
catTable <- table(catData)
catTable

catTest <- chisq.test(catTable, p=daysMonths_probabilities)

catTest
catTest$expected-catTest$observed

diceData$outcome <- factor(diceData$outcome, levels=c("one", "two", "three", "four", "five", "six"))
diceTable <- table(diceData)
diceExpected <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)*71
diceExpected
ObsExp <- data.frame(diceTable, diceExpected)
ObsExp <- setNames(ObsExp, c("Outcome", "Observed", "Expected"))
ObsExp
library(reshape2)
ObsExp <- melt(ObsExp)
ObsExp

library(ggplot2)
ggplot(ObsExp, aes(x = Outcome, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() +
  ylab("N") +
  scale_fill_discrete(name = "Frequency") +
  ggtitle("Observed and expected frequencies of sides from a fair die (N=71)")

diceTest <- chisq.test(diceTable, p=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
diceTest$expected-diceTest$observed

diceTest

# pea

observedGenotypes <- c(24, 43, 13)
sum(observedGenotypes)

# get proportions of R and r
frequencyOfG <- ((2*24)+43)/(80*2)
frequencyOfG

frequencyOfg <- 1-frequencyOfG
frequencyOfg

# use hardy weinberg to calculate expected frequencies
expectedGG <- frequencyOfG^2
expectedGg <- 2*frequencyOfG*frequencyOfg
expectedgg <- frequencyOfg^2

sum(expectedGG, expectedGg, expectedgg) # should sum to 1

# run the test!
peaTest <- chisq.test(observedGenotypes, p=c(expectedGG, expectedGg, expectedgg))
peaTest

summary(peaTest)
peaTest$observed
peaTest$expected

peaTest$residuals
