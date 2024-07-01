#Labs using R: 6. Frequency data

#1. cell phone use in hospitals
#1a. 510 tests, 6 disruptions, Agresti-Coull 95%CI
binom.confint(6,510,method="ac")

#1b. 300 tests, 0 disruptions
#proportion
prop <- 0/300
prop

#95% CI of proportion
SEprop <- sqrt((prop*(1-prop))/300)
SEprop
prop-2*SEprop
prop+2*SEprop

#2 four pairs of identical stockings; 52 trials
#2a expected frequencies under null that all pairs have equal prob of being chosen
expFreq <- c(52,52,52,52)/4
expFreq

#2b chi-sq test using table from data frame
stock <- read.csv("DataForLabs/stockings.csv")
obsData <- table(stock$choice)
chisq.test(obsData,p=c(0.25,0.25,0.25,0.25))

#2c chi-sq test using vector of observed counts
chisq.test(x = c(6,9,16,21), p = c(0.25,0.25,0.25,0.25))

#2b and 2c should give same answer

#3 month of birth and soccer
#3a plot data
soccerData <- read.csv("DataForLabs/soccer_birth_quarter.csv")
library(ggplot2)
ggplot(soccerData,aes(x=birth_quarter))+geom_bar()

#re-order levels to follow calendar
soccerData$birth_quarter <- factor(soccerData$birth_quarter,levels=
                                    c("Aug-Oct","Nov-Jan","Feb-Apr","May-July"))

ggplot(soccerData,aes(x=birth_quarter))+geom_bar()

#more players in U-20 tournament born in quarters near cut-off month of Aug

#2b collect birth month data from Canada, compare to soccer data
canadaData <- read.csv("DataForLabs/Canadian_births.csv")
View(canadaData)

canadaAugOct <- sum(canadaData$canada_births_proportion[8:10])
canadaNovJan <- sum(canadaData$canada_births_proportion[11:12])+canadaData$canada_births_proportion[1]
canadaFebApr <- sum(canadaData$canada_births_proportion[2:4])
canadaMayJuly <- sum(canadaData$canada_births_proportion[5:7])
expProp <- c(canadaAugOct,canadaNovJan,canadaFebApr,canadaMayJuly)
expProp
expFreq <- expProp*length(soccerData$birth_quarter)
expFreq

soccerTable <- table(soccerData$birth_quarter)
soccerTable

chisq.test(soccerTable,p=expProp)

#the P-value is well below 0.05. You reject the null that birth quarter
#frequencies of U-20 soccer players matches the expected frequencies
#from Canada population. There is an excess of soccer players in the
#Aug-Oct and Nov-Jan quarters and a deficiency in the other two quarters

#4 poisson distribution question (SKIP)