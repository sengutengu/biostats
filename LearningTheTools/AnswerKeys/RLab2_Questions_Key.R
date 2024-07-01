# R Lab 2 - Intro to R Part 2

#Q1
#a create vector, copy+paste from website
ACE_ngL <- c(640, 1070, 780, 70, 160, 130, 60, 50, 2110, 70, 350, 30, 210, 90, 470, 580, 250, 310, 460, 430, 140, 1070, 130)

#b vector mean
mean(ACE_ngL)

#c convert to ml urine per L pool water assumine pools are 4000 L
ACE_urineL <- ACE_ngL/4000
ACE_urineL

#d mean conc or ml urine per L
mean(ACE_urineL)

#e calculate mean without using mean()
sum(ACE_urineL)/length(ACE_urineL)

#f calc ave ml of urine in 500,000 liter pool
mean(ACE_urineL)*500000

#gross!

#Q2
#a make vectors for feeding and non-feeding values
feeding <- c(71.0, 77.3, 82.6, 96.1, 106.6, 112.8, 121.2, 126.4, 127.5, 143.1)
nonFeeding <- c(42.2, 51.7, 59.8, 66.5, 81.9, 82.0, 81.3, 81.3, 96.0, 104.1)

#b check vector lengths
length(feeding)
length(nonFeeding)

#c difference of two vectors (feeding minus nonFeeding)
MetabolismDifference <- feeding-nonFeeding
MetabolismDifference

#d ave difference
mean(MetabolismDifference)

#e ratio of two vectors (feeding over nonFeeding)
MetabolismRatio <- feeding/nonFeeding

#f calc natural log of each ratio, calc mean of vector or natural logs
MetabolismRatioLn <- log(MetabolismRatio)
mean(MetabolismRatioLn)

#3
#a use read.csv to read in data frame countries.csv
countries <- read.csv("DataForLabs/countries.csv")

#b summaries data frame
summary(countries)

#note that first three variables are country, total_population_in_thousands_2015, and gross_national_income_per_capita_2013

#c using output from summary(), how many countries are in Africa
# HERE THERE IS AN ISSUE ASSOICATED WITH A SWITCH FROM R VERSION 3 TO 4
# the lab was written using v3, for which summary() will treat variables with text as "factors" and for the variable continent would show the number of times Africa appears
# in v4, variables with text are treated as "characters" and the summary() output does not show the number of times Africa appears

# one way around this is to tell R to treat continent as a "factor"
# note that the syntax for a variable within a data frame is dataframe$variable
countries$continent <- factor(countries$continent)
summary(countries)

#54 countries in Africa

#d types of variables using summary()
#continents = character (text)
#cell_phone_subscriptions_per_100_people_2012 = numerical
#total_population_in_thousands_2015 = numerical
#fines_for_tobacco_advertising_2014 = character (text)

#str() is a useful function for this, which gives the "structure" of the data frame, including type of each variable
str(countries)
#note chr=character, num=number, int=integer

#e add column with ecological_footprint_2000 minus ecological_footprint_2012, calc mean diff
countries$ecological_footprint_diff <- countries$ecological_footprint_2000-countries$ecological_footprint_2012
mean(countries$ecological_footprint_diff,na.rm=T)

#Q4
#create new data frame for only African countries 
library(dplyr)
countriesAfrica <- filter(countries,continent=="Africa")
sum(countriesAfrica$total_population_in_thousands_2015)