# R Homework 2, Week 1
# Seung-hwan Leo Kim

# Question 1
# Answer 1
# a
pool_ACE_levels_ng_mL <- c(640, 1070, 780, 70, 160, 130, 60, 50, 2110, 70, 350, 30, 210, 90, 470, 580, 250, 310, 460, 430, 140, 1070, 130)
# b
mean(pool_ACE_levels)
# [1] 420

# c
pool_urine_levels_mL_L <- pool_ACE_levels_ng_mL/4000
# d
mean(pool_urine_levels_mL_L)
# [1] 0.105

# e
sum(pool_ACE_levels_ng_mL)/length(pool_ACE_levels_ng_mL)
# yes

# f
0.105 * 500000
# [1] 52500

# Question 2
# Answer 2

feeding_dive_o2 <- c(71.0, 77.3, 82.6, 96.1, 106.6, 112.8, 121.2, 126.4, 127.5, 143.1)
nonfeeding_dive_o2 <- c(42.2, 51.7, 59.8, 66.5, 81.9, 82.0, 81.3, 81.3, 96.0, 104.1)
length(feeding_dive_o2) == length(nonfeeding_dive_o2)

MetabolismDifference <- feeding_dive_o2 - nonfeeding_dive_o2
mean(MetabolismDifference)
MetabolismRatio <- feeding_dive_o2/nonfeeding_dive_o2
log(MetabolismRatio)
mean(log(MetabolismRatio))

countries <-read.csv("DataForLabs/countries.csv")
summary(countries)
filter(countries, continent=="Africa")
# 54 Countries
countries$diff_ecological_footprint_2012_2000 <-countries$ecological_footprint_2012 - countries$ecological_footprint_2000
summary(countries)
mean(countries$diff_ecological_footprint_2012_2000, na.rm=TRUE)
AfricaData <- filter(countries, continent=="Africa")
summary(AfricaData)
sum(AfricaData$total_population_in_thousands_2015)
