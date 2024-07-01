# R Lab 5 - Describing Data

#Q1 Skip; we did not do this exercise

#Q2 caffine dataset
#a) mean of caffeine_mg_16oz
c <- read.csv("DataForLabs/caffeine.csv")
mean(c$caffeine_mg_16oz)

#b) 95% CI of the mean
t.test(c$caffeine_mg_16oz)$conf.int

#c)
#sd
sd(c$caffeine_mg_16oz)

#coefficient of variation
100*sd(c$caffeine_mg_16oz) / mean(c$caffeine_mg_16oz) 

#plot
ggplot(c,aes(x=caffeine_mg_16oz))+geom_histogram()+xlab("Caffeine (mg/16oz)")+ylab("Frequency")+theme_classic()

#d) switch to Starbucks data set
star <- read.csv("DataForLabs/caffeineStarbucks.csv")
mean(star$caffeine_mg_16oz)

#mean for Starbucks considerably higher (almost 2x)
mean(star$caffeine_mg_16oz)/mean(c$caffeine_mg_16oz)

#Q3 confidence intervals
#a) 99% CI of mean in caffeine data set
t.test(c$caffeine_mg_16oz,conf.level = 0.99)$conf.int

#b) the 99% CI is wider. This makes sense because the confidence is the range of values surrounding an estimate that is likely to contain the parameter. Thus, a higher CI should be a wider range.

#c) 2.5% to 97.5% quantiles
quantile(c$caffeine_mg_16oz,c(0.025,0.975))

# The 95% CI of the mean is different from the 2.5-97.5% quantile. These are measuring two different things. The first is the range of values surrounding the mean estimate that is likely to contain the parameter, whereas the 2.5-97.5% quantiles are measuring the spread of the data sample.

#Q4 Skip; we did not do this exercise

#Q5 countries dataset
#a) plots
countries <- read.csv("DataForLabs/countries.csv")
ggplot(countries,aes(x=ecological_footprint_2000))+geom_boxplot()
#right skew

ggplot(countries,aes(x=cell_phone_subscriptions_per_100_people_2012))+geom_boxplot()
#no skew

ggplot(countries,aes(x=life_expectancy_at_birth_female))+geom_boxplot()
#left skew

#b) compare median and mean
#ecological_footprint_2000
mean(countries$ecological_footprint_2000,na.rm=T)
median(countries$ecological_footprint_2000,na.rm=T)
#with right skew the mean is higher than the median

#cell_phone_subscriptions_per_100_people_2012
mean(countries$cell_phone_subscriptions_per_100_people_2012,na.rm=T)
median(countries$cell_phone_subscriptions_per_100_people_2012,na.rm=T)
#with no skew the mean and median are about the same

#life_expectancy_at_birth_female
mean(countries$life_expectancy_at_birth_female,na.rm=T)
median(countries$life_expectancy_at_birth_female,na.rm=T)
#with left skew the mean is lower than the median