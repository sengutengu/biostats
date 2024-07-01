## R Lab 3 - Graphics

#Q1 identify features better in left and right graphs of same data
#a left has scale for y-axis and color; right has better font for labels
#b left has better axis labels; right has better size of data points

#Q2
#a read in countries.csv
countries <- read.csv("DataForLabs/countries.csv")

#b load ggplot2 library, which is needed to access ggplot functions for graphing
library(ggplot2)

#c histogram of measles_immunization_oneyearolds
#default plot
ggplot(countries,aes(x=measles_immunization_oneyearolds))+geom_histogram()

#improved plot
ggplot(countries,aes(x=measles_immunization_oneyearolds))+geom_histogram()+xlab("Measles immunizations in 1 year olds (%)")+ylab("Frequency")+theme_classic()


#d bar graph of numbers of countries in each continent
#default plot
ggplot(countries,aes(x=continent))+geom_bar()

#improved plot
ggplot(countries,aes(x=continent))+geom_bar()+xlab("Continent")+ylab("Frequency")+theme_classic()

#e scatter plot of life_expectancy_at_birth_male and life_expectancy_at_birth_female
#default plot
ggplot(countries,aes(x=life_expectancy_at_birth_male,y=life_expectancy_at_birth_female))+geom_point()

#improved plot
ggplot(countries,aes(x=life_expectancy_at_birth_male,y=life_expectancy_at_birth_female))+geom_point()+xlab("Male life expectancy at birth (years)")+ylab("Female life expectancy at birth (years)")+theme_classic()

#Q3
#a plot relationship between ecological footpring in 2000 and 2012
ggplot(countries,aes(x=ecological_footprint_2000,y=ecological_footprint_2012))+geom_point()+xlab("Ecological footprint 2000")+ylab("Ecological footprint 2012")+theme_classic()

#b describe relationship
#there is a positive association; 2000 data can predict 2012 data (e.g., high value in 2000 likely results in high value is 2012)

#c do values go up or down?
#add a line for a 1:1 relationship between years
ggplot(countries,aes(x=ecological_footprint_2000,y=ecological_footprint_2012))+geom_point()+xlab("Ecological footprint 2000")+ylab("Ecological footprint 2012")+theme_classic()+geom_abline(intercept=0,slope=1)

#more points below line than above line; means that values tend to be lower in 2012 compared to 2000

#Q4 plot female life expectancy by continent
#default plot
ggplot(countries,aes(x=continent,y=life_expectancy_at_birth_female))+geom_boxplot()

#improved plot
#note that y-axis does not start at zero; find max value and set limits accordingly
max(countries$life_expectancy_at_birth_female,na.rm=T)
ggplot(countries,aes(x=continent,y=life_expectancy_at_birth_female))+geom_boxplot()+ylim(0,90)+xlab("Continent")+ylab("Female life expectancy at birth (years)")+theme_classic()

#Africa is low; Europe is high; Africa, Asia, and Oceania more variable; N. America and S. America less variable

#Q5
#a import bat tongue length data frame
bat_tongues <- read.csv("DataForLabs/BatTongues.csv")

#b scatter plot of palate versus tongue length
ggplot(bat_tongues,aes(x=palate_length,y=tongue_length))+geom_point()+xlab("Palate length (mm)")+ylab("Tongue length (mm)")+theme_classic()

#weak positive association

#c one outlier species with a much longer tongue than expected given its palate length

#d load library dplyr
library(dplyr)

#e find outlier
filter(bat_tongues,tongue_length>80)

#Q6 SKIP

#Q7 I made some improvements above, but think about labels, colors, axis limits, etc