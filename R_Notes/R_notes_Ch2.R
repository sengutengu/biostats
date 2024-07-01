# stringsAsFactors reads strings as factors rather than as simple strings.
titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors = TRUE)

# show structure of titanicData
str(titanicData)

# mean with missing data cells returns NA.
mean(titanicData$age)

# to avoid this, we have to skip the NA cells (na.rm).
mean(titanicData$age, na.rm = T)

# give dataframe first, then give aesthetics, then specify type of plot
ggplot(titanicData, aes(x=sex,y=age)) + geom_boxplot()

# simple histogram - y axis is frequency/count
ggplot(titanicData, aes(x=age)) + geom_histogram()

# bar chart
ggplot(titanicData, aes(x=sex)) + geom_bar()

# boxplots
ggplot(titanicData, aes(x=sex, y=age)) + geom_boxplot()

# scatterplots
guppyFatherSonData <- read.csv("DataForLabs/chap02e3bGuppyFatherSonAttractiveness.csv", stringsAsFactors=TRUE)

ggplot(guppyFatherSonData, aes(x=fatherOrnamentation, y=sonAttractiveness)) + geom_point()

gradYear <- read.csv("DataForLabs/gradyear.csv", stringsAsFactors = T)
str(gradYear)

ggplot(gradYear, aes(x=GradYear)) +
  geom_bar(binwidth=1, color="black", fill="white") +
  xlab("Graduation Year") +
  ylab("Number of Students") +
  annotate(geom="text", x=2020, y=20, label="n=19") +
  annotate(geom="text", x=2021, y=23, label="n=22") +
  annotate(geom="text", x=2022, y=5, label="n=4") +
  theme_classic()

summary(gradYear)
table(gradYear)
