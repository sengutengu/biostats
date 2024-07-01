# R Notes Ch 11

# Calculating the 95% confidence interval for MEAN

# eye stalk measurements
eyes <-  c(8.69, 8.15, 9.25, 9.45, 8.96, 8.65, 8.43, 8.79, 8.63)

t.test(eyes)$conf.int
# we are 95% confident that the population mean
# lies between 8.472 and 9.084

# One sample t-test!

bt <- read.csv("DataForLabs/chap11e3Temperature.csv", stringsAsFactors=T)
str(bt) # 25 observations

t.test(bt$temperature, mu=98.6) # assume 98.6F as pop parameter
# p value is 0.5802 > 0.05. so we fail to reject null

# HW 11
1.93/sqrt(140)

indigoBirds <- read.csv("DataForLabs/indigobird_tail.csv", stringsAsFactors=T)
str(indigoBirds)
t.test(indigoBirds)$conf.int

humanTemps <- read.csv("DataForLabs/human_temps.csv", stringsAsFactors=T)
str(humanTemps)
head(humanTemps)
t.test(humanTemps$Temperature, mu=98.6)

brains <- read.csv("DataForLabs/brains.csv", stringsAsFactors=T)
str(brains)
t.test(brains$Size, mu=3750)

# Quiz 11
ratMaze <- read.csv("DataForLabs/rat_maze.csv", stringsAsFactors=T)
head(ratMaze)
t.test(ratMaze$Time, mu=12.3)$conf.int

seY <- 0.13/sqrt(5)
seY
(0.47-0.50)/seY

elevationChange <- read.csv("DataForLabs/elevation_change.csv", stringsAsFactors = T)
head(elevationChange)
summary(elevationChange)
elevationChange
t.test(elevationChange$changeExtent, mu=0)$parameter

butterflyWings <- read.csv("DataForLabs/butterfly_wings.csv", stringsAsFactors=T)
head(butterflyWings)
t.test(butterflyWings$wingLength, mu=37.4)

bananas <- read.csv("DataForLabs/bananas.csv", stringsAsFactors=T)
summary(bananas$coefficient)
mean(bananas$coefficient)
sd(bananas$coefficient)
head(bananas)
t.test(bananas$coefficient, mu=0.02)
t.test(bananas$coefficient)

seY_BC <- 110/sqrt(20)
(1280-1119)/seY_BC
