install.packages("binom", dependencies=T) 
library(binom) # need binom package

# Agresti-Coull method for confidence interval

influenza <- read.csv("DataForLabs/influenza.csv", stringsAsFactors=T)
summary(influenza) # 210 positive, 507 total
binom.confint(x=210, n=507, method="ac") # method="ac" for Agresti-Coull

# Binomial test

# p more extreme than 14
binom.test(x=14, n=18, p=0.5) #p-value=0.03088 < 0.05

# p of exactly 14
dbinom(14, 18, 0.5)

# Adding all possibilities as or more extreme than 14 and multiplying by 2
# gives us the p value from binom.test()
moreExtreme <- c(dbinom(14, 18, 0.5), dbinom(15, 18, 0.5), dbinom(16, 18, 0.5), dbinom(17, 18, 0.5), dbinom(18, 18, 0.5))
2*sum(moreExtreme) # == 0.03088

# adding all is tedious. We can take a shortcut. 
# Given n=27 and expected proportion = 0.25,
xsuccesses <- c(0:27)
probs <- dbinom(xsuccesses, 27, 0.25)
probTable <- data.frame(xsuccesses, probs)
probTable

library(ggplot2)
ggplot(probTable, aes(x=xsuccesses, y=probs)) + 
  geom_bar(stat="identity") +
  xlab("Number of left-handed flowers (x)") +
  ylab("Probability") +
  theme_classic()

# binom test
binom.test(6, 27, 0.25)

# Agresti-Coull
binom.confint(6, 27, method="ac")




# quiz
binom.confint(68, 100, method="ac")

binom.test(68, 100, 0.75)

dbinom(50, 100, 0.5)

binom.test(6101, 9821, 0.5)

binom.confint(4, 10, method="ac")
binom.test(4, 10, 0.2)

binom.test(4, 10, 0.3)

binom.confint(10, 200, method="ac")
423-35

sqrt(((35/423)*(388/423))/423)

xsuccesses <- c(40, 41, 42, 43, 44, 45)
probs <- dbinom(xsuccesses, 50, 0.5)
probs
sum(probs)
dbinom(40, 50, 0.5)
