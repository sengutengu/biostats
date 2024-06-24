# Ch. 12 Labs & Notes

titanicData <- read.csv("DataForLabs/titanic.csv", stringsAsFactors=T)
library(ggplot2)
library(car)

summary(titanicData)

# strip chart
ggplot(titanicData, aes(x=survive, y=age)) +
  geom_jitter(position=position_jitter(0.05)) +
  theme_minimal()

# multiple histograms
ggplot(titanicData, aes(x=age)) +
  geom_histogram() +
  facet_wrap(~ survive, ncol=1)
  # facet_wrap breaks up histogram accd to categorical var

# violin plots
ggplot(titanicData, aes(x=survive, y=age, fill=survive)) +
  geom_violin() +
  xlab("Survival") +
  ylab("Age") +
  theme_classic() +
  scale_fill_manual(values=c("gold", "darkred")) +
  stat_summary(fun.y=mean, geom="point", color="black")+
  theme(legend.position = "none") +
  theme(aspect.ratio=1)

# t test 
# age by survival (are ages different pending on survival)
# var.equal=T tells R that we assume variances for two samples are equal
t.test(age ~ survive, data=titanicData, var.equal=T)
# 95% CI gives 95 CI for difference between means

# Welch's t test 
# DOES NOT ASSUME EQUAL VARIANCE
# var.equal=F
t.test(age ~ survive, data=titanicData, var.equal=F)

# Paired t-test
blackbird <- read.csv("DataForLabs/chap12e2BlackbirdTestosterone.csv", stringsAsFactors=T)
str(blackbird)
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant) # WELCH's t-test
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired=T) # PAIRED t-test

#Levene's test
leveneTest(data=titanicData, age ~ survive, center=mean)
# p=0.04855 so reject the null that survived & died groups have same pop. variances.