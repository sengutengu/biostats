# Jolly Rancher Chi-Square Goodness oF Fit Test
# https://www.reddit.com/r/mildlyinteresting/comments/1d6lxdd/uneven_distribution_of_jolly_ranchers_in_2_400g/

grape <- 4
cherry <- 8
gapple <- 12
watermelon <- 24
raspberry <- 20

count <- c(grape, cherry, gapple, watermelon, raspberry)
expected_value <- sum(count)/length(count)

expected_value
Xsq <- sum(((count-expected_value)^2)/expected_value)
Xsq
df <- length(count)-1
pchisq(Xsq, df, lower.tail=F) # p=0.0004487

# verify results
result <- chisq.test(count, p=rep(0.2, 5))
result
