## RLab 10 Practice Questions Key ##

## Question 1 ##

# A - Plot a multiple histogram showing cuckoo egg lengths by host species
cuckoo <- read.csv("DataForLabs/cuckooeggs.csv")
library(ggplot2)
ggplot(cuckoo, aes(x = egg_length)) + geom_histogram() + facet_wrap(~ host_species, ncol = 1)

# B - Calculate a table that shows the mean and standard deviation of length of cuckoo eggs for each host species
library(dplyr)
cuckoo_by_species <- group_by(cuckoo,host_species)
summarise(cuckoo_by_species, species_mean = mean(egg_length, na.rm=TRUE), species_sd = sd(egg_length, na.rm=TRUE))

# A tibble: 6 x 3
# host_species  species_mean species_sd
# <fct>                <dbl>      <dbl>
# 1 Hedge Sparrow         23.1      1.07 
# 2 Meadow Pipit          22.3      0.921
# 3 Pied Wagtail          22.9      1.07 
# 4 Robin                 22.6      0.685
# 5 Tree Pipit            23.1      0.901
# 6 Wren                  21.1      0.744

# C - Look at the graph and the table. For these data, would ANOVA be a valid method to test for differences between host species in the lengths of cuckoo eggs in their nests?
# The data from the multiple histogram look sufficiently normal, with somewhat similar spreads that ANOVA would be appropriate. 
# The table indicates that the standard deviations are very similar, which means that these data fit the equal variance assumption of ANOVA

# D - Use ANOVA to test for a difference between host species in the mean size of the cuckoo eggs in their nests. What is your conclusion?
cuckooANOVA <- lm(egg_length ~ host_species, data = cuckoo)
anova(cuckooANOVA)

# Analysis of Variance Table

# Response: egg_length
# Df Sum Sq Mean Sq F value    Pr(>F)    
# host_species   5 42.940  8.5879  10.388 3.152e-08 ***
#   Residuals    114 94.248  0.8267                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p-value is 3.152e-08, which is less than 0.05. Because the P-value is very small, we reject the null hypothesis of no differences in mean egg_length among the host species

# E - Assuming that ANOVA rejected the null hypotheses of no mean differences, use a Tukey-Kramer test to decide which pairs of host species are significantly different from each other in cuckoo egg mean length. What is your conclusion?
TukeyHSD(aov(cuckooANOVA))

# Tukey multiple comparisons of means
# 95% family-wise confidence level

# fit: aov(formula = cuckooANOVA)

# $host_species
# diff          lwr         upr     p adj
# Meadow Pipit-Hedge Sparrow -0.82253968 -1.629133605 -0.01594576 0.0428621
# Pied Wagtail-Hedge Sparrow -0.21809524 -1.197559436  0.76136896 0.9872190
# Robin-Hedge Sparrow        -0.54642857 -1.511003196  0.41814605 0.5726153
# Tree Pipit-Hedge Sparrow   -0.03142857 -1.010892769  0.94803563 0.9999990
# Wren-Hedge Sparrow         -1.99142857 -2.970892769 -1.01196437 0.0000006
# Pied Wagtail-Meadow Pipit   0.60444444 -0.181375330  1.39026422 0.2324603
# Robin-Meadow Pipit          0.27611111 -0.491069969  1.04329219 0.9021876
# Tree Pipit-Meadow Pipit     0.79111111  0.005291337  1.57693089 0.0474619
# Wren-Meadow Pipit          -1.16888889 -1.954708663 -0.38306911 0.0004861
# Robin-Pied Wagtail         -0.32833333 -1.275604766  0.61893810 0.9155004
# Tree Pipit-Pied Wagtail     0.18666667 -0.775762072  1.14909541 0.9932186
# Wren-Pied Wagtail          -1.77333333 -2.735762072 -0.81090459 0.0000070
# Tree Pipit-Robin            0.51500000 -0.432271433  1.46227143 0.6159630
# Wren-Robin                 -1.44500000 -2.392271433 -0.49772857 0.0003183
# Wren-Tree Pipit            -1.96000000 -2.922428738 -0.99757126 0.0000006

# Meadow Pipit-Hedge Sparrow, Wren-Hedge Sparrow, Tree Pipit-Meadow Pipit, Wren-Meadow Pipit, Wren-Pied Wagtail, Wren-Robin, and Wren-Tree Pipit are the pairs of host species that are significantly different
# The Meadow Pipit and Wren appear to be the species that are most different for the other host species in terms of egg length


## Question 2 ##
# A - Plot a multiple histogram to show the relationship between level of maize production and the incidence of malaria.
malaria <- read.csv("DataForLabs/malaria vs maize.csv")
library(ggplot2)
ggplot(malaria, aes(x = incidence_rate_per_ten_thousand)) + geom_histogram() + facet_wrap(~ maize_yield, ncol = 1)

# B - ANOVA is a logical choice of method to test differences in the mean rate of malaria between sites differing in level of maize production. Calculate the standard deviation of the incidence rate for each level of maize yield. Do these data seem to conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.
library(dplyr)
malaria_by_region <- group_by(malaria,maize_yield)
summarise(malaria_by_region, region_mean = mean(incidence_rate_per_ten_thousand, na.rm=TRUE), region_sd = sd(incidence_rate_per_ten_thousand, na.rm=TRUE))

# A tibble: 3 x 3
# maize_yield region_mean region_sd
# <fct>             <dbl>     <dbl>
# 1 High              234.      126. 
# 2 Low                17.7      17.1
# 3 Medium             95.4      41.4

# Looking at the multiple histogram, maize yield regions do not appear to be normally distributed
# Data table indicates that the sd of the regions is not similar, with more than 10x difference

# C - Compute the log of the incidence rate and redraw the multiple histograms for different levels of maize yield. Calculate the standard deviation of the log incidence rate for each level of maize yield. Does the log-transformed data better meet the assumptions of ANOVA than did the untransformed data?
malaria_by_region$ln.incidence <- log(malaria$incidence_rate_per_ten_thousand)
summarise(malaria_by_region, region_sd = sd(ln.incidence, na.rm=TRUE))

# A tibble: 3 x 2
# maize_yield region_sd
# <fct>           <dbl>
# 1 High            0.719
# 2 Low             1.13 
# 3 Medium          0.394

# The sd of the log-transformed incidence rates are more similar and meet the assumptions of ANOVA better

# D - Test for an association between maize yield and malaria incidence.
malariaANOVA <- lm(ln.incidence ~ maize_yield, data = malaria_by_region)
anova(malariaANOVA)

# Analysis of Variance Table

# Response: ln.incidence
# Df Sum Sq Mean Sq F value    Pr(>F)    
# maize_yield  2 29.488 14.7441  22.222 2.411e-05 ***
#  Residuals   16 10.616  0.6635                      
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(aov(malariaANOVA))

# Tukey multiple comparisons of means
# 95% family-wise confidence level

# Fit: aov(formula = malariaANOVA)

# $maize_yield
# diff        lwr        upr     p adj
# Low-High    -2.8949350 -4.0300404 -1.7598295 0.0000180
# Medium-High -0.7881599 -1.9863742  0.4100543 0.2365763
# Medium-Low   2.1067751  0.8340678  3.3794823 0.0016001

# Based on very low p-value for the ANOVA (p-value = 2.411e-05), we reject the null hypothesis of no differences in mean malaria incidence among the different maize yield regions
# Tukey-Kramer test indicates that there is a significant difference in malaria incidencce between Low-High and Medium-Low maize yield regions


## Question 3 ##

# A - Plot a histogram of each of the three groups. Do these data match the assumptions of an ANOVA?
circadian <- read.csv("DataForLabs/circadian mutant health.csv")
library(ggplot2)
ggplot(circadian, aes(x = days_to_death)) + geom_histogram() + facet_wrap(~ genotype, ncol = 1)

# The data does not appear to be normally distributed

# B - Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups of flies.
kruskal.test(days_to_death ~ genotype, data = circadian)

# Kruskal-Wallis rank sum test

# data:  days_to_death by genotype
# Kruskal-Wallis chi-squared = 41.736, df = 2, p-value = 8.653e-10

# Based on this output, the Kruskal-Wallis test strongly rejects the null hypothesis of equality of lifespan for the different genotypes/groups of flies






