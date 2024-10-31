install.packages(c("car", "ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2) 
library(ggpubr)
library(tidyverse)
library(broom) 
library(AICcmodavg)
library(car)
library(readr)

# Reading in tsv file
analgesics <- read_tsv("analgesics.txt")

# Visualizing the data
# Creating boxplots to visualize the pain levels of each drug category
boxplot(analgesics$Pain ~ analgesics$Drug,
        data = analgesics,
        main = "Pain level by Analgesic Drug",
        xlab = "Drug",
        ylab = "Pain",
        col = "steelblue",
        border = "black")

# Running one way ANOVA test
# H0: u1 = u2 = u3, alpha = 0.05
analgesic_1w <- aov(analgesics$Pain ~ analgesics$Drug)
summary.aov(analgesic_1w)

# The p-value of the test was found to be 0.000256 therefore we can reject the null hypothesis and report
# that there is a significant difference between the performace of the medications.

# Running TukeyHSD to determine which analgesic(s) had a different impacts on pain level
TukeyHSD(analgesic_1w)

# The test showed that B-A and C-A were significantly different with p-values of 0.0011 and 0.0006 respectively. 
# These groups also do not contain 0 in their confidence intervals which is another indicator of them being
# significantly different.

# Checking model assumptions
plot(analgesic_1w)

# Performing Levene test to test for equal variance
leveneTest(analgesics$Pain ~ analgesics$Drug, data = analgesics)

# Conclusion:
# A one way ANOVA test was performed to compare the effect of analgesic drugs on pain level.
# The test revealed that there was a significant difference between the drug and pain level (F-value = 11.91, p-value = 0.000256)
# The TukeyHSD test revealed that there was a significant difference between groups B-A (p-value = 0.0011107, 
# with a different of 2.1111 pain) and between groups C-A (p-value = 0.0006453, with a difference of 2.2222). There was no 
# significant difference between groups C-B (p-value = 0.9745). This is a strong indiction that group A results in 
# significantly different pain levels.