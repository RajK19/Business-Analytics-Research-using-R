# Two-Sample T Test (Independent)
# ===============================
lungCapData <- read.table(file.choose(), header=T, sep="\t")
levels(as.factor(lungCapData$Smoke))
boxplot(lungCapData$LungCap~lungCapData$Smoke)
t.test(lungCapData$LungCap~lungCapData$Smoke, mu=0, alt="less",paired=F,var.equal=F,conf.level=0.95)

# paired = F since the samples are independent of each other. If the samples are same or dependent on each other, then paired = T
# var.equal = value is dependent on the variance of the samples, need to check if they are equal/unequal
# Null Hypothesis (H0) -> mu=0

Two-Sample Dependent T Test (Paired T Test)
============================================
  bloodPressureData <- read.table(file.choose(), header=T, sep="\t")

# using boxplot
boxplot(bloodPressureData$Before,bloodPressureData$After)

# using scatter plot
plot(bloodPressureData$Before,bloodPressureData$After)
abline(a=0,b=1)

#H0: Mean difference in SBP is 0
# two-sided test

t.test(bloodPressureData$Before, bloodPressureData$After, mu=0, alt="two.sided",paired=T,conf.level=0.99)

One-Way ANOVA
==============
  dietData <- read.table(file.choose(), header=T, sep="\t")
levels(as.factor(dietData$Diet))
boxplot(dietData$WeightLoss~dietData$Diet)
# H0 - Mean Wight Loss is the same for all diets
aov(dietData$WeightLoss~dietData$Diet)
