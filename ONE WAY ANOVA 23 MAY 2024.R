library(dplyr)
data <- PlantGrowth
head(data)
View(data)
levels(data$group)
data$group <- ordered(data$group, levels = c("ctrl", "trt1", "trt2"))
group_by(data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )


# Null hypothesis (H0): All group means are equal (no difference in effectiveness among the different groups of plants).

#(mean same toh different categories ka affect nahi pad rha)

# Alternative hypothesis (H1): At least one group mean is different (at least one plant group is more or less effective than the others).

# (mean different aaya toh cateegories ka affect par rha)

plot(weight ~ group, data = PlantGrowth)
res.aov <- aov(weight ~ group, data = data) 
#weight is independent and #group is dependent
summary(res.aov)



########################################
# When to reject , when to accept?###################

#   Reject the Null Hypothesis: Typically, if the p-value is less than the chosen significance level (often 0.05), you reject the null hypothesis. This doesn't mean you prove the alternative hypothesis (e.g., that the drug works), but rather that you have found sufficient evidence to suggest the null hypothesis (no effect) isn't likely true.
# 
# Fail to Reject the Null Hypothesis: If the p-value is greater than or equal to the significance level, you do not reject the null hypothesis or (we fail to reject the null hypotheses). You conclude that your sample data does not provide strong enough evidence to refute the null hypothesis, and any effects observed in your sample might be due to random variation.