getwd()

data = read.table('LungCapData.txt',header = TRUE)
data
head(data)

install.packages('caTools')
library(caTools)
split <- sample.split(data, SplitRatio = 0.7)
train <- subset(data, split = 'TRUE')
test <- subset(data, split = 'FALSE')

split
train
test


#create the model
#ctrl + shift + c


Model <- lm(data,data=train)
Model

summary(Model)


# Adjusted R-squared = 0.8532 or 85.32% of variation outcome variable affected by outcome variables age,height,smoke,gender, caesaerean

# age height and gendermale - these 3 have positive relationship
# smokeyes and caesaeran yes has negative relationship, because of negative values


# prediction
pred <- predict(Model, test)
pred

summary(pred)
