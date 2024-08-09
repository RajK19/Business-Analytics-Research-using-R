dietdata = read.table('DietWeigthLoss.txt', header = T)
dietdata


# categorical variable & categorical variable ---> chi square
# cont - cat ---> logisitc regression
# cat - cont ----> T-test/ anova
# cont - contt ----> Linear regression
#                                         Dependent y variable
# +-----------------------------+----------------------+-----------------------------------+
#     |   Relationship Type     | Categorical variable    | conitnuous variable         |
#     +-----------------------------+----------------------+-----------------------------------+
#     | Categorical   | chi square test                   | T -test / anova                |
#     +-----------------------------+----------------------+-----------------------------------+
#     | Continuous    | logistic regression         | Linear Regression              |
#   
#   independent x variable


# dietData <- read.table(file.choose(), header=T, sep="\t")
levels(as.factor(dietdata$Diet))
boxplot(dietdata$WeightLoss~dietdata$Diet)
# H0 - Mean Wight Loss is the same for all diets
aov(dietdata$WeightLoss~dietdata$Diet)


analysis_of_var = aov(dietdata$WeightLoss~dietdata$Diet)
summary(analysis_of_var)


ab=subset(dietdata, Diet %in% c("A","B"))
# ?subset
t.test(ab$WeightLoss~ab$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)

bc=subset(dietdata, Diet %in% c("B","C"))
t.test(bc$WeightLoss~bc$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)

cd=subset(dietdata, Diet %in% c("C","D"))
t.test(cd$WeightLoss~cd$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)

ad=subset(dietdata, Diet %in% c("A","D"))
t.test(ad$WeightLoss~ad$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)

ac=subset(dietdata, Diet %in% c("A","C"))
t.test(ac$WeightLoss~ac$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)

bd=subset(dietdata, Diet %in% c("B","D"))
t.test(bd$WeightLoss~bd$Diet, mn=0,alt='less',paired=F,var.equal = F,conf.level = 0.95)



            Interpretation
            
# a --> b 0.6143>0.05
# a --> c 0.0002965 < 0.005 

## Refer photo sent on whatsapp by mkour

# H0 : Mean Length for different combinations of supp. and dose are equal
# Ha : Mean length for different combinations of supp. and dose are not equal


