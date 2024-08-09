install.packages("interactionR")
install.packages("interactions")
library(readxl)
library(caTools)
library(psych)
library(dplyr)
library(interactions)
library(ggplot2)
getwd()
# setwd('C:\Documents')
getwd()

data = read_xlsx("CyberFraud_part2.xlsx")
# View(data)

#alpha_complete = alpha
#alpha_complete

head(data)

# MVP1 <- data.frame(data[,7])
# MVP1
# 
# alpha_MVP = psych::alpha(MVP1,check.keys=TRUE)
# alpha_MVP

###MVR
MVR <- data.frame(data[,8:12])
head(MVR)

alpha_MVR = psych::alpha(MVR, check.keys=TRUE)
alpha_MVR

MVR_new <- data.frame(MVR[,-3])
head(MVR_new)

alpha_MVR_new = psych::alpha(MVR_new)
alpha_MVR_new

##MVMP
MVMP <- data.frame(data[,13:17])
head(MVMP)

alpha_MVMP = psych::alpha(MVMP)
alpha_MVMP

##MVBA
MVBA <- data.frame(data[,18:20])
head(MVBA)

alpha_MVBA = psych::alpha(MVBA)
alpha_MVBA

MVBA_new <- data.frame(MVBA[,-3])
head(MVBA_new)

alpha_MVBA = psych::alpha(MVBA_new)  #here we increased alpha value
alpha_MVBA


##MVFF

MVFF <- data.frame(data[,21:24])
head(MVFF)

# ?alpha()
alpha_MVFF = psych::alpha(MVFF,check.keys=TRUE)
alpha_MVFF

##MVCK
MVCK <- data.frame(data[,25:26])
head(MVCK)

alpha_MVCK = psych::alpha(MVCK)
alpha_MVCK

##MVSF
MVSF <- data.frame(data[,27:29])
head(MVSF)

alpha_MVSF = psych::alpha(MVSF)
alpha_MVSF

##OVPB
OVPB <- data.frame(data[,30:33])
head(OVPB)

alpha_OVPB = psych::alpha(OVPB)
alpha_OVPB


# alpha_OVPB = alpha(OVPB)
# alpha_OVPB


####################### Reading Moderating variables ##################

# M1 = Age
M1 <- data.frame(data[,3])
M1

M1_data = M1%>%mutate(M1 = recode(M1, '20 - 40'= 0,'40 - 60'= 1))
M1_data

#M2 = Gender

M2 <- data.frame(data[,4])
M2

M2_data = M2%>%mutate(M2 = recode(M2, 'Male'=1,'Female'=0))
M2_data

#M3 = Income

M3 = data.frame(data[,5])
M3

M3_data = M3%>%mutate(M3 = recode(M3, '0 - 2.5'=0,'2.5 - 5'=1,'5 - 10'=2,'10 and above'=3))
M3_data

# ?recode()


#M4 - Education

M4 = data.frame(data[,6])
M4

M4_data = M4%>%mutate(M4 = recode(M4, "Master's degree"=1,"Bachelor's degree"=0))
M4_data



#### CALCULATING MEAN FOR EACH OF THE VARIABLES
MVR_new$mean = rowMeans(MVR_new)
MVMP$mean = rowMeans(MVMP)
MVBA$mean = rowMeans(MVBA)
MVFF$mean = rowMeans(MVFF)
MVCK$mean = rowMeans(MVCK)
MVSF$mean = rowMeans(MVSF)
OVPB$mean = rowMeans(OVPB)

MVR_new$mean
MVMP$mean
MVBA$mean
MVFF$mean
MVCK$mean
MVSF$mean
OVPB$mean
# creating a data frame combining all average column,moderating column and outcome column
MODERATING_DATA =data.frame(MVR_new$mean,MVMP$mean,MVBA$mean,MVFF$mean,MVCK$mean,MVSF$mean,M1_data,M2_data,M3_data,M4_data,OVPB$mean)
MODERATING_DATA

head(MODERATING_DATA)


############# LINEAR REGRESSION ############

split = sample.split(MODERATING_DATA, SplitRatio = 0.7)
train = subset(MODERATING_DATA, split = 'TRUE')
test = subset(MODERATING_DATA, split = 'FALSE')

Model = lm(MODERATING_DATA, data = train)
summary(Model)


model1 <- lm(OVPB.mean ~ MVR_new.mean * M1 ,MODERATING_DATA)
summary(model1)
interactions::interact_plot(model1, pred = MVR_new.mean, modx = M1)


model2 <- lm(OVPB.mean ~ MVR_new.mean * M2 ,MODERATING_DATA)
summary(model2)
interactions::interact_plot(model2, pred = MVR_new.mean, modx = M2)


model3 <- lm(OVPB.mean ~ MVR_new.mean * M3 ,MODERATING_DATA)
summary(model3)
interactions::interact_plot(model3, pred = MVR_new.mean, modx = M3)


model4 <- lm(OVPB.mean ~ MVR_new.mean * M4 ,MODERATING_DATA)
summary(model4)
interactions::interact_plot(model4, pred = MVR_new.mean, modx = M4)
# ?interact_plot

model5 <- lm(OVPB.mean ~ MVMP.mean * M1 ,MODERATING_DATA)
summary(model5)
interactions::interact_plot(model5, pred = MVMP.mean, modx = M1)


model6 <- lm(OVPB.mean ~ MVMP.mean * M2 ,MODERATING_DATA)
summary(model6)
interactions::interact_plot(model6, pred = MVMP.mean, modx = M2)


model7 <- lm(OVPB.mean ~ MVMP.mean * M3 ,MODERATING_DATA)
summary(model7)
interactions::interact_plot(model7, pred = MVMP.mean, modx = M3)


model8 <- lm(OVPB.mean ~ MVMP.mean * M4 ,MODERATING_DATA)
summary(model8)
interactions::interact_plot(model8, pred = MVMP.mean, modx = M4)


model9 <- lm(OVPB.mean ~ MVBA.mean * M1 ,MODERATING_DATA)
summary(model9)
interactions::interact_plot(model9, pred = MVBA.mean, modx = M1)


model10 <- lm(OVPB.mean ~ MVBA.mean * M2 ,MODERATING_DATA)
summary(model10)
interactions::interact_plot(model10, pred = MVBA.mean, modx = M2)


model11 <- lm(OVPB.mean ~ MVBA.mean * M3 ,MODERATING_DATA)
summary(model11)
interactions::interact_plot(model11, pred = MVBA.mean, modx = M3)


model12 <- lm(OVPB.mean ~ MVBA.mean * M4 ,MODERATING_DATA)
summary(model12)
interactions::interact_plot(model12, pred = MVBA.mean, modx = M4)


model13 <- lm(OVPB.mean ~ MVFF.mean * M1 ,MODERATING_DATA)
summary(model13)
interactions::interact_plot(model13, pred = MVFF.mean, modx = M1)


model14 <- lm(OVPB.mean ~ MVFF.mean * M2 ,MODERATING_DATA)
summary(model14)
interactions::interact_plot(model14, pred = MVFF.mean, modx = M2)


model15 <- lm(OVPB.mean ~ MVFF.mean * M3 ,MODERATING_DATA)
summary(model15)
interactions::interact_plot(model15, pred = MVFF.mean, modx = M3)


model16 <- lm(OVPB.mean ~ MVFF.mean * M4 ,MODERATING_DATA)
summary(model16)
interactions::interact_plot(model16, pred = MVFF.mean, modx = M4)


model17 <- lm(OVPB.mean ~ MVCK.mean * M1 ,MODERATING_DATA)
summary(model17)
interactions::interact_plot(model17, pred = MVCK.mean, modx = M1)


model18 <- lm(OVPB.mean ~ MVCK.mean * M2 ,MODERATING_DATA)
summary(model18)
interactions::interact_plot(model18, pred = MVCK.mean, modx = M2)


model19 <- lm(OVPB.mean ~ MVCK.mean * M3 ,MODERATING_DATA)
summary(model19)
interactions::interact_plot(model19, pred = MVCK.mean, modx = M3)


model20 <- lm(OVPB.mean ~ MVCK.mean * M4 ,MODERATING_DATA)
summary(model20)
interactions::interact_plot(model20, pred = MVCK.mean, modx = M4)


model21 <- lm(OVPB.mean ~ MVSF.mean * M1 ,MODERATING_DATA)
summary(model21)
interactions::interact_plot(model21, pred = MVSF.mean, modx = M1)


model22 <- lm(OVPB.mean ~ MVSF.mean * M2 ,MODERATING_DATA)
summary(model22)
interactions::interact_plot(model22, pred = MVSF.mean, modx = M2)


model23 <- lm(OVPB.mean ~ MVSF.mean * M3 ,MODERATING_DATA)
summary(model23)
interactions::interact_plot(model23, pred = MVSF.mean, modx = M3)


model24 <- lm(OVPB.mean ~ MVSF.mean * M4 ,MODERATING_DATA)
summary(model24)
interactions::interact_plot(model24, pred = MVSF.mean, modx = M4)

