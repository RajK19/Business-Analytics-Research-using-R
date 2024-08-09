library(readxl)
library(psych)

setwd('C:/Documents')
data = read_excel('Reliability Tutorial.xlsx')


getwd()
data

#View(data)
head(data)

install.packages("psych",dependencies=TRUE)

alpha_complete = alpha(data)
alpha_complete

BEN = data.frame(data[,1:8])
#View(BEN)
head(BEN)

BLN = data.frame(data[,9:13])
BPN = data.frame(data[,14:19])
BSA = data.frame(data[,20:24])
BSN = data.frame(data[,25:31])
PS = data.frame(data[,32:40])
PV = data.frame(data[,41:46])
RE = data.frame(data[,47:53])
SE = data.frame(data[,54:59])

#alpha_BEN
alpha_BEN = alpha(BEN)
alpha_BEN

#alpha_BLN
alpha_BLN = alpha(BLN)
alpha_BLN

#alpha_BPN
alpha_BPN = alpha(BPN)
alpha_BPN


BPN_New = data.frame(data[,14:18])
alpha_BPN_New = alpha(BPN_New)
alpha_BPN_New


alpha_BSA = alpha(BSA)
alpha_BSA


alpha_BSN = alpha(BSN)
alpha_BSN


alpha_PS = alpha(PS)
alpha_PS

alpha_PV = alpha(PV)
alpha_PV

#PV = data.frame(data_file$PV1,data_file$PV2,data_file$PV3,data_file$PV4,data_file$PV5)
#alpha(PV)
PV_new = data.frame(data$PV1,data$PV2,data$PV4,data$PV5, data$PV6)
alpha(PV_new)

alpha_SE = alpha(SE)
alpha_SE

cor(data$SE1,data$SE2)
cor(data$SE1,data$SE3)
cor(data$SE1,data$SE4)
cor(data$SE1, data$SE5)
cor(data$SE1, data$SE6)
cor(data$SE2, data$SE3)
cor(data$SE2, data$SE4)
cor(data$SE2, data$SE5)
cor(data$SE2, data$SE6)
cor(data$SE3, data$SE4)
cor(data$SE3, data$SE5)
cor(data$SE3, data$SE6)
cor(data$SE4, data$SE5)
cor(data$SE4, data$SE6)
cor(data$SE5, data$SE6)


alpha_SE = alpha(SE, check.keys = TRUE)
alpha_SE

