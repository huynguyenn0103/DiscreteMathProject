#-----------------Part 1(Import data)------------------#
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
#HNX Index
data1 <- read_excel("C:/Users/HP/Desktop/R_programming/data10.xlsx")

#SLS(f&b_section)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data9.xlsx")
#OCH(tourism_section)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data8.xlsx")
#EVS(financial_section)
data1 <- read_excel("C:/Users/HP/Desktop/R_programming/data1.xlsx")

#CLM(top1_highest_growth)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data3.xlsx")
#VLA(top2_highest_growth)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data4.xlsx")
#SGD(top3_highest_growth)
data1 <- read_excel("C:/Users/HP/Desktop/R_programming/data5.xlsx")

#THD(top1_highest_drop)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data2.xlsx")
#SHS(top2_highest_drop)
data1 <- read_excel("C:/Users/HP/Desktop/R_programming/data6.xlsx")
#ART(top3_highest_drop)
data2 <- read_excel("C:/Users/HP/Desktop/R_programming/data7.xlsx")

View(data1) 

#-----------------Part 2(Cleaning data)------------------#
install.packages("tidyr")
library("tidyr")
apply(is.na(data1), 2, which)

#-----------------Part 3(Remove outliers + Check for normality)-----------------#
quartiles <- quantile(data1$Price, probs = c(.25,.75), na.rm = TRUE)
IQR <- IQR(data1$Price)
Lower <- quartiles[1] -1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
standard_data <- subset(data1, data1$Price > Lower & data1$Price <Upper)

quartiles <- quantile(data2$Price, probs = c(.25,.75), na.rm = TRUE)
IQR <- IQR(data2$Price)
Lower <- quartiles[1] -1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
standard_data2 <- subset(data2, data2$Price > Lower & data2$Price <Upper)

#-------------------------------------------------------------------------------#
library(pastecs)
par(mfrow = c(1,2))
par(mfrow = c(1,1))
install.packages("moments")
library(moments)
d <-density(standard_data$Price)
plot(d, main = "Closing Price")
polygon(d,col = "red", border = "blue")
skewness(standard_data$Price)

qqnorm((standard_data$Price),main = "Normal Q-Q Plot of price")
qqline((standard_data$Price))




#----------------Part 4(Data visualization)---------------#
par(mfrow = c(1,3))
scaling_data <- standard_data
scaling_data <- as.data.frame(sapply(data1, function(x) (x-mean(x))/sd(x)))
scaling_data2 <- as.data.frame(sapply(data2, function(x) (x-mean(x))/sd(x)))
install.packages("corrplot")
library("corrplot")
M <-cor(scaling_data)
corrplot(M ,method = 'color')
#----------------Part 5(multiple linear regression model)--------#
options(scipen=100)
model <- lm(Price~Low + High + Vol + Open + Avg, data =scaling_data)
summary(model)
plot(model)

#--------------Predict and compare with actual value------------#

plot(predict(model),scaling_data2$Price,xlab = "predict value", ylab = "actual value")
abline(a=0,b=1)
  par(mfrow = c(2,2))

