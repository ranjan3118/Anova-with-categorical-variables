# 27th April

#Anova with categorical variables
#reference data file:data.csv

#import
data<-read.csv("C:\\Users\\ayush\\OneDrive\\Desktop\\R working directory\\R files - 5 April\\data - r 5 april.csv", header = T, sep=",")
View(data)
names(data)
str(data)
mode(data$Brands) 
as.factor(data$Brands) #to change the format of the variable

length(data$Brands)
summary(data)
boxplot(data$Mileage)
boxplot(data$Mileage~data$Brands, col=rainbow(4))
#boxplot is best to understand outlier cases, easily seen in boxplot

#to find the statistics of the box plot
??boxplot.stats
boxplot.stats(data$Mileage[data$Brands=="CEAT"])
#outlier is 41.05
boxplot.stats(data$Mileage[data$Brands=="Apollo"])
boxplot.stats(data$Mileage)

# to find brand-wise average Mileage
tapply(data$Mileage, data$Brands, mean)
tapply(data$Mileage, data$Brands, var)
tapply(data$Mileage, data$Brands, sd)

#it allows to segregate data for cat variable, cat variable will always come in the middle
model1<-aov(data$Mileage~data$Brands)
summary(model1)
#Fvalue = mean sum square/ residual sum square , Pr(>Fvalue) <0.05, hence reject null
#Df Sum Sq Mean Sq F value   Pr(>F)    
#data$Brands  3  256.3   85.43   17.94 2.78e-08 ***
  #Residuals   56  266.6    4.76  

#TO IDENTIFY AND REMOVE OUTLIER 
install.packages("DescTools")
library(DescTools)
outlier(data$Mileage, method = c("boxplot"), value = TRUE, na.rm = FALSE)
outlier(data$Mileage[data$Brands=="CEAT"] value = TRUE, na.rm = TRUE)

install.packages("outliers")
library(outliers)
b<-rm.outliers(data$Mileage, fill=F, median=F, opposite=F)
length(b)
#COME BACK TO THIS LATER

#regression  model

#table 7.6
data<-read.table("C:\\Users\\ayush\\OneDrive\\Desktop\\R working directory\\Table 7.6.txt", header = T, sep = "", skip=10)
#skip to skip any junk info
View(data)
names(data)

#to edit the data frame

names(data)<-c("year", "qtysold", "roseprice", "nationprice", "dpi", "timetrend" )
#changing the original data's variable for ease
View(data)

attach(data)
#two varaible model
summary(model1<-lm(qtysold~roseprice))

#Reject the null of coefficient is zero, H0: B2=0)

model2<-lm(qtysold~.-year, data=data)
summary(model2)

#. means all the variables in the data
#Check out Step wise regression (Backward and Forward method)

full.model<-lm(qtysold~qtysold+roseprice+carnationprice+dpi+timetrend)
summary(full.model)

#step wise regression
#AIC - Alkyanie Information Criteria, Maximum Likelihood - extent of the loss of info
#lesser the loss of info, better the model
step(full.model, direction = "backward")

#main agenda is to get to the lowest AIC

#forward step regression
min.model<-lm(qtysold~1)
fwd.model<-step(min.model, direction = "forward", scope = (~qtysold+roseprice+carnationprice+dpi+timetrend))


model3<-lm(qtysold~roseprice+timetrend)
summary(model3)
plot(model3)
install.packages("car")
library(car)
outlierTest(model3)

b<-rm.outlier(data, fill=F, median=F, opposite = F)
length(b)
length(b$qtysold)
dim(data)
dim(b)

model4<-lm(qtysold~roseprice+timetrend, data=b)
summary(model4)
#mean residual always 0 in OLS
#read diagnostic methods which ma'am sent. (VERY VERY IMPORTANT)

# functional forms
logdata<-log(b)
model4<-lm(qtysold~roseprice+timetrend, data=logdata)
summary(model4)

model4<-lm(qtysold~poly(roseprice,2)+timetrend, data=b)
summary(model4)

model4<-lm(qtysold~roseprice*carnationprice+timetrend, data=log(b))
summary(model4)
