data<-read.csv(file.choose())
data$City1<-NULL
data$City2<-NULL
data$market_leading_airline<-NULL
data$Low_price_airline<-NULL
#data$market_share.1<-NULL
#data$Average_fare1<-NULL
names(data)
str(data)
attach(data)
data$Average_Fare<-as.numeric(data$Average_Fare)
data$Distance<-as.numeric(data$Distance)
data$Average_weekly_passengers<-as.numeric(data$Average_weekly_passengers)
data$market_share<-as.numeric(data$market_share)
data$Price<-as.numeric(data$Price)
data$market_share.1<-as.numeric(data$market_share.1)
data$Average_fare1<-as.numeric(data$Average_fare1)
str(data)

boxplot(data$Average_Fare)
boxplot(data$Distance) #no
boxplot(data$Average_weekly_passengers)
boxplot(data$Price)
boxplot(data$market_share) #no
boxplot(data$market_share.1) #no
boxplot(data$Average_fare1)

summary(Average_Fare)
upper<-197.39+1.5*IQR(Average_Fare);upper
Average_Fare[Average_Fare>upper]<-upper
boxplot(Average_Fare)

summary(Average_fare1)
upper<-202.98+1.5*IQR(Average_fare1);upper
Average_fare1[Average_fare1>upper]<-upper
boxplot(Average_fare1)

summary(Average_weekly_passengers)
upper<-769.9+1.5*IQR(Average_weekly_passengers);upper
Average_weekly_passengers[Average_weekly_passengers>upper]<-upper
boxplot(Average_weekly_passengers)

summary(Price)
upper<-168.53+1.5*IQR(Price);upper
Price[Price>upper]<-upper
boxplot(Price)

xyz<-data

library(caret)
Train<-createDataPartition(xyz$Price,p=0.70,list=FALSE)

training<-xyz[Train,]
testing<-xyz[-Train,]

cor(training)
model<-lm(Price~.,data=training)
summary(model)

library(car)
vif(model)
model<-lm(Price~.-Average_Fare,data=training)
summary(model)
hist(xyz$Price) 
hist((1/xyz$Price)) 
hist(log(xyz$Price)) 
model2<-step(lm(log(Price)~.-Average_Fare,data=training), direction="both")
summary(model2) 
vif(model2)
par(mfrow=c(2,2)) 
plot(model2)
library(lmtest) 
dwtest(model2) 
library(car) 
ncvTest(model2) 
testing$fitted<-predict(model2, testing)
testing$Original<-exp(testing$fitted)
#training$new_Price<-log(training$Price)


