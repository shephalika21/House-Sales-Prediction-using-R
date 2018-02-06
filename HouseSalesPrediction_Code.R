#read data
rm(list=ls())
getwd()
setwd("path to file")
housedata = read.table("kc_house_data.csv", header=TRUE, sep=",")
head(housedata)
str(housedata)


#Hypothesis testing
mydata
library(dplyr)
bedrooms=mydata$bedrooms
price=mydata$price
mydata_2bhk = mydata %>% filter(bedrooms==2)
mydata_2bhk = mydata_2bhk %>% sample_frac(0.1)
nrow(mydata_2bhk)
mydata_2bhk
price_2bhk = mydata_2bhk$price
mydata_3bhk = mydata %>% filter(bedrooms==3)
mydata_3bhk = mydata_3bhk %>% sample_frac(0.1)
nrow(mydata_3bhk)
mydata_3bhk
price_3bhk = mydata_3bhk$price
price_2bhk
price_3bhk
summary(price_2bhk)
summary(price_3bhk)
library(psych)
describe(price_2bhk)
describe(price_3bhk)

par(mfrow=c(1,2))
boxplot(price_2bhk, col="red", main="2 BHK")
boxplot(price_3bhk, col="blue", main="3 BHK")

#confidence interval for average price for 2 BHK
x2=mean(price_2bhk)
s2 = sd(price_2bhk)
n2=length(price_2bhk)
err2 = (qnorm(0.975)*s2)/sqrt(n2)
n2
left2 = x2 - err2
right2 = x2+err2
left2
right2

#confidence interval for average price for 3BHK
x3=mean(price_3bhk)
s3 = sd(price_3bhk)
n3=length(price_3bhk)
n3
err3 = (qnorm(0.975)*s3)/sqrt(n3)
left3 = x3 - err3
right3 = x3 + err3
left3
right3

library(BSDA)
z.test(price_2bhk,price_3bhk,alternative = "greater", mu=0, sigma.x = sd(price_2bhk), sigma.y = sd(price_3bhk), conf.level = 0.95)

#according to z test we accept null hypothesis that avg price of 2BHK is less than avg price of 3BHK
#checking the correctness of our hypothesis testing
mydata_2bhk = mydata %>% filter(bedrooms==2)
mean(mydata_2bhk$price)
mydata_3bhk = mydata %>% filter(bedrooms==3)
mean(mydata_3bhk$price) # actual mean of 3 BHK greater 

#one sample hypothesis testing
x=mydata$price
mean(x) # actual mean
price_data = mydata %>% sample_frac(0.1) # 10% sample data
price=price_data$price
describe(price)
length(price)
par(mfrow=c(1,1))
boxplot(price, col="red", main="House Prices")

#confidence interval
x4=mean(price)
s4 = sd(price)
n4 = length(price)
err4 = (qnorm(0.975)*s4)/sqrt(n4)
n4
left4 = x4 - err4
right4 = x4 + err4
left4
right4 # actual mean mean(x)=540088.1 lies in the confidence intervals

#one sample hypothesis testing alternative hypothesis: avg mean < 54000
z.test(price, NULL, alternative = "greater", mu=54000, sigma.x=sd(price), conf.level = 0.95 )

#installing required packages and loading it
install.packages("corrplot")
install.packages("caret")
library(corrplot)
library(caret)
library(ggplot2)
library(lubridate)

#column "id" and "date" are not useful for predicting house sales so removing them
mydata = housedata[,-1]
head(mydata)
#checking if any missing values
colSums(is.na(mydata))
colSums(is.na(housedata))
sum(is.na(mydata))
sum(is.na(housedata))

#converting date to numerical variable so that we can use it for correlation
# Changing date to yyyymmdd format
housedata$date<-(substr(housedata$date, 1, 8))
housedata$date<- ymd(housedata$date)
housedata$date<-as.numeric(as.Date(housedata$date))

#finding correlation between the variables
par(mfrow=c(1,1))
corhouse = cor(housedata)
corrplot(corhouse, type="full", method = "circle", main="Correlation")
corrplot(corhouse, method = "color", outline = T, cl.pos = 'n', rect.col = "blue",  tl.col = "blue", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green","white","purple"))(100))
print(round(corhouse, digits=2))


head(train.data)
nrow(train.data)
nrow(test.data)

#plot the data to see the correlation
par(mfrow=c(4,5))
plot(date, price, main="Price vs Date", ylab="price", xlab="date", col='steelblue')
plot(bedrooms, price, main="Price vs bedrooms", ylab="price", xlab="bedrooms", col='steelblue')
plot(bathrooms, price, main="Price vs bathrooms", ylab="price", xlab="bathrooms", col='steelblue')
plot(sqft_living, price, main="Price vs sqft_living", ylab="price", xlab="sqft_living", col='steelblue')
plot(sqft_lot, price, main="Price vs sqft_lot", ylab="price", xlab="sqft_lot", col='steelblue')
plot(floors, price, main="Price vs floors", ylab="price", xlab="floors", col='steelblue')
plot(waterfront, price, main="Price vs waterfront", ylab="price", xlab="waterfront", col='steelblue')
plot(view, price, main="Price vs view", ylab="price", xlab="view", col='steelblue')
plot(condition, price, main="Price vs condition", ylab="price", xlab="condition", col='steelblue')
plot(grade, price, main="Price vs grade", ylab="price", xlab="grade", col='steelblue')
plot(sqft_above, price, main="Price vs sqft_above", ylab="price", xlab="sqft_above", col='steelblue')
plot(sqft_basement, price, main="Price vs sqft_basement", ylab="price", xlab="sqft_basement", col='steelblue')
plot(yr_built, price, main="Price vs yr_built", ylab="price", xlab="yr_built", col='steelblue')
plot(yr_renovated, price, main="Price vs yr_renovated", ylab="price", xlab="yr_renovated", col='steelblue')
plot(zipcode, price, main="Price vs zipcode", ylab="price", xlab="zipcode", col='steelblue')
plot(lat, price, main="Price vs lat", ylab="price", xlab="lat", col='steelblue')
plot(long, price, main="Price vs long", ylab="price", xlab="long", col='steelblue')
plot(sqft_living15, price, main="Price vs sqft_living15", ylab="price", xlab="sqft_living15", col='steelblue')
plot(sqft_lot15, price, main="Price vs sqft_lot15", ylab="price", xlab="sqft_lot15", col='steelblue')

library(lubridate)
library(GGally)
library(ggplot2)
library(hydroGOF)
library(mvtnorm)
library(minqa)
library(car)

df1=housedata
# replace extreme values
print(subset(df1, df1$bedrooms > 10))
bed3 <- subset(df1,df1$bedrooms == 3)
print(tapply(bed3$sqft_living, bed3$bedrooms, mean))
df1[15871,4] = 3

# converting categorical variables to factors
df1$zipcode <- as.factor(df1$zipcode)
df1$grade <- as.factor(df1$grade)
df1$waterfront <- as.factor(df1$waterfront)
df1$floors <- as.factor(df1$floors)
df1$bedrooms <- as.factor(df1$bedrooms)
df1$view = as.factor(df1$view)
df1$condition = as.factor(df1$condition)

library(minqa)
library(car)

# Now Selecting 80% of data as sample from total 'n' rows of the data  
set.seed(100)
sample <- sample.int(n=nrow(df1), size = floor(0.80*nrow(df1)), replace = F)

# Splitting train and test data
train <- df1[sample, ]
test  <- df1[-sample, ]


install.packages("Metrics")
library(Metrics)

#Model 1: Elimination by p value and vif
full=lm(log(price)~date+bedrooms+bathrooms+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+long+sqft_living15+sqft_lot15, data=train)
summary(full)
y1=predict.glm(full,test)
y1=exp(y1)
y=test$price
rmse_1 = sqrt((y-y1)%*%(y-y1))/nrow(test)
rmse_1

vif(full)
par(mfrow = c(2, 2))
plot(full)

head(y1)
head(y)

# Another model using forward elimination
base=lm(log(price)~date,data=train)
model3=step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F)
model3 = lm(formula = log(price) ~ date + grade +   view + 
              waterfront + condition + yr_renovated + floors + sqft_above + 
              bedrooms + bathrooms + sqft_lot + sqft_living15 + long + 
              sqft_lot15 + yr_built, data = train)

summary(model3)
par(mfrow = c(2, 2))
plot(model3)

vif(model3)


y3=predict.glm(model3,test)
y=test$price
y3=exp(y3)
rmse_3 = sqrt((y-y3)%*%(y-y3))/nrow(test)
rmse_3 # Calculating RMSE

head(y3)
head(y) 

# Model 4
library(MASS)
modelAIC<-stepAIC(model1, direction="both")

summary(modelAIC)
par(mfrow = c(2, 2))
plot(modelAIC)

modelAIClog = lm(formula = log(price) ~ date + bedrooms + bathrooms + sqft_living + 
                   sqft_lot + floors + waterfront + view + condition + grade + 
                   sqft_above + yr_built + yr_renovated + zipcode + lat + long + 
                   sqft_living15 + sqft_lot15, data = train)
summary(modelAIClog)
par(mfrow = c(2, 2))
plot(modelAIClog)

yaic=predict.glm(modelAIClog,test)
y=test$price
yaic=exp(yaic)
rmse_aic = sqrt((y-yaic)%*%(y-yaic))/nrow(test)
rmse_aic

head(yaic)
head(y)


# Creating linear model
mod <- lm(price ~ sqft_living + grade + waterfront + view + condition +zipcode+bathrooms+bedrooms, data = train)
summary(mod)

# Looking at the summary, it seems bedrooms contribution are not significant with o
#ther considered features in the model
# Taking the bedroom out and creating another model mod1
mod1 <- lm(log(price) ~ sqft_living +grade + waterfront + view + condition +zipcode+bathrooms, data = train)
summary(mod1)

predP <- predict(mod1, test)
act_pred <- data.frame(obs=test$price, pred=predP)
defaultSummary(act_pred)

head(exp(predP))
head(test$price)
#Taking df1 and train and test(done above)

model1=lm(price~date+bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15, data=train)
summary(model1)

model2=lm(price~date+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15, data=train)
summary(model2) #adj R2 84%

base=lm(price~date,data=train)
model3=step(base, scope=list(upper=model1, lower=~1), direction="forward", trace=F)
summary(model3)
vif(model3)

model4=step(model1, direction="backward", trace=F)
summary(model4)
lm(formula = price ~ date + bedrooms + bathrooms + sqft_living + 
     sqft_lot + floors + waterfront + view + condition + grade + 
     sqft_above + yr_built + yr_renovated + zipcode + lat + long + 
     sqft_living15, data = train.data)

model5=step(base, scope=list(upper=model1, lower=~1), direction="both", trace=F)
summary(model5)

vif(model1) 

#lat and lon has vif value greater than 5 so removing them in below model
model6=lm(formula = log(price) ~ date  + lat + view + waterfront + 
            long + sqft_living15 + yr_renovated + sqft_above + bathrooms + 
            sqft_lot + sqft_lot15, data = train)
summary(model6)
vif(model6)

y1=predict.glm(model1,test)
y5=predict.glm(model5,test)
y=test$price

rmse_1 = sqrt((y-y1)%*%(y-y1))/nrow(test)
rmse_5 = sqrt((y-y5)%*%(y-y5))/nrow(test)

rmse_4
rmse_5

#Residual analysis

res4=rstandard(model6)
plot( fitted(model6), res4, main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

qqnorm(res4)
qqline(res4,col=2)

res1=rstandard(model1)

plot( fitted(model1), res1, main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

qqnorm(res1)
qqline(res1,col=2)

y6=predict.glm(model6,test)
y=test$price
rmse_7 = sqrt((y-y6)%*%(y-y6))/nrow(test)
rmse_7

head(exp(y7))
head(y)

y5=predict.glm(model5,test)
y=test$price
rmse_7 = sqrt((y-y5)%*%(y-y5))/nrow(test)
rmse_7

head(y5)
head(y)

model7=lm(formula = log(price) ~ date + grade + zipcode + sqft_living + waterfront + 
            view + condition + yr_renovated + sqft_above + floors + bedrooms + 
            bathrooms + sqft_living15 + sqft_lot +  yr_built, 
          data = train)
summary(model7)
vif(model7)

res7=rstandard(model7)
plot( fitted(model7), res7, main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

qqnorm(res7)
qqline(res7,col=2)

y7=predict.glm(model7,test)
y=test$price
rmse_7 = sqrt((y-y7)%*%(y-y7))/nrow(test)
rmse_7

head(exp(y7))
head(y)

influence.measures(model)

#########ANOVA!!!###################
boxplot(price~floors, xlab='floors',ylab='price', main='price vs floors', col=c("steelblue","grey"))

anova1=lm(price~floors) #floors=1 taken as baseline
summary(anova1)

floors = relevel(floors, ref=5) #floors=2.5 taken as baseline
anova2=lm(price~floors)
summary(anova2)
##############################################

#Time series
rm(list=ls())
setwd("E:/Data Analytics ITMD527/Project/data")
housedata = read.table("kc_house_data.csv", header=TRUE, sep=",")
library(dplyr)
head(housedata)
tail(housedata)

library(corrplot)
library(caret)
library(ggplot2)
library(lubridate)

housedata$date<-(substr(housedata$date, 1, 8))
housedata$date<- ymd(housedata$date)

library(tseries)
library(fBasics)
library(forecast)
library(zoo)

rt= log(housedata$price+1)
pricets=ts(rt,start =c(2014,05), end=c(2015,05), frequency=365)
start(pricets)
end(pricets)

par(mfcol=c(1,1))
pricelogts=log(pricets+1)
basicStats(rt)
basicStats(pricelogts)

hist(pricets, xlab="House Price", prob=TRUE)
# add approximating normal density curve
xfit<-seq(min(pricets),max(pricets),length=40)
yfit<-dnorm(xfit,mean=mean(pricets),sd=sd(pricets))
lines(xfit, yfit, col="blue", lwd=2)

#CREATE NORMAL PROBABILITY PLOT
#Plotting QQ Plot to analyze if data come from normal distribution or not:
#For normally distributed data, the points lie on the line.
qqnorm(pricets)
qqline(pricets, col = "indianred")

# use time series object to draw time plot indexed with time
plot(pricets, type='l', xlab='time', ylab='House Price Log Returns', main="Plot without differencing", col="steelblue")

plot(pricelogts, type='l', xlab='time', ylab='House Price Log Returns', col="steelblue")

plot(diff(pricelogts), xlab='time', ylab='House Price Log Returns', main="Plot after differencing", col="steelblue")
abline(reg=lm(dpricets~time(dpricets)))

jarque.bera.test(housedata$price) #price is coming from normal distribution
#According to Jarque-Bera test we get pvalue less than 2.2e-16 which refers to pvalue less than 0.05(alpha) which means that the distribution is normal. 
#This is consistent with the histogram and qq plots as we got normal distributed curve above with slight negative skewness.

#plots acf(correlogram)
#H0: Series is not correlated and autocorrelations of rt is zero
#H1: Series is correlated

acf(rt, plot=T, lag=20)
# plots pacfvalues up to lag 15.
pacf(rt, lag = 15)

# Ljung Box test to find serial correlation
Box.test(rt,lag=12,type = 'Ljung') 
Box.test(rt,lag=6,type = 'Ljung')

#Applying differencing and check the above 
dpricelogts = diff(pricelogts)
dpricets = diff(pricets)
basicStats(dpricelogts)

normalTest(dpricets, method=c("jb"))


hist(dpricets, xlab="Diff Log House Price Returns", prob=TRUE)
# add approximating normal density curve
xfit<-seq(min(dpricets),max(dpricets),length=40)
yfit<-dnorm(xfit,mean=mean(dpricets),sd=sd(dpricets))
lines(xfit, yfit, col="blue", lwd=2)

#CREATE NORMAL PROBABILITY PLOT
#Plotting QQ Plot to analyze if data come from normal distribution or not:
#For normally distributed data, the points lie on the line.
qqnorm(dpricets)
qqline(dpricets, col = 2)



#ACF plot - p value in (p,d,q)
acf(coredata(dpricets), lag=30) #MA q=1

# plots pacf values up to lag 15.
pacf(coredata(dpricets), lag = 30) #AR p=1

# COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 6
Box.test(dpricets,lag=6,type='Ljung')
# to Lag 12
Box.test(dpricets,lag=12,type='Ljung')

source("EACF.R")
EACF(dpricets) #p=1 and q=2
model_ARMA=arima(dpricets, order=c(1,0,2), method='ML', include.mean = T)
model_ARMA

library(forecast)
model_arima=auto.arima(coredata(dpricets), max.p = 20,max.q = 20 ,stationary = TRUE,ic = c("aic"), stepwise = TRUE)
model_arima

#AR Model
model_AR=arima(dpricets, order=c(1,0,0))
model_AR

#MA Model
model_MA=arima(dpricets, order=c(0,0,1))
model_MA #lowest AIC value

#Residual Analysis for MA model
tsdiag(model_MA)
names(model_MA)
plot(model_MA$residuals,type='l')
qqnorm(model_MA$residuals)
qqline(model_MA$residuals,col=2)

#Check whether residual is white noise or not
# by ACF/PACF plot
acf(coredata(model_MA$residual), plot=T, lag=20)
pacf(coredata(model_MA$residual), plot=T, lag=20)

#Check whether residual is white noise or not
# by LjungBox test
# Test if residuals are white noise using the Ljung-Box test for m=6, 12, and 18
Box.test(model_MA$resid, lag=6, type='Ljung') #Residuals are white noise
Box.test(model_MA$resid, lag=12, type='Ljung')
Box.test(model_MA$resid, lag=18, type='Ljung')

# COMPUTE PREDICTIONS
x.fore= predict(model_MA, n.ahead=30)
x.fore
# plot the forecasts
U = x.fore$pred+ 2*x.fore$se
L = x.fore$pred-2*x.fore$se
minx=min(dpricets,L)
maxx=max(dpricets,U)
ts.plot(dpricets, x.fore$pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

# Command to predict the model
p1=predict(model_MA, n.ahead=20, se.fit = T)
p1

fr=forecast(p1$pred)
plot(fr)

# Model fitting by automatically identify the value of the p
m2_AU=ar(dpricets, method='mle')
m2_AU

#Residual Analysis for model_AR model
tsdiag(model_AR)
names(model_AR)
plot(model_AR$residuals,type='l')
qqnorm(model_AR$residuals)
qqline(model_AR$residuals,col=2)

#Check whether residual is white noise or not
# by ACF/PACF plot
acf(coredata(model_AR$residual), plot=T, lag=20)
pacf(coredata(model_AR$residual), plot=T, lag=20)

#Check whether residual is white noise or not
# by LjungBox test
# Test if residuals are white noise using the Ljung-Box test for m=6, 12, and 18
Box.test(model_AR$resid, lag=6, type='Ljung') #Residuals are white noise
Box.test(model_AR$resid, lag=12, type='Ljung')
Box.test(model_AR$resid, lag=18, type='Ljung')

# COMPUTE PREDICTIONS
x.fore= predict(model_AR, n.ahead=30)
x.fore
# plot the forecasts
U = x.fore$pred+ 2*x.fore$se
L = x.fore$pred-2*x.fore$se
minx=min(dpricets,L)
maxx=max(dpricets,U)
ts.plot(dpricets, x.fore$pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

#Residual Analysis for model_ARMA model
tsdiag(model_ARMA)
names(model_ARMA)
plot(model_ARMA$residuals,type='l')
qqnorm(model_ARMA$residuals)
qqline(model_ARMA$residuals,col=2)

#Check whether residual is white noise or not
# by ACF/PACF plot
acf(coredata(model_ARMA$residual), plot=T, lag=20)
pacf(coredata(model_ARMA$residual), plot=T, lag=20)

#Check whether residual is white noise or not
# by LjungBox test
# Test if residuals are white noise using the Ljung-Box test for m=6, 12, and 18
Box.test(model_ARMA$resid, lag=6, type='Ljung') #Residuals are white noise
Box.test(model_ARMA$resid, lag=12, type='Ljung')
Box.test(model_ARMA$resid, lag=18, type='Ljung')

# COMPUTE PREDICTIONS
x.fore= predict(model_ARMA, n.ahead=30)
x.fore
# plot the forecasts
U = x.fore$pred+ 2*x.fore$se
L = x.fore$pred-2*x.fore$se
minx=min(dpricets,L)
maxx=max(dpricets,U)
ts.plot(dpricets, x.fore$pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

# Command to predict the model
p1=predict(model_ARMA, n.ahead=30)
p1

fr=forecast(model_ARMA$pred)
plot(fr)

