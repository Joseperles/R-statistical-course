#load libraries for time series data
library(forecast)
library(PerformanceAnalytics)
library(zoo)
library(tseries)


# Load data
Series <- read.csv("~/Series.csv")
head(Series)
class(Series)

# Declare data as temporary series.

Series<-ts(Series, frequency=1, start=1970)
head(Series)
class(Series)


#Plot series

Spain.mshare<-Series[,3]
Turkey.mshare<-Series[,52]
plot(Series[,3])
plot(Series[,52])
plot(Spain.mshare)
plot(Turkey.mshare)
chart.TimeSeries(Spain.mshare, main="Spain Tourism Market Share")
chart.TimeSeries(Turkey.mshare, main="Turkey Tourism Market Share")

plot(Series[,c(3,52)], main="Spain and Turkey Market Share")
mshare<-cbind(Spain.mshare,Turkey.mshare)
class(mshare)
plot(mshare, main="Spain and Turkey Market Share")

plot.zoo(mshare,col="blue", lwd=2, main="Spain and Turkey Market Share")

#Other graphs

#Panel plot of total servicolor sales
par(mfrow=c(1,2))
hist(mshare[,1], main="Spain", xlab="Market Share")
hist(mshare[,2], main="Turkey", xlab="Market Share")
boxplot(mshare[,1], main="Spain", xlab="Market Share")
boxplot(mshare[,2], main="Turkey", xlab="Market Share")
plot(density(mshare[,1], main="Spain", xlab="Market Share"))
plot(density(mshare[,2], main="Turkey", xlab="Market Share"))
qqnorm(mshare[,1], col="slateblue1", main="Spain")
qqline(mshare[,1])
qqnorm(mshare[,2], col="slateblue1", main="Turkey")
qqline(mshare[,2])


#Desscriptive statistics

table.Stats(mshare)


#Decompose a time series data

### decompose(mshare) ### Only for monthly data

#Estimating some univariate time series models

##AR models

Spain.ar1<-ar(mshare[,1])
Spain.ar1
Spain.ar1.ols<-ar(mshare[,1], method = "ols")
Spain.ar1.ols

#Estimating optimal models


fit.Spain <- auto.arima(mshare[,1])
fit.Spain
plot(forecast(fit.Spain,h=20))

fit.Turkey <- auto.arima(mshare[,2])
fit.Turkey
plot(forecast(fit.Turkey,h=20))


#Forecasting with forecast package
par(mfrow=c(1,1))
plot(forecast(mshare[,1]))
plot(forecast(mshare[,2]))

# Unit root tests with urca

library(urca)
adf.test(Spain.mshare)
adf.test(Turkey.mshare)

PP.test(Spain.mshare)
PP.test(Turkey.mshare)

kpss.test(Spain.mshare)
kpss.test(Turkey.mshare)

kpss.test(Spain.mshare, null=c("Trend"))
kpss.test(Turkey.mshare, null=c("Trend"))

# Unit root tests with fUnitroots

#ADF
library(fUnitRoots)
urdfTest(Spain.mshare, lags = 3, type = c("nc"), doplot = FALSE)
urdfTest(Spain.mshare, lags = 3, type = c("c"), doplot = FALSE)
urdfTest(Spain.mshare, lags = 3, type = c("ct"), doplot = FALSE)

#ERS
urersTest(Spain.mshare, type = c("DF-GLS"), model = c("trend"),lag.max = 4, doplot = FALSE)
urersTest(Spain.mshare, type = c("P"), model = c("trend"),lag.max = 4, doplot = FALSE)

#za structural change
urzaTest(Spain.mshare, model = c("both"), lag=1, doplot = FALSE)

#other test like lee strazicich for two structural breaks have not been implemented at the moment.

##Regression as usual

Spain.lm<-lm(CMERLIBTEN~time+timesq+precios+cemento+PIBSPA05+tcnespa+CAMASESPANA, data=Series)
summary(Spain.lm)
plot(Spain.lm)

#Robust standard error via sandwich.
library(car)
library(lmtest)
library(sandwich)
library(boot)

sandwich(Spain.lm)
coeftest(Spain.lm, vcov=vcovHAC(Spain.lm))

#Robust standard error via bootstrap.

set.seed(123)
boot.Spain.lm<-Boot(Spain.lm,f=coef,labels=names(coef(Spain.lm)), R=999, method=c("residual"))
summary(boot.Spain.lm)
boot.ci(boot.out=boot.Spain.lm, type="bca", index=2) 
boot.ci(boot.out=boot.Spain.lm, type="bca", index=3) 

#Granger causality
library(vars)
var.mshare <- VAR(mshare, p = 2, type = "const")
causality(var.mshare, cause = "Spain.mshare")
causality(var.mshare, cause = "Turkey.mshare")

#Other code could be fond for Toda-Yamamoto procedure etc...




