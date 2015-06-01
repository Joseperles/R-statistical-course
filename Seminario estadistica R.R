Datos <- read.csv("~/GitHub/seminarioestadistica/Datos.csv")

#Exploring file
class(Datos)
dim(Datos)
str(Datos)
head(Datos)

#Exploratory analysis Base package for basic plots

#Plot Historgrams

hist(Datos$Pob)
hist(Datos$Pob, breaks=50)

hist(Datos$Sup)
hist(Datos$Acomin)
hist(Datos$Acomin, breaks=50)

#Boxplot
boxplot(Datos$Pob)
boxplot(Datos$Sup)
boxplot(Datos$Acomin)

#Create variable residential

hist(Datos$porvpal)
Datos$residential[Datos$porvpal<50]<- "residential"
Datos$residential[Datos$porvpal>=50]<- "holiday"
head(Datos$residential)


Datos$IDC<-(Datos$Acomin / Datos$Pob)*1000

#Boxplot by category
boxplot(Datos$Pob~Datos$residential, data=Datos)
boxplot(Datos$IDC~Datos$residential, data=Datos)

#Create an histogram by category with base.

par(mfrow=c(1,2))
residential<-subset(Datos, Datos$residential=="residential")
holiday<-subset(Datos, Datos$residential=="holiday")
hist(residential$IDC)
hist(holiday$IDC)
plot(density(residential$IDC), main="Residential")
plot(density(holiday$IDC), main="Holiday")

#Load ggplot2 library for better graphs.

library(ggplot2)
ggplot(data=Datos)+geom_histogram(aes(x=IDC))
ggplot(data=Datos, aes(x=IDC))+geom_histogram()+facet_wrap(~residential)
ggplot(data=Datos, aes(y=IDC, x=residential))+geom_violin()

#Descriptive data via psych
library(psych)
describe(Datos)
describe(Datos$IDC)
describeBy(x=Datos$IDC, group=Datos$residential)

#Descriptive data via plyr
library(plyr)
aggregate(IDC~residential, Datos, FUN=function(x) c(mean=mean(x), var=var(x)))
aggregate(IDC~residential+Nivec04, Datos, FUN=function(x) c(mean=mean(x), var=var(x)))

#t.test (one sample)

t.test(Datos$IDC, alternative="two.sided", mu=20)
t.test(Datos$IDC, alternative="greater", mu=27)

#two sample t.test residential versus holiday
aggregate(IDC~residential, Datos, FUN=function(x) c(mean=mean(x), var=var(x))) #evaluate variance
shapiro.test(Datos$IDC) #evaluate normality
shapiro.test(Datos$IDC[Datos$residential=="residential"]) #evaluate normality residential group
shapiro.test(Datos$IDC[Datos$residential=="holiday"]) #evaluate normality residential group

#We use the non-parametric Ansari-Bradley tests to examine the equality of variances

ansari.test(IDC~residential, Datos)

# The test indicates that the variances are equal, meaning we can use the standard two-sample t.test

t.test(IDC~residential, data=Datos, var.equal=TRUE)

#Plotting the confidence interval

IDCsummary<-ddply(Datos, "residential", summarize,
                  IDC.mean=mean(IDC), IDC.sd=sd(IDC),
                  Lower=IDC.mean-2*IDC.sd/sqrt(NROW(Datos)),
                  Upper=IDC.mean+2*IDC.sd/sqrt(NROW(Datos)))

IDCsummary

ggplot(IDCsummary, aes(x=IDC.mean, y=residential))+geom_point()+geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)


#ANOVA

par(mfrow=c(1,1))
boxplot(Datos$IDC~Datos$Nivec04)
class(Datos$Nivec04)
Datos$Nivec04<-as.factor(as.character(Datos$Nivec04))
ggplot(Datos, aes(y=IDC, x=Nivec04))+geom_boxplot()

IDCanova<-aov(IDC~Nivec04 -1, Datos)  #without intercept
summary(IDCanova)

IDCanova.intercept<-aov(IDC~Nivec04, Datos)
summary(IDCanova.intercept)


# Linear Regression

ggplot(Datos, aes(x=Pob, y=Acomin))+geom_point()+geom_smooth(method="lm")+labs(x="Population", y="Retail")
ggplot(Datos, aes(x=Pob, y=IDC))+geom_point()+geom_smooth(method="lm")+labs(x="Population", y="Retail Density")

Datos$LIDC<-log(Datos$IDC)
Datos$LPob<-log(Datos$Pob)

ggplot(Datos, aes(x=LPob, y=LIDC))+geom_point()+geom_smooth(method="lm")+labs(x="Population", y="Log Retail Density")


# Set color by residential
ggplot(Datos, aes(x=LPob, y=LIDC, color=residential)) + geom_point(shape=1)

# Same, but with different colors and add regression lines
ggplot(Datos, aes(x=LPob, y=LIDC, color=residential)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region


# Set shape by cond
ggplot(Datos, aes(x=LPob, y=LIDC, shape=residential)) + geom_point()

# Same, but with different shapes
ggplot(Datos, aes(x=LPob, y=LIDC, shape=residential)) + geom_point() +
  scale_shape_manual(values=c(1,2))  # Use a hollow circle and triangle

LIDClm<-lm(LIDC~LPob, data=Datos)
summary(LIDClm)

Extended.LIDClm<-lm(LIDC~LPob+residential+Nivec04+Sup, data=Datos)
summary(Extended.LIDClm)


library(coefplot)
coefplot(LIDClm)
coefplot(Extended.LIDClm)

AIC(LIDClm, Extended.LIDClm)
BIC(LIDClm, Extended.LIDClm)


# Same by using glm package for using crossvalidation


LIDCglm<-glm(LIDC~LPob, data=Datos, family=gaussian(link="identity"))
summary(LIDCglm)
Extended.LIDCglm<-glm(LIDC~LPob+residential+Nivec04+Sup, data=Datos, family=gaussian(link="identity"))
summary(Extended.LIDCglm)

library(boot)

LIDCglmCV<-cv.glm(Datos,LIDCglm,K=5)
Extended.LIDCglmCV<-cv.glm(Datos,Extended.LIDCglm,K=5)
cvResults<-as.data.frame(rbind(LIDCglmCV$delta,Extended.LIDCglmCV$delta))
names(cvResults)<-c("Error", "Adjusted.Error")
cvResults

#Robust standard error via sandwich.
library(car)
library(lmtest)
library(sandwich)
library(boot)

robust.Extended.LIDClm<-lm(LIDC~LPob+residential+Nivec04+Sup, data=Datos)
sandwich(robust.Extended.LIDClm)
coeftest(robust.Extended.LIDClm, vcov=vcovHC(robust.Extended.LIDClm,type="HC1"))

#Robust standard error via bootstrap.

set.seed(123)
boot.Extended.LIDClm<-Boot(Extended.LIDClm,f=coef,labels=names(coef(Extended.LIDClm)), R=999, method=c("case"))
summary(boot.Extended.LIDClm)
boot.ci(boot.out=boot.Extended.LIDClm, type="bca", index=2) 
boot.ci(boot.out=boot.Extended.LIDClm, type="bca", index=3) 


 















