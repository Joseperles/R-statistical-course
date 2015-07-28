library(plm)
crime <- read.delim("c:\\Projects\\r-studio\\R-statistical-course\\data\\nc_crime.csv")

head(crime)
class(crime)

# defininf the model
model <- lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban

# Pooled model
panel.ols <- plm(model, data=crime , model="pooling")
summary(panel.ols)

#Between effects reproduce exactly the data of the paper
panel.be<-plm(model, data=crime , model="between")
summary(panel.be)  #Baltagi column 1 table 7.1 page 133

#Fixed effect with time dummies
panel.fe<-plm(modelt, data=crime , model="within")
summary(panel.fe)   #Baltagi column 2 table 7.1 page 133
summary(fixef(panel.fe))

iv.fe<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="within")
summary(iv.fe)  #FE2SLS Baltagi Table 7.1 p.133 column 3

iv.be<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="between")
summary(iv.be)  #BE2SLS Baltagi Table 7.1 p.133 column 4

iv.re<-plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="random", inst.method="baltagi")
summary(iv.re) #BE2SLS Baltagi Table 7.1 p.133 column 5
