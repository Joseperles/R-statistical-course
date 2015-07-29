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

# with dummy variables
modelSecond <- lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban + d82 + d83 + d84 + d85 + d86 + d87
  
#Fixed effect with time dummies
panel.fe <- plm(modelSecond, data=crime , model="within")
summary(panel.fe)   #Baltagi column 2 table 7.1 page 133
summary(fixef(panel.fe))

#
# Change the variable with additional "instrumental" influence
#

modelWithInstrumentsBasic <- plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="within")
summary(modelWithInstrumentsBasic)  #FE2SLS Baltagi Table 7.1 p.133 column 3

modelWithInstrumentsBE <- plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="between")
summary(modelWithInstrumentsBE)  #BE2SLS Baltagi Table 7.1 p.133 column 4

modelWithInstrumentsRandomVariables <- plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+lpctymle+lpctmin+west+central+urban+d82+d83+d84+d85+d86+d87|.-lprbarr -lpolpc + ltaxpc + lmix, data=crime, model="random", inst.method="baltagi")
summary(modelWithInstrumentsRandomVariables) #BE2SLS Baltagi Table 7.1 p.133 column 5
