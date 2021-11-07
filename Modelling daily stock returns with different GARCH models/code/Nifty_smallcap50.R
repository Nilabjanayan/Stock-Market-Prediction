library(PerformanceAnalytics)
library(astsa)
library(itsmr)
library(lubridate)
library(zoo)
library(randtests)
library(forecast)
library(urca)
library(aTSA)
library(ggplot2)
library(tsoutliers)
library(rugarch)
library(sgt)
library(quantmod) 
library(xts)
library(corrplot)
library(gridExtra)
#library(MuMIn)

setwd('/home/nilabja/Desktop/Time Series/project/code')

nfty50 <- read.csv('../data/NIFTY SMALLCAP 50_Data.csv')

nfty50 <- nfty50[,c(1,5)]

nfty50[,1] <- dmy(nfty50[,1])
par(mfrow = c(1,1))
#plot(nfty50$Close, ylab="Stock Prices",main="Figure : Closing prices of the stocks",type = 'l')

tso <- zoo(nfty50$Close, nfty50$Date)
plot(tso, xlab = 'Time', ylab="Stock Prices",main="Figure : Closing price of the stocks",type = 'l', col = 'blue')

#getSymbols("^NSMIDCP" ,from = "2020-03-23" ,to = "2021-09-24")
#chartSeries(NSMIDCP["2021-09"])

############################ Return ##################################
######################################################################
return <- log(tso[2:length(tso)]/(rev(nfty50$Close)[-length(nfty50$Close)]))
return_tso <- zoo(return, rev(nfty50$Date)[-1])
#ret = log(rev(nfty50$Close))
#return = -(ret[1:(length(ret)-1)] - ret[2:length(ret)])
Return=CalculateReturns(tso, method = 'log')
ggplot() + geom_line(mapping = aes(x = 1:length(return), y = return, colour = 'NIFTY 50')) +
  #  geom_line(mapping = aes(x = 1:length(returnNftnxt50), y = returnNftnxt50, colour = 'NFTY NEXT 50')) +
  scale_colour_manual('Stock', breaks = c("NIFTY 50"), # "NFTY NEXT 50"),
                      values = c("blue")) + #, "blue")) +
  scale_y_continuous("Log Returns") +
  labs(title = "Log Returns of the Closing Prices of NIFTY SMALLCAP 50", y = "Log Return of the Stock Prices", x = "Time points")

#plot(na.omit(return),main='Log Return of NIFTY 50 Stock', type = 'l')
#Return

chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))

x = rep(0, as.integer(length(return)/22))
for (i in seq(10,(length(return)),22)){
  m = Return[i-10:i+11]
  x[i] = sd(m)
}

acf2(x,max.lag=21)
a<- ggAcf(na.omit(as.vector(x)), col='red',main='Acf of volatility of NIFTY 50');
p<- ggPacf(na.omit(as.vector(x)),col='steelblue',main='PAcf of volatility of NIFTY 50')
grid.arrange(a,p, ncol = 2, nrow = 1)

## Check for unit root ###############################
#####################################
#### Augmented Dickey Fuller Test ###
#####################################

summary(ur.df(na.omit(as.vector(tso)),type='drift'))
summary(ur.df(na.omit(as.vector(return)),type='drift'))
#summary(ur.df(na.omit(as.vector(sq_Return)),type='drift'))

#####################################
# partial Auto Correlation Function #
#####################################
a<- ggAcf(na.omit(as.vector(return)), col='red',main='Acf of Log Return of NIFTY NEXT 50')
p<- ggPacf(na.omit(as.vector(return)),col='steelblue',main='PAcf of Log Return of NIFTY NEXT 50')
grid.arrange(a,p, ncol = 2, nrow = 1)

a<- ggAcf(na.omit(as.vector(sq_Return)), col='red',main='Acf of Square Absolute Return of NIFTY NEXT 50')
p<- ggPacf(na.omit(as.vector(sq_Return)),col='steelblue',main='PAcf of Square Absolute Return of NIFTY NEXT 50')
grid.arrange(a,p, ncol = 2, nrow = 1)

############## Remarks  #############
#Stylized Facts of Financial Data
#Distributions of Returns is not normal.
#Absence of significant auto correlation in returns.
#Slowly decreasing auto correlation in squared or absolute returns.
#Volatility clustering.
#####################################

###############################
#### Volatility Clustering ####
###############################
chart.RollingPerformance(na.omit(return),width = 22,FUN = 'sd.annualized',scale=252, main = 'Rolling 1 month Volatility')
chart.RollingPerformance(na.omit(return^2),width = 22,FUN = 'sd.annualized',scale=252, main = 'Rolling 1 month Volatility')

###############################
####### Normality Check #######
###############################
#ggplot(aes(as.vector(na.omit(Return))), data=na.omit(Return)) + geom_histogram(bins = 19,col='white',fill='blue') + ggtitle('Return of MSFt')
chart.Histogram(Return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
############## QQ Plot ###############
ggplot(mapping = aes(sample = as.vector(na.omit(Return)))) +
  stat_qq() +
  stat_qq_line(col='red') + ggtitle('QQ plot of Nifty Returns')

#######  Normality Test ##############
shapiro.test(na.omit(as.vector(Return)))

######################################
############## Box-Test ##############
######################################
Box.test(na.omit(as.vector(tso)), type = "Ljung-Box")
Box.test(na.omit(as.vector(return)), type = "Ljung-Box")

#########################################################################
############################ Optimum Garch Model ########################
#########################################################################
i=0
aic = 0
bic = 0
aicc = 0
NIFTY_p<-list()
NIFTY_q<-list()
NIFTY_P<-list()
NIFTY_Q<-list()
NIFTY_AIC<-list()
NIFTY_BIC<-list()
NIFTY_AICc<-list()

for (p in seq(0,5)){
  for (q in seq(0,5)){
    for (P in seq(0,5)){
      for (Q in seq(0,5)){
        try({
          k <- ugarchspec(mean.model = list(armaOrder=c(p,q)),
                          variance.model = list(model = 'sGARCH',
                                                garchOrder = c(P,Q)),distribution = 'sstd')
          r <- ugarchfit(spec = k, data= na.omit(return)) 
          n = length(return)
          m = p+q+P+Q
          AICind<-infocriteria(r)[1]
          BICind<-infocriteria(r)[2]
          AICcind<- AICind + ((2*m^2+2*m)/(n-m-1))
          if (aic>AICind){
            aic = AICind
            paic = p
            qaic = q
            Paic = P
            Qaic = Q}
          if (bic>BICind){
            bic = BICind
            pbic = p
            qbic = q
            Pbic = P
            Qbic = Q}
          if (aicc>AICcind){
            aicc = AICcind
            paicc = p
            qaicc = q
            Paicc = P
            Qaicc = Q}})
        i=i+1
        NIFTY_p[[i]]<-p
        NIFTY_q[[i]]<-q
        NIFTY_P[[i]]<-P
        NIFTY_Q[[i]]<-Q
        try({
          NIFTY_AIC[[i]]<-AICind
          NIFTY_BIC[[i]]<-BICind 
          NIFTY_AICc[[i]]<-AICcind
        })
        print(i)
      }
    }
  }
}

NIFTYMODELS<-data.frame(matrix(nrow=1296,ncol=7))
columns<-c("p","q","P","Q","AIC","BIC","AICc")
colnames(NIFTYMODELS)<-columns

NIFTYMODELS$pp<-as.character(NIFTY_p)
NIFTYMODELS$qq<-as.character(NIFTY_q)
NIFTYMODELS$PP<-as.character(NIFTY_P)
NIFTYMODELS$QQ<-as.character(NIFTY_Q)
NIFTYMODELS$AIC<-as.character(NIFTY_AIC)
NIFTYMODELS$BIC<-as.character(NIFTY_BIC)
NIFTYMODELS$AICC<-as.character(NIFTY_AICc)
View(NIFTYMODELS)
#write.csv(NIFTY_IT_MODELS,file = "niftyITtable.csv",sep=",")
#************************************

##################################################################################
################################# Prediction ####################################
##################################################################################


##################################################################################
## Model 1: Fit ARMA(3,3)-GARCH(5,1) model with Student t-distribution ########
##################################################################################
n50_garch_1 <- ugarchspec(mean.model = list(armaOrder=c(3,3)),variance.model = list(model = 'sGARCH',garchOrder = c(5,1)),distribution = 'sstd')
fit_garch_1 <- ugarchfit(spec = n50_garch_1, data= na.omit(return), out.sample = 20)
coef(fit_garch_1)
infocriteria(fit_garch_1)
plot(fit_garch_1,which='all')

#################### Forecast ###################

for_cast1 <-ugarchforecast(fit_garch_1,data=return,n.ahead=20,n.roll = 20)
for_cast1
par(mfrow=c(1,2))
plot(for_cast1,which=1)
plot(for_cast1,which=2)
#plot(for_cast1,which='all')

################### MSE #########################

a1 <- for_cast1@forecast
pred1 <- a1$seriesFor[,1] 
sum((return[(length(return)-19):length(return)] - pred1)^2)/20

##################################################################################
## Model 1_n: Fit ARMA(0,0)-GARCH(1,1) model with normal distribution ########
##################################################################################
#n50_garch_1_n <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model = 'fGARCH', submodel = 'GARCH',
#                                                                                    garchOrder = c(1, 1)),distribution = 'norm')
#fit_garch_1_n <- ugarchfit(spec = n50_garch_1, data= na.omit(Return))
#coef(fit_garch_1_n)
#fit_garch_1_n
#plot(fit_garch_1_n,which='all')

##################################################################################
## Model 2: Fit ARMA(0,0)-GARCH(1,1) model with Student t-distribution ########
##################################################################################
n50_garch_2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model = 'sGARCH',garchOrder = c(1, 1)),distribution = 'sstd')
fit_garch_2 <- ugarchfit(spec = n50_garch_2, data= na.omit(return), out.sample = 20)
coef(fit_garch_2)
infocriteria(fit_garch_2)
plot(fit_garch_2,which='all')

#################### Forecast ###################

for_cast2 <-ugarchforecast(fit_garch_2,data=return,n.ahead=20,n.roll = 20)
for_cast2
par(mfrow=c(1,2))
plot(for_cast2,which=1)
plot(for_cast2,which=2)
#plot(for_cast1,which='all')

################### MSE #########################

a2 <- for_cast2@forecast
pred2 <- a2$seriesFor[,1] 
sum((return[(length(return)-19):length(return)] - pred2)^2)/20






