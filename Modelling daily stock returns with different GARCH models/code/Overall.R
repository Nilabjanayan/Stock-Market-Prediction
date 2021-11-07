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

setwd('/home/nilabja/Desktop/Time Series/project/code')

nfty50 <- read.csv('../data/NIFTY 50_Data.csv')
nftynxt50 <- read.csv('../data/NIFTY NEXT 50_Data.csv')
nftymidcap <- read.csv('../data/NIFTY MIDCAP 50_Data.csv')
nftysmallcap <- read.csv('../data/NIFTY SMALLCAP 50_Data.csv')

nfty50 <- nfty50[,c(1,5)]
nftynxt50 <- nftynxt50[,c(1,5)]
nftymidcap <- nftymidcap[,c(1,5)]
nftysmallcap <- nftysmallcap[,c(1,5)]
nfty50[,1] <- dmy(nfty50[,1])
nftynxt50[,1] <- dmy(nftynxt50[,1])
nftymidcap[,1] <- dmy(nftymidcap[,1])
nftysmallcap[,1] <- dmy(nftysmallcap[,1])
ggplot() + geom_line(mapping = aes(x = 1:length(nfty50$Close), y = rev(nfty50$Close), colour = 'NIFTY 50')) +
  geom_line(mapping = aes(x = 1:length(nftynxt50$Close), y = rev(nftynxt50$Close), colour = 'NIFTY NEXT 50')) +
  geom_line(mapping = aes(x = 1:length(nftymidcap$Close), y = rev(nftymidcap$Close), colour = 'NIFTY MIDCAP 50')) +
  geom_line(mapping = aes(x = 1:length(nftysmallcap$Close), y = rev(nftysmallcap$Close), colour = 'NIFTY SMALLCAP 50')) +
  scale_colour_manual('Different Stocks', breaks = c("NIFTY 50", "NIFTY NEXT 50", "NIFTY MIDCAP 50", "NIFTY SMALLCAP 50"),
                      values = c("red", "blue", "green", "black")) +
  scale_y_continuous("Stock Prices") +
  labs(title = "Closing Prices of Different Stocks", y = "Stock Prices", x = "Time points")

####################################################
################# Log Return #######################
####################################################

tso <- zoo(nfty50$Close, nfty50$Date)
returnNft50 <- log(tso[2:length(tso)]/(rev(nfty50$Close)[-length(nfty50$Close)]))
nxttso <- zoo(nftynxt50$Close, nftynxt50$Date)
returnNftnxt50 <- log(nxttso[2:length(nxttso)]/(rev(nftynxt50$Close)[-length(nftynxt50$Close)]))
midtso <- zoo(nftymidcap$Close, nftymidcap$Date)
returnNftmid50 <- log(midtso[2:length(midtso)]/(rev(nftymidcap$Close)[-length(nftymidcap$Close)]))
smalltso <- zoo(nftysmallcap$Close, nftysmallcap$Date)
returnNftsml50 <- log(smalltso[2:length(smalltso)]/(rev(nftysmallcap$Close)[-length(nftysmallcap$Close)]))
ggplot() + geom_line(mapping = aes(x = 1:length(returnNft50), y = returnNft50, colour = 'NIFTY 50')) +
  geom_line(mapping = aes(x = 1:length(returnNftnxt50), y = returnNftnxt50, colour = 'NIFTY NEXT 50')) +
  geom_line(mapping = aes(x = 1:length(returnNftmid50), y = returnNftmid50, colour = 'NIFTY MIDCAP 50')) +
  geom_line(mapping = aes(x = 1:length(returnNftsml50), y = returnNftsml50, colour = 'NIFTY SMALLCAP 50')) +
  scale_colour_manual('Different Stocks', breaks = c("NIFTY 50", "NIFTY NEXT 50", "NIFTY MIDCAP 50", "NIFTY SMALLCAP 50"),
                      values = c("red", "blue", "green", "black")) +
  scale_y_continuous("Log Returns of the Stock Prices") +
  labs(title = "Log Returns of the Closing Prices of Different Stocks", y = "Stock Prices", x = "Time points")

ggplot() + geom_line(mapping = aes(x = 1:length(tso), y = tso, colour = 'NIFTY 50')) +
  #  geom_line(mapping = aes(x = 1:length(returnNftnxt50), y = returnNftnxt50, colour = 'NFTY NEXT 50')) +
  scale_colour_manual('Stock', breaks = c("NIFTY 50"), # "NFTY NEXT 50"),
                      values = c("red")) + #, "blue")) +
  scale_y_continuous("Stock Price") +
  labs(title = "NIFTY 50 Data from March 23,2020 to Sep 24,2021", y = "Log Return of the Stock Prices", x = "Time points")

##################################################
################### Normality ####################
##################################################
par(mfrow = c(2,2))
chart.Histogram(returnNft50,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chart.Histogram(returnNftnxt50,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chart.Histogram(returnNftmid50,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chart.Histogram(returnNftsml50,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))


ggplot(mapping = aes(sample = as.vector(na.omit(returnNft50)))) +
  stat_qq() +
  stat_qq_line(col='red') #+ ggtitle('QQ plot of Nifty Returns')
ggplot(mapping = aes(sample = as.vector(na.omit(returnNftnxt50)))) +
  stat_qq() +
  stat_qq_line(col='red') #+ ggtitle('QQ plot of Nifty Returns')
ggplot(mapping = aes(sample = as.vector(na.omit(returnNftmid50)))) +
  stat_qq() +
  stat_qq_line(col='red') #+ ggtitle('QQ plot of Nifty Returns')
ggplot(mapping = aes(sample = as.vector(na.omit(returnNftsml50)))) +
  stat_qq() +
  stat_qq_line(col='red') #+ ggtitle('QQ plot of Nifty Returns')


#########################################################################
#################### Optimum Garch Model:Nifty50 ########################
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
          r <- ugarchfit(spec = k, data= na.omit(returnNft50), out.sample = 20) 
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

NIFTYMODELS$p<-as.character(NIFTY_p)
NIFTYMODELS$q<-as.character(NIFTY_q)
NIFTYMODELS$P<-as.character(NIFTY_P)
NIFTYMODELS$Q<-as.character(NIFTY_Q)
NIFTYMODELS$AIC<-as.character(NIFTY_AIC)
NIFTYMODELS$BIC<-as.character(NIFTY_BIC)
NIFTYMODELS$AICc<-as.character(NIFTY_AICc)
View(NIFTYMODELS)

write.csv(NIFTYMODELS,file = "niftymodelsGARCH.csv")
#************************************
#*
#########################################################################
#################### Optimum Garch Model:NiftyNext50 ########################
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
          r <- ugarchfit(spec = k, data= na.omit(returnNftsml50), out.sample = 20) 
          n = length(return)
          m = p+q+P+Q
          AICind<-infocriteria(r)[1]
          BICind<-infocriteria(r)[2]
          AICcind<- AICind + ((2*m^2+2*m)/(n-m-1))
          print(AICcind)
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
#View(NIFTYMODELS)
NIFTYMODELS<-data.frame(matrix(nrow=1296,ncol=7))
columns<-c("p","q","P","Q","AIC","BIC","AICc")
colnames(NIFTYMODELS)<-columns

NIFTYMODELS$p<-as.character(NIFTY_p)
NIFTYMODELS$q<-as.character(NIFTY_q)
NIFTYMODELS$P<-as.character(NIFTY_P)
NIFTYMODELS$Q<-as.character(NIFTY_Q)
NIFTYMODELS$AIC<-as.character(NIFTY_AIC)
NIFTYMODELS$BIC<-as.character(NIFTY_BIC)
NIFTYMODELS$AICc<-as.character(NIFTY_AICc)
View(NIFTYMODELS)

write.csv(NIFTYMODELS,file = "niftysmlmodelsGARCH.csv")
#*
####################################################
################# correlation ######################
x = cbind(nfty50$Close,nftynxt50$Close,nftymidcap$Close,nftysmallcap$Close)
cor = cor(x)
corrplot(cor, method = 'number')

Nifty50 = rev(nfty50$Close)
NiftyNext50 = rev(nftynxt50$Close)
NiftyMidcap = rev(nftymidcap$Close)
NiftySmallcap = rev(nftysmallcap$Close)
astsa::lag2.plot(Nifty50,NiftyNext50,11)
astsa::lag2.plot(Nifty50,NiftySmallcap,11)
