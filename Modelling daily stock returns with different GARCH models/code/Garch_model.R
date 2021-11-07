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
library("gridExtra")
library(rugarch)

nfty50 <- read.csv('../data/NIFTY 50_Data.csv')

nfty50 <- nfty50[,c(1,5)]

nfty50[,1] <- dmy(nfty50[,1])
par(mfrow = c(1,1))
#plot(nfty50$Close, ylab="Stock Prices",main="Figure : Closing prices of the stocks",type = 'l')
nfty50
tso <- zoo(nfty50$Close, nfty50$Date)
plot(tso)
####################
#####log return#####
####################
Return=CalculateReturns(tso, method = 'log')
plot(na.omit(Return),main='Log Return of NIFTY-50')
Return

sq_return <- Return^2
plot(na.omit(sq_Return),main='Squared Log Return of NIFTY-50')
Return

#####################################
#### Augmented Dickey Fuller Test ###
#####################################
#adf.test(na.omit(as.vector(tso)))
summary(ur.df(as.vector(tso),type='drift'))
#adf.test(na.omit(as.vector(Return)))
summary(ur.df(na.omit(as.vector(Return)),type='drift'))
summary(ur.df(na.omit(as.vector(sq_Return)),type='drift'))
#####################################
##### Auto Correlation Function #####
#####################################
acf(na.omit(as.vector(Return)), lag.max = 40, main='ACF of Return Values',col='red')
#Return

#####################################
# partial Auto Correlation Function #
#####################################
pacf(na.omit(as.vector(Return)), lag.max = 40, main='PACF of Return Values',col='blue')
############## Remarks  #############
#Stylized Facts of Financial Data
#Distributions of Returns is not normal.
#Absence of significant auto correlation in returns.
#Slowly decreasing auto correlation in squared or absolute returns.
#Volatility clustering.
#####################################

######Skewness Kurtois ##############
ggplot(aes(as.vector(na.omit(Return))), data=na.omit(Return)) + geom_histogram(bins = 100,col='black',fill='red') + ggtitle('Return of MSFt')
skewness((as.vector(na.omit(Return)))); kurtosis((as.vector(na.omit(Return))))
############## QQ Plot ##############
ggplot(data=nfty50, aes(sample = as.vector(Return))) +
  stat_qq() +
  stat_qq_line(col='red') + ggtitle('QQ plot of Nifty Returns')
#######  Normality Test ##############
#jarque.bera.test(na.omit(as.vector(Return)))
shapiro.test(na.omit(as.vector(Return)))

######################################
Box.test(na.omit(as.vector(Return)), type = "Ljung-Box")
######################################

#Absolute Return or Squared of Return are auto correlated.

a<- ggAcf(abs(na.omit(as.vector(Return))), col='red',main='Acf of Absolute Return of NIFTY')
p<- ggPacf(abs(na.omit(as.vector(Return))),col='steelblue',main='PAcf of Absolute Return of NIFTY')
grid.arrange(a,p, ncol = 2, nrow = 1)


c <- ggAcf(na.omit(as.vector(Return))^2, lag.max = 40, col='red', main='ACF of squared Return Values')
d<- ggPacf(na.omit(as.vector(Return))^2,lag.max = 40, col='steelblue',main= 'PACF of squared Return Values')
grid.arrange(c,d, ncol = 2, nrow = 1)

###############################
#### Volatility Clustering ####
###############################
chart.RollingPerformance(na.omit(Return),width = 22,FUN = 'sd.annualized',scale=252, main = 'Rolling 1 month Volatility')


##################################################################################
################################# GARCH Model ####################################
##################################################################################

##################################################################################
## Model 1: Fit ARMA(0,0)-gjrGARCH(1,1) model with Student t-distribution ########
##################################################################################

n50_garch_1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model = 'fGARCH', submodel = 'GARCH',
                                                                                    garchOrder = c(1, 1)),distribution = 'std')
fit_garch_1 <- ugarchfit(spec = n50_garch_1, data= na.omit(Return))
coef(fit_garch_1)
plot(fit_garch_1,which='all')


##################################################################################
## Model 2: Fit ARMA(1,1)-gjrGARCH(1,1) model with Student t-distribution ########
##################################################################################
#egarch
n50_garch_2 <- ugarchspec(mean.model = list(armaOrder=c(1,1)),variance.model = list(model = 'fGARCH', submodel = 'GARCH', 
                                                                                    garchOrder = c(1, 1)),distribution = 'std')

fit_garch_2 <- ugarchfit(spec = n50_garch_2, data= na.omit(Return))

coef(fit_garch_2)

plot(fit_garch_2,which='all')

##################################################################################
## Model 3: Fit ARMA(2,2)-gjrGARCH(1,1) model with Student t-distribution ########
##################################################################################

#egarch
n50_garch_3 <- ugarchspec(mean.model = list(armaOrder=c(2,2)),variance.model = list(model = 'eGARCH', 
                                                                                    garchOrder = c(1, 1)),distribution = 'std')

fit_garch_3 <- ugarchfit(spec = n50_garch_3, data= na.omit(Return))
fit_garch_3
plot(fit_garch_3,which='all')


##################################################################################
## Model 4: Fit ARMA(0,0)-eGARCH(1,2) model with Student t-distribution ########
##################################################################################
n50_garch_4 <- ugarchspec(mean.model = list(armaOrder=c(1,2)),variance.model = list(model = 'eGARCH', 
                                                                                    garchOrder = c(1, 1)),distribution = 'std')

fit_garch_4 <- ugarchfit(spec = n50_garch_4, data= na.omit(Return))
fit_garch_4
plot(fit_garch_4,which='all')


##################################################################################
##Model 5: Fit ARMA(1,1)-eGARCH(2,1) model with Student t-distribution    ########
##################################################################################
#egarch
n50_garch_5 <- ugarchspec(mean.model = list(armaOrder=c(2,1)),variance.model = list(model = 'eGARCH', 
                                                                                    garchOrder = c(1, 1)),distribution = 'std')

fit_garch_5 <- ugarchfit(spec = n50_garch_5, data= na.omit(Return))
fit_garch_5
plot(fit_garch_5,which='all')
######################
## Model Selection ###
######################
persistence(fit_garch_1)    #Persistence of valatility
#################################
#### Convergence of the Model ###
#################################

print(convergence(fit_garch_1))   # The model converge

#################################
######### Forecasting ###########
#################################
for_cast1 <-ugarchforecast(fit_garch_1,data=tso,n.ahead=20)
for_cast1
#################################
######### Rolling Forecat #######
#################################
fit_roll <- ugarchfit(n50_garch_1, data= na.omit(Return),out.sample =500)
fore_roll <- ugarchforecast(fit_roll, n.ahead=20, n.roll=50)
fore_roll
par(mfrow=c(1,2))
plot(fore_roll,which=1)


plot(fore_roll,which=2)
par(mfrow=c(1,2))
plot(fore_roll,which=3)
plot(fore_roll,which=4)

#######################################
#### Forecasting using Bootstrap ######
#######################################

par(mfrow=c(1,2))
fore_boot <- ugarchboot(fit_garch_5,data = na.omit(Return), method = c("Partial", "Full")[1], n.ahead = 20, n.bootpred = 500)
plot(fore_boot,which=2)
plot(fore_boot,which=3)

