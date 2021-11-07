library(astsa)
library(itsmr)
library(lubridate)
library(zoo)
library(randtests)
library(forecast)
library(urca)
library(tseries)

nfty50 <- read.csv('../data/NIFTY 50_Data.csv')

nfty50 <- nfty50[,c(1,5)]
nfty50[,1] <- dmy(nfty50[,1])
par(mfrow = c(1,1))
plot(nfty50$Close, ylab="Stock Prices",main="Figure : Closing prices of the stocks",type = 'l')

tso <- zoo(nfty50$Close, nfty50$Date)

#plot.new()
#frame()
# trend with moving avg. ##################################################
###########################################################################

d = 261
tso_ma = rollapply(tso, d, mean, fill = NA)
tso_ma = na.fill(tso_ma, 'extend')
par(mfrow = c(1,1))
plot(tso, ylab="Stock Prices",main="Figure : Closing prices of the stocks",type = 'l')
lines(tso_ma, type ='l', col = 'red')
res_ma = tso - tso_ma
plot(res_ma, ylab="Stock Prices",main="Figure : Residuals after Trend elimination",type = 'l')
# [y(i-1)<y(i)>y(i+1) or y(i-1)>y(i)<y(i+1)] testing of iid ness
#turning.point.test(as.numeric(res_ma))       
# [y(i)<y(i+1)] testing of iid ness
difference.sign.test(as.numeric(res_ma))                              
# [y(i)<y(j)] testing of iid nes
rank.test(as.numeric(res_ma))
x = rev(seq(1:length(res_ma)))
tso_fit <- lm(res_ma~x)
summary(tso_fit)
tso_res_reg <- zoo(rev(predict(tso_fit)), nfty50$Date)
plot(res_ma, type = 'l')
lines(tso_res_reg, type = 'l', col = 'red')
new_res <- res_ma - tso_res_reg

rank.test(as.numeric(new_res))
# qq plot
qqnorm(as.numeric(new_res))
qqline(as.numeric(new_res), col = 'red')
# shapiro R-square (checking of normality)
shapiro.test(as.numeric(new_res))
# checking of normality
ks.test(as.numeric(new_res), "pnorm") 
Box.test(new_res, lag = 10, type = "Ljung-Box", fitdf = 0)  

acf2(as.numeric(new_res), main = 'ACF & PACF plot')

#############################################################################
# trend with exponential moving avg.
############################################################################

alpha=0.1
tso_exp = array(0,length(tso))
tso_exp[1] = tso[1]
for(i in 2:length(tso))
{
  tso_exp[i] = (1-alpha)*tso_exp[i-1] + alpha*tso[i]
}
tso_exp = zoo(tso_exp, rev(nfty50$Date))                                 # exponential smoothed time series
plot(tso, type='l',ylab = 'stock price', main = 'Figure : Closing prices of the stocks'); lines(tso_exp, type='l', col = 'red')               # raw data with fitted exponentially smoothed line
res_exp <- tso - tso_exp
plot(res_exp, type = 'l',ylab = 'stock price',main = 'Residuals after trend removal')
# [y(i-1)<y(i)>y(i+1) or y(i-1)>y(i)<y(i+1)] testing of iid ness
turning.point.test(as.numeric(res_exp))       
# [y(i)<y(i+1)] testing of iid ness
difference.sign.test(as.numeric(res_exp))  

x = seq(1:length(res_exp))
x_sq = x^2
tso_expres_fit <- lm(res_exp~x+x_sq)
summary(tso_expres_fit)
tso_expres_reg <- zoo(rev(predict(tso_expres_fit)), nfty50$Date)
plot(res_exp, type = 'l',main = 'Residuals')
lines(tso_expres_reg, type = 'l', col = 'red')
tso_final_regres <- res_exp - tso_expres_reg

difference.sign.test(as.numeric(tso_final_regres))  
# [y(i)<y(j)] testing of iid nes
rank.test(as.numeric(tso_final_regres))   
# qq plot
qqnorm(as.numeric(tso_final_regres))
qqline(as.numeric(tso_final_regres), col = 'red')
# shapiro R-square (checking of normality)
shapiro.test(as.numeric(tso_final_regres))
# checking of normality
ks.test(as.numeric(tso_final_regres), "pnorm") 
Box.test(tso_final_regres, lag = 10, type = "Ljung-Box", fitdf = 0)  
acf2(as.numeric(tso_final_regres))
#summary(ur.df(tso_final_regres))

expma = arima(tso_final_regres, c(1,0,0))

predict_expma <- predict(expma)
predict_expma$pred
predict_expma$se
expma_forecast <- predict(expma, n.ahead = 10)$pred
expma_forecast_se <- predict(ma, n.ahead = 10)$se
#plot(tso, ylab = 'stock prices', xlab = 'year')
#points(expma_forecast, type = "l")
#points(expma_forecast - 2*expma_forecast_se, type = "l", col = 2, lty = 2)
#points(expma_forecast + 2*expma_forecast_se, type = "l", col = 2, lty = 2)

#############################################################################
# trend with regression
##############################################################################

x = seq(1:length(tso))
x_sq = x^2
x_cu = x^3
tso_fit <- lm(tso~x+x_sq+x_cu)
summary(tso_fit)
tso_reg <- zoo(predict(tso_fit), rev(nfty50$Date))
par(mfrow = c(1,1))
plot(tso, ylab="Stock Prices",main="Figure : Closing prices of the stocks",type = 'l',xlab = 'year')
lines(tso_reg, type = 'l', col = 'red')
res_reg = tso - tso_reg
plot(res_reg, type = 'l', main = 'Residuals',xlab = 'year', ylab = 'Stock Prices')
# [y(i-1)<y(i)>y(i+1) or y(i-1)>y(i)<y(i+1)] testing of iid ness
turning.point.test(as.numeric(res_reg))       
# [y(i)<y(i+1)] testing of iid ness
difference.sign.test(as.numeric(res_reg))                              
# [y(i)<y(j)] testing of iid nes
rank.test(as.numeric(res_reg)) 

#x = seq(1:length(res_reg))
#x_sq = x^2
#tso_regres_fit <- lm(res_reg~x+x_sq)
#summary(tso_regres_fit)
#tso_regres_reg <- zoo(rev(predict(tso_regres_fit)), nfty50$Date)
#plot(res_reg, type = 'l')
#lines(tso_regres_reg, type = 'l', col = 'red')
#tso_final_res <- res_reg - tso_regres_reg

#rank.test(as.numeric(tso_final_res)) 
# qq plot
qqnorm(res_reg)
qqline(res_reg, col = 'red')

#qqnorm(as.numeric(tso_final_res))
#qqline(tso_final_res, col = 'red')
# shapiro R-square (checking of normality)
shapiro.test(as.numeric(res_reg))
#shapiro.test(as.numeric(tso_final_res))
# checking of normality
ks.test(as.numeric(res_reg), "pnorm")

acf2(as.numeric(res_reg))
summary(ur.df(res_reg,type='drift'))


###########################################################################
# trend with differencing
###########################################################################

dif <- diff(nfty50$Close)
plot(dif, type = 'l')

# [y(i-1)<y(i)>y(i+1) or y(i-1)>y(i)<y(i+1)] testing of iid ness
turning.point.test(dif)       
# [y(i)<y(i+1)] testing of iid ness
difference.sign.test(dif)                              
# [y(i)<y(j)] testing of iid nes
rank.test(dif) 

m = diff(dif)
#x = seq(1:length(dif))
#x_sq = x^2
#tso_dif_fit <- lm(dif~x)
#summary(tso_dif_fit)
#tso_difres_reg <- zoo(rev(predict(tso_dif_fit)), nfty50$Date[-1418])
#plot(dif, type = 'l')
#lines(tso_difres_reg, type = 'l', col = 'red')
#tso_final_dif <- dif - tso_difres_reg

#rank.test(as.numeric(tso_final_dif)) 
rank.test(as.numeric(m)) 

# qq plot
qqnorm(as.numeric(m))
qqline(as.numeric(m), col = 'red')
# shapiro R-square (checking of normality)
shapiro.test(as.numeric(m))
# checking of normality
#ks.test(as.numeric(m), "pnorm")

acf2(nfty50$Close, main = 'ACF & PACF of the original data')
acf2(diff(nfty50$Close)), main = 'ACF & PACF after first differencing')
acf2(diff(diff(rev(nfty50$Close))), main = 'ACF & PACF after second differencing')

res_d = diff(diff(nfty50$Close))
plot(res_d, type = 'l',xlab = 'stock price',ylab = 'years',main = 'Residuals after second differencing')

#summary(ur.df(tso,type='drift'))
#summary(ur.df(diff(tso),type='drift'))
summary(ur.df(diff(diff(tso),type='drift')))
#adf.test(diff(diff(tso)),k=0)
acf2(res_d)
ma = arima(res_d, c(0,0,1))
#res = zoo(res_d, nfty50$Date[-1:-2])
predict_ma <- predict(ma)
predict_ma$pred
predict_ma$se

#plot(tso)
ma_forecast <- predict(ma, n.ahead = 10)$pred
ma_forecast_se <- predict(ma, n.ahead = 10)$se
points(ma_forecast, type = "l", col = 2)
points(ma_forecast - 2*ma_forecast_se, type = "l", col = 2, lty = 2)
points(ma_forecast + 2*ma_forecast_se, type = "l", col = 2, lty = 2)
# acf-pacf plot 

# ma
acf2(as.numeric(new_res))

# exp smooting

acf2(as.numeric(tso_final_regres))

# reg

acf2(as.numeric(res_reg))



# ARMA model selection ##################################################

# ma

summary(ur.df())
ma_m = Arima(as.ts(new_res), order=c(p,0,q), seasonal = c(0,0,0),include.constant = TRUE)  
print(ma_m)  

# exp_smoothing

exp_ma = Arima(as.ts(tso_final_regres), order=c(p,0,q), seasonal = c(0,0,0),include.constant = TRUE) 
print(exp_ma)

# reg

# diff


