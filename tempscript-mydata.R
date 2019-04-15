
rangeStatList = c('市辖区', 'Districts', 'BetaD/')
setwd(paste0('C:/Sync/CoolGirl/Fhe/Results/OLS_180'))
load(paste0('sumlmHorizontal_',rangeStatList[2],'.Rdata'))


dfBeta = sumlmHorizontal[sumlmHorizontal$yIndex=='GDP',]
xt = dfBeta$Beta
require(tseries)
adf.test(xt)
adf.test(diff(xt))
plot(diff(xt))

acf(xt)
acf(diff(xt))

require(forecast)
auto.arima(xt)

fit <- arima(xt, c(0, 1, 0))
pred <- predict(fit)
ts.plot(xt,pred$pred, log = "y", lty = c(1,3))

