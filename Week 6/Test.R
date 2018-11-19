library(astsa)
library(forecast)
HSX = read.csv("CafeF.HSX.Upto16.11.2018.csv")
HOT = HSX[HSX$X.Ticker. == "HOT",]

UPCOM = read.csv("CafeF.UPCOM.Upto16.11.2018.csv")
HOT = UPCOM[UPCOM$X.Ticker. == "HOT",]

HOT_High =HOT$X.High.
HOT_Low =HOT$X.Low.
HOT_Close =HOT$X.Close.
HOT_Open =HOT$X.Open.

ts_HOT_Close = ts(rev(head(HOT_Close,250)))
ts_HOT_Close = ts(rev(HOT_Close))
ts_HOT_Close = log(ts_HOT_Close)
plot(ts_HOT_Close)
plot(ts(ts_HOT_Close))

diff_ts_HOT_Close = diff(ts_HOT_Close)
plot(diff_ts_HOT_Close)


diff_ts_HOT_Close = diff(diff(ts_HOT_Close), 32)
Box.test(diff_ts_HOT_Close, lag = log(length(diff_ts_HOT_Close)))
plot(diff_ts_HOT_Close)
acf(diff_ts_HOT_Close, lag.max = 100) #SMA 32, MA <= 1, SMA <= 1
acf(ts_HOT_Close, lag.max = 100) #SMA 32, MA <= 1, SMA <= 0
pacf(diff_ts_HOT_Close, lag.max = 100) #pacf 32, AR <= 2 SAR <= 2 
qqnorm(diff_ts_HOT_Close)
d = 1
D = 1
per = 32

n = round(length(ts_HOT_Close)*4/5)
for (p in 1:3){
  for(q in 1:2){
    for(i in 1:3){
      for(j in 1:2){
          if(p+d+q+i+D+j < 10){
          model<-arima(x=ts_HOT_Close, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
          }
      }
    }
  }
}


{model<-arima(x=ts_HOT_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
sse<-sum(model$residuals^2)
#aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_HOT_Close,d=1,D=1,max.p=1, max.q=1, max.P = 4, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),pa)

model = sarima(ts_HOT_Close, 1,1,2,1,1,2,32) 
model<- arima(x=ts_HOT_Close, order = c(1,1,2), seasonal = list(order=c(1,1,2), period=32))
plot(forecast(model))
optim(hessian = FALSE) 
