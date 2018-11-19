library(astsa)
library(forecast)
HSX = read.csv("CafeF.HSX.Upto16.11.2018.csv")
LAF = HSX[HSX$X.Ticker. == "LAF",]

UPCOM = read.csv("CafeF.UPCOM.Upto16.11.2018.csv")
LAF = UPCOM[UPCOM$X.Ticker. == "LAF",]

LAF_High =LAF$X.High.
LAF_Low =LAF$X.Low.
LAF_Close =LAF$X.Close.
LAF_Open =LAF$X.Open.

ts_LAF_Close = ts(rev(head(LAF_Close,200)))
ts_LAF_Close = ts(rev(LAF_Close))
ts_LAF_Close = log(ts_LAF_Close)
plot(ts_LAF_Close)
plot(ts(ts_LAF_Close))

diff_ts_LAF_Close = diff(ts_LAF_Close)
plot(diff_ts_LAF_Close)


diff_ts_LAF_Close = diff(diff(ts_LAF_Close), 15)
Box.test(diff_ts_LAF_Close, lag = log(length(diff_ts_LAF_Close)))
plot(diff_ts_LAF_Close)
acf(diff_ts_LAF_Close, lag.max = 100) #SMA 15, MA <= 1, SMA <=1
pacf(diff_ts_LAF_Close, lag.max = 100) #pacf 90, AR <= 1, SAR <= 1
qqnorm(diff_ts_LAF_Close)
d = 1
D = 0
per = 15

n = round(length(ts_LAF_Close)*4/5)
for (p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        model<-arima(x=ts_LAF_Close, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per), optim.method="BFGS")
        pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
        sse<-sum(model$residuals^2)
        #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
        cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
      }
    }
  }
}


{model<-arima(x=ts_LAF_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
  pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
  sse<-sum(model$residuals^2)
  #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
  cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_LAF_Close,d=1,D=1,max.p=1, max.q=1, max.P = 4, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),pa)

model = sarima(ts_LAF_Close, 0,1,1,1,0,0,9) 
model<- arima(x=ts_LAF_Close, order = c(0,1,1), seasonal = list(order=c(1,0,0), period=9))
plot(forecast(model))
optim(hessian = FALSE) 
