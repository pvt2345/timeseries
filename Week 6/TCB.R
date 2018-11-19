library(astsa)
library(forecast)
HSX = read.csv("CafeF.HSX.Upto16.11.2018.csv")
TCB = HSX[HSX$X.Ticker. == "TCB",]

UPCOM = read.csv("CafeF.UPCOM.Upto16.11.2018.csv")
TCB = UPCOM[UPCOM$X.Ticker. == "TCB",]

TCB_High = TCB$X.High.
TCB_Low = TCB$X.Low.
TCB_Close = TCB$X.Close.
TCB_Open = TCB$X.Open.

ts_TCB_Close = ts(rev(TCB_Close))
#ts_TCB_Close = ts(rev(TCB_Close)[280:498])
ts_TCB_Close = log(ts_TCB_Close)
plot(ts_TCB_Close)
plot(ts(ts_TCB_Close))

diff_ts_TCB_Close = diff(ts_TCB_Close)
plot(diff_ts_TCB_Close)


diff_ts_TCB_Close = diff(diff(ts_TCB_Close), 9)
Box.test(diff_ts_TCB_Close, lag = log(length(diff_ts_TCB_Close)))
plot(diff_ts_TCB_Close)
acf(diff_ts_TCB_Close, lag.max = 100) #SMA 90, MA <= 1, SMA <=1
pacf(diff_ts_TCB_Close, lag.max = 1000) #pacf 90, AR <= 2, SAR <= 0
qqnorm(diff_ts_TCB_Close)
d = 1
D = 0
per = 9

n = round(length(ts_TCB_Close)*4/5)
for (p in 1:3){
  for(q in 1:2){
    for(i in 1:1){
      for(j in 1:2){
        model<-arima(x=ts_TCB_Close, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per), optim.method="BFGS")
        pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
        sse<-sum(model$residuals^2)
        #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
        cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
      }
    }
  }
}


{model<-arima(x=ts_TCB_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
  pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
  sse<-sum(model$residuals^2)
  #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
  cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_TCB_Close,d=1,D=1,max.p=1, max.q=1, max.P = 4, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),pa)

model = sarima(ts_TCB_Close, 0,1,1,0,0,1,9) 
model<- arima(x=ts_TCB_Close, order = c(0,1,1), seasonal = list(order=c(0,0,1), period=9))
plot(forecast(model, ))
optim(hessian = FALSE) 
