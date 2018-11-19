library(astsa)
library(forecast)
data = read.csv("CafeF.HSX.Upto16.11.2018.csv")
VTF = data[data$X.Ticker. == "VTF",]

VTF_High = VTF$X.High.
VTF_Low = VTF$X.Low.
VTF_Close = VTF$X.Close.
VTF_Open = VTF$X.Low.

ts_VTF_Close = ts(VTF_Close)
ts_VTF_Close = log(ts_VTF_Close)
plot(ts_VTF_Close)
plot(diff(ts_VTF_Close))

diff_ts_VTF_Close = diff(ts_VTF_Close)
diff_ts_VTF_Close = diff(diff(ts_VTF_Close), 30)
Box.test(diff_ts_VTF_Close)
plot(diff_ts_VTF_Close)
acf(diff_ts_VTF_Close, lag.max = 100)$acf #SMA 90, MA <= 1, SMA <=1
pacf(diff_ts_VTF_Close, lag.max = 1000) #pacf 90, AR <= 1, SAR <= 4
d = 1
D = 1
per = 30

n = round(length(ts_VTF_Close)*4/5)
for (p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
          model<-arima(x=ts_VTF_Close[1:n], order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per), optim.method="BFGS")
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
      }
    }
  }
}


{model<-arima(x=ts_VTF_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
sse<-sum(model$residuals^2)
#aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_VTF_Close,d=1,D=1,max.p=1, max.q=1, max.P = 4, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),pa)

model = sarima(ts_VTF_Close, 1,1,1,4,1,1,30) 
optim(hessian = FALSE) 
