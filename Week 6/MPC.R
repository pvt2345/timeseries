
library(astsa)
library(forecast)
MPC = read.csv("MPC.csv")
MPC = HSX[HSX$X.Ticker. == "MPC",]
MPC = head(MPC, 250)

MPC$X.High. = round(rev(High), digits = 2)
MPC$X.Close. = round(rev(Close), digits = 2)
MPC$X.Open. =  round(rev(Open), digits = 2)
MPC$X.Low. = round(rev(Low), digits = 2)

write.csv(MPC, "MPC.csv")

MPC_High =MPC$X.High.
MPC_Low =MPC$X.Low.
MPC_Close =MPC$X.Close.
MPC_Open =MPC$X.Open.

ts_MPC_Close = ts(rev(head(MPC_Close,250)))
ts_MPC_Close = ts(rev(MPC_Close))
ts_MPC_Close = log(ts_MPC_Close)
plot(ts_MPC_Close)
plot(ts(ts_MPC_Close))

diff_ts_MPC_Close = diff(ts_MPC_Close)
plot(diff_ts_MPC_Close)


diff_ts_MPC_Close = diff(diff(ts_MPC_Close), 90)
Box.test(diff_ts_MPC_Close, lag = log(length(diff_ts_MPC_Close)))
plot(diff_ts_MPC_Close)
acf(diff_ts_MPC_Close, lag.max = 100) #SMA 32, MA <= 1, SMA <= 1
acf(ts_MPC_Close, lag.max = 100) #SMA 32, MA <= 1, SMA <= 0
pacf(diff_ts_MPC_Close, lag.max = 100) #pacf 32, AR <= 2 SAR <= 2 
qqnorm(diff_ts_MPC_Close)
d = 0
D = 1
per = 30

n = round(length(ts_MPC_Close)*4/5)
for (p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+D+j < 10){
          model<-arima(x=ts_MPC_Close, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}


{model<-arima(x=ts_MPC_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
  pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
  sse<-sum(model$residuals^2)
  #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
  MPC(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_MPC_Close,d=1,D=1,max.p=1, max.q=1, max.P = 4, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),pa)

model = astsa::sarima(ts_MPC_Close, 1,0,1,1,1,1,30) 

model<- arima(x=ts_MPC_Close, order = c(1,0,1), seasonal = list(order=c(1,1,1), period=30))
plot(forecast(model))

forecast(model, h = 3)
optim(hessian = FALSE) 
