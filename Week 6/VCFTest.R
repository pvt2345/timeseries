library(astsa)
library(forecast)
data = read.csv("CafeF.HSX.Upto16.11.2018.csv")
data[data$X.Ticker. == "VCF"]
data[data$X.Ticker. == "VCF",]
VCF = data[data$X.Ticker. == "VCF",]

VCF_High = VCF$X.High.
VCF_Low = VCF$X.Low.
VCF_Close = VCF$X.Close.
VCF_Open = VCF$X.Low.

ts_VCF_Close = ts(VCF_Close)
plot(diff(ts_VCF_Close))

diff_ts_VCF_Close = diff(diff(ts_VCF_Close), 90)
plot(diff_ts_VCF_Close)
acf(diff_ts_VCF_Close, lag.max = 100)$acf #SMA 90, MA <= 1, SMA <=1
pacf(diff_ts_VCF_Close, lag.max = 100) #pacf 90, AR <= 6, SAR <=1

d = 1
D = 1
per = 90
n = length(ts_VCF_Close)
for (p in 1:7){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
          model<-arima(x=ts_VCF_Close[1:1316], order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per), optim.control = )
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
      }
    }
  }
}


{model<-arima(x=ts_VCF_Close[1:1316], order = c(6,1,1), seasonal = list(order=c(0,1,1), period=per))
pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
sse<-sum(model$residuals^2)
#aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')}


auto.arima(ts_VCF_Close[1:1316],d=1,D=1,xreg=1:1316,max.p=6, max.q=1, max.P = 1, max.Q = 1,
           seasonal=TRUE,stepwise=FALSE,approximation = TRUE,ic=("aic"),parallel=TRUE)

model = sarima(ts_VCF_Close, 6,1,1,0,1,1,90)
  
 