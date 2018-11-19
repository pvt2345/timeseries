library(astsa)
library(forecast)
data = read.csv('monthly-milk-production-pounds-p.csv')
Milk = data$Pounds
milk = ts(data$Pounds) #keep original
plot(milk)
par(mfrow=c(2,1))
acf(milk)
pacf(milk)

data_ = diff(milk) # take 1 differencing
plot(data_)
par(mfrow=c(2,1))
acf(data_)
pacf(data_)

#data__ = ts(diff(diff(data$Pounds), 12)) #1 nonss, 1 ss
data__ = diff(diff(milk), 12)
plot(data__)
acf(data__, lag.max = 50) #MA(1) SMA(1) q = 1, Q = 1
pacf(data__)  #AR(1) SAR(1)   p = 1, P = 1

d = 1
D = 1
per = 12
n = length(ts_VCF_Close)
for (p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
          if(p+d+q+i+D+j<=10){
            model<-arima(x=milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
            pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
            sse<-sum(model$residuals^2)
            aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
            cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=', aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

sarima(milk, 1, 1, 0, 0 ,1, 1, 12)
model<- arima(x=milk, order = c(1,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
