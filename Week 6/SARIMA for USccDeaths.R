library(datasets)
library(forecast)

plot(USAccDeaths)

data = diff(USAccDeaths, 12)
plot(data)

data = diff(diff(USAccDeaths), 12)
plot(data)
par(mfrow=c(2,1))
acf(data, lag.max = 50) #q = 1, Q = 1
pacf(data, lag.max = 50) #p = 1, P = 1

d =1
D =1
per = 12

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+D+j<=10){
          model<-arima(x=USAccDeaths, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

sarima(USAccDeaths, 0, 1, 1, 0, 1, 1, 12)
sarima.for(USAccDeaths, 3, 0, 1, 1, 0, 1, 1, 12)
model<- arima(x=USAccDeaths, order = c(0, 1, 1), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))
