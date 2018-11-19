library(astsa)

plot(jj, col="blue")

data = diff(log(jj))
plot(ts, col="blue")
acf(ts, lag.max = length(ts))


data_ = diff(diff(log(jj)), 4) #1 diff for non-seasonal and 1 for seasonal
Box.test(data_, lag = log(length(data_)))

acf(data_, main = "ACF of diff(diff(log(jj), 4)") #MA(1) SMA(1)
acf(data_, type="partial") #SAR(1), AR(1)

par(mfrow=c(2,1))

d=1
D=1

per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+D+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

sarima(log(jj), 0,1,1,1,1,0,4)

