library(datasets)
plot(BJsales)
plot(diff(BJsales))

acf(diff(diff(BJsales))) #MA(1)
acf(diff(diff(BJsales)), type="partial") #AR(3)

d=2

for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=8){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

model = arima(BJsales, order=c(0,2,1))
model
