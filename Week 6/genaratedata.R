library(CombMSC)
library(forecast)
library(astsa)


#####################
#generate data
sdat1<- sarima.Sim(n=250/30, period=30, model = list(order = c(1,1,0), ar=-0.3),
                   list(order= c(0,1,1), ma = -0.5))
plot(sdat1)
                   
sdat1 = sdat1 - min(sdat1)
sdat1 = sdat1 + 5
plot(sdat1)

MPC = NULL
MPC$Close = sdat1
MPC$Close = round((sdat1), digits = 2)[1:200]
MPC$DTYYYYMMDD = MPC_$X.DTYYYYMMDD.[1:200]
write.csv(x = MPC, file = "MPC.csv")

plot(ts(MPC$Close))

MPC_Test = ts(sdat1[201:250])

MPC_ = read.csv("MPC_2.csv")

d = 1
D = 1
per = 30

for (p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+D+j < 10){
          model<-arima(x=MPC$Close, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),D,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          #aic = log(sse/n) + (n + 2*(p + d + q + i + D + j - 4))/n
          cat(p-1,d,q-1,i-1,D,j-1,per, 'AIC=',model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

model = astsa::sarima(MPC$Close, 1,1,0,0,1,1,30) 
model<- arima(x=MPC$Close, order = c(1,1,0), seasonal = list(order=c(0,1,1), period=30))

plot(forecast(model))
predict = forecast(model, 50)$mean

#predict va test  
plot(predict)
plot(MPC_Test)

#xuat ra file
MPC_Predict = NULL
MPC_Predict$pred = predict
write.csv(MPC_Predict, "MPC_pred.csv")


s = 0

for (i in 2:51){
  if ((MPC_Test[i] - MPC_Test[i-1]) * (predict[i] - predict[i-1]) > 0){
      s = s + 1
  }
}


cat("Average Relative Error: ", 0.06424, "\nTrend error: ", s/50)
cat("Trend error: ", s/50)
