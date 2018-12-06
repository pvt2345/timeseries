library(forecast)
par(mfrow= c(2,1))
a = forecast(model)
plot(forecast(model), main = "Predict for MPC using SARIMA(1,0,1,1,1,1,30)")
ts.plot(a$mean,test, gpars = list(col = c("blue", "red")), lwd=3, main = "Test data")

cat("Du bao gia dong cua ngay mai: ", a$mean[1])
cat("Khoang tin cay 95% cho gia dong cua ngay mai: (", a$lower[1,2], ",", a$upper[1,2], ")" )

        
