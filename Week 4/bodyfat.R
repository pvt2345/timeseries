library(isdals)
data(bodyfat)
attach(bodyfat)
pairs(cbind(Fat,Triceps,Thigh, Midarm))
Fat.hat = predict(lm(Fat~Thigh)) #dự đoán Fat theo Thigh sử dụng linear regression
Triceps.hat = predict(lm(Triceps~Thigh)) #partial out the thigh 
cor(Fat-Fat.hat, Triceps-Triceps.hat)
cor(Fat, Triceps)
