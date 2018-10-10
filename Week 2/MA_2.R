#Generate noise
noise=rnorm(10000)
#introduce a variable
ma_2=NULL

#loop for generating MA(2) process
for(i in 3:10000){
  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}

#shift data
moving_average_process=ma_2[3:10000]
moving_average_process=ts(moving_average_process)

#partition output graphics: frame of 2 rows and 1 column
par(mfrow=c(2,1))

#plot
plot(moving_average_process, main='A moving average process of order 2', xlab='Time', ylab='', col='blue')
acf(moving_average_process, main='Corellogram of a moving average process of order 2')

