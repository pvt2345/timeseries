library(astsa)
library(sarima)
set.seed(2017);
phi1 = .5;
phi2 = -.4;

arima(xt2, order=c(2,1,1), seasonal=list(order=c(1,1,1), period = 12))
mod = SARIMA(ar=c(.3,.5), i=1, ma=.1, sar=.2, si = 1, sma = .4, s = 12, sigma2 = 1.5)
X.ts = arima.sim(list(ar= c(phi1, phi2)), n = 1000);
#sarima(1,0,1,1,1,0)
X.ts = sim_sarima(n = 250, model = list(ar=c(phi1), iorder = 0, ma = c(0.2)), period =30, seasonal= list(ar=c(0.2), siorder = 1), sigma = 0.2)

Close = X.ts - min(X.ts) + 5
Z_High = rnorm(n = 250, mean = 0.2, sd = 0.02)
High = Close + Z_High
Low = Close - Z_High
Open = Close + Z_High


plot(ts(Close))
par(mfrow = c(2,1));
plot(X.ts, main=paste("AR(2), phi=", phi1, ",", phi2));

X.acf = acf(X.ts, main=paste("AR(2) ACF for phi =", phi1, ",", phi2))
Close = ts(Close)
model = astsa.sarima(Close, 2,1,2,1,1,2,90)
