set.seed(2017);
phi1 = .5;
phi2 = -.4;
X.ts = arima.sim(list(ar= c()), n = 1000);
par(mfrow = c(2,1));
plot(X.ts, main=paste("AR(2), phi=", phi1, ",", phi2));
X.acf = acf(X.ts, main=paste("AR(2) ACF for phi =", phi1, ",", phi2))
