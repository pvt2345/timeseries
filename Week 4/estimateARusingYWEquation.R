set.seed(2017)

sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
phi

n=10000

ar.process=arima.sim(n,model=list(ar=c(1/3,1/2)), sd=4)
ar.process[1:5]

r=NULL
r[1:2]=acf(ar.process, plot=F)$acf[2:3] #array start at 1, rho(0) == 1
r

R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
R

R[1,2]=r[1] # only diagonal entries are edited #rho(1)
R[2,1]=r[1] # only diagonal entries are edited #rho(1)

b=matrix(r,nrow=2,ncol=1)# b- column vector with no entries
b

solve(R,b)

#phi.hat=matrix(c(solve(R,b)[1,1], solve(R,b)[2,1]),2,1)
phi.hat = solve(R, b)
phi.hat

c0=acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat

par(mfrow=c(3,1))
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')R