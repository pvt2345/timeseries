b = matrix(c(0.8, 0.6, 0.2), ncol=1, nrow =3)

R = matrix(1, 3, 3)
R[1,2] = b[1]
R[1,3] = b[2]
R[2,1] = b[1]
R[2,3] = b[1]
R[3,1] = b[2]
R[3,2] = b[1]

c0 = 5
phi.hat = solve(R, b)
c0*(1 - t(phi.hat) %*% b)
