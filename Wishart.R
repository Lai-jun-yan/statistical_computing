#先定義共變異數矩陣
Sigma = matrix(c(1,0.5,0.5,2),nrow = 2,ncol = 2)

#產生X ~ Multinormal distribution(n,mean,Sigma)
rmvn.eigen = function(n,mu,Sigma){
  d = length(mu)
  ev = eigen(Sigma,symmetric = TRUE)
  lamda = ev$values
  V = ev$vectors
  R = V %*% diag(sqrt(lamda)) %*% t(V)
  Z = matrix(rnorm(n*d),nrow = n,ncol = d)
  X = Z %*% R + matrix(mu,n,d, byrow = TRUE)
  X
}  

#產生一個X ~ Wishart distribution(d = 2, Sigma)
w = function(d,Sigma){
  mu = rep(0,d)
  x = rmvn.eigen(1,mu,Sigma)
  M = t(x) %*% x
  M
}

#產生n組X ~ Wishart distribution(d = 2, Sigma)
W = function(n,d,Sigma){
  x = list()
  for (i in 1:n) {
    x[[i]] = w(d,Sigma)
  }
  x
}

#執行
W(100,2,Sigma)




