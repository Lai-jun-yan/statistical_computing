#先定義共變異數矩陣
Sigma = matrix(c(1,0.5,0.5,2),nrow = 2,ncol = 2)

#產生A矩陣
a = function(d){
  # 產生 d x d 的矩陣
  A <- matrix(0, nrow = d, ncol = d)
  
  # 生成符合 N(0, 1) 分佈的隨機數填充 A[i, j] (i > j)
  for (i in 1:d) {
    for (j in 1:(max(i - 1, 1))) {
      A[i, j] <- rnorm(1)
    }
  }
  
  # 生成符合 sqrt(chisq(n-i+1)) 分佈的隨機數填充對角線 A[i, i]
  for (i in 1:d) {
    A[i, i] <- sqrt(rchisq(1, df = d - i + 1))
  }
  A
}

#產生一組X ~ Wishart distribution(d = 2, Sigma)
w2 = function(d,Sigma){
  L = chol(Sigma)
  A = a(d)
  x = L %*% A %*% t(A) %*% t(L)
  x
}


#產生n組X ~ Wishart distribution(d = 2, Sigma)
Bartlett_decomposition = function(n,d,Sigma){
  x = list()
  for (i in 1:n) {
    x[[i]] = w2(d,Sigma)
  }
  x
}

#執行
Bartlett_decomposition(100,2,Sigma)

