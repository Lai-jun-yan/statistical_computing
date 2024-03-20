library(MASS)

#定義共變異數矩陣
Sigma = matrix(c(1,0.5,0.5,1),nrow = 2,ncol = 2)

#比較效率
system.time({
  W(1000000,2,Sigma)
})

system.time({
  Bartlett_decomposition(1000000,2,Sigma)
})



