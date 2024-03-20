#產生Pareto distribution(α = 2,β = 4)的隨機變數，並畫出直方圖
generationP = function(a = 2,b = 4){
  n = 1000
  u = runif(n)
  x = ((16/(1-u))^(1/b))-a #inverse transformation method
  hist(x,prob = TRUE,main = "")
  y = seq(0,8,0.01)
  lines(y,(b*(a^b))/((y+a)^(b+1)))
}

#執行函數
generationP()