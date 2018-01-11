n_ftest = function(sig, pow, n.groups, effect, std){
  l <- 1
  r <- 0
  i <- 1

  while (l>r){
    n <- i*n.groups
    if (i == 1) {n <- n+1}
    nc <- (n / n.groups) * (effect / std)^2

    l <- qf(1-sig, df1 = n.groups-1, df2 = n - n.groups, ncp = 0)
    r <- qf(1-pow, df1 = n.groups-1, df2 = n - n.groups, ncp = nc)

    i<-i+1
   }

  if (n == n.groups + 1) {cat("WARNING: Group size small! \n")}

  return(list(n_per_group = n / n.groups, n = n))
}
