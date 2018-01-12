n_chisq_mult_groups <- function(sig,pow,n.groups,p_A){

  p_A_m <- mean(p_A)
  effect <- sum((p_A-p_A_m)^2)
  l <- 1
  r <- 0
  while (l>r){
    n <- i*n.groups
    nc <- (n / n.groups) * (effect / (p_A_m*(1-p_A_m)))^2

    l <- qchisq(1-sig, df = n.groups - 1, ncp = 0)
    r <- qchisq(1-pow, df = n.groups - 1, ncp = nc)

    i<-i+1
  }


  return(list(n_per_group = n / n.groups, n = n))
}
