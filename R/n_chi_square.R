n_chi_square <- function(sig,pow,pow.exact=FALSE,r,r.strict=TRUE,p_Y,p_X){
  cat("Sample Size Calculation \n")
  cat("Chi-Square test for rates comparing two groups\n")
  cat("using abolute rate difference for comparison\n")

  p_0 <- (p_X + r*p_Y) / (1+r)
  effect <- p_Y - p_X

  if (pow.exact==FALSE) {
    n_x_num <- ( qnorm(1-sig/2) * sqrt((1+r)*p_0*(1-p_0)) +
                 qnorm(pow) * sqrt(r*p_X*(1-p_X)+p_Y*(1-p_Y)) )^2
    n_x_den <- r * effect^2
    n_x <- n_x_num / n_x_den
    N<-.group_balance(n_x = n_x, r = r, r.strict = r.strict)
    power <- .binomial_exact_power(sig = sig, n_x = N$n_x, n_y = N$n_y, p_X = p_X, p_Y = p_Y)
    result <- append(N,power)
    names(result)<-c("n_x", "n_y", "n", "power")
  } else {
    result <- .n_binomial_exact(sig=sig, pow=pow, p_X=p_X, p_Y=p_Y, r=r, r.strict=r.strict)
  }

  return(result)
}
