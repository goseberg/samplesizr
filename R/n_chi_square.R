n_chisq <- function(alpha, power, power.exact=FALSE, r, r.strict=TRUE, p_X, p_Y){

  p_0 <- (p_X + r*p_Y) / (1+r)
  effect <- p_Y - p_X

  if (power.exact==FALSE) {
    n_x_num <- ( qnorm(1-alpha/2) * sqrt((1+r)*p_0*(1-p_0)) +
                 qnorm(power) * sqrt(r*p_X*(1-p_X)+p_Y*(1-p_Y)) )^2
    n_x_den <- r * effect^2
    n_x <- n_x_num / n_x_den
    N <- .group_balance(n_X = n_x, r = r, r.strict = r.strict)
    ex_power <- .binomial_exact_power(alpha = alpha, n_X = N$n_X, n_Y = N$n_Y, p_X = p_X, p_Y = p_Y)
    result <- append(N,ex_power)
    names(result)<-c("n_x", "n_y", "n", "power")
  } else {
    result <- .n_binomial_exact(alpha=alpha, pow=power, p_X=p_X, p_Y=p_Y, r=r, r.strict=r.strict)
  }

  return(result)

}
