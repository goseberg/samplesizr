.binomial_exact_power <-function(sig, n_x, n_y, p_X, p_Y)
{
  x<-0
  y<-0
  power <- 0

  for (x in 0:n_x)
  {
    for (y in 0:n_y)
    {
      if ((x + y == 0) || (x + y == n_x + n_y)) {
        entscheidung=0
      } else {
        P_0 <- (x+y)/(n_x+n_y)
        u <- sqrt((n_x*n_y)/(n_x+n_y))  *  (((y/n_y)-(x/n_x)) / sqrt( P_0 * (1 - P_0) ))
        entscheidung <- (u>=qnorm(1-sig/2))
      }

      #Control group
      c   <- dbinom (x,prob=p_X,n_x)

      # intervention group
      i   <- dbinom (y,prob=p_Y,n_y)
      power <- power + c*i*entscheidung
    }
  }
  return (power)
}

.n_binomial_exact<-function(sig, pow, p_X, p_Y, r, r.strict=TRUE){
  if (r.strict == TRUE) {
    num <- .get_fraction(r)$numerator
    den <- .get_fraction(r)$denominator
    if ( (r %% 1) == 0 ) {den <- 1}
    i <-1
    p<-0

    while (p<pow){
      .n_x <- den*i
      .n_y <- num*i
      i <- i+1
      p <- .binomial_exact_power(sig = sig, n_x = .n_x, n_y = .n_y, p_X = p_X, p_Y = p_Y)
    }
  result <- list(n_x = .n_x, n_y = .n_y, n = .n_x + .n_y, power = p)
  return(result)
  }
  if (r.strict == FALSE) {
    cat("ERROR: Non-strict allocation handling to the exact power is going to be implemented in near future.")
  }
}
