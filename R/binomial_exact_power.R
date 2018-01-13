.binomial_exact_power <-function(alpha, n_X, n_Y, p_X, p_Y)
{
  x<-0
  y<-0
  power <- 0

  for (x in 0:n_X)
  {
    for (y in 0:n_Y)
    {
      if ((x + y == 0) || (x + y == n_X + n_Y)) {
        entscheidung=0
      } else {
        P_0 <- (x+y)/(n_X + n_Y)
        u <- sqrt((n_X*n_Y)/(n_X+n_Y))  *  (((y/n_Y)-(x/n_X)) / sqrt( P_0 * (1 - P_0) ))
        entscheidung <- (u>=qnorm(1-alpha/2))
      }

      #Control group
      c   <- dbinom (x,prob=p_X,n_X)

      # intervention group
      i   <- dbinom (y,prob=p_Y,n_Y)
      power <- power + c*i*entscheidung
    }
  }
  return (power)
}

.n_binomial_exact<-function(alpha, power, p_X, p_Y, r, r.strict=TRUE){
  if (r.strict == TRUE) {
      num <- .get_fraction(r)$numerator
      den <- .get_fraction(r)$denominator
      if ( (r %% 1) == 0 ) {den <- 1}
      i <-1
      p <- 0

      while (p<power){
        .n_x <- den*i
        .n_y <- num*i
        i <- i+1
        p <- .binomial_exact_power(alpha = alpha, n_X = .n_x, n_Y = .n_y, p_X = p_X, p_Y = p_Y)
      }

    result <- list(n_X = .n_x, n_Y = .n_y, n = .n_x + .n_y)
    return(result)
  }
  else {
    stop("ERROR: Non-strict allocation handling to the exact power is going to be implemented in near future.")
  }
}
