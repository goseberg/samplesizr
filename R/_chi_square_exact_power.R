.binomial_exact_power <-function(sig, p_X, p_Y, n_x, n_y)
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

# Sample size exact power
nexact<-function(napprox,px,py,r,powerapprox,alpha){
  if (powerapprox<0.8){
    n<-napprox
    p<-powerapprox
    while(p<0.8){
      if (r==0.5) {n<- n + (1+1/r)
      nx<-ceiling(n/(1+1/r))*(1/r)
      ny<-ceiling(r*nx)
      }
      if (r!=0.5) {n<- n + (1+r)
      nx<-ceiling(n/(1+r))
      ny<-ceiling(r*nx)
      }
      n<-nx+ny
      p<- exPower(alpha, px, py, nx, ny,r)
    }
    N<-n
    return(N)
  }
  if (powerapprox>0.8){
    n<-napprox
    p<-powerapprox
    while(p>0.8){
      if (r==0.5) {n<- n - (1+1/r)
      nx<-ceiling(n/(1+1/r))*(1/r)
      ny<-ceiling(r*nx)
      }
      if (r!=0.5) {n<- n - (1+r)
      nx<-ceiling(n/(1+r))
      ny<-ceiling(r*nx)
      }
      n<-nx+ny
      p<- exPower(alpha, px, py, nx, ny,r)

    }
    #N<-n+(1+r)
    if (r==0.5){N<- n + (1+1/r)}
    if (r!=0.5){N<- n + (1+r)}
    return(N)
  }
  return(N)
}

### Sample size non-central chi square
nchi<-function(napprox,px,py,r, powerchisqnapprox,chi1alpha,alpha){
  if (powerchisqnapprox<0.8){
    n<-napprox
    p<-powerchisqnapprox
    while(p<0.8){
      if (r==0.5) {n<- n + (1+1/r)
      nx<-ceiling(n/(1+1/r))*(1/r)
      ny<-ceiling(r*nx)
      }
      if (r!=0.5) {n<- n + (1+r)
      nx<-ceiling(n/(1+r))
      ny<-ceiling(r*nx)
      }
      n<-nx+ny
      lambda<-n**2 * ( (nx*(px-((nx*px+ny*py)/n))**2/ ((nx*px+ny*py)* (n-(nx*px+ny*py)))) +
                         (ny*(py-((nx*px+ny*py)/n))**2/ ((nx*px+ny*py)* (n-(nx*px+ny*py)))) )
      p<-pchisq(chi1alpha, 1, ncp = lambda, lower.tail = FALSE, log.p = FALSE)
    }
    N<-n
    return(N)
  }
  if ( powerchisqnapprox>0.8){
    n<-napprox
    p<- powerchisqnapprox
    while(p>0.8){


      if (r==0.5) {n<- n - (1+1/r)
      nx<-ceiling(n/(1+1/r))*(1/r)
      ny<-ceiling(r*nx)
      }
      if (r!=0.5) {n<- n - (1+r)
      nx<-ceiling(n/(1+r))
      ny<-ceiling(r*nx)
      }

      n<-nx+ny
      lambda<-n**2 * ( (nx*(px-((nx*px+ny*py)/n))**2/ ((nx*px+ny*py)* (n-(nx*px+ny*py)))) +
                         (ny*(py-((nx*px+ny*py)/n))**2/ ((nx*px+ny*py)* (n-(nx*px+ny*py)))) )
      p<-pchisq(chi1alpha, 1, ncp = lambda, lower.tail = FALSE, log.p = FALSE)
    }

    if (r==0.5){N<- n + (1+1/r)}
    if (r!=0.5){N<- n + (1+r)}
    return(N)
  }
  return(N)
}

