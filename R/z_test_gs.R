n_gstest<-function(sig,pow,r,r.strict,effect,std){
  n_x_z<-((1+r)/r) *(qnorm(1-sig/2)+qnorm(pow))^2 * (std/effect)^2
  n_x <- n_x_z + (qnorm(1-sig/2)^2) / (2*(1+r))

  result<-.group_balance(n_x,r,r.strict)
  return(result)
}
