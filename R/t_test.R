

.h_ttest<-function(sig,r,n_x,effect,std){
  nc  <- sqrt((r/(1+r))*n_x) * ((effect)/(std))
  q_0 <- qt(p=1-sig/2,df=(1+r)*n_x-2,ncp=0)
  h <- pt(q_0,df=(1+r)*n_x-2,ncp=nc)
  return(h)
}

n_ttest<-function(sig,pow,r,effect,std){
  n_x <- 2
  while(.h_ttest(sig=sig,r=r,n_x=n_x,effect=effect,std=std) >= 1-pow) {n_x <- n_x+1}
  return(c(n_x,(1+r)*n_x))
}
