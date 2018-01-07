n_ancova = function(sig,pow,r,effect,std,corr,gs=FALSE){

  cat("Sample Size Calculation for ANCOVA\n")
  cat("Approximative calculation by Frison and Pocock[1999]\n")

  n_x <- ((1+r)/r) *(qnorm(1-sig/2)+qnorm(pow))^2 * (1-corr^2) * (std/effect)^2

  if (gs == TRUE) {
    n_x <- n_x + (qnorm(1-sig/2)^2) / (2*(1+r))
    cat("Correction with Guenther/Schouten performed\n")
  }

  result<-.group_balance(n_x,r,r.strict)
  return(result)
}
