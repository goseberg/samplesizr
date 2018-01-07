#' Sample size for z-Test
#'
#' @param sig  Significance level
#' @param pow  Power
#' @param r Allocation
#' @param effect
#' @param std Std. deviation
#'
#' @return Sample size for Group 1

n_ztest = function(sig,pow,r,r.strict,effect,std){
  n_x<-((1+r)/r) *(qnorm(1-sig/2)+qnorm(pow))^2 * (std/effect)^2
  result<-.group_balance(n_x,r,r.strict)
  return(result)
}


