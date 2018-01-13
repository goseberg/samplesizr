# Local Functions .get_fraction and .group_balance
#
# To deal with certain allocation r, the function .group_balance
# rounds up to the next sample size n which hits the quotient
# r:= n_Y/ n_X exactly. With r.strict = FALSE indeed, it will just
# perform a normal ceiling.

.get_fraction <- function(x) {
  tmp <- strsplit(attr(fractions(x),"fracs"), "/")[[1]]
  list(numerator=as.numeric(tmp[1]),denominator=as.numeric(tmp[2]))
}

.group_balance <- function(n_X,r,r.strict) {

  if (r.strict == FALSE) {

    n_Y <- ceiling(r*n_X)
    n_X <- ceiling(n_X)

  } else {

    num <- .get_fraction(r)$numerator
    den <- .get_fraction(r)$denominator
    if ( (r %% 1) == 0 ) {den <- 1}

    n_Y <- r * n_X

    if (r >= 1) {
      n_X <- ceiling(n_X/den) * den
      n_Y <- r * n_X
    } else {
      n_Y <- ceiling(n_Y/num) * num
      n_X <- (1/r) * n_Y
    }

  }

 n_groups<- list(n_X = n_X, n_Y = n_Y, n = n_X + n_Y)
 return(n_groups)

}
