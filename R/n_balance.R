# FUNCTION GROUP_BALANCE
#
# To deal with certain allocation r, this function
#
.get_fraction <- function(x) {
  tmp <- strsplit(attr(fractions(x),"fracs"), "/")[[1]]
  list(numerator=as.numeric(tmp[1]),denominator=as.numeric(tmp[2]))
}

.group_balance <- function(n_x,r,r.strict=TRUE) {

  if (r.strict == FALSE) {
    .n_y <- ceiling(r*n_x)
    .n_x <- ceiling(n_x)
  } else
  {
    num <- .get_fraction(r)$numerator
    den <- .get_fraction(r)$denominator
    if ( (r %% 1) == 0 ) {den <- 1}
    n_y <- r * n_x
    if (r >= 1) {
      .n_x <- ceiling(n_x/den) * den
      .n_y <- r * .n_x
    } else {
      .n_y <- ceiling(n_y/num) * num
      .n_x <- (1/r) * .n_y
    }
  }
 n_groups<- list(n_x=.n_x,n_y=.n_y,n=.n_x+.n_y)
 return(n_groups)
}

