#' Sample Size Calculation for the Chi-Square Test
#'
#' \code{n_chisq} performs the Sample Size calculation for two
#'   independent samples with binary data using the absolute rate
#'   difference quantifying the effect of an intervention.
#'   The method used here is based on the pages 21 - 26 in [1].
#'   The Sample Size is calculated using an iterative approach,
#'   recalculating the exact power, the sum of all discrete combinations
#'   fullfilling the alternative hypothesis to the approximative test
#'   for rising group sizes \code{n}. See p. 23 in [1] for further details.
#'
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{p_Y - p_X = 0}}
#'   \item{Alternative Hypothesis:}{\eqn{|p_Y - p_X| \ge \Delta_A}}
#' }
#'
#' @param p_Y         Event rate of Group Y on the alternative.
#' @param p_X         Event rate of Group X on the alternative.
#' @param alpha       Significance level \eqn{\alpha}.
#' @param power       Desired Power \eqn{1-\beta}.
#' @param r           Quotient of Sample sizes \eqn{r = n_Y / n_X}.
#' @param power.exact Default = TRUE. If set to FALSE an approximative formula
#'   is used for calculating the sample size, given by (5.7a) in [1]. On TRUE
#'   the iterative approach is used.
#'
#' @return \code{n_chisq} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_chisq(p_Y = .5, p_X = .3, alpha = .05, power = .8, r = 2)
#' n_chisq(p_Y = .5, p_X = .3, alpha = .05, power = .8, r = 2)$n
#' n_chisq(p_Y = .5, p_X = .3, alpha = .05, power = .8, r = 2, power.exact = FALSE)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018],
#' 1th Edition

# I n_chisq

n_chisq <- function(p_Y, p_X, alpha, power, r, power.exact = TRUE) {

  .stopcheck(
    var_1  = p_Y,
    var_2  = p_X,
    var_3  = r,
    alpha  = alpha,
    power  = power,
    binary = TRUE
  )

  p_0     <- (p_X + r*p_Y) / (1+r)
  effect  <- p_Y - p_X
  z_alpha <- qnorm( 1 - alpha/2 )

  if (power.exact == FALSE) {

    # Use (5.7a)
    n_X_num <- ( z_alpha * sqrt((1+r)*p_0*(1-p_0)) +
                 qnorm(power) * sqrt(r*p_X*(1-p_X)+p_Y*(1-p_Y)) )^2
    n_X_den <- r * effect^2
    n_X     <- n_X_num / n_X_den

    # Balance group sizes
    n.results <- .group_balance(n_X = n_X, r = r, r.strict = TRUE)

    power_out <- power_binomial(
      alpha = alpha,
      n_X = n.results$n_X,
      n_Y = n.results$n_Y,
      p_X = p_X,
      p_Y = p_Y,
      power.exact = TRUE
    )

  } else {

    # Use iterative approach
    n.results <- .n_binomial_exact(
      alpha = alpha,
      power = power,
      p_X   = p_X,
      p_Y   = p_Y,
      r     = r
    )

    power_out <- power_binomial(
      alpha = alpha,
      n_X = n.results$n_X,
      n_Y = n.results$n_Y,
      p_X = p_X,
      p_Y = p_Y,
      power.exact = TRUE
    )
  }

  # exact power for Chi-Square-Test
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- c(alpha    = alpha,
                  power    = power,
                  power.exact = power.exact,
                  r        = r,
                  p_Y      = p_Y,
                  p_X      = p_X
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_chisq", class(results))

  return(results)

}


#' Power Calculation for the Chi-Square Test
#'
#' \code{power_chisq} performs the power calculation for two
#'   independent samples with respect to binary data using the absolute rate
#'   difference quantifying the effect of an intervention.
#'   The method used here is based on the pages 21 - 26 in [1].
#'
#' @param p_Y         Event rate of group Y on the alternative.
#' @param p_X         Event rate of group X on the alternative.
#' @param n_Y         Sample size of group Y.
#' @param n_X         Sample size of group X.
#' @param alpha       Significance level \eqn{\alpha}.
#' @param power.exact If set to FALSE an approximative distributive
#'   is used for calculating the power, given the alternative distribution at the
#'   bottom of p. 22 in [1]. On TRUE
#'   the iterative approach is used.
#'
#' @return \code{power_binomial} returns the power.
#'
#' @examples
#' power_binomial(p_Y = .5, p_X = .3, n_Y = 100, n_X = 50, alpha = .05, power.exact = TRUE)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition


# II Power Function
power_binomial <-function(p_Y, p_X, n_Y, n_X, alpha, power.exact)
{

  if (power.exact == TRUE){
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
        i     <- dbinom (y,prob=p_Y,n_Y)
        power <- power + c*i*entscheidung
      }
    }
  } else { # Power calculation following the bottom of p. 22
    z_alpha <- qnorm(1 - alpha/2)
    r       <- n_Y / n_X
    p_0     <- (p_X + r*p_Y) / (1 + r)
    sigma_0 <- sqrt( ((1 + r) / r) * (1 / n_X) * p_0 * (1 - p_0) )
    sigma_A <- (1 / n_X) * ( p_X * (1-p_X) + ( (p_Y * (1 - p_Y)) / r ) )
    nc      <- (p_Y - p_X) / sigma_0
    sd      <- sigma_A / sigma_0
    power   <- 1 - pnorm(z_alpha, mean = nc, sd = sd)
  }
  return (power)

}

# III n to exact power Function

.n_binomial_exact<-function(p_X, p_Y, alpha, power, r){
  #find start value
  #
  pow_st <- max(0, power-.15)
  n_st <- n_chisq(
    p_Y = p_Y,
    p_X = p_X,
    alpha = alpha,
    power = pow_st,
    r,
    power.exact = FALSE
  )

  num <- .get_fraction(r)$numerator
  den <- .get_fraction(r)$denominator
  if ( (r %% 1) == 0 ) {den <- 1}

  i <- n_st$n_X / den
  p <- 0

  while (p < power){
   .n_x <- den * i
   .n_y <- num * i
   i    <- i + 1
   p    <- power_binomial(alpha = alpha,
    n_X = .n_x,
    n_Y = .n_y,
    p_X = p_X,
    p_Y = p_Y,
    power.exact = TRUE
   )
  }

  result <- list(n_X = .n_x, n_Y = .n_y, n = .n_x + .n_y)
  return(result)
}

# IV Print Function

print.n_chisq <- function(x, ...){

  cat("Sample size calculation for the Chi-Square test for two independent\n")
  cat("samples with respect to binary data using the absolute rate difference.\n\n")

  cat(sprintf(
    "Input Parameters \n
Rate intervention group : %.3f
Rate conrol group : %.3f
Significance level (two-sided): %.4f
Desired power : %.2f %%
Allocation : %.2f \n
Results of sample size calculation \n
n intervention group : %i
n control group : %i
n total : %i
Actual power : %.5f %%"
    ,

    x$p_Y,
    x$p_X,
    x$alpha,
    x$power*100,
    x$r,
    x$n_Y,
    x$n_X,
    x$n,
    x$power_out*100
  )
  )

}

