
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
#' @param alpha       Significance level \eqn{\alpha}.
#' @param power       Desired Power \eqn{1-\beta}.
#' @param power.exact Default = TRUE. If set to FALSE an approximative formula
#'   is used for calculating the sample size, given by (5.7a) in [1]. On TRUE
#'   the iterative approach is used.
#' @param r           Quotient of Sample sizes \eqn{r = n_Y / n_X}.
#' @param r.strict    Allocation Handling. Default = TRUE. If set to TRUE,
#'   the Sample size  per Group is round up to the next combination hitting the
#'   specified allocation exactly. If set to FALSE, the Sample size per
#'   Group is round up to the next natural number.
#' @param p_Y         Event rate of Group Y on the alternative.
#' @param p_X         Event rate of Group X on the alternative.
#'
#' @return \code{n_chisq} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_chisq(alpha =.05, power=.8, power.exact=TRUE, r=2, p_Y = .5, p_X = .3)
#' n_chisq(alpha =.05, power=.8, power.exact=TRUE, r=2, p_Y = .5, p_X = .3)$n
#' n_chisq(alpha =.05, power=.8, power.exact=FALSE, r=2, r.strict=FALSE, p_Y = .5, p_X = .3)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_chisq

n_chisq <- function(alpha, power, power.exact = TRUE, r, r.strict = TRUE, p_Y, p_X){

  p_0     <- (p_X + r*p_Y) / (1+r)
  effect  <- p_Y - p_X
  z_alpha <- qnorm(1-alpha/2)

  if (power.exact==FALSE) {

    # Use (5.7a)
    n_X_num <- ( z_alpha * sqrt((1+r)*p_0*(1-p_0)) +
                 qnorm(power) * sqrt(r*p_X*(1-p_X)+p_Y*(1-p_Y)) )^2
    n_X_den <- r * effect^2
    n_X     <- n_X_num / n_X_den

    # Balance group sizes
    n.results <- .group_balance(n_X = n_X, r = r, r.strict = r.strict)

  } else {

    # Use iterative approach
    n.results <- .n_binomial_exact(
                   alpha = alpha,
                   pow   = power,
                   p_X   = p_X,
                   p_Y   = p_Y,
                   r     = r,
                   r.strict = r.strict
                  )

  }

  # Calculate exact power for Chi-Square-Test
  power_out <- .binomial_exact_power(
    alpha = alpha,
    n_X = n.results$n_X,
    n_Y = n.results$n_Y,
    p_X = p_X,
    p_Y = p_Y)
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- c(alpha    = alpha,
                  power    = power,
                  power.exact = power.exact,
                  r        = r,
                  r.strict = r.strict,
                  p_Y      = p_Y,
                  p_X      = p_X
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_chisq", class(results))

  return(results)

}


# II Print Function

print.n_chisq <- function(x, ...){

  cat("Sample size calculation for the Chi-Square test for two independent\n")
  cat("samples with binary data using the absolute rate difference.\n\n ")

  cat(sprintf(
    "Input Parameters \n
Significance level : %.2f
Desired Power : %.2f %%
Allocation : %.2f
Rate Group Y : %.2f
Rate Group X : %.2f

Results of sample size calculation \n
n Group X : %.0f
n Group Y : %.0f
n Total : %.0f
Resulting Power : %.2f %%"
    ,

    x$alpha,
    x$power*100,
    x$r,
    x$p_Y,
    x$p_X,
    x$n_X,
    x$n_Y,
    x$n,
    x$power_out*100
  )
  )

}

