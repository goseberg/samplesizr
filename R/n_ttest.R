#' Sample Size Calculation for the two-sided t-Test
#'
#' \code{n_ttest} performs the Sample size calculation for the t-Test
#'   to the level \eqn{\alpha} comparing two independent samples with respect
#'   to normal data.
#'   The method is based on the pages 16 - 18 in [1].
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_Y - \mu_X = 0}}
#'   \item{Alternative Hypothesis:}{\eqn{|\mu_Y - \mu_X| \ge \Delta_A}}
#' }
#'
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param sd       Standard deviation \eqn{\sigma} of the Data on the
#' alternative hypothesis.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param r        Default = 1. Quotient of Group sizes \eqn{r = n_Y / n_X}.
#'
#' @return \code{n_ttest} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_ttest(effect = 10, sd = 20, alpha = .05, power = .90)
#' n_ttest(effect = 10, sd = 20, alpha = .05, power = .90, r = 2)$n
#' n_ttest(effect = 10, sd = 20, alpha = .05, power = .90, r = 2)$power_out
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_ttest and .pow_ttest
#
# The Sample size is calculated using an iterative aproach with
# the non-central t distribution. Therefore a power function is defined
# first. This approach is following the notation and description on
# p. 16 in [1].
# The modified ceiling function .group_balance ensures that the
# allocation is hit according to the exact calculation.
# Afterwards the exact power is calculated.

n_ttest<-function(effect, sd, alpha, power, r = 1){

  # Check user input
  .stopcheck(
    var_1 = effect,
    var_2 = sd,
    alpha = alpha,
    power = power,
    var_3 = r
  )

  n_X <- 2

  # Iterative call of power function
  while (power_ttest(
           alpha  = alpha,
           r      = r,
           n_X    = n_X,
           effect = effect,
           sd     = sd
        ) < power
  ){
    n_X <- n_X + 1
  }

  # Balance group sizes
  n.results <- .group_balance(n_X = n_X, r = r, r.strict = TRUE)

  # Organize Output for print function
  power_out <- power_ttest(
                  alpha  = alpha,
                  r      = r,
                  n_X    = n.results$n_X,
                  effect = effect,
                  sd     = sd
                )
  power_out <- list(power_out = power_out)

  input_list <- c(alpha    = alpha,
                  power    = power,
                  r        = r,
                  effect   = effect,
                  sd       = sd
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_ttest", class(results))

  return(results)

}

# II Power function

#' Power Calculation for the two-sided t-Test
#'
#' \code{n_ttest} performs the power calculation for the t-Test
#'   comparing two independent samples.
#'   The method is based on the pages 16 - 18 in [1].
#'
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param sd       Standard deviation \eqn{\sigma}.
#' @param n_X      Sample size of group X.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param r        Default = 1. Quotient of Group sizes \eqn{r = n_Y / n_X}.
#'
#' @return \code{n_ttest} returns the power.
#'
#' @examples
#' power_ttest(effect = 10, sd = 20, n_X = 70, alpha = .05)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition


power_ttest <- function(effect, sd, n_X, alpha, r = 1){

  effect <- abs(effect)

  nc  <- sqrt( (r/(1+r)) * n_X ) * (effect / sd)
  q_0 <- qt(p = 1 - alpha/2, df = (1 + r) * n_X - 2, ncp = 0)
  h   <- pt(q = q_0, df = (1+r) * n_X - 2, ncp = nc)

  return(1 - h)

}

# III Print Function
print.n_ttest <- function(x, ...){

  cat("Sample size calculation for Student's t test comparing two independent \n")
  cat("samples with respect to normal data (two-sided alternative).\n\n")
  cat(sprintf(
"Input Parameters \n
Significance level : %.3f
Desired power : %.2f %%
Effect size : %.2f
Standard deviation : %.2f
Allocation : %.2f \n
Results of sample size calculation \n
n control group : %i
n intervention group : %i
n total : %i
Actual power : %.5f %%",

    x$alpha,
    x$power*100,
    x$effect,
    x$sd,
    x$r,
    x$n_X,
    x$n_Y,
    x$n,
    x$power_out*100
  )
  )

}


