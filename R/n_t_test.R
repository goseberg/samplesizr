#' Sample Size Calculation for the two-sided t-Test
#'
#' \code{n_ttest} performs the Sample Size calculation for the t-Test
#'   to the level \eqn{\alpha} comparing two independent samples.
#'   The method is based on the pages 16 - 18 in [1].
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_Y - \mu_X = 0}}
#'   \item{Alternative Hypothesis:}{\eqn{|\mu_Y - \mu_X| \ge \Delta_A}}
#' }
#'
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param r        Quotient of Sample sizes \eqn{r = n_Y / n_X}.
#' @param r.strict Allocation Handling. Default = TRUE. If set to TRUE,
#'   the Sample size  per Group is round up to the next combination hitting the
#'   specified allocation exactly. If set to FALSE, the Sample size per
#'   Group is round up to the next natural number.
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param sd       Standard deviation \eqn{\sigma}.
#'
#' @return \code{n_ttest} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_ttest(alpha = .05, power = .90, r = 1, effect = 10, sd = 20)
#' n_ttest(alpha = .05, power = .90, r = 2, effect = 10, sd = 20)$n
#' n_ttest(alpha = .05, power = .90, r = 2, effect = 10, sd = 20)$power_out
#' n_ttest(alpha = .05, power = .90, r = 2, r.strict = FALSE, effect = 10, sd = 20)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_ttest and .pow_ttest
#
# The Sample size is calculated using an iterative aproach with
# the non-central t distribution. Therefore a power function is defined
# first. This approach is following the notation and description on
# p. 16 in [1].
# The modified ceiling function .group_balance ensures that the
# allocation is hit according to user's choice.
# Afterwards the exact power is calculated.

n_ttest<-function(alpha, power, r, r.strict=TRUE, effect, sd){

  n_X <- 1

  # Iterative call of power function
  while (.pow_ttest(
           alpha = alpha,
           r = r,
           n_X = n_X,
           effect = effect,
           sd     = sd
          ) < power
  ){
    n_X <- n_X + 1
  }

  # Balance group sizes
  n.results <- .group_balance(n_X = n_X, r = r, r.strict = r.strict)

  # Calculate exact power for t-Test
  power_out <- .pow_ttest(
                  alpha = alpha,
                  r = r,
                  n_X = n.results$n_X,
                  effect = effect,
                  sd     = sd
                )
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- c(alpha    = alpha,
                  power    = power,
                  r        = r,
                  r.strict = r.strict,
                  effect   = effect,
                  sd       = sd
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_ttest", class(results))

  return(results)

}

# Calculate exact power for the t-test

.pow_ttest<-function(alpha, r, n_X, effect, sd){

  nc  <- sqrt( (r/(1+r)) * n_X ) * (effect / sd)
  q_0 <- qt(p = 1 - alpha/2, df = (1 + r) * n_X - 2, ncp = 0)
  h   <- pt(q = q_0, df = (1+r) * n_X - 2, ncp = nc)

  return(1 - h)

}

# II Print Function
print.n_ttest <- function(x, ...){

  cat("Sample size calculation for Student's t-test comparing two independent \n")
  cat("Samples (two-sided alternative).\n\n")
  cat(sprintf(
"Input Parameters \n
Significance level : %.2f
Desired Power : %.2f %%
Effect size : %.2f
Standard deviation : %.2f
Allocation : %.2f \n
Results of sample size calculation \n
n Group X : %.0f
n Group Y : %.0f
n Total : %.0f
Resulting Power : %.2f %%",

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


