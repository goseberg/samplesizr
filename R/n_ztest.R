#' Sample Size Calculation for the two-sided z-Test
#'
#' \code{n_ztest} performs the Sample Size calculation for the z-Test
#'   to the level \eqn{\alpha} comparing two independent samples.
#'   The method is based on the pages 13 - 16 in [1].
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_Y - \mu_X = 0}}
#'   \item{Alternative Hypothesis:}{\eqn{\mu_Y - \mu_X \ge \Delta_A}}
#' }
#'
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param sd       Standard deviation \eqn{\sigma}.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param r        Quotient of Group sizes \eqn{r = n_Y / n_X}.
#'
#' @return \code{n_ztest} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}
#'
#' @examples
#' n_ztest(effect = .5, sd = 1, alpha = .05, power = .90, r = 2)
#' n_ztest(effect = .5, sd = 1, alpha = .05, power = .90, r = 2)$n
#' n_ztest(effect = .5, sd = 1, alpha = .05, power = .90, r = 2)$power_out
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_ztest
#
# The exact sample size is calculated with the formula (4.5a) in [1].
# The modified ceiling function .group_balance ensures that the
# allocation is hit according to the exact calculation.
# Afterwards the exact power is calculated.

n_ztest = function(effect, sd, alpha, power, r){

  # Check user input
  .stopcheck(
    var_1 = effect,
    var_2 = sd,
    alpha = alpha,
    power = power,
    var_3 = r
  )

  # Formula (4.5a) in [1]
  z_alpha <- qnorm(1-alpha/2)
  n_X     <- ((1+r)/r) * (z_alpha + qnorm(power))^2 * (sd/effect)^2

  # Balance group sizes
  n.results <- .group_balance(n_X = n_X, r = r, r.strict = TRUE)

  # Organize Input_Output for print function
  power_out  <- list(power_out = power_ztest(
    effect = effect,
    sd     = sd,
    n_X    = n.results$n_X,
    alpha  = alpha,
    r      = r
  ))

  input_list <- c(
    alpha    = alpha,
    power    = power,
    r        = r,
    effect   = effect,
    sd       = sd
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_ztest", class(results))

  return(results)

}

# II Power Function
#' Power calculation for the two-sided z-Test
#'
#' \code{n_ztest} performs the power calculation for the z-Test
#'   to the level \eqn{\alpha} comparing two independent samples.
#'   The method is based on the pages 13 - 16 in [1].
#'
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param sd       Standard deviation \eqn{\sigma}.
#' @param n_X      Sample size of group X.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param r        Default = 1. Quotient of Group sizes \eqn{r = n_Y / n_X}.
#'
#' @return \code{n_ztest} returns the power.
#'
#' @examples
#' power_ztest(effect = 10, sd = 20, n_X = 70, alpha = .05)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

power_ztest <- function(effect, sd, n_X, alpha, r = 1){
  z_alpha <- qnorm(1 - alpha/2)

  nc    <- sqrt( (r/(1+r)) * n_X ) * (effect / sd)
  power <- 1 - pnorm(z_alpha, mean = nc, sd = 1)
  return(power)
}

# III Print Function

print.n_ztest <- function(x, ...){

  cat("Sample size calculation for the gaussian test with two-sided alternative.\n\n")

  cat(sprintf(
      "Input Parameters \n
Significance level : %.3f
Desired Power : %.2f %%
Effect size : %.2f
Standard deviation : %.2f
Allocation : %.2f \n
Results of sample size calculation \n
n Group X : %i
n Group Y : %i
n Total : %i
Resulting Power : %.5f %%"
      ,

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
