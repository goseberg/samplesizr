#' Sample Size Calculation for the Chi-Square Test comparing rates of
#' \eqn{k > 2} groups
#'
#' \code{n_chisq_mult_groups} performs the Sample Size calculation for
#'   the Chi-Square test comparing rates of \eqn{k > 2} independent samples.
#'   The method used here is based on the page 30 in [1].
#'   The Sample Size is calculated using an iterative approach using (6.3) in [1].
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{p_1 = p_2 = ... = p_k}}
#'   \item{Alternative Hypothesis:}{There are \eqn{i,j} with \eqn{p_i \ne p_j}}
#' }
#'
#' @param p_A      Vector \eqn{\p_A} of expected rates on the alternative.
#' @param n.groups Number of Groups \eqn{k}.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#'
#' @return \code{n_chisq_mult_groups} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_per_group}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_chisq_mult_groups(p_A = c(.3, .5, .4), n.groups = 3, alpha = .05, power = .8)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_chisq_mult_groups

n_chisq_mult_groups <- function(p_A, n.groups, alpha, power){

  .stopcheck(
    var_1  = p_A,
    var_2  = 1,
    var_3  = n.groups,
    alpha  = alpha,
    power  = power,
    two.groups = FALSE,
    binary = TRUE
  )

  n_per_group <- 1

  # Using (6.3)
  while (power_chisq_mult_groups(
    p_A = p_A,
    n.groups = n.groups,
    n_per_group = n_per_group,
    alpha = alpha
  ) < power
  ) {
    n_per_group <- n_per_group + 1
  }

  n.results <- list(n_per_group = n_per_group, n = n_per_group * n.groups)

  # Calculate exact power for f-Test
  power_out <- power_chisq_mult_groups(
    p_A = p_A,
    n.groups = n.groups,
    n_per_group = n_per_group,
    alpha = alpha)
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- list(alpha    = alpha,
                     power    = power,
                     n.groups = n.groups,
                     p_A     = p_A
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_chisq_mult_groups", class(results))

  return(results)
}

#' Power Calculation for the Chi-Square Test comparing rates of
#' \eqn{k > 2} groups
#'
#' \code{power_chisq_mult_groups} performs the power calculation for
#'   the Chi-Square test comparing rates of \eqn{k > 2} independent samples.
#'   The method used here is based on the page 30 in [1].
#'   The power is calculated using (6.2) in [1].
#'
#' @param p_A      Vector \eqn{\p_A} of expected rates on the alternative.
#' @param n.groups Number of Groups \eqn{k}.
#' @param n_per_group  Sample size per group.
#' @param alpha    Significance level \eqn{\alpha}.
#'
#' @return \code{power_chisq_mult_groups} returns the power.
#'
#' @examples
#' power_chisq_mult_groups(p_A = c(.3, .5, .4), n.groups = 3, n_per_group = 70, alpha = .05)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# II Power function
power_chisq_mult_groups <- function(p_A, n.groups, n_per_group, alpha) {

    p_A_m  <- mean(p_A)
    effect <- sqrt( sum((p_A-p_A_m)^2) )
    n      <- n_per_group * n.groups
    nc     <- n_per_group * ( effect^2 / (p_A_m*(1-p_A_m)) )

    chisq_alpha <- qchisq(1 - alpha, df = n.groups - 1, ncp = 0)
    power <- 1 - pchisq(chisq_alpha, df = n.groups - 1, ncp = nc)

    return(power)

}

# III Print function

print.n_chisq_mult_groups <- function(x, ...){

  cat("Sample size calculation for the Chi-Square test on rate difference for\n")
  cat("more than two groups.\n\n ")

  cat(sprintf("Input Parameters \n
Significance level : %.3f
Desired Power : %.2f %%
Number of groups : %i
Expectation on Alternative : %s

Results of sample size calculation \n
n per group : %i
n total : %i
Resulting Power : %.5f %%",

x$alpha,
x$power*100,
x$n.groups,
paste(x$p_A, collapse = ","),
x$n_per_group,
x$n,
x$power_out*100
)
)
}
