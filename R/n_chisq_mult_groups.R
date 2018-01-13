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
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param n.groups Number of Groups \eqn{k}.
#' @param p_A      Vector \eqn{\p_A} of expected rates on the alternative.
#'
#' @return \code{n_chisq_mult_groups} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_per_group}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_chisq_mult_groups(alpha = .05, power = .8, n.groups = 3, p_A = c(.3, .5, .4))
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_chisq_mult_groups

n_chisq_mult_groups <- function(alpha, power, n.groups, p_A){

  p_A_m <- mean(p_A)
  effect <- sum((p_A-p_A_m)^2)
  l <- 1
  r <- 0

  # Using (6.3)
  while (l>r){
    n  <- i * n.groups
    nc <- (n / n.groups) * (effect / (p_A_m*(1-p_A_m)))^2

    l <- qchisq(1 - alpha, df = n.groups - 1, ncp = 0)
    r <- qchisq(1 - power, df = n.groups - 1, ncp = nc)
    p <- 1 - pchisq(l, df = n.groups - 1, ncp = nc)

    i <- i + 1
  }

  n.results <- list(n_per_group = n / n.groups, n = n)

  # Calculate exact power for f-Test
  power_out <- list(power_out = p)

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

# II Print function

print.n_chisq_mult_groups <- function(x, ...){

  cat("Sample size calculation for the Chi-Square test on rate difference for\n")
  cat("more than two groups.\n\n ")

  cat(sprintf("Input Parameters \n
Significance level : %.2f
Desired Power : %.2f %%
Number of groups : %.0f
Expectation on Alternative : %s

Results of sample size calculation \n
n per group : %.0f
n total : %.0f
Resulting Power : %.2f %%",

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
