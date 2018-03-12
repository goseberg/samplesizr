#' Sample Size Calculation for the f Test comparing means of \eqn{k > 2} groups
#'
#' \code{n_ftest} performs the Sample Size calculation for  the F-test
#'   comparing means of \eqn{k > 2} independent samples.
#'   The method used here is based on the page 29 in [1].
#'   The Sample Size is calculated using an iterative approach.
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_1 = \mu_2 = ... = \mu_k}}
#'   \item{Alternative Hypothesis:}{There are \eqn{i,j} with \eqn{\mu_i \ne \mu_j}}
#' }
#'
#' @param mu_A     Vector \eqn{\mu_A} of expected means on the alternative.
#' @param sd       Standard deviation \eqn{sigma}.
#' @param n.groups Number of Groups \eqn{k}.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#'
#' @return \code{n_ftest} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_per_group}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_ftest(mu_A = c(1, 2, 3), sd = 20, n.groups = 3, alpha = .025, power = .8)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_ftest

n_ftest = function(mu_A, sd, n.groups, alpha, power){

  .stopcheck(
    var_1  = mu_A,
    var_2  = sd,
    var_3  = n.groups,
    alpha  = alpha,
    power  = power,
    two.groups = FALSE
  )

  # Initialize start parameters
  n_per_group <- 2

  mu_A_m <- mean(mu_A)
  effect <- sum( (mu_A - mu_A_m)^2 )

  while (power_ftest(
    mu_A        = mu_A,
    sd          = sd,
    n.groups    = n.groups,
    n_per_group = n_per_group,
    alpha       = alpha
  ) < power
  ){
    n_per_group  <- n_per_group + 1
  }

  n <- n_per_group * n.groups

  n.results <- list(n_per_group = n_per_group, n = n)

  # Calculate exact power for f-Test
  power_out <- power_ftest(
    mu_A        = mu_A,
    sd          = sd,
    n.groups    = n.groups,
    n_per_group = n_per_group,
    alpha       = alpha
  )
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- list(alpha    = alpha,
                  power    = power,
                  n.groups = n.groups,
                  mu_A     = mu_A,
                  sd       = sd
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_ftest", class(results))

  return(results)

}

# II Power function

#' Power Calculation for the F Test comparing means of \eqn{k > 2} groups
#'
#' \code{power_ftest} performs the power calculation for the f-test
#'   comparing means of \eqn{k > 2} independent samples.
#'   The method used here is based on the page 29 in [1].
#'
#' @param mu_A     Vector \eqn{\mu_A} of expected means on the alternative.
#' @param sd       Standard deviation \eqn{sigma}.
#' @param n.groups Number of Groups \eqn{k}.
#' @param n_per_group   Sample size per group.
#' @param alpha    Significance level \eqn{\alpha}.
#'
#' @return \code{n_ftest} returns the power.
#'
#' @examples
#' power_ftest(mu_A = c(1, 2, 3), sd = 20, n.groups = 3, n_per_group = 70, alpha = .025)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition


power_ftest <- function(mu_A, sd, n.groups, n_per_group, alpha) {

    mu_A_m  <- mean(mu_A)
    effect  <- sqrt( sum( (mu_A - mu_A_m)^2 ) )
    n       <- n_per_group * n.groups
    nc      <- (n_per_group) * (effect / sd)^2
    f_alpha <-  qf(1 - alpha, df1 = n.groups-1, df2 = n - n.groups, ncp = 0)
    power   <- 1 - pf(f_alpha, df1 = n.groups-1, df2 = n - n.groups, ncp = nc)

    return(power)

}

# III Print function

print.n_ftest <- function(x, ...){

  cat("Sample size calculation for the F-test comparing\n")
  cat("more than two independent groups with respect to normal data.\n ")

  cat(sprintf(
    "Input Parameters \n
Significance level : %.3f
Desired power : %.2f %%
Number of groups : %i
Means of the groups : %s
Standard deviation : %.2f

Results of sample size calculation \n
n per group : %i
n total : %i
Resulting power : %.5f %%",

    x$alpha,
    x$power*100,
    x$n.groups,
    paste(x$mu_A, collapse = ","),
    x$sd,
    x$n_per_group,
    x$n,
    x$power_out*100
  )
  )

}
