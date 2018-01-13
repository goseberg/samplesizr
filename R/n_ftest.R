#' Sample Size Calculation for the f Test comparing means of \eqn{k > 2} groups
#'
#' \code{n_ftest} performs the Sample Size calculation for  the f-test
#'   comparing means of \eqn{k > 2} independent samples.
#'   The method used here is based on the page 29 in [1].
#'   The Sample Size is calculated using an iterative approach.
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_1 = \mu_2 = ... = \mu_k}}
#'   \item{Alternative Hypothesis:}{There are \eqn{i,j} with \eqn{\mu_i \ne \mu_j}}
#' }
#'
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param n.groups Number of Groups \eqn{k}.
#' @param mu_A     Vector \eqn{\mu_A} of expected means on the alternative.
#' @param sd       Standard deviation \eqn{sigma}.
#'
#' @return \code{n_ftest} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_per_group}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'
#' @examples
#' n_ftest(alpha = .025, power = .8, n.groups = 3, mu_A = c(1, 2, 3), sd = 20)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition

# I n_ftest

n_ftest = function(alpha, power, n.groups, mu_A, sd){

  # Initialize start parameters
  l <- 1
  r <- 0
  i <- 1

  mu_A_m <- mean(mu_A)
  effect <- sum( (mu_A - mu_A_m)^2 )

  while (l > r){
    n  <- i * n.groups
    if (i == 1) {n <- n + 1}
    nc <- (n / n.groups) * (effect / (sd^2))

    l <- qf(1 - alpha, df1 = n.groups-1, df2 = n - n.groups, ncp = 0)
    r <- qf(1 - power, df1 = n.groups-1, df2 = n - n.groups, ncp = nc)
    p <- 1 - pf(l , df1 = n.groups-1, df2 = n - n.groups, ncp = nc)

    i<-i+1
   }

  if (n == n.groups + 1) {stop("Resulting group size too small. Check input.")}

  n.results <- list(n_per_group = (n / n.groups), n = n)

  # Calculate exact power for f-Test
  power_out <- list(power_out = p)

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

# II Print function

print.n_ftest <- function(x, ...){

  cat("Sample size calculation for the f-test on mean difference for\n")
  cat("more than two groups.n\n ")

  cat(sprintf(
    "Input Parameters \n
    Significance level : %.2f
    Desired Power : %.2f %%
    Number of groups : %.0f
    Expectation on Alternative : %s
    Standard deviation : %.2f

    Results of sample size calculation \n
    n per group : %.0f
    n total : %.0f
    Resulting Power : %.2f %%",

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
