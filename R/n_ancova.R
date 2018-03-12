#' Sample size calculation for the Analysis of Covariances (ANCOVA)
#'
#' \code{n_ancova} performs the Sample size calculation for the test on
#'   mean difference for two samples with respect to normal data using the
#'   Analysis of Covariances (ANCOVA).
#'   The method is based on the pages 18 - 20 in [1]. The Sample Size
#'   is calculated using an approximative formula,
#'   suggested by Frison & Pocock (1999), implemented (4.14) in [1].
#'
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_Y - \mu_X = 0}}
#'   \item{Alternative Hypothesis:}{\eqn{|\mu_Y - \mu_X| \ge \Delta_A}}
#' }
#'
#' @param effect   Effect \eqn{\Delta_A} used as alternative hypothesis.
#' @param corr     Correlation  \eqn{rho} to Covariate.
#' @param sd       Standard deviation \eqn{\sigma}.
#' @param alpha    Significance level \eqn{\alpha}.
#' @param power    Desired Power \eqn{1-\beta}.
#' @param r        Quotient of Sample sizes \eqn{r = n_Y / n_X}.
#' @param gs       Guenther/Schouten correcture. Default = TRUE. If set to TRUE,
#'                 the G/S correcture like in (4.15) in [1] is used.
#'
#' @return \code{n_ancova} returns an object of type list. The resulting
#'   Sample sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'
#' @examples
#' n_ancova(effect = 10, corr = .5, sd = 20, alpha = .05, power = .90, r = 1)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition
#' Exact power output is going to be implemented in the future.

n_ancova <- function(effect, corr, sd, alpha, power, r = 1, gs = TRUE){

  .stopcheck(
    var_1 = effect,
    var_2 = sd,
    var_3 = r,
    alpha = alpha,
    power = power
  )

  #4 th input variable check
  if ( corr > 1 || corr < 0 || (is.numeric(corr) == FALSE) ){
    stop("corr has to be a numeric between 0 an 1.")
  }

  z_alpha <- qnorm(1 - alpha/2)
  n_X <- ((1+r)/r) * (z_alpha+qnorm(power))^2 * (1-corr^2) * (sd/effect)^2

  if (gs == TRUE) {
    n_X <- n_X + (z_alpha^2) / (2*(1+r))
  }

  # Balance group sizes
  n.results <- .group_balance(n_X = n_X, r = r, r.strict = TRUE)

  # Calculate exact power for ANCOVA
  #power_out <- warning("No exact power calculation for ANCOVA implemented.")


  # Organize Output for print function
  input_list <- c(alpha    = alpha,
                  power    = power,
                  r        = r,
                  effect   = effect,
                  corr     = corr,
                  sd       = sd,
                  gs       = gs
  )

  results         <- append(input_list, n.results)
  class(results)  <- c("n_ancova", class(results))

  return(results)

}

print.n_ancova <- function(x, ...){

  cat("Sample size calculation for ANCOVA comparing two
samples with respect to normal data(two-sided alternative).\n"
  )
  if(x$gs == TRUE) {cat("Guenther/Schouten correction performed. \n")}
  else {cat("No Guenther/Schouten correction performed. \n")}

  cat(sprintf(
    "Input Parameters \n
Significance level : %.3f
Desired power : %.2f %%
Effect size : %.2f
Correlation : %.2f
Standard deviation : %.2f
Allocation : %.2f \n
Results of sample size calculation \n
n group X : %i
n group Y : %i
n total : %i" ,

    x$alpha,
    x$power*100,
    x$effect,
    x$corr,
    x$sd,
    x$r,
    x$n_X,
    x$n_Y,
    x$n
  )
  )
}

