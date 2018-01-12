#' Sample Size Calculation for the Analysis of Covariances (ANCOVA)
#'
#' \code{n_ancova} performs the Sample Size calculation for the test on
#'   mean difference for two samples with an Analysis of
#'   Covariances (ANCOVA).
#'   The method is based on the pages 18 - 20 in [1]. The Sample Size
#'   is calculated using an approximative formula,
#'   suggested by Frison & Pocock (1999), implemented (4.14) in [1].
#' \describe{
#'   \item{Null Hypothesis:}{\eqn{\mu_Y - \mu_X = 0}}
#'   \item{Alternative Hypothesis:}{|\mu_Y - \mu_X| \ge \Delta_A}}
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
#' @param corr     Correlation to Covariate.
#' @param sd       Standard deviation \eqn{\sigma}.
#' @param gs       Guenther/Schouten correcture. Default = TRUE. If set to TRUE,
#'                 the G/S correcture like in (4.15) in [1] is used.
#'
#' @return \code{n_ancova} returns an object of type list. The resulting
#'   Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
#'   The resulting power is named \code{power_out}.
#'   WARNING: This function is not implemented yet.
#'
#' @examples
#' n_ancova(alpha = .05, power = .90, r = 1, effect = 10, corr = .5, sd = 20)
#' n_ancova(alpha = .05, power = .90, r = 1, effect = 10, corr = .5, sd = 20)$n
#' n_ancova(alpha = .05, power = .90, r = 2, effect = 10, sd = 20)$power_out
#' n_ancova(alpha = .05, power = .90, r = 2, r.strict = FALSE, effect = 10, corr = .5, sd = 20, gs = FALSE)
#'
#' @details [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition
#'  WARNING: Exact power output is going to be implemented in the future.

# I n_ancova
#

n_ancova = function(alpha, power, r, r.strict = TRUE, effect, corr, sd, gs = TRUE){

  z_alpha <- qnorm(1 - alpha/2)
  n_X <- ((1+r)/r) * (z_alpha+qnorm(power))^2 * (1-corr^2) * (sd/effect)^2

  if (gs == TRUE) {
    n_X <- n_X + (z_alpha^2) / (2*(1+r))
  }

  # Balance group sizes
  n.results <- .group_balance(n_X = n_X, r = r, r.strict = r.strict)

  # Calculate exact power for ANCOVA
  power_out <- "WARNING. See Documentation."
  power_out <- list(power_out = power_out)

  # Organize Output for print function
  input_list <- c(alpha    = alpha,
                  power    = power,
                  r        = r,
                  r.strict = r.strict,
                  effect   = effect,
                  corr     = corr,
                  sd       = sd,
                  gs       = gs
  )

  results         <- append(append(input_list, n.results), power_out)
  class(results)  <- c("n_ancova", class(results))

  return(results)

}


print.n_ancova <- function(x, ...){

  cat("Sample size calculation for ANCOVA comparing two \n")
  cat("samples (two-sided alternative).\n")
  if(x$gs == TRUE) {cat("Guenther/Schouten correcture performed.\n\n")}
  else {cat("No Guenther/Schouten correcture performed.\n\n")}

  cat(sprintf(
    "Input Parameters \n
Significance level : %.2f
Desired Power : %.2f %%
Effect size : %.2f
Correlation : %.2f
Standard deviation : %.2f
Allocation : %.2f \n
Results of sample size calculation \n
n Group X : %.0f
n Group Y : %.0f
n Total : %.0f
Resulting Power : %s" ,

    x$alpha,
    x$power*100,
    x$effect,
    x$corr,
    x$sd,
    x$r,
    x$n_X,
    x$n_Y,
    x$n,
    x$power_out
  )
  )

}

