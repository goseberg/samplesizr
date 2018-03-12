#' samplesizr: Sample size calculation with R based on the Book:
#'
#' [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition.
#'
#' @section Functions:
#' \itemize{
#'
#'   \item{\code{\link{n_ancova}}: }{
#'     Sample Size Calculation for the Analysis of Covariances (ANCOVA)
#'     for test on mean difference for two samples correlated to a covariate.
#'     See pages 18 - 20 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{n_chisq}}: }{
#'     Sample size Calculation for the Chi-Square Test for two
#'     independent samples with binary data using the absolute rate
#'     difference quantifying the effect of an intervention.
#'     See pages 21 - 26 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{n_chisq_mult_groups}}: }{
#'     Sample size calculation for the Chi-Square test comparing
#'     rates of \eqn{k > 2} independent samples with respect to binary data.
#'     See page 30 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{n_ftest}}: }{
#'     Sample size calculation for the F-Test comparing
#'     means of \eqn{k > 2} independent samples with respect to normal data.
#'     See page 29 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{n_ttest}}: }{
#'     Sample size calculation for the t-Test comparing
#'     two independent samples with respect to normal data.
#'     See pages 16 - 18 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{n_ztest}}: }{
#'     Sample size calculation for the z-Test comparing
#'     two independent samples with respect to normal data.
#'     See pages 13 - 16 in [1] for further details.
#'   }
#'}
#'
#'\itemize{
#'   \item{\code{\link{power_binomial}}: }{
#'     Power Calculation for the Chi-Square Test for two
#'     independent samples with respect to binary data using the absolute rate
#'     difference quantifying the effect of an intervention.
#'     See pages 21 - 26 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{power_chisq_mult_groups}}: }{
#'     Power calculation for the Chi-Square test comparing
#'     rates of \eqn{k > 2} independent samples with respect to binary data.
#'     See page 30 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{power_ftest}}: }{
#'     Power calculation for the F-Test comparing
#'     means of \eqn{k > 2} independent samples with respect to normal data.
#'     See page 29 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{power_ttest}}: }{
#'     Power calculation for the t-Test comparing
#'     two independent samples with respect to normal data.
#'     See pages 16 - 18 in [1] for further details.
#'   }
#'
#'   \item{\code{\link{power_ztest}}: }{
#'     Power calculation for the z-Test comparing
#'     two independent samples with respect to normal data.
#'     See pages 13 - 16 in [1] for further details.
#'   }
#'
#' }
#'
#' @docType package
#' @name samplesizr

NULL
