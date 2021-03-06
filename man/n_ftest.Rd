% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_ftest.R
\name{n_ftest}
\alias{n_ftest}
\title{Sample Size Calculation for the f Test comparing means of \eqn{k > 2} groups}
\usage{
n_ftest(mu_A, sd, n.groups, alpha, power)
}
\arguments{
\item{mu_A}{Vector \eqn{\mu_A} of expected means on the alternative.}

\item{sd}{Standard deviation \eqn{sigma}.}

\item{n.groups}{Number of Groups \eqn{k}.}

\item{alpha}{Significance level \eqn{\alpha}.}

\item{power}{Desired Power \eqn{1-\beta}.}
}
\value{
\code{n_ftest} returns an object of type list. The resulting
  Sample Sizes are located in entrys named \code{n_per_group}, \code{n}.
  The resulting power is named \code{power_out}.
}
\description{
\code{n_ftest} performs the Sample Size calculation for  the f-test
  comparing means of \eqn{k > 2} independent samples.
  The method used here is based on the page 29 in [1].
  The Sample Size is calculated using an iterative approach.
\describe{
  \item{Null Hypothesis:}{\eqn{\mu_1 = \mu_2 = ... = \mu_k}}
  \item{Alternative Hypothesis:}{There are \eqn{i,j} with \eqn{\mu_i \ne \mu_j}}
}
}
\details{
[1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition
}
\examples{
n_ftest(mu_A = c(1, 2, 3), sd = 20, n.groups = 3, alpha = .025, power = .8)

}
