% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_fisher_boschloo.R
\name{power_fisher_boschloo}
\alias{power_fisher_boschloo}
\title{Power Calculation for the Fisher-Boschloo-Test}
\usage{
power_fisher_boschloo(p_Y, p_X, n_Y, n_X, alpha, SW = 1e-04, MAXH = 10)
}
\arguments{
\item{p_Y}{Event rate of Group Y on the alternative.}

\item{p_X}{Event rate of Group X on the alternative.}

\item{n_Y}{Sample size of Group Y.}

\item{n_X}{Sample size of Group X.}

\item{alpha}{Significance level \eqn{\alpha}.}

\item{SW}{Default = .001. Step width.}

\item{MAXH}{Default = 10.}
}
\value{
\code{n_fisher_boschloo} returns an object of type list. The resulting
  Sample Sizes are located in entrys named \code{n_X}, \code{n_Y}, \code{n}.
  The resulting power is named \code{power_out}.
}
\description{
\code{power_fisher_boschloo} performs the exact power calculation for the
  fisher-boschloo test.
  The method used here is written by S. Wellek.
  See [2] for further details.
}
\details{
[2] S. Wellek: Nearly exact sample size calculation for powerful
non-randomized tests for differences between binomial proportions [2015],
Statistica Neerlandica
}
\examples{
power_fisher_boschloo(p_Y = .5, p_X = .3, n_Y = 70, n_X = 40, alpha = .025)

}
