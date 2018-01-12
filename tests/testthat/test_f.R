# TEST n_ftest
#
# This code is testing the function "n_ftest" according to [1].

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 p. 29.

context("Test n_ftest")

# I DATA

case_1 <- c(0, 10, 5)
case_2 <- c(0, 10, 10)
cases  <- data.frame(case_1,case_2)
n_f    <- c(348, 267)

# II TEST LOOP

# TABLE 4.2

for (i_case in 1:ncol(cases)) {
  test_that("Test n_ftest with p.29 [1]", {

    sig      <- .05
    pow      <- .90
    n.groups <- 3
    mu       <- cases[ ,i_case]
    std      <- 20

    val_f <- n_ftest(
      sig      = sig,
      pow      = pow,
      n.groups = n.groups,
      mu       = mu,
      std      = std
    )$n

    val_table <- n_f[i_case]

   expect_equal(
    val_f, val_table,
    info = sprintf(
      "params: sig=%.2f, power=%.2f, mu=%s, std=%.2f",
      sig, pow, paste(mu, collapse = ","), std
    )
   )}

  )
}
