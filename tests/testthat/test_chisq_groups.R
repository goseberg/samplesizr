# TEST n_ftest
#
# This code is testing the function "n_chisq_groups" according to [1].

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 p. 30.

context("Test n_chisq_mult_groups")

# I DATA

case_1 <- c(.3, .5, .4)
case_2 <- c(.3, .5, .5)
cases  <- data.frame(case_1, case_2)

n      <- c(348, 267)

# II TEST LOOP

# TABLE 4.2

for (i_case in 1:ncol(cases)) {
  test_that("Test n_chisq_mult_groups with p.30 [1]", {

    alpha      <- .05
    power      <- .80
    n.groups <- 3
    p_A      <- cases[ , i_case]

    val_f <- n_chisq_mult_groups(
      alpha    = alpha,
      power    = power,
      n.groups = n.groups,
      p_A      = p_A
    )$n

    val_table <- n[i_case]

    expect_equal(
      val_f, val_table,
      info = sprintf(
        "params: alpha=%.2f, power=%.2f, n.groups=%.2f, p_A=%.s",
        alpha, power, n.groups, paste(p_A,collapse = ",")
      )
    )}

  )
}
