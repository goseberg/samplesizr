# TEST n_chisq
#
# This code is testing the function "n_chisq" according to [1].

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 Tabelle 5.2, p. 24.

context("Test n_chisq")

# I DATA

# TABLE 5.2

p_X_vector <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
p_Y_vector <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# r = 1

n.r1.chisq_approx   <- c(124, 164, 186, 194, 186, 164, 124)
n.r1.chisq_exact    <- c(118, 160, 188, 194, 188, 160, 118)
pow.r1.chisq_approx <- c(81.0, 80.7, 79.9, 80.7, 79.9, 80.7, 81.0)
pow.r1.chisq_exact  <- c(80.5, 80.1, 80.0, 80.7, 80.0, 80.1, 80.5)

table_52_r1 <- data.frame(
              p_X = p_X_vector,
              p_Y = p_Y_vector,
              n_exact    = n.r1.chisq_exact,
              pow_exact  = pow.r1.chisq_exact,
              n_approx   = n.r1.chisq_approx,
              pow_approx = pow.r1.chisq_approx
            )

# r = 2

n.r2.chisq_approx   <- c(147, 189, 213, 219, 207, 180, 132)
n.r2.chisq_exact    <- c(141, 186, 207, 219, 207, 177, 129)
pow.r2.chisq_approx <- c(82.4, 81.1, 80.4, 80.3, 80.3, 81.3, 81.5)
pow.r2.chisq_exact  <- c(80.2, 80.3, 80.1, 80.3, 80.3, 80.5, 80.5)

table_52_r2 <- data.frame(
              p_X = p_X_vector,
              p_Y = p_Y_vector,
              n_exact    = n.r2.chisq_exact,
              pow_exact  = pow.r2.chisq_exact,
              n_approx   = n.r2.chisq_approx,
              pow_approx = pow.r2.chisq_approx
            )

# p.24

r_vector  <- c(3,5)
n.r       <- c(252, 342)

# II TEST LOOP

# TABLE 5.2

for (r in 1:2) {
  for (i_p_X in 1:length(p_X_vector)) {

    alpha <- .05
    power <- .80
    p_X <- p_X_vector[i_p_X]
    p_Y <- p_Y_vector[i_p_X]

    val_f_exact <- n_chisq(
      alpha      = alpha,
      power      = power,
      power.exact= TRUE,
      r        = r,
      p_X      = p_X_vector[i_p_X],
      p_Y      = p_Y_vector[i_p_X]
    )

    val_f_approx <- n_chisq(
      alpha      = alpha,
      power      = power,
      power.exact= FALSE,
      r        = r,
      p_X      = p_X_vector[i_p_X],
      p_Y      = p_Y_vector[i_p_X]
    )

    if (r == 1) {val_table <- table_52_r1[i_p_X, ]}
    if (r == 2) {val_table <- table_52_r2[i_p_X, ]}

    test_that("Test n_chisq with Table 5.2 [1]", {

      expect_equal(
        val_f_exact$n, val_table$n_exact,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, r = %.2f, p_X=%.2f, p_Y=%.2f",
          alpha, power, r, p_X, p_Y
        )
      )

      expect_equal(
        round(val_f_exact$power_out,3) * 100, val_table$pow_exact,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, r = %.2f, p_X=%.2f, p_Y=%.2f",
          alpha, power, r, p_X, p_Y
         )
        )

      expect_equal(
        val_f_approx$n, val_table$n_approx,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, r = %.2f, p_X=%.2f, p_Y=%.2f",
          alpha, power, r, p_X, p_Y
        )
      )

      expect_equal(
        round(val_f_approx$power_out,3) * 100, val_table$pow_approx,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, r = %.2f, p_X=%.2f, p_Y=%.2f",
          alpha, power, r, p_X, p_Y
         )
        )

    })
  }
}

# p.24 [1]


for (i_r in 1:length(r_vector)) {

  alpha <- .05
  power <- .80
  p_X <- .3
  p_Y <- .5
  r   <- r_vector[i_r]

  val_f <- n_chisq(
    alpha      = alpha,
    power      = power,
    power.exact= TRUE,
    r        = r,
    p_X      = p_X,
    p_Y      = p_Y
  )

  val_table <- n.r[i_r]

  test_that("Test n_chisq with p.24 [1]", {

    expect_equal(
      val_f$n, val_table,
      info = sprintf(
        "params: alpha=%.2f, power=%.2f, r = %.2f, p_X=%.2f, p_Y=%.2f",
        alpha, power, r, p_X, p_Y
      )
    )

  })
}
