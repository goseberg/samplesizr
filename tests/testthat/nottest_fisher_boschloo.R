# TEST n_ttest
#
# This code is testing the function "n_fisher_boschloo" according to [1].

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 Tabelle 5.3, p. 27.

context("Test n_fisher_boschloo")

# I DATA

# TABLE 5.2

p_X_vector <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
p_Y_vector <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# r = 1

n.r1.fb_approx   <- c(130, 172, 196, 206, 196, 172, 130)
n.r1.fb_exact    <- c(126, 168, 190, 204, 190, 168, 126)
pow.r1.fb_approx <- c(81.7, 81.4, 81.8, 80.8, 81.8, 81.4, 81.7)
pow.r1.fb_exact  <- c(80.0, 80.2, 80.1, 80.1, 80.1, 80.2, 80.0)

table_53_r1 <- data.frame(
  p_X = p_X_vector,
  p_Y = p_Y_vector,
  n_exact    = n.r1.fb_exact,
  pow_exact  = pow.r1.fb_exact,
  n_approx   = n.r1.fb_approx,
  pow_approx = pow.r1.fb_approx
)

# r = 2

n.r2.fb_approx   <- c(150, 195, 225, 237, 219, 192, 144)
n.r2.fb_exact    <- c(147, 186, 213, 219, 210, 186, 138)
pow.r2.fb_approx <- c(82.4, 82.2, 82.7, 80.8, 81.8, 82.2, 81.5)
pow.r2.fb_exact  <- c(80.7, 80.3, 80.4, 80.2, 80.3, 80.7, 80.7)

table_53_r2 <- data.frame(
  p_X = p_X_vector,
  p_Y = p_Y_vector,
  n_exact    = n.r2.fb_exact,
  pow_exact  = pow.r2.fb_exact,
  n_approx   = n.r2.fb_approx,
  pow_approx = pow.r2.fb_approx
)


# II TEST LOOP

# TABLE 5.3

for (r in 1:2) {
  for (i_p_X in 1:length(p_X_vector)) {

    alpha <- .025
    power <- .80
    p_X <- p_X_vector[i_p_X]
    p_Y <- p_Y_vector[i_p_X]

    val_f_exact <- n_fisher_boschloo(
      alpha       = alpha,
      power       = power,
      power.exact = TRUE,
      r        = r,
      p_X      = p_X_vector[i_p_X],
      p_Y      = p_Y_vector[i_p_X]
    )

    val_f_approx <- n_fisher_boschloo(
      alpha      = alpha,
      power      = power,
      power.exact= FALSE,
      r        = r,
      p_X      = p_X_vector[i_p_X],
      p_Y      = p_Y_vector[i_p_X]
    )

    if (r == 1) {val_table <- table_53_r1[i_p_X, ]}
    if (r == 2) {val_table <- table_53_r2[i_p_X, ]}

    test_that("Test n_fisher_boschloo with Table 5.3 [1]", {

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
