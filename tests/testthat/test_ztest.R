# TEST n_ztest
#
# This code is testing the function "n_ztest" according to[1] .

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 Tabelle 4.2, p. 17-18.

context("Test n_z_test")

# I DATA TABLE
#
#   CAVE: Note that the sample sizes are aranged in columns sorted in ascending
#         order

pow_vector    <- c(0.80, 0.90)
effect_vector <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.25, 1.5)
n_ztest.80    <- c(3140, 786, 350, 198, 126, 88, 66, 50, 40, 32, 22, 14)
n_ztest.90    <- c(4204, 1052, 468, 264, 170, 118, 86, 66, 52, 44, 28, 20)

table_42_z <- data.frame(
              effect = effect_vector,
              n.80   = n_ztest.80,
              n.90   = n_ztest.90
            )


# II TEST LOOP

for (i_effect in 1:length(effect_vector)) {

  for (i_pow in 1:length(pow_vector)){

    alpha    <- .05
    power    <- pow_vector[i_pow]
    effect <- effect_vector[i_effect]
    sd    <- 1
    r      <- 1

    val_f <- n_ztest(
        alpha      = alpha,
        power      = power,
        r        = r,
        effect   = effect,
        sd      = sd
    )$n

    val_table <- table_42_z[i_effect, i_pow + 1]

    test_that("Test n_z_test", {

      expect_equal(
        val_f, val_table,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, effect=%.2f, sd=%.2f, r = %.2f",
          alpha, power, effect, sd, r
        )

      )
    })
  }
}
