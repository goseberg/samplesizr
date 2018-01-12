# TEST n_ttest
#
# This code is testing the function "n_ttest" according to [1].

# [1] M. Kieser:  Fallzahlberechnung in der medizinischen Forschung [2018],
#                 Tabelle 4.2, p. 17-18.

context("Test n_ttest")

# I DATA TABLE
#
#   CAVE: Note that the sample sizes are aranged in columns sorted in ascending
#         order

pow_vector    <- c(0.80, 0.90)
effect_vector <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.25, 1.5)
n_ttest.80    <- c(3142, 788,  352, 200, 128, 90, 68, 52, 42, 34, 24, 18)
n_ttest.90    <- c(4206, 1054, 470, 266, 172, 120, 88, 68, 54, 46, 30, 22)

r             <- c(1, 2, 3)
n_ttest_r     <- c(172, 192, 228)

table_42_t <- data.frame(
                effect = effect_vector,
                n.80   = n_ttest.80,
                n.90   = n_ttest.90
              )

# II TEST LOOP

# TABLE 4.2

for (i_effect in 1:length(effect_vector)) {

  for (i_pow in 1:length(pow_vector)){

    alpha    <- .05
    power    <- pow_vector[i_pow]
    effect <- effect_vector[i_effect]
    std    <- 1
    r      <- 1

    val_f <- n_ttest(
        alpha    = alpha,
        power    = power,
        r        = r,
        r.strict = TRUE,
        effect   = effect,
        sd      = sd
    )$n

    val_table <- table_42[i_effect, i_pow + 1]

    test_that("Test n_ttest with Table 4.2 [1]", {

      expect_equal(
        val_f, val_table,
        info = sprintf(
          "params: alpha=%.2f, power=%.2f, effect=%.2f, std=%.2f, r = %.2f",
          alpha, power, effect, std, r
        )

      )
    })
  }
}

# p.18 [1]

for (i_r in 1:length(r_vector)) {

  alpha    <- .05
  power    <- .90
  effect <- .5
  std    <- 1
  r      <- r_vector[i_r]

  val_f <- n_ttest(
    alpha      = alpha,
    power      = power,
    r        = r,
    r.strict = TRUE,
    effect   = effect,
    sd      = sd
  )$n

  val_table <- n_ttest_r[i_r]

  test_that("Test n_ttest with p. 18 [1]", {

    expect_equal(
      val_f, val_table,
      info = sprintf(
        "params: alpha=%.2f, power=%.2f, effect=%.2f, std=%.2f, r = %.2f",
        alpha, power, effect, std, r
      )
    )})
}
