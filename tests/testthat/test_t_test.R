context("Test n_t_test")

table_4_2_t_test_data <- c( 3142, 788,  352, 200, 128, 90,
                            4206, 1054, 470, 266, 172, 120)

table_4_2_t_test <- array(table_4_2_t_test_data,dim=c(6,2))

# Check Table 4_2

pow_vector <- c(0.80, 0.90)
effect <- 0.1 * (1:6)

for (i_effect in 1:6) {
  for (i_pow in 1:2){
    val_f <- n_ttest(sig = .05, pow = pow_vector[i_pow], r = 1, r.strict = TRUE, effect = effect[i_effect], std = 1)$n
    val_table <- table_4_2_t_test[i_effect, i_pow]

    test_that("Test n_t_test", {
      expect_equal(val_f,val_table)
    })
  }
}
