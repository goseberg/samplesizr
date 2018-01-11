context("Test n_z_test")

table_4_1_data <- c(11.7, 14.9, 17.8, 24.0,
                    7.90, 10.5, 13.0, 18.4,
                    6.40, 8.60, 10.8, 15.8
)

table_4_2_z_test_data <- c( 3140, 786,  350, 198, 126, 88,
                            4204, 1052, 468, 264, 170, 118)

table_4_1 <- array(table_4_1_data,dim=c(4,3))
table_4_2_z_test <- array(table_4_2_z_test_data,dim=c(6,2))

# Check Table 4_1

# sig_vector <- c(.01, .05, .10)
# pow_vector <- c(0.80, 0.90, 0.95, 0.99)
#
# for (i_sig in 1:3) {
#
#   for (i_pow in 1:4){
#
#     sig    <- sig_vector[i_sig]
#     pow    <- pow_vector[i_pow]
#     effect <- 1
#     std    <- 1
#     r      <- 1
#
#     val_f <- n_ztest(
#         sig      = sig,
#         pow      = pow,
#         r        = r,
#         r.strict = TRUE,
#         effect   = effect,
#         std      = std
#     )$n_x
#
#     val_table <- 2 * table_4_1[i_pow,i_sig]
#     check     <- .group_balance(
#       n_x= val_table, r = 1, r.strict = FALSE
#     )$n_x
#
#     test_that("Test n_z_test", {
#
#       expect_equal(
#         val_f, check,
#         info = sprintf(
#           "params: sig=%.2f, power=%.2f, effect=%.2f, std=%.2f, r = %.2f",
#           sig, pow, effect, std, r
#         )
#       )
#     })
#   }
# }

# Check Table 4_2

effect <- 0.1 * (1:6)
#pow_vector_4_2 <- c(0.8, 0.9)

for (i_effect in 1:6) {
  for (i_pow in 1:2){
    val_f <- n_ztest(sig = .05, pow = pow_vector[i_pow], r = 1, r.strict = TRUE, effect = effect[i_effect], std = 1)$n
    val_table <- table_4_2_z_test[i_effect, i_pow]

    test_that("Test n_z_test", {
      expect_equal(val_f,val_table)
    })
  }
}
