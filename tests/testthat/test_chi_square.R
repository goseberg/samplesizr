context("Test n_chi_square")

table_5_2_data <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
                     0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                     124, 164, 186, 194, 186, 164, 124,
                     118, 160, 188, 194, 188, 160, 118,

                     0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
                     0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                     147, 189, 213, 219, 207, 180, 132,
                     141, 186, 207, 219, 207, 177, 129
                     )
table_5_2 <- array(table_5_2_data,dim=c(7,4,2))

r <- 1
p_X <- 0.1
exact <- 1
n <- 0

for (r in 1:2) {
   for (p_X in (1:7)*0.1){
     p_Y <- p_X + 0.2
     for (exact in 0:1) {
       n <- n_chi_square(sig=.05,pow=.8,pow.exact=exact,r=r,r.strict=TRUE, p_Y=p_Y,p_X=p_X)$n
       check <- table_5_2[p_X*10,exact+3,r]

       test_that("Test n_chi_square", {
         expect_equal(n,check)
        })
     }
    }
}
