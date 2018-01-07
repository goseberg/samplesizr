context("Test n_chi_square")

allocation <- c(1,2)
p<- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7)
table_5_2 <- array(c(allocation,p,method))

test_that("f(1) == 1", {


  expect_equal(f(1), 1)

})


# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("approximativ","exact")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("","Matrix2")

# Take these vectors as input to the array.
result <- array(c(vector1,vector2,c(1,2)),dim = c(3,3,2),dimnames = list(row.names,column.names,
                                                                  matrix.names))
print(result)

re
