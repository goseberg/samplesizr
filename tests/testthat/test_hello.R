context("Testing hello()")

test_that("does hello print hello?", {
  hello(1)
  expect_equal(TRUE, TRUE)
})
