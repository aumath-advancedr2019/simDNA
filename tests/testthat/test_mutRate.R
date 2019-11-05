
test_that("that output is a list with 2 elements",{
  expect_is(mutRate(c(1,3,0,0)),"list")
  expect_equal(length(mutRate(c(1,3,0,0))),2)
})

test_that("that output is bigger or equal 0",{
  expect_equal(mutRate(c(0,0,0,0))$Watterson,0)
  expect_equal(mutRate(c(0,0,0,0))$pairwDiff,0)
  expect_gt(mutRate(c(1,3,0,0))$Watterson,0)
  expect_gt(mutRate(c(1,3,0,0))$pairwDiff,0)
})

test_that("mutRate throw an error if SFS is negative",{
  expect_error(mutRate(c(-1,-3,0,0)))
})
