
mat <- matrix(1,nrow=2,ncol=3)
mat1 <- mat
mat1[1,1] <- 4

test_that("SFS throws error/warning when it should",{
  expect_error(SFS(mat1))
  expect_warning(SFS(mat))
})

test_that("output does what it should",{
  expect_gte(min(SFS(matrix(c(1,0,0,0,0,0),nrow=2))),0)
  expect_equal(SFS(matrix(c(1,0,0,0,
                            0,1,1,1,
                            0,1,1,1,
                            0,0,0,0),nrow=4,byrow=T)),c(1,3,0))
  expect_equal(SFS(matrix(c(0,1,0,0,
                            0,1,0,0,
                            0,1,0,0,
                            0,1,0,0),nrow=4,byrow=T)),c(0,0,0))
  expect_equal(length(SFS(matrix(0,nrow=100,ncol=200))),99)

})
