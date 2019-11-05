
mat <- matrix(c(0,1,1,0,1,0,0,
                0,0,1,0,0,0,1,
                0,0,0,0,1,0,0,
                0,0,1,0,0,0,1),4,7,byrow=T)

test_that("it throws an error",{
  expect_error(SNP(matrix(0.5,nrow=5,ncol=2)))
})

test_that("that output is correct",{
  expect_equal(SNP(mat)$positions,c(2,3,5,7))
  expect_equal(SNP(mat)$SNPmat,mat[,-c(1,4,6)])
  expect_equal(length(SNP(mat)),2)
  expect_is(SNP(mat),"list")
})
