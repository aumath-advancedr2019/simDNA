

test_that("it throws an error",{
  expect_error(TajimaD(c(-1,3,0,0)))
  expect_error(TajimaD(c(1.5,3,0,0)))
})

