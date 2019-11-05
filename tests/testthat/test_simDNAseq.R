
test_that("output has the right class",{
  expect_is(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                      proportion = 0.9),"matrix")
  expect_is(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                      proportion = 0.9),"sudExpPop")
  expect_is(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
            expRate = 1.5),"matrix")
  expect_is(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
                      expRate = 1.5),"varPop")
  expect_is(simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop"),"matrix")
  expect_is(simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop"),"fixedPop")
})

test_that("output is as expected",{

})
