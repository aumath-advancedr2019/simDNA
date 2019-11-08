
test_that("output has the right class",{
  expect_is(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                      proportion = 0.9),"matrix")
  expect_is(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                      proportion = 0.9),"sudExpPop")
  expect_is(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                      proportion = 0.9),"population")
  expect_is(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
            expRate = 1.5),"matrix")
  expect_is(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
                      expRate = 1.5),"varPop")
  expect_is(simDNAseq(n = 8, seqLen = 20, mutRate = 5, popType = "varPop",
                      expRate = 1.5),"population")
  expect_is(simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop"),"matrix")
  expect_is(simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop"),"fixedPop")
  expect_is(simDNAseq(n = 10, seqLen = 15, mutRate = 2, popType = "fixedPop"),"population")
})

test_that("output is as expected",{
  expect_equal(as.numeric(names(table(simDNAseq(n = 50, seqLen = 38, mutRate = 2, popType = "sudExpPop", expansionTime = 2,
                        proportion = 0.9)))),c(0,1))
  expect_equal(as.numeric(names(table(simDNAseq(n = 20, seqLen = 15, mutRate = 6, popType = "varPop",
                                                expRate = 1.8)))),c(0,1))
  expect_equal(as.numeric(names(table(simDNAseq(n = 13, seqLen = 21, mutRate = 8.5, popType = "fixedPop")))),c(0,1))
  expect_equal(dim(simDNAseq(n = 50, seqLen = 38, mutRate = 2, popType = "sudExpPop", expansionTime = 2,
                                                proportion = 0.9)),c(50,38))
})

test_that("that it throws an error",{
  expect_error(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                         proportion = 0))
  expect_error(simDNAseq(n = 25, seqLen = 30, mutRate = 8, popType = "sudExpPop", expansionTime = 2,
                         proportion = 8))
})





