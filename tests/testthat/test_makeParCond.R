test_that("makeParCond", {
  mrOneC <- c(0.2, 0.4, 0.6, 0.8)
  peakOneL <- c(11, 12, 13, 99)
  degOneC <- c(-0.005, -0.0025, 0)
  paramCond <- makeParCond(mrOneC, peakOneL, degOneC)
  expect_equal(ncol(paramCond), length(mrOneC) * length(peakOneL) * length(degOneC))
})
