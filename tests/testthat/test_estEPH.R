test_that("estEPH", {
  peakOneL <- 15
  sizeOneL <- 121.35
  numMc <- 3
  tempMean <- 7000
  mrOneC <- 0.3
  degOneC <- -0.0025
  sizeMean <- 215.76
  bsrPeakOneL <- 11.1547
  fsrPeakOneL <- 11.1547
  dsrPeakOneL <- 11.1547
  aeParOneL <- c(0.111, 37.5)
  hbParOneL <- c(0.0113, 198)
  bsrParOneL <- c(0.00943, -0.0463, 27.1)
  fsrParOneL <- c(0.000402, 0.00101, 1996)
  dsrParOneL <- c(0.000463, -0.00150, 1036)
  m2srParOneL <- c(0, 0)
  minAE <- 0.058
  minHb <- 0.046
  maxBSR <- 0.7
  maxFSR <- 0.35
  maxDSR <- 0.13
  maxM2SR <- 0.12

  set.seed(1)
  ephData <- estEPH(peakOneL, sizeOneL, numMc, tempMean, mrOneC, degOneC, sizeMean, bsrPeakOneL, fsrPeakOneL, dsrPeakOneL, aeParOneL, hbParOneL, bsrParOneL, fsrParOneL, dsrParOneL, m2srParOneL, minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR)
  expect_equal(round(ephData[[1]][1, 1], 4), 236.5660)
  expect_equal(round(ephData[[2]][1, 1], 5), 10.91188)
  expect_equal(round(ephData[[3]][1, 1], 8), 0.01776852)
  expect_equal(round(ephData[[4]][1, 1], 8), 0.08681668)
  expect_equal(ephData[[5]][1, 1], 0)
  expect_equal(as.numeric(ephData[[6]][1, 1]), 0.3)
  expect_equal(as.numeric(ephData[[6]][2, 1]), 15)
  expect_equal(as.numeric(ephData[[6]][3, 1]), -0.0025)
})

