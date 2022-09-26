test_that("estGamma", {
  peakOneL <- 15
  sizeOneL <- 121.35
  numMc <- 1000
  tempMean <- 7000
  mrOneC <- 0.3
  degOneC <- -0.0025
  sizeMean <- 215.76
  bsrPeakOneL <- 11.1547
  fsrPeakOneL <- 11.1547
  dsrPeakOneL <- 11.1547
  aeParamOneL <- c(0.111, 37.5)
  hbParamOneL <- c(0.0113, 198)
  bsrParamOneL <- c(0.00943, -0.0463, 27.1)
  fsrParamOneL <- c(0.000402, 0.00101, 1996)
  dsrParamOneL <- c(0.000463, -0.00150, 1036)
  m2srParamOneL <- c(0, 0)
  minAE <- 0.058
  minHb <- 0.046
  maxBSR <- 0.7
  maxFSR <- 0.35
  maxDSR <- 0.13
  maxM2SR <- 0.12

  set.seed(1)
  ephData <- estEPH(peakOneL, sizeOneL, numMc, tempMean, mrOneC, degOneC, sizeMean, bsrPeakOneL, fsrPeakOneL, dsrPeakOneL, aeParamOneL, hbParamOneL, bsrParamOneL, fsrParamOneL, dsrParamOneL, m2srParamOneL, minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR)
  gammaAl <- estGamma(ephData[[1]])
  gammaBs <- estGamma(ephData[[2]])
  gammaFs <- estGamma(ephData[[3]])
  gammaDs <- estGamma(ephData[[4]])
  gammaM2s <- estGamma(ephData[[5]])

  expect_equal(round(gammaAl[1, 1], 5), 25.86290)
  expect_equal(round(gammaAl[2, 1], 5), 9.67065)
  expect_equal(round(gammaBs[1, 1], 6), 8.388144)
  expect_equal(round(gammaBs[2, 1], 6), 1.829865)
  expect_equal(round(gammaFs[1, 1], 7), 0.2816043)
  expect_equal(round(gammaFs[2, 1], 7), 21.9175423)
  expect_equal(round(gammaDs[1, 1], 7), 0.3040519)
  expect_equal(round(gammaDs[2, 1], 7), 8.6015010)
  expect_equal(gammaM2s[1, 1], 0)
  expect_equal(gammaM2s[2, 1], 0)
})
