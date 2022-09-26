test_that("makeSrX, modelName = Allele", {
  peakOneL <- c(9, 10, 11, 12, 8) #8: alQOneL
  modelName <- "Allele"
  srName <- "BSR"
  alCorOneL <- matrix("", 6, 6)
  colnames(alCorOneL) <- c("Marker", "Allele", "LUS", "CA_BSR", "CA_FSR", "CA_DSR")
  alCorOneL[, 1] <- "D3S1358"
  alCorOneL[, 2] <- 7:12
  alCorOneL[, 3] <- 1:6
  alCorOneL[, 4] <- 11:16
  alCorOneL[, 5] <- 21:26
  alCorOneL[, 6] <- 31:36
  peakSr <- makeSrX(peakOneL, modelName, srName, alCorOneL)
  expect_equal(length(peakSr), 5)
  expect_equal(peakSr[1], 9)
  expect_equal(peakSr[2], 10)
  expect_equal(peakSr[3], 11)
  expect_equal(peakSr[4], 12)
  expect_equal(peakSr[5], 8)
})

test_that("makeSrX, modelName = LUS", {
  peakOneL <- c(9, 10, 11, 12, 8) #8: alQOneL
  modelName <- "LUS"
  srName <- "BSR"
  alCorOneL <- matrix("", 6, 6)
  colnames(alCorOneL) <- c("Marker", "Allele", "LUS", "CA_BSR", "CA_FSR", "CA_DSR")
  alCorOneL[, 1] <- "D3S1358"
  alCorOneL[, 2] <- 7:12
  alCorOneL[, 3] <- 1:6
  alCorOneL[, 4] <- 11:16
  alCorOneL[, 5] <- 21:26
  alCorOneL[, 6] <- 31:36
  peakSr <- makeSrX(peakOneL, modelName, srName, alCorOneL)
  expect_equal(length(peakSr), 5)
  expect_equal(peakSr[1], 2)
  expect_equal(peakSr[2], 3)
  expect_equal(peakSr[3], 4)
  expect_equal(peakSr[4], 5)
  expect_equal(peakSr[5], 6)
})

test_that("makeSrX, modelName = Multi-seq, CA_BSR", {
  peakOneL <- c(9, 10, 11, 12, 8) #8: alQOneL
  modelName <- "Multi-seq"
  srName <- "BSR"
  alCorOneL <- matrix("", 6, 6)
  colnames(alCorOneL) <- c("Marker", "Allele", "LUS", "CA_BSR", "CA_FSR", "CA_DSR")
  alCorOneL[, 1] <- "D3S1358"
  alCorOneL[, 2] <- 7:12
  alCorOneL[, 3] <- 1:6
  alCorOneL[, 4] <- 11:16
  alCorOneL[, 5] <- 21:26
  alCorOneL[, 6] <- 31:36
  peakSr <- makeSrX(peakOneL, modelName, srName, alCorOneL)
  expect_equal(length(peakSr), 5)
  expect_equal(peakSr[1], 12)
  expect_equal(peakSr[2], 13)
  expect_equal(peakSr[3], 14)
  expect_equal(peakSr[4], 15)
  expect_equal(peakSr[5], 16)
})

test_that("makeSrX, modelName = Multi-seq, CA_FSR", {
  peakOneL <- c(9, 10, 11, 12, 8) #8: alQOneL
  modelName <- "Multi-seq"
  srName <- "FSR"
  alCorOneL <- matrix("", 6, 6)
  colnames(alCorOneL) <- c("Marker", "Allele", "LUS", "CA_BSR", "CA_FSR", "CA_DSR")
  alCorOneL[, 1] <- "D3S1358"
  alCorOneL[, 2] <- 7:12
  alCorOneL[, 3] <- 1:6
  alCorOneL[, 4] <- 11:16
  alCorOneL[, 5] <- 21:26
  alCorOneL[, 6] <- 31:36
  peakSr <- makeSrX(peakOneL, modelName, srName, alCorOneL)
  expect_equal(length(peakSr), 5)
  expect_equal(peakSr[1], 22)
  expect_equal(peakSr[2], 23)
  expect_equal(peakSr[3], 24)
  expect_equal(peakSr[4], 25)
  expect_equal(peakSr[5], 26)
})
