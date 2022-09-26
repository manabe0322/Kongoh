test_that("addQ, !setequal(popAl, peakOneL)", {
  peakOneL <- c(9, 10, 11, 12)
  sizeOneL <- c(96, 100, 104, 108)
  heightOneL <- c(100, 2000, 100, 1800)
  popFreq <- c(0.03, 0.3, 0.25, 0.22, 0.18, 0.02)
  names(popFreq) <- 8:13
  lenRUOneL <- 4
  kitInfoOneL <- matrix("", 4, 6)
  colnames(kitInfoOneL) <- c("Marker", "Dye", "Length_of_the_repeat_unit", "Sex_chromosomal_marker", "Allele", "Size")
  kitInfoOneL[, 1] <- "D3S1358"
  kitInfoOneL[, 2] <- "B"
  kitInfoOneL[, 3] <- 4
  kitInfoOneL[, 5] <- 9:12
  kitInfoOneL[, 6] <- c(96.48, 100.64, 104.815, 108.84)
  addQData <- addQ(peakOneL, sizeOneL, heightOneL, popFreq, lenRUOneL, kitInfoOneL)
  peakOneL <- addQData[[1]]
  sizeOneL <- addQData[[2]]
  heightOneL <- addQData[[3]]
  QFreq <- addQData[[4]]
  expect_equal(length(peakOneL), 5)
  expect_equal(peakOneL[1], 9)
  expect_equal(peakOneL[2], 10)
  expect_equal(peakOneL[3], 11)
  expect_equal(peakOneL[4], 12)
  expect_equal(peakOneL[5], 8)
  expect_equal(length(sizeOneL), 5)
  expect_equal(sizeOneL[1], 96)
  expect_equal(sizeOneL[2], 100)
  expect_equal(sizeOneL[3], 104)
  expect_equal(sizeOneL[4], 108)
  expect_equal(sizeOneL[5], 92)
  expect_equal(length(heightOneL), 5)
  expect_equal(heightOneL[1], 100)
  expect_equal(heightOneL[2], 2000)
  expect_equal(heightOneL[3], 100)
  expect_equal(heightOneL[4], 1800)
  expect_equal(heightOneL[5], 0)
  expect_equal(QFreq, 0.05)
})

test_that("addQ, setequal(popAl, peakOneL)", {
  peakOneL <- c(8, 9, 10, 11, 12, 13)
  sizeOneL <- c(92, 96, 100, 104, 108, 112)
  heightOneL <- c(100, 100, 2000, 100, 1800, 100)
  popFreq <- c(0.03, 0.3, 0.25, 0.22, 0.18, 0.02)
  names(popFreq) <- 8:13
  lenRUOneL <- 4
  kitInfoOneL <- matrix("", 4, 6)
  colnames(kitInfoOneL) <- c("Marker", "Dye", "Length_of_the_repeat_unit", "Sex_chromosomal_marker", "Allele", "Size")
  kitInfoOneL[, 1] <- "D3S1358"
  kitInfoOneL[, 2] <- "B"
  kitInfoOneL[, 3] <- 4
  kitInfoOneL[, 5] <- 9:12
  kitInfoOneL[, 6] <- c(96.48, 100.64, 104.815, 108.84)
  addQData <- addQ(peakOneL, sizeOneL, heightOneL, popFreq, lenRUOneL, kitInfoOneL)
  peakOneL <- addQData[[1]]
  sizeOneL <- addQData[[2]]
  heightOneL <- addQData[[3]]
  QFreq <- addQData[[4]]
  expect_equal(length(peakOneL), 6)
  expect_equal(peakOneL[1], 8)
  expect_equal(peakOneL[2], 9)
  expect_equal(peakOneL[3], 10)
  expect_equal(peakOneL[4], 11)
  expect_equal(peakOneL[5], 12)
  expect_equal(peakOneL[6], 13)
  expect_equal(length(sizeOneL), 6)
  expect_equal(sizeOneL[1], 92)
  expect_equal(sizeOneL[2], 96)
  expect_equal(sizeOneL[3], 100)
  expect_equal(sizeOneL[4], 104)
  expect_equal(sizeOneL[5], 108)
  expect_equal(sizeOneL[6], 112)
  expect_equal(length(heightOneL), 6)
  expect_equal(heightOneL[1], 100)
  expect_equal(heightOneL[2], 100)
  expect_equal(heightOneL[3], 2000)
  expect_equal(heightOneL[4], 100)
  expect_equal(heightOneL[5], 1800)
  expect_equal(heightOneL[6], 100)
  expect_equal(QFreq, 0)
})

test_that("addQ, length(peakOneL) = 0", {
  peakOneL <- numeric(0)
  sizeOneL <- numeric(0)
  heightOneL <- numeric(0)
  popFreq <- c(0.03, 0.3, 0.25, 0.22, 0.18, 0.02)
  names(popFreq) <- 8:13
  lenRUOneL <- 4
  kitInfoOneL <- matrix("", 4, 6)
  colnames(kitInfoOneL) <- c("Marker", "Dye", "Length_of_the_repeat_unit", "Sex_chromosomal_marker", "Allele", "Size")
  kitInfoOneL[, 1] <- "D3S1358"
  kitInfoOneL[, 2] <- "B"
  kitInfoOneL[, 3] <- 4
  kitInfoOneL[, 5] <- 9:12
  kitInfoOneL[, 6] <- c(96.48, 100.64, 104.815, 108.84)
  addQData <- addQ(peakOneL, sizeOneL, heightOneL, popFreq, lenRUOneL, kitInfoOneL)
  peakOneL <- addQData[[1]]
  sizeOneL <- addQData[[2]]
  heightOneL <- addQData[[3]]
  QFreq <- addQData[[4]]
  expect_equal(length(peakOneL), 1)
  expect_equal(peakOneL[1], 9)
  expect_equal(length(sizeOneL), 1)
  expect_equal(sizeOneL[1], 96.48)
  expect_equal(length(heightOneL), 1)
  expect_equal(heightOneL[1], 0)
})
