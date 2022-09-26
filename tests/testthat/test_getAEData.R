test_that("getAEData", {
  calData2 <- calData2One <- list()
  mat1 <- matrix(c(16, 125.35, 233), ncol = 1)
  mat2 <- matrix(c(17, 181.05, 229, 18, 185.18, 246), ncol = 2)
  mat3 <- matrix(c(9, 244.08, 196, 12, 256.22, 181), ncol = 2)
  mat4 <- matrix(c(9, 295.17, 321, 11, 302.93, 311), ncol = 2)
  mat5 <- matrix(c(8, 350.86, 35, 11, 362.94, 140), ncol = 2)
  rownames(mat1) <- rownames(mat2) <- rownames(mat3) <- rownames(mat4) <- rownames(mat5) <- c("peakOneL2", "sizeOneL2", "heightOneL2")
  calData2One[[1]] <- mat1
  calData2One[[2]] <- mat2
  calData2One[[3]] <- mat3
  calData2One[[4]] <- mat4
  calData2One[[5]] <- mat5
  calData2[[1]] <- calData2One
  aeDataList <- getAEData(calData2)
  aeData <- aeDataList[[1]]
  aeHeightData <- aeDataList[[2]]
  aeHeight <- c(233 / 2, (229 + 246) / 2, (196 + 181) / 2, (321 + 311) / 2, (35 + 140) / 2)
  ae <- aeHeight / mean(aeHeight)
  for(i in 1:5){
    expect_equal(aeData[1, i], ae[i])
    expect_equal(aeHeightData[1, i], aeHeight[i])
  }

  calData2One[[1]] <- 0
  calData2One[[4]] <- 0
  calData2[[1]] <- calData2One
  aeDataList <- getAEData(calData2)
  aeData <- aeDataList[[1]]
  aeHeightData <- aeDataList[[2]]
  expect_equal(nrow(aeData), 0)
  expect_equal(nrow(aeHeightData), 0)
})

