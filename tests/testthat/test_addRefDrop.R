test_that("addRefDrop pattern1 only-gt", {
  dropAl <- 17
  pgResult <- matrix(0, 8, 7)
  colnames(pgResult) <- c("U", "U", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")
  pgResult[1, ] <- c(15, 15, 15, 15, 0.5, 0.05, 0.2)
  pgResult[2, ] <- c(15, 15, 15, 99, 0.4, 0.04, 0.15)
  pgResult[3, ] <- c(15, 15, 99, 99, 0.3, 0.03, 0.1)
  pgResult[4, ] <- c(15, 99, 15, 15, 0.2, 0.02, 0.15)
  pgResult[5, ] <- c(15, 99, 15, 99, 0.025, 0.0025, 0.2)
  pgResult[6, ] <- c(15, 99, 99, 99, 0.025, 0.0025, 0.05)
  pgResult[7, ] <- c(99, 99, 15, 15, 0.025, 0.0025, 0.1)
  pgResult[8, ] <- c(99, 99, 15, 99, 0.025, 0.0025, 0.05)
  hypBestId <- c(0, 0)
  refOneL <- c(15, 16, 15, 17)
  
  pgResultAdd <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
  
  expect_equal(nrow(pgResultAdd), 27)
  expect_equal(ncol(pgResultAdd), 8)
  
  expect_equal(as.numeric(pgResultAdd[1, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[2, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[3, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[3, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[3, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[3, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[4, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[4, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[4, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[4, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[5, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[5, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[5, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[5, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[6, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[6, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[6, 3]), 99)
  expect_equal(as.numeric(pgResultAdd[6, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[7, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[7, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[7, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[7, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[8, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[8, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[8, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[8, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[9, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[9, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[9, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[9, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[10, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[10, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[10, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[10, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[11, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[11, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[11, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[11, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[12, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[12, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[12, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[12, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[13, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[13, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[13, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[13, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[14, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[14, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[14, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[14, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[15, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[15, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[15, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[15, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[16, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[16, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[16, 3]), 17)
  expect_equal(as.numeric(pgResultAdd[16, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[17, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[17, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[17, 3]), 99)
  expect_equal(as.numeric(pgResultAdd[17, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[18, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[18, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[18, 3]), 99)
  expect_equal(as.numeric(pgResultAdd[18, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[19, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[19, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[19, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[19, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[20, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[20, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[20, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[20, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[21, 1]), 99)
  expect_equal(as.numeric(pgResultAdd[21, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[21, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[21, 4]), 15)
  
  expect_equal(as.numeric(pgResultAdd[22, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[22, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[22, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[22, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[23, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[23, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[23, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[23, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[24, 1]), 99)
  expect_equal(as.numeric(pgResultAdd[24, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[24, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[24, 4]), 17)
  
  expect_equal(as.numeric(pgResultAdd[25, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[25, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[25, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[25, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[26, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[26, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[26, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[26, 4]), 99)
  
  expect_equal(as.numeric(pgResultAdd[27, 1]), 99)
  expect_equal(as.numeric(pgResultAdd[27, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[27, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[27, 4]), 99)
})

test_that("addRefDrop pattern2 only-gt", {
  dropAl <- 17
  pgResult <- matrix(0, 6, 7)
  colnames(pgResult) <- c("U", "U", "victim", "victim", "Weight (0-1 scale)", "Weight", "Genotype prob")
  pgResult[1, ] <- c(15, 15, 15, 16, 0.5, 0.05, 0.2)
  pgResult[2, ] <- c(15, 16, 15, 16, 0.4, 0.04, 0.15)
  pgResult[3, ] <- c(16, 16, 15, 16, 0.3, 0.03, 0.1)
  pgResult[4, ] <- c(15, 99, 15, 16, 0.2, 0.02, 0.15)
  pgResult[5, ] <- c(16, 99, 15, 16, 0.05, 0.005, 0.2)
  pgResult[6, ] <- c(99, 99, 15, 16, 0.05, 0.005, 0.05)
  hypBestId <- c(0, 1)
  refOneL <- c(15, 16, 15, 17)
  
  pgResultAdd <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
  
  expect_equal(nrow(pgResultAdd), 10)
  expect_equal(ncol(pgResultAdd), 8)
  
  expect_equal(as.numeric(pgResultAdd[1, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 2]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[2, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 2]), 16)
  expect_equal(as.numeric(pgResultAdd[2, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[3, 1]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 2]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[3, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[4, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[4, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[4, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[4, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[5, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[5, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[5, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[5, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[6, 1]), 16)
  expect_equal(as.numeric(pgResultAdd[6, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[6, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[6, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[7, 1]), 16)
  expect_equal(as.numeric(pgResultAdd[7, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[7, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[7, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[8, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[8, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[8, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[8, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[9, 1]), 17)
  expect_equal(as.numeric(pgResultAdd[9, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[9, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[9, 4]), 16)
  
  expect_equal(as.numeric(pgResultAdd[10, 1]), 99)
  expect_equal(as.numeric(pgResultAdd[10, 2]), 99)
  expect_equal(as.numeric(pgResultAdd[10, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[10, 4]), 16)
})

test_that("addRefDrop pattern3-1 all", {
  dropAl <- 17
  pgResult <- matrix(0, 3, 7)
  colnames(pgResult) <- c("suspect", "suspect", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")
  pgResult[1, ] <- c(15, 99, 15, 16, 0.7, 0.07, 0.24)
  pgResult[2, ] <- c(15, 99, 16, 16, 0.2, 0.02, 0.1)
  pgResult[3, ] <- c(15, 99, 16, 99, 0.1, 0.01, 0.18)
  hypBestId <- c(2, 0)
  refOneL <- c(15, 16, 15, 17)
  
  pgResultAdd <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
  
  expect_equal(nrow(pgResultAdd), 4)
  expect_equal(ncol(pgResultAdd), 8)
  
  expect_equal(unique(pgResultAdd[, 1]), 15)
  expect_equal(unique(pgResultAdd[, 2]), 17)
  
  expect_equal(as.numeric(pgResultAdd[1, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 3]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 3]), 16)
  expect_equal(as.numeric(pgResultAdd[4, 3]), 16)
  
  expect_equal(as.numeric(pgResultAdd[1, 4]), 16)
  expect_equal(as.numeric(pgResultAdd[2, 4]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 4]), 17)
  expect_equal(as.numeric(pgResultAdd[4, 4]), 99)
  
  expect_equal(round(as.numeric(pgResultAdd[1, 5]), 8), round(0.7 / 1.1, 8))
  expect_equal(round(as.numeric(pgResultAdd[2, 5]), 8), round(0.2 / 1.1, 8))
  expect_equal(round(as.numeric(pgResultAdd[3, 5]), 8), round(0.1 / 1.1, 8))
  expect_equal(round(as.numeric(pgResultAdd[4, 5]), 8), round(0.1 / 1.1, 8))
  
  expect_equal(as.numeric(pgResultAdd[1, 6]), 0.07)
  expect_equal(as.numeric(pgResultAdd[2, 6]), 0.02)
  expect_equal(as.numeric(pgResultAdd[3, 6]), 0.01)
  expect_equal(as.numeric(pgResultAdd[4, 6]), 0.01)
  
  expect_equal(as.numeric(pgResultAdd[1, 7]), 0.24)
  expect_equal(as.numeric(pgResultAdd[2, 7]), 0.1)
  expect_equal(as.numeric(pgResultAdd[3, 7]), 0.18)
  expect_equal(as.numeric(pgResultAdd[4, 7]), 0.18)
  
  expect_equal(as.numeric(pgResultAdd[1, 8]), 0)
  expect_equal(as.numeric(pgResultAdd[2, 8]), 0)
  expect_equal(as.numeric(pgResultAdd[3, 8]), 1)
  expect_equal(as.numeric(pgResultAdd[4, 8]), 1)
})

test_that("addRefDrop pattern3-2 all", {
  dropAl <- c(17, 18)
  pgResult <- matrix(0, 3, 7)
  colnames(pgResult) <- c("suspect", "suspect", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")
  pgResult[1, ] <- c(15, 99, 15, 16, 0.7, 0.07, 0.24)
  pgResult[2, ] <- c(15, 99, 16, 16, 0.2, 0.02, 0.1)
  pgResult[3, ] <- c(15, 99, 16, 99, 0.1, 0.01, 0.18)
  hypBestId <- c(2, 0)
  refOneL <- c(15, 16, 15, 17)
  
  pgResultAdd <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
  
  expect_equal(nrow(pgResultAdd), 5)
  expect_equal(ncol(pgResultAdd), 8)
  
  expect_equal(unique(pgResultAdd[, 1]), 15)
  expect_equal(unique(pgResultAdd[, 2]), 17)
  
  expect_equal(as.numeric(pgResultAdd[1, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[2, 3]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 3]), 16)
  expect_equal(as.numeric(pgResultAdd[4, 3]), 16)
  expect_equal(as.numeric(pgResultAdd[5, 3]), 16)
  
  expect_equal(as.numeric(pgResultAdd[1, 4]), 16)
  expect_equal(as.numeric(pgResultAdd[2, 4]), 16)
  expect_equal(as.numeric(pgResultAdd[3, 4]), 17)
  expect_equal(as.numeric(pgResultAdd[4, 4]), 18)
  expect_equal(as.numeric(pgResultAdd[5, 4]), 99)
  
  expect_equal(round(as.numeric(pgResultAdd[1, 5]), 8), round(0.7 / 1.2, 8))
  expect_equal(round(as.numeric(pgResultAdd[2, 5]), 8), round(0.2 / 1.2, 8))
  expect_equal(round(as.numeric(pgResultAdd[3, 5]), 8), round(0.1 / 1.2, 8))
  expect_equal(round(as.numeric(pgResultAdd[4, 5]), 8), round(0.1 / 1.2, 8))
  expect_equal(round(as.numeric(pgResultAdd[5, 5]), 8), round(0.1 / 1.2, 8))
  
  expect_equal(as.numeric(pgResultAdd[1, 6]), 0.07)
  expect_equal(as.numeric(pgResultAdd[2, 6]), 0.02)
  expect_equal(as.numeric(pgResultAdd[3, 6]), 0.01)
  expect_equal(as.numeric(pgResultAdd[4, 6]), 0.01)
  expect_equal(as.numeric(pgResultAdd[5, 6]), 0.01)
  
  expect_equal(as.numeric(pgResultAdd[1, 7]), 0.24)
  expect_equal(as.numeric(pgResultAdd[2, 7]), 0.1)
  expect_equal(as.numeric(pgResultAdd[3, 7]), 0.18)
  expect_equal(as.numeric(pgResultAdd[4, 7]), 0.18)
  expect_equal(as.numeric(pgResultAdd[5, 7]), 0.18)
  
  expect_equal(as.numeric(pgResultAdd[1, 8]), 0)
  expect_equal(as.numeric(pgResultAdd[2, 8]), 0)
  expect_equal(as.numeric(pgResultAdd[3, 8]), 1)
  expect_equal(as.numeric(pgResultAdd[4, 8]), 1)
  expect_equal(as.numeric(pgResultAdd[5, 8]), 1)
})

test_that("addRefDrop pattern4 all", {
  dropAl <- 17
  pgResult <- matrix(0, 1, 7)
  colnames(pgResult) <- c("suspect", "suspect", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")
  pgResult[1, ] <- c(15, 99, 15, 16, 1, 0.07, 0.24)
  hypBestId <- c(2, 1)
  refOneL <- c(15, 16, 15, 17)
  
  pgResultAdd <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
  
  expect_equal(nrow(pgResultAdd), 1)
  expect_equal(ncol(pgResultAdd), 8)
  
  expect_equal(as.numeric(pgResultAdd[1, 1]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 2]), 17)
  expect_equal(as.numeric(pgResultAdd[1, 3]), 15)
  expect_equal(as.numeric(pgResultAdd[1, 4]), 16)
  expect_equal(as.numeric(pgResultAdd[1, 5]), 1)
  expect_equal(as.numeric(pgResultAdd[1, 6]), 0.07)
  expect_equal(as.numeric(pgResultAdd[1, 7]), 0.24)
  expect_equal(as.numeric(pgResultAdd[1, 8]), 0)
})

test_that("gtProb and LR whether addRefDrop is true or not", {
  pg_uu_false <- matrix(0, 8, 7)
  pg_uu_false[1, ] <- c(11, 11, 11, 11, 0.466115545, 0.000906558, 0.016170004)
  pg_uu_false[2, ] <- c(11, 99, 11, 11, 0.375301766, 0.000729932, 0.058350659)
  pg_uu_false[3, ] <- c(99, 99, 11, 11, 0.158582689, 0.000308431, 0.052640669)
  pg_uu_false[4, ] <- c(11, 11, 11, 99, 3.20E-22, 6.22E-25, 0.058350659)
  pg_uu_false[5, ] <- c(11, 99, 11, 99, 2.19E-24, 4.26E-27, 0.210562675)
  pg_uu_false[6, ] <- c(99, 99, 11, 99, 1.26E-26, 2.45E-29, 0.189957754)
  pg_uu_false[7, ] <- c(11, 11, 99, 99, 1.72E-70, 3.35E-73, 0.052640669)
  pg_uu_false[8, ] <- c(11, 99, 99, 99, 3.81E-75, 7.41E-78, 0.189957754)
  colnames(pg_uu_false) <- c("U", "U", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")

  pg_uu_true <- matrix(0, 27, 7)
  pg_uu_true[1, ] <- c(11, 11, 11, 11, 0.275405964, 0.000906558, 0.016170004)
  pg_uu_true[2, ] <- c(8, 11, 11, 11, 0.221748332, 0.000729932, 0.058350659)
  pg_uu_true[3, ] <- c(11, 99, 11, 11, 0.221748332, 0.000729932, NA)
  pg_uu_true[4, ] <- c(8, 8, 11, 11, 0.093699124, 0.000308431, 0.052640669)
  pg_uu_true[5, ] <- c(8, 99, 11, 11, 0.093699124, 0.000308431, NA)
  pg_uu_true[6, ] <- c(99, 99, 11, 11, 0.093699124, 0.000308431, NA)
  pg_uu_true[7, ] <- c(11, 11, 8, 11, 1.89E-22, 6.22E-25, 0.058350659)
  pg_uu_true[8, ] <- c(11, 11, 11, 99, 1.89E-22, 6.22E-25, NA)
  pg_uu_true[9, ] <- c(8, 11, 8, 11, 1.30E-24, 4.26E-27, 0.210562675)
  pg_uu_true[10, ] <- c(11, 99, 8, 11, 1.30E-24, 4.26E-27, NA)
  pg_uu_true[11, ] <- c(8, 11, 11, 99, 1.30E-24, 4.26E-27, NA)
  pg_uu_true[12, ] <- c(11, 99, 11, 99, 1.30E-24, 4.26E-27, NA)
  pg_uu_true[13, ] <- c(8, 8, 8, 11, 7.45E-27, 2.45E-29, 0.189957754)
  pg_uu_true[14, ] <- c(8, 99, 8, 11, 7.45E-27, 2.45E-29, NA)
  pg_uu_true[15, ] <- c(99, 99, 8, 11, 7.45E-27, 2.45E-29, NA)
  pg_uu_true[16, ] <- c(8, 8, 11, 99, 7.45E-27, 2.45E-29, NA)
  pg_uu_true[17, ] <- c(8, 99, 11, 99, 7.45E-27, 2.45E-29, NA)
  pg_uu_true[18, ] <- c(99, 99, 11, 99, 7.45E-27, 2.45E-29, NA)
  pg_uu_true[19, ] <- c(11, 11, 8, 8, 1.02E-70, 3.35E-73, 0.052640669)
  pg_uu_true[20, ] <- c(11, 11, 8, 99, 1.02E-70, 3.35E-73, NA)
  pg_uu_true[21, ] <- c(11, 11, 99, 99, 1.02E-70, 3.35E-73, NA)
  pg_uu_true[22, ] <- c(8, 11, 8, 8, 2.25E-75, 7.41E-78, 0.189957754)
  pg_uu_true[23, ] <- c(11, 99, 8, 8, 2.25E-75, 7.41E-78, NA)
  pg_uu_true[24, ] <- c(8, 11, 8, 99, 2.25E-75, 7.41E-78, NA)
  pg_uu_true[25, ] <- c(11, 99, 8, 99, 2.25E-75, 7.41E-78, NA)
  pg_uu_true[26, ] <- c(8, 11, 99, 99, 2.25E-75, 7.41E-78, NA)
  pg_uu_true[27, ] <- c(11, 99, 99, 99, 2.25E-75, 7.41E-78, NA)
  colnames(pg_uu_true) <- c("U", "U", "U", "U", "Weight (0-1 scale)", "Weight", "Genotype prob")

  gtComb_1 <- pg_uu_false[, 1:4]
  prod_1 <- pg_uu_false[, 5]
  gtComb_2 <- pg_uu_true[, 1:4]
  prod_2 <- pg_uu_true[, 5]

  popFreq <- c(1357, 354, 100, 1072, 114, 3, 2)
  popFreq <- popFreq / sum(popFreq)
  names(popFreq) <- c(8, 9, 10, 11, 12, 13, 14)
  theta <- 0.01

  hp <- c(1, 0)
  hd <- c(0, 0)

  knownGt <- c(99, 99, 11, 11)
  QFreq <- 1 - popFreq[4]
  gtp_hp_1 <- gtp_hd_1 <- rep(0, nrow(gtComb_1))
  for(i in 1:nrow(gtComb_1)){
    gtCombOne <- gtComb_1[i, ]
    gtp_hp_1[i] <- calcProbGtComb(gtCombOne, hp, popFreq, theta, knownGt, QFreq)
    gtp_hd_1[i] <- calcProbGtComb(gtCombOne, hd, popFreq, theta, knownGt, QFreq)
  }

  knownGt <- c(8, 8, 11, 11)
  QFreq <- 1 - popFreq[1] - popFreq[4]
  gtp_hp_2 <- gtp_hd_2 <- rep(0, nrow(gtComb_2))
  for(i in 1:nrow(gtComb_2)){
    gtCombOne <- gtComb_2[i, ]
    gtp_hp_2[i] <- calcProbGtComb(gtCombOne, hp, popFreq, theta, knownGt, QFreq)
    gtp_hd_2[i] <- calcProbGtComb(gtCombOne, hd, popFreq, theta, knownGt, QFreq)
  }

  lr_1 <- sum(gtp_hp_1 * prod_1) / sum(gtp_hd_1 * prod_1)
  lr_2 <- sum(gtp_hp_2 * prod_2) / sum(gtp_hd_2 * prod_2)

  expect_equal(sum(gtp_hp_1), sum(gtp_hp_2))
  expect_equal(sum(gtp_hd_1), sum(gtp_hd_2))
  expect_equal(lr_1, lr_2)
})
