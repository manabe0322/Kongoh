test_that("MP and LR whether addRefDrop is true or not", {
  pg_uu_false <- matrix(0, 8, 7)
  pg_uu_false[1, ] <- c(11, 11, 11, 11, 0.466115545, 0.000906558, 0.016170004)
  pg_uu_false[2, ] <- c(11, 99, 11, 11, 0.375301766, 0.000729932, 0.058350659)
  pg_uu_false[3, ] <- c(99, 99, 11, 11, 0.158582689, 0.000308431, 0.052640669)
  pg_uu_false[4, ] <- c(11, 11, 11, 99, 3.20E-22, 6.22E-25, 0.058350659)
  pg_uu_false[5, ] <- c(11, 99, 11, 99, 2.19E-24, 4.26E-27, 0.210562675)
  pg_uu_false[6, ] <- c(99, 99, 11, 99, 1.26E-26, 2.45E-29, 0.189957754)
  pg_uu_false[7, ] <- c(11, 11, 99, 99, 1.72E-70, 3.35E-73, 0.052640669)
  pg_uu_false[8, ] <- c(11, 99, 99, 99, 3.81E-75, 7.41E-78, 0.189957754)
  colnames(pg_uu_false) <- c("U", "U", "U", "U", "weight", "product", "genotype prob")

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
  colnames(pg_uu_true) <- c("U", "U", "U", "U", "weight", "product", "genotype prob")

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
  mp_hp_1 <- mp_hd_1 <- rep(0, nrow(gtComb_1))
  for(i in 1:nrow(gtComb_1)){
    gtCombOne <- gtComb_1[i, ]
    mp_hp_1[i] <- calcProbGtComb(gtCombOne, hp, popFreq, theta, knownGt, QFreq)
    mp_hd_1[i] <- calcProbGtComb(gtCombOne, hd, popFreq, theta, knownGt, QFreq)
  }

  knownGt <- c(8, 8, 11, 11)
  QFreq <- 1 - popFreq[1] - popFreq[4]
  mp_hp_2 <- mp_hd_2 <- rep(0, nrow(gtComb_2))
  for(i in 1:nrow(gtComb_2)){
    gtCombOne <- gtComb_2[i, ]
    mp_hp_2[i] <- calcProbGtComb(gtCombOne, hp, popFreq, theta, knownGt, QFreq)
    mp_hd_2[i] <- calcProbGtComb(gtCombOne, hd, popFreq, theta, knownGt, QFreq)
  }

  lr_1 <- sum(mp_hp_1 * prod_1) / sum(mp_hd_1 * prod_1)
  lr_2 <- sum(mp_hp_2 * prod_2) / sum(mp_hd_2 * prod_2)

  expect_equal(sum(mp_hp_1), sum(mp_hp_2))
  expect_equal(sum(mp_hd_1), sum(mp_hd_2))
  expect_equal(lr_1, lr_2)
})
