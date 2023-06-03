test_that("makeGtComb hnc = 2", {
  hnc <- 2
  mrFltr <- 4
  hbFltr <- 0.25
  srFltr <- c(0.7, 0.35, 0.13, 0.12)
  st <- 500

  #example D3S1358
  peakOneL <- c(15, 16, 17, 18, 19, 99)
  heightOneL <- c(1800, 262, 1494, 2067, 2035, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example vWA
  peakOneL <- c(14, 16, 17, 18, 99)
  heightOneL <- c(1640, 1419, 1529, 1425, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example D16S539
  peakOneL <- c(8, 9, 10, 12, 13, 99)
  heightOneL <- c(137, 1311, 3200, 113, 1680, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 64)

  #example CSF1PO
  peakOneL <- c(9, 10, 11, 12, 99)
  heightOneL <- c(1831, 165, 1328, 2853, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 43)

  #example TPOX
  peakOneL <- c(8, 10, 11, 99)
  heightOneL <- c(1150, 2395, 1100, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 12)

  #example D8S1179
  peakOneL <- c(11, 12, 13, 14, 99)
  heightOneL <- c(262, 3557, 4117, 2783, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 18)

  #example D21S11
  peakOneL <- c(28, 30, 31.2, 32.2, 99)
  heightOneL <- c(1715, 2502, 169, 2050, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 18)

  #example D18S51
  peakOneL <- c(13, 14, 17, 99)
  heightOneL <- c(400, 7163, 1908, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 24)

  #example D2S441
  peakOneL <- c(10, 11, 12, 14, 99)
  heightOneL <- c(1526, 1500, 1875, 1522, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example D19S433
  peakOneL <- c(12, 13, 13.2, 15, 99)
  heightOneL <- c(134, 2321, 1139, 1056, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 18)

  #example TH01
  peakOneL <- c(6, 7, 8, 9.3, 99)
  heightOneL <- c(922, 929, 628, 776, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example FGA
  peakOneL <- c(19, 21, 22, 23, 24, 99)
  heightOneL <- c(1731, 137, 1914, 150, 3676, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 24)

  #example D22S1045
  peakOneL <- c(15, 16, 17, 18, 99)
  heightOneL <- c(1508, 155, 3497, 1428, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 18)

  #example D5S818
  peakOneL <- c(9, 10, 11, 12, 13, 99)
  heightOneL <- c(1657, 169, 1792, 2128, 1398, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example D13S317
  peakOneL <- c(7, 8, 9, 99)
  heightOneL <- c(2071, 1955, 3971, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 27)

  #example D7S820
  peakOneL <- c(9, 10, 11, 99)
  heightOneL <- c(1656, 3747, 2469, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 27)

  #example SE33
  peakOneL <- c(18, 19, 23.2, 24.2, 25.2, 26.2, 99)
  heightOneL <- c(182, 1940, 106, 1670, 2094, 2088, 0)
  srConsiderOneL <- rep(TRUE, 4)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 6)

  #example D10S1248
  peakOneL <- c(13, 14, 15, 17, 99)
  heightOneL <- c(231, 2959, 2107, 1490, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 18)

  #example D1S1656
  peakOneL <- c(14, 15, 16, 17, 17.3, 99)
  heightOneL <- c(190, 4297, 220, 1812, 2013, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, TRUE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 24)

  #example D12S391
  peakOneL <- c(17, 18, 19, 20, 99)
  heightOneL <- c(161, 2919, 184, 3492, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 40)

  #example D2S1338
  peakOneL <- c(18, 19, 20, 99)
  heightOneL <- c(214, 892, 2961, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 46)
})

test_that("makeGtComb hnc = 3", {
  hnc <- 3
  mrFltr <- 4
  hbFltr <- 0.25
  srFltr <- c(0.7, 0.35, 0.13, 0.12)
  st <- 500

  #example D3S1358
  peakOneL <- c(15, 16, 17, 18, 19, 99)
  heightOneL <- c(1800, 262, 1494, 2067, 2035, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 636)

  #example vWA
  peakOneL <- c(14, 16, 17, 18, 99)
  heightOneL <- c(1640, 1419, 1529, 1425, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 420)

  #example D16S539
  peakOneL <- c(8, 9, 10, 12, 13, 99)
  heightOneL <- c(137, 1311, 3200, 113, 1680, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 2203)
  ##Memo_220926 The number may be reduced by the function "makeResult" in the final output.

  #example CSF1PO
  peakOneL <- c(9, 10, 11, 12, 99)
  heightOneL <- c(1831, 165, 1328, 2853, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 1150)

  #example TPOX
  peakOneL <- c(8, 10, 11, 99)
  heightOneL <- c(1150, 2395, 1100, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 369)

  #example D8S1179
  peakOneL <- c(11, 12, 13, 14, 99)
  heightOneL <- c(262, 3557, 4117, 2783, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 693)

  #example D21S11
  peakOneL <- c(28, 30, 31.2, 32.2, 99)
  heightOneL <- c(1715, 2502, 169, 2050, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 693)

  #example D18S51
  peakOneL <- c(13, 14, 17, 99)
  heightOneL <- c(400, 7163, 1908, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 451)

  #example D2S441
  peakOneL <- c(10, 11, 12, 14, 99)
  heightOneL <- c(1526, 1500, 1875, 1522, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 420)

  #example D19S433
  peakOneL <- c(12, 13, 13.2, 15, 99)
  heightOneL <- c(134, 2321, 1139, 1056, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 693)

  #example TH01
  peakOneL <- c(6, 7, 8, 9.3, 99)
  heightOneL <- c(922, 929, 628, 776, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 420)

  #example FGA
  peakOneL <- c(19, 21, 22, 23, 24, 99)
  heightOneL <- c(1731, 137, 1914, 150, 3676, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 1191)

  #example D22S1045
  peakOneL <- c(15, 16, 17, 18, 99)
  heightOneL <- c(1508, 155, 3497, 1428, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 669)

  #example D5S818
  peakOneL <- c(9, 10, 11, 12, 13, 99)
  heightOneL <- c(1657, 169, 1792, 2128, 1398, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 636)

  #example D13S317
  peakOneL <- c(7, 8, 9, 99)
  heightOneL <- c(2071, 1955, 3971, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 526)

  #example D7S820
  peakOneL <- c(9, 10, 11, 99)
  heightOneL <- c(1656, 3747, 2469, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 526)

  #example SE33
  peakOneL <- c(18, 19, 23.2, 24.2, 25.2, 26.2, 99)
  heightOneL <- c(182, 1940, 106, 1670, 2094, 2088, 0)
  srConsiderOneL <- rep(TRUE, 4)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 930)

  #example D10S1248
  peakOneL <- c(13, 14, 15, 17, 99)
  heightOneL <- c(231, 2959, 2107, 1490, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 693)

  #example D1S1656
  peakOneL <- c(14, 15, 16, 17, 17.3, 99)
  heightOneL <- c(190, 4297, 220, 1812, 2013, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, TRUE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 1153)

  #example D12S391
  peakOneL <- c(17, 18, 19, 20, 99)
  heightOneL <- c(161, 2919, 184, 3492, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 1050)

  #example D2S1338
  peakOneL <- c(18, 19, 20, 99)
  heightOneL <- c(214, 892, 2961, 0)
  srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
  gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  expect_equal(nrow(gtComb), 625)
})
