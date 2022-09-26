test_that("judgeGtCombHb", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 2000, 1900, 2000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
  heightOneL <- c(1000, 10000, 1900, 2000)
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), FALSE)
  heightOneL <- c(1900, 2000, 1000, 10000)
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), FALSE)
  heightOneL <- c(200, 2000, 1900, 2000)
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
  gtCombOne <- c(10, 11, 11, 12)
  heightOneL <- c(1000, 10000, 1900, 2000)
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
})
