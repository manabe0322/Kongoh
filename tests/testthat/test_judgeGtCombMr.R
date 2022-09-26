test_that("judgeGtCombMr", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 2000, 19000, 20000)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
  heightOneL <- c(19000, 20000, 1000, 2000)
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), FALSE)
  heightOneL <- c(19000, 20000, 1000, 2000)
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), FALSE)
  st <- 25000
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
})
