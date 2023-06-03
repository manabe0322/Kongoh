test_that("judgeGtCombMr condition 1", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 2000, 19000, 20000)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
})

test_that("judgeGtCombMr condition 2", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(19000, 20000, 1000, 2000)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), FALSE)
})

test_that("judgeGtCombMr condition 3", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(19000, 20000, 1000, 2000)
  mrFltr <- 1000
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
})

test_that("judgeGtCombMr condition 4", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(19000, 20000, 1000, 2000)
  mrFltr <- 4
  st <- 50000
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
})

test_that("judgeGtCombMr condition 5", {
  gtCombOne <- c(17, 29.2, 23.2, 27.2, 15, 29.2)
  peakOneL <- c(14, 15, 17, 22.2, 23.2, 26.2, 27.2, 28.2, 29.2, 99)
  heightOneL <- c(225, 3996, 452, 199, 2413, 270, 2823, 449, 3828, 0)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), TRUE)
})

test_that("judgeGtCombMr condition 6", {
  gtCombOne <- c(15, 29.2, 23.2, 27.2, 17, 29.2)
  peakOneL <- c(14, 15, 17, 22.2, 23.2, 26.2, 27.2, 28.2, 29.2, 99)
  heightOneL <- c(225, 3996, 452, 199, 2413, 270, 2823, 449, 3828, 0)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), FALSE)
})

test_that("judgeGtCombMr condition 7", {
  gtCombOne <- c(23.2, 27.2, 17, 29.2, 15, 29.2)
  peakOneL <- c(14, 15, 17, 22.2, 23.2, 26.2, 27.2, 28.2, 29.2, 99)
  heightOneL <- c(225, 3996, 452, 199, 2413, 270, 2823, 449, 3828, 0)
  mrFltr <- 4
  st <- 500
  expect_equal(judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st), FALSE)
})
