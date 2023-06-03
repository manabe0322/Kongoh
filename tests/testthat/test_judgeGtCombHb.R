test_that("judgeGtCombHb condition 1", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 2000, 1900, 2000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
})

test_that("judgeGtCombHb condition 2", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 10000, 1900, 2000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), FALSE)
})

test_that("judgeGtCombHb condition 3", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1900, 2000, 1000, 10000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), FALSE)
})

test_that("judgeGtCombHb condition 4", {
  gtCombOne <- c(9, 10, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(200, 2000, 1900, 2000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
})

test_that("judgeGtCombHb condition 5", {
  gtCombOne <- c(10, 11, 11, 12)
  peakOneL <- c(9, 10, 11, 12)
  heightOneL <- c(1000, 10000, 1900, 2000)
  hbFltr <- 0.25
  st <- 500
  expect_equal(judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st), TRUE)
})
