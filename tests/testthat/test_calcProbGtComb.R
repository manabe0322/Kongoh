test_that("calcProbGtComb, hypIdOne = c(0, 0), theta = 0", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(0, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 2 * 0.25 * 0.1 * 2 * 0.1 * 0.2)
})

test_that("calcProbGtComb, hypIdOne = c(1, 0), theta = 0, pattern 1", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 2 * 0.1 * 0.2)
})

test_that("calcProbGtComb, hypIdOne = c(1, 0), theta = 0, pattern 2", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  knownGt <- c(13, 14, 12, 13)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 0)
})

test_that("calcProbGtComb, hypIdOne = c(1, 2), theta = 0, pattern 1", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 2)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 1)
})

test_that("calcProbGtComb, hypIdOne = c(1, 2), theta = 0, pattern 2", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 2)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  knownGt <- c(13, 14, 12, 13)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 0)
})

test_that("calcProbGtComb, hypIdOne = c(0, 0), theta = 0.01", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(0, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 4 * (theta + (1 - theta) * 0.25) * (2 * theta + (1 - theta) * 0.1) * (3 * theta + (1 - theta) * 0.1) * (theta + (1 - theta) * 0.2) / ((1 + 3 * theta) * (1 + 4 * theta) * (1 + 5 * theta) * (1 + 6 * theta)))
})

test_that("calcProbGtComb, hypIdOne = c(1, 0), theta = 0.01, pattern 1", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 2 * (2 * theta + (1 - theta) * 0.1) * (theta + (1 - theta) * 0.2) / ((1 + 3 * theta) * (1 + 4 * theta)))
})

test_that("calcProbGtComb, hypIdOne = c(1, 0), theta = 0.01, pattern 2", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 0)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  knownGt <- c(13, 14, 12, 13)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 0)
})

test_that("calcProbGtComb, hypIdOne = c(1, 2), theta = 0.01, pattern 1", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 2)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  knownGt <- c(12, 13, 13, 14)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 1)
})

test_that("calcProbGtComb, hypIdOne = c(1, 2), theta = 0.01, pattern 0", {
  gtCombOne <- c(12, 13, 13, 14)
  hypIdOne <- c(1, 2)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  knownGt <- c(13, 14, 12, 13)
  prob <- calcProbGtComb(gtCombOne, hypIdOne, popFreq, theta, knownGt, QFreq = 0)
  expect_equal(prob, 0)
})
