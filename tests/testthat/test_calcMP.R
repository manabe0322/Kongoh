test_that("calcMP, hetero, theta = 0, nObs = c(0, 0)", {
  gt <- c(13, 14)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  nObs <- c(0, 0)
  names(nObs) <- gt
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 0.04)
})

test_that("calcMP, hetero, theta = 0, nObs = c(1, 1)", {
  gt <- c(13, 14)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  nObs <- c(1, 1)
  names(nObs) <- gt
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 0.04)
})

test_that("calcMP, hetero, theta = 0.01, nObs = c(0, 0)", {
  gt <- c(13, 14)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  nObs <- c(0, 0)
  names(nObs) <- gt
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 2 * ((1 - theta) * 0.1) * ((1 - theta) * 0.2) / (1 - theta))
})

test_that("calcMP, hetero, theta = 0.01, nObs = c(1, 1)", {
  gt <- c(13, 14)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  nObs <- c(1, 1)
  names(nObs) <- gt
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 2 * (theta + (1 - theta) * 0.1) * (theta + (1 - theta) * 0.2) / ((1 + theta) * (1 + 2 * theta)))
})

test_that("calcMP, homo, theta = 0, nObs = 0", {
  gt <- c(13, 13)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  nObs <- 0
  names(nObs) <- unique(gt)
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 0.01)
})

test_that("calcMP, homo, theta = 0, nObs = 1", {
  gt <- c(13, 13)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0
  nObs <- 1
  names(nObs) <- unique(gt)
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, 0.01)
})

test_that("calcMP, homo, theta = 0.01, nObs = 0", {
  gt <- c(13, 13)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  nObs <- 0
  names(nObs) <- unique(gt)
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, ((1 - theta) * 0.1) * (theta + (1 - theta) * 0.1) / (1 - theta))
})

test_that("calcMP, homo, theta = 0.01, nObs = 2", {
  gt <- c(13, 13)
  popFreq <- c(0.4, 0.25, 0.1, 0.2, 0.05)
  names(popFreq) <- 11:15
  theta <- 0.01
  nObs <- 2
  names(nObs) <- unique(gt)
  mp <- calcMP(gt, popFreq, theta, nObs, QFreq = 0)
  expect_equal(mp, (2 * theta + (1 - theta) * 0.1) * (3 * theta + (1 - theta) * 0.1) / ((1 + theta) * (1 + 2 * theta)))
})
