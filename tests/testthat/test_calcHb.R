test_that("calcHb 1 repeat difference", {
  peak <- c(12, 13, 14, 15, 16)
  height <- c(100, 200, 3000, 3000, 100)
  gtOneL <- c(14, 15)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 0)
  expect_equal(as.numeric(hbH[2]), 0)
})

test_that("calcHb 2 repeat difference", {
  peak <- c(12, 13, 14, 15, 16, 17)
  height <- c(100, 200, 3000, 300, 3000, 100)
  gtOneL <- c(14, 16)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 3400 / 3300)
  expect_equal(as.numeric(hbH[2]), 3350)
})

test_that("calcHb 3 repeat difference", {
  peak <- c(12, 13, 14, 15, 16, 17, 18)
  height <- c(100, 200, 3000, 200, 300, 3000, 100)
  gtOneL <- c(14, 17)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 3400 / 3300)
  expect_equal(as.numeric(hbH[2]), 3450)
})

test_that("calcHb 0.2 repeat difference", {
  peak <- c(12, 12.2, 13, 13.2, 14, 14.2, 15, 15.2)
  height <- c(200, 100, 400, 500, 4000, 4000, 300, 200)
  gtOneL <- c(14, 14.2)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 4800 / 4900)
  expect_equal(as.numeric(hbH[2]), 4850)
})

test_that("calcHb 0.8 repeat difference", {
  peak <- c(12.2, 13, 13.2, 14, 14.2, 15, 15.2, 16)
  height <- c(200, 100, 400, 500, 4000, 4000, 300, 200)
  gtOneL <- c(14.2, 15)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 4800 / 4900)
  expect_equal(as.numeric(hbH[2]), 4850)
})

test_that("calcHb homozygote", {
  peak <- c(12, 13, 14, 15)
  height <- c(200, 400, 4000, 300)
  gtOneL <- c(14)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 0)
  expect_equal(as.numeric(hbH[2]), 0)
})

test_that("calcHb drop-out", {
  peak <- c(12, 13, 14, 15)
  height <- c(200, 400, 4000, 300)
  gtOneL <- c(14, 16)
  srConsiderOneL <- rep(TRUE, 4)
  hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
  expect_equal(as.numeric(hbH[1]), 0)
  expect_equal(as.numeric(hbH[2]), 0)
})
