test_that("estSizeDrop repLength = 3", {
  expect_equal(estSizeDrop(12, c(11, 13), c(102, 109.5), 3), 105, 3)
  expect_equal(estSizeDrop(12, c(13, 14), c(106, 109.5), 3), 103, 3)
  expect_equal(estSizeDrop(12, c(9, 10), c(106, 109.5), 3), 115.5, 3)
})

test_that("estSizeDrop repLength = 4", {
  expect_equal(estSizeDrop(12, c(11, 13), c(102, 109.5), 4), 106, 4)
  expect_equal(estSizeDrop(12, c(13, 14), c(106, 109.5), 4), 102, 4)
  expect_equal(estSizeDrop(12, c(9, 10), c(106, 109.5), 4), 117.5, 4)
})
