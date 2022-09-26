test_that("setMR pattern 1", {
  mrOne <- c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.3, 0.4, 0.5)
  mr2 <- setMR(2, mrOne)
  expect_equal(nrow(mr2), 9)
  mr3 <- setMR(3, mrOne)
  expect_equal(nrow(mr3), 35)
  mr4 <- setMR(4, mrOne)
  expect_equal(nrow(mr4), 98)
})

test_that("setMR pattern 2", {
  mrOne <- c(0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)
  mr2 <- setMR(2, mrOne)
  expect_equal(nrow(mr2), 12)
  mr3 <- setMR(3, mrOne)
  expect_equal(nrow(mr3), 65)
  mr4 <- setMR(4, mrOne)
  expect_equal(nrow(mr4), 256)
})
