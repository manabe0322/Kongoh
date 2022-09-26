test_that("calcWlus", {
  lusOneAl <- c(12, 13, 12, 14)
  countOneAl <- c(99, 400, 1, 300)
  wLus <- calcWlus(lusOneAl, countOneAl)
  expect_equal(wLus, 13.25)
})
