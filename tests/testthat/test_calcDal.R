test_that("calcDal d = 0", {
  expect_equal(calcDal(0, 100, 200), 1)
  expect_equal(calcDal(0, 500, 200), 1)
})

test_that("calcDal d != 0", {
  expect_equal(round(calcDal(-0.05, 100, 200), 4), 148.4132)
  expect_equal(calcDal(-1, 500, 200), 5.1482e-131)
})
