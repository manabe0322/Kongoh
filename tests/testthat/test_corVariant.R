test_that("corVariant_repLength2", {
  expect_equal(corVariant(21, 2), 21)
  expect_equal(corVariant(21.1, 2), 21.5)
})

test_that("corVariant_repLength3", {
  expect_equal(corVariant(21, 3), 21)
  expect_equal(corVariant(21.1, 3), 21 + 1 / 3)
  expect_equal(corVariant(21.2, 3), 21 + 2 / 3)
})

test_that("corVariant_repLength4", {
  expect_equal(corVariant(21, 4), 21)
  expect_equal(corVariant(21.1, 4), 21.25)
  expect_equal(corVariant(21.2, 4), 21.5)
  expect_equal(corVariant(21.3, 4), 21.75)
})

test_that("corVariant_repLength5", {
  expect_equal(corVariant(21, 5), 21)
  expect_equal(corVariant(21.1, 5), 21.2)
  expect_equal(corVariant(21.2, 5), 21.4)
  expect_equal(corVariant(21.3, 5), 21.6)
  expect_equal(corVariant(21.4, 5), 21.8)
})

test_that("corVariant_repLength6", {
  expect_equal(corVariant(21, 6), 21)
  expect_equal(corVariant(21.1, 6), 21 + 1 / 6)
  expect_equal(corVariant(21.2, 6), 21 + 2 / 6)
  expect_equal(corVariant(21.3, 6), 21 + 3 / 6)
  expect_equal(corVariant(21.4, 6), 21 + 4 / 6)
  expect_equal(corVariant(21.5, 6), 21 + 5 / 6)
})

