test_that("estSizeDrop2 repLength = 3", {
  expect_equal(estSizeDrop2(13, 12:16, seq(120, 132, 3), 3), 123, 3)
  expect_equal(estSizeDrop2(11, 12:16, seq(120, 132, 3), 3), 117, 3)
  expect_equal(estSizeDrop2(12.2, 12:16, seq(120, 132, 3), 3), 122, 3)
})

test_that("estSizeDrop2 repLength = 4", {
  expect_equal(estSizeDrop2(13, 12:16, seq(120, 136, 4), 4), 124, 4)
  expect_equal(estSizeDrop2(11, 12:16, seq(120, 136, 4), 4), 116, 4)
  expect_equal(estSizeDrop2(12.2, 12:16, seq(120, 136, 4), 4), 122, 4)
})
