test_that("calcSr 1 repeat difference", {
  peak <- c(12, 13, 13.2, 14, 14.2, 15, 16)
  height <- c(100, 200, 150, 3000, 150, 3000, 100)
  gtOneL <- c(14, 15)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 0)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 0)
  expect_equal(as.numeric(fsr[2]), 0)
  expect_equal(as.numeric(dsr[1]), 0)
  expect_equal(as.numeric(dsr[2]), 0)
  expect_equal(as.numeric(m2sr[1]), 0)
  expect_equal(as.numeric(m2sr[2]), 0)
  expect_equal(as.numeric(al[1]), 0)
  expect_equal(as.numeric(al[2]), 0)
  expect_equal(as.numeric(srH[1]), 0)
  expect_equal(as.numeric(srH[2]), 0)
})

test_that("calcSr 2 repeat difference", {
  peak <- c(12, 13, 13.2, 14, 15, 15.2, 16, 17)
  height <- c(100, 200, 150, 3000, 300, 150, 3000, 100)
  gtOneL <- c(14, 16)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 200 / 3000)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 0)
  expect_equal(as.numeric(fsr[2]), 100 / 3000)
  expect_equal(as.numeric(dsr[1]), 100 / 3000)
  expect_equal(as.numeric(dsr[2]), 0)
  expect_equal(as.numeric(m2sr[1]), 150 / 3000)
  expect_equal(as.numeric(m2sr[2]), 150 / 3000)
  expect_equal(as.numeric(al[1]), 14)
  expect_equal(as.numeric(al[2]), 16)
  expect_equal(as.numeric(srH[1]), 100 + 200 + 150 + 3000)
  expect_equal(as.numeric(srH[2]), 300 + 150 + 3000 + 100)
})

test_that("calcSr 3 repeat difference", {
  peak <- c(12, 13, 13.2, 14, 15, 16, 16.2, 17, 18)
  height <- c(100, 200, 150, 3000, 200, 300, 150, 3000, 100)
  gtOneL <- c(14, 17)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 200 / 3000)
  expect_equal(as.numeric(bsr[2]), 300 / 3000)
  expect_equal(as.numeric(fsr[1]), 0)
  expect_equal(as.numeric(fsr[2]), 100 / 3000)
  expect_equal(as.numeric(dsr[1]), 100 / 3000)
  expect_equal(as.numeric(dsr[2]), 0)
  expect_equal(as.numeric(m2sr[1]), 150 / 3000)
  expect_equal(as.numeric(m2sr[2]), 150 / 3000)
  expect_equal(as.numeric(al[1]), 14)
  expect_equal(as.numeric(al[2]), 17)
  expect_equal(as.numeric(srH[1]), 100 + 200 + 150 + 3000)
  expect_equal(as.numeric(srH[2]), 300 + 150 + 3000 + 100)
})

test_that("calcSr 0.2 repeat difference", {
  peak <- c(12, 12.2, 13, 13.2, 14, 14.2, 15, 15.2)
  height <- c(200, 100, 400, 500, 4000, 4000, 300, 200)
  gtOneL <- c(14, 14.2)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 400 / 4000)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 300 / 4000)
  expect_equal(as.numeric(fsr[2]), 200 / 4000)
  expect_equal(as.numeric(dsr[1]), 200 / 4000)
  expect_equal(as.numeric(dsr[2]), 100 / 4000)
  expect_equal(as.numeric(m2sr[1]), 0)
  expect_equal(as.numeric(m2sr[2]), 0)
  expect_equal(as.numeric(al[1]), 14)
  expect_equal(as.numeric(al[2]), 14.2)
  expect_equal(as.numeric(srH[1]), 200 + 400 + 4000 + 300)
  expect_equal(as.numeric(srH[2]), 100 + 500 + 4000 + 200)
})

test_that("calcSr 0.8 repeat difference", {
  peak <- c(12.2, 13, 13.2, 14, 14.2, 15, 15.2, 16)
  height <- c(200, 100, 400, 500, 4000, 4000, 300, 200)
  gtOneL <- c(14.2, 15)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 400 / 4000)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 300 / 4000)
  expect_equal(as.numeric(fsr[2]), 200 / 4000)
  expect_equal(as.numeric(dsr[1]), 200 / 4000)
  expect_equal(as.numeric(dsr[2]), 100 / 4000)
  expect_equal(as.numeric(m2sr[1]), 0)
  expect_equal(as.numeric(m2sr[2]), 0)
  expect_equal(as.numeric(al[1]), 14.2)
  expect_equal(as.numeric(al[2]), 15)
  expect_equal(as.numeric(srH[1]), 200 + 400 + 4000 + 300)
  expect_equal(as.numeric(srH[2]), 100 + 500 + 4000 + 200)
})

test_that("calcSr homozygote", {
  peak <- c(12, 13, 14, 15)
  height <- c(200, 400, 4000, 300)
  gtOneL <- c(14)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 400 / 4000)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 300 / 4000)
  expect_equal(as.numeric(fsr[2]), 0)
  expect_equal(as.numeric(dsr[1]), 200 / 4000)
  expect_equal(as.numeric(dsr[2]), 0)
  expect_equal(as.numeric(m2sr[1]), 0)
  expect_equal(as.numeric(m2sr[2]), 0)
  expect_equal(as.numeric(al[1]), 14)
  expect_equal(as.numeric(al[2]), 0)
  expect_equal(as.numeric(srH[1]), 200 + 400 + 4000 + 300)
  expect_equal(as.numeric(srH[2]), 0)
})

test_that("calcSr drop-out", {
  peak <- c(12, 13, 14, 15)
  height <- c(200, 400, 4000, 300)
  gtOneL <- c(14, 16)
  srConsiderOneL <- rep(TRUE, 4)
  srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
  bsr <- srData[[1]]
  fsr <- srData[[2]]
  dsr <- srData[[3]]
  m2sr <- srData[[4]]
  al <- srData[[5]]
  srH <- srData[[6]]
  expect_equal(as.numeric(bsr[1]), 400 / 4000)
  expect_equal(as.numeric(bsr[2]), 0)
  expect_equal(as.numeric(fsr[1]), 0)
  expect_equal(as.numeric(fsr[2]), 0)
  expect_equal(as.numeric(dsr[1]), 200 / 4000)
  expect_equal(as.numeric(dsr[2]), 0)
  expect_equal(as.numeric(m2sr[1]), 0)
  expect_equal(as.numeric(m2sr[2]), 0)
  expect_equal(as.numeric(al[1]), 14)
  expect_equal(as.numeric(al[2]), 0)
  expect_equal(as.numeric(srH[1]), 200 + 400 + 4000)
  expect_equal(as.numeric(srH[2]), 0)
})
