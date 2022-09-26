test_that("pickRep with repeat motif", {
  repSeq <- "TCTA [TCTG]2 [TCTA]8"
  motifRep <- pickRep(repSeq)
  expect_equal(length(motifRep), 2)
  expect_equal(motifRep[1], 2)
  expect_equal(motifRep[2], 8)
})

test_that("pickRep without repeat motif", {
  repSeq <- "TCTA TCTG TCTA"
  motifRep <- pickRep(repSeq)
  expect_equal(motifRep, numeric(0))
})
