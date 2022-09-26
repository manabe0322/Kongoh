test_that("makePopFreq Dirichlet", {
  afOneL <- c(12, 232, 137, 306, 313)
  afOneLDir <- (afOneL + 1) / sum(afOneL + 1)
  afAlOneL <- 12:16
  popFreq <- makePopFreq(afOneL, afAlOneL, "Dirichlet")
  popAl <- names(popFreq)
  for(i in 1:length(popFreq)){
    expect_equal(as.numeric(popFreq[i]), afOneLDir[i])
    expect_equal(as.numeric(popAl[i]), afAlOneL[i])
  }
})

test_that("makePopFreq Dirichlet unobsAl", {
  afOneL <- c(12, 232, 137, 306, 313)
  afOneL2 <- c(afOneL, 0)
  afOneLDir <- (afOneL2 + 1) / (sum(afOneL2) + length(afOneL2))
  afAlOneL <- 12:16
  afAlOneL2 <- c(afAlOneL, 17)
  popFreq <- makePopFreq(afOneL, afAlOneL, "Dirichlet", unobsAl = 17)
  popAl <- names(popFreq)
  for(i in 1:length(popFreq)){
    expect_equal(as.numeric(popFreq[i]), afOneLDir[i])
    expect_equal(as.numeric(popAl[i]), afAlOneL2[i])
  }
})

test_that("makePopFreq MAF", {
  afOneL <- c(12, 232, 137, 306, 313)
  afOneL <- afOneL / sum(afOneL)
  afAlOneL <- 12:16
  popFreq <- makePopFreq(afOneL, afAlOneL, "MAF")
  popAl <- names(popFreq)
  for(i in 1:length(popFreq)){
    expect_equal(as.numeric(popFreq[i]), afOneL[i])
    expect_equal(as.numeric(popAl[i]), afAlOneL[i])
  }
})

test_that("makePopFreq MAF maf", {
  afOneL <- c(12, 232, 137, 306, 313)
  afOneL <- afOneL / sum(afOneL)
  afOneLMaf <- c(afOneL, rep(1 / 1000, 2))
  afAlOneL <- 12:16
  afAlOneL2 <- c(afAlOneL, 17, 18)
  popFreq <- makePopFreq(afOneL, afAlOneL, "MAF", maf = 1 / 1000, unobsAl = c(17, 18))
  popAl <- names(popFreq)
  for(i in 1:length(popFreq)){
    expect_equal(as.numeric(popFreq[i]), afOneLMaf[i])
    expect_equal(as.numeric(popAl[i]), afAlOneL2[i])
  }
})
