test_that("calcCA", {
  minUS <- 4
  motifLengthOneAl <- list()
  motifLengthOneAl[[1]] <- c(5, 6, 3, 3, 2, 11) #"[TCTA]5 [TCTG]6 [TCTA]3 TA [TCTA]3 TCA [TCTA]2 TCCATA [TCTA]11 TA TCTA"
  motifLengthOneAl[[2]] <- c(5, 6, 3, 2, 2, 12) #"[TCTA]5 [TCTG]6 [TCTA]3 TA [TCTA]2 TCA [TCTA]2 TCCATA [TCTA]12 TA TCTA"
  motifLengthOneAl[[3]] <- c(5, 5, 3, 3, 2, 12) #"[TCTA]5 [TCTG]5 [TCTA]3 TA [TCTA]3 TCA [TCTA]2 TCCATA [TCTA]12 TA TCTA"
  motifLengthOneAl[[4]] <- c(4, 6, 3, 3, 2, 12) #"[TCTA]4 [TCTG]6 [TCTA]3 TA [TCTA]3 TCA [TCTA]2 TCCATA [TCTA]12 TA TCTA"
  motifLengthOneAl[[5]] <- c(5, 5, 3, 2, 2, 13) #"[TCTA]5 [TCTG]5 [TCTA]3 TA [TCTA]2 TCA [TCTA]2 TCCATA [TCTA]13 TA TCTA"
  motifLengthOneAl[[6]] <- c(6, 6, 3, 3, 2, 10) #"[TCTA]6 [TCTG]6 [TCTA]3 TA [TCTA]3 TCA [TCTA]2 TCCATA [TCTA]10 TA TCTA"
  seqCountOneAl <- c(133, 13, 9, 3, 1, 1)
  ca <- calcCA(minUS, motifLengthOneAl, seqCountOneAl)
  expect_equal(round(ca, 4), 10.0875)
})

