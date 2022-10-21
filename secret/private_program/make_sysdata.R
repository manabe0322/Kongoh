#Kit
kitDefault <- "GlobalFiler"

#Parameters for Monte Carlo simulation
parDefault <- "GF_29cycles_3500xL_24sec.csv"

#Allele repeat correction
alCorDefault <- "GF_repeat_correction.csv"

#Method for estimating allele frequencies
afMethDefault <- "Dirichlet"

#Minimum allele frequency
mafDefault <- 0.001

#Theta
thetaDefault <- 0

#Number of Monte Carlo simulation
numMCDefault <- 1000

#Analytical thresholds (GlobalFiler)
atDefault <- rep(100, 21)
names(atDefault) <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                      "D8S1179", "D21S11", "D18S51",
                      "D2S441", "D19S433", "TH01", "FGA",
                      "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                      "D10S1248", "D1S1656", "D12S391", "D2S1338")

#Analytical threshold (single locus)
atSingleDefault <- 100

#Mixture ratios (MRn) of one contributor (1 to n-1)
mrDefault <- c(0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)

#Degradation parameter (d)
degDefault <- round(seq(-0.05, 0, 0.0025), 4)

#Excluding unrealistic genotype combinations
stDefault <- 500
mrFltrDefault <- 4
hbFltrDefault <- 0.25
bsrFltrDefault <- 0.7
fsrFltrDefault <- 0.35
dsrFltrDefault <- 0.13
m2srFltrDefault <- 0.12

#Minimum or maximum of each parameter for Monte Carlo simulation
aeMinDefault <- 0.058
hbMinDefault <- 0.046
bsrMaxDefault <- 0.7
fsrMaxDefault <- 0.35
dsrMaxDefault <- 0.13
m2srMaxDefault <- 0.12

#Cut mr and deg
mrDegCut <- 0.0001

#Minimum threshold
mtDefault <- 30

#######################################
# Information of generally used locus #
#######################################

#Autosomal STR markers
strMar <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX", "D8S1179", "D21S11", "D18S51", "D2S441", "D19S433", "TH01", "FGA", "D22S1045", "D5S818", "D13S317", "D7S820", "SE33", "D10S1248", "D1S1656", "D12S391", "D2S1338", "D6S1043", "Penta D", "Penta E")
nStr <- length(strMar)

#Candidate model of AE
candAE_Default <- matrix("", nStr + 1, 2)
rownames(candAE_Default) <- c(strMar, "General")
colnames(candAE_Default) <- c("Cand_1", "Default")
candAE_Default[, 1] <- "Log-normal"
candAE_Default[, 2] <- "Log-normal"

#Candidate model of Hb
candHb_Default <- matrix("", nStr + 1, 2)
rownames(candHb_Default) <- c(strMar, "General")
colnames(candHb_Default) <- c("Cand_1", "Default")
candHb_Default[, 1] <- "Log-normal"
candHb_Default[, 2] <- "Log-normal"

#Candidate model of BSR
candBSR_Default <- matrix("", nStr + 1, 6)
rownames(candBSR_Default) <- c(strMar, "General")
colnames(candBSR_Default) <- c("Cand_1", "Cand_2", "Cand_3", "Cand_4", "Default", "Meth")
candBSR_Default[, 1] <- "Best"
candBSR_Default[, 2] <- "Allele"
candBSR_Default[, 3] <- "LUS"
candBSR_Default[, 4] <- "Multi-seq"
candBSR_Default[, 5] <- "Best"
candBSR_Default[, 6] <- "Locus specific"
candBSR_Default[rownames(candBSR_Default) == "General", 6] <- ""

#Candidate model of FSR
candFSR_Default <- matrix("", nStr + 1, 7)
rownames(candFSR_Default) <- c(strMar, "General")
colnames(candFSR_Default) <- c("Cand_1", "Cand_2", "Cand_3", "Cand_4", "Cand_5", "Default", "Meth")
candFSR_Default[, 1] <- "Best"
candFSR_Default[, 2] <- "Allele"
candFSR_Default[, 3] <- "LUS"
candFSR_Default[, 4] <- "Multi-seq"
candFSR_Default[, 5] <- "Uniform"
candFSR_Default[, 6] <- "Best"
candFSR_Default[, 7] <- "Multiple loci together"
candFSR_Default[rownames(candFSR_Default) == "D22S1045", 7] <- "Locus specific"
candFSR_Default[rownames(candFSR_Default) == "General", 7] <- ""

#Candidate model of DSR
candDSR_Default <- matrix("", nStr + 1, 7)
rownames(candDSR_Default) <- c(strMar, "General")
colnames(candDSR_Default) <- c("Cand_1", "Cand_2", "Cand_3", "Cand_4", "Cand_5", "Default", "Meth")
candDSR_Default[, 1] <- "Best"
candDSR_Default[, 2] <- "Allele"
candDSR_Default[, 3] <- "LUS"
candDSR_Default[, 4] <- "Multi-seq"
candDSR_Default[, 5] <- "Uniform"
candDSR_Default[, 6] <- "Best"
candDSR_Default[, 7] <- "Multiple loci together"
candDSR_Default[rownames(candDSR_Default) == "General", 7] <- ""

#Candidate model of M2SR
candM2SR_Default <- matrix("", nStr + 1, 3)
rownames(candM2SR_Default) <- c(strMar, "General")
colnames(candM2SR_Default) <- c("Cand_1", "Default", "Meth")
candM2SR_Default[, 1] <- "Uniform"
candM2SR_Default[, 2] <- "Uniform"
candM2SR_Default[, 3] <- "Not consider"
candM2SR_Default[rownames(candM2SR_Default) == "SE33", 3] <- "Locus specific"
candM2SR_Default[rownames(candM2SR_Default) == "D1S1656", 3] <- "Locus specific"
candM2SR_Default[rownames(candM2SR_Default) == "General", 3] <- ""

#MLE conditions for AE
mleValAE <- c(1, -0.5, 2, 50, 1, 500)
mleCondAE_Default <- matrix("", length(mleValAE) * (nStr + 1), 5)
colnames(mleCondAE_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondAE_Default[, 5] <- rep(mleValAE, nStr + 1)
mleCondAE_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
mleCondAE_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
mleCondAE_Default[, 2] <- "Log-normal"
mleCondAE_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValAE)))

#MLE conditions for Hb
mleValHb <- c(1, 0.9, 1.1, 50, 1, 500)
mleCondHb_Default <- matrix("", length(mleValHb) * (nStr + 1), 5)
colnames(mleCondHb_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondHb_Default[, 5] <- rep(mleValHb, nStr + 1)
mleCondHb_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
mleCondHb_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
mleCondHb_Default[, 2] <- "Log-normal"
mleCondHb_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValHb)))

#MLE conditions for BSR
mleValBSR <- c(0.01, 0, 0.1, -0.05, -1, 1, 50, 1, 1000,
               0.01, 0, 0.1, -0.05, -1, 1, 50, 1, 1000,
               0.01, 0, 0.1, -0.05, -1, 1, 50, 1, 1000, 2, 2, 30)
mleCondBSR_Default <- matrix("", length(mleValBSR) * (nStr + 1), 5)
colnames(mleCondBSR_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondBSR_Default[, 5] <- rep(mleValBSR, nStr + 1)
mleCondBSR_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 10 * (nStr + 1))
mleCondBSR_Default[, 3] <- rep(c("Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance", "X_value", "X_value", "X_value"), nStr + 1)
mleCondBSR_Default[, 2] <- rep(c(rep("Allele", 9), rep("LUS", 9), rep("Multi-seq", 12)), nStr + 1)
mleCondBSR_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValBSR)))

#MLE conditions for FSR
mleValFSR <- c(0.005, 0, 0.1, -0.01, -1, 1, 500, 1, 5000,
               0.005, 0, 0.1, -0.01, -1, 1, 500, 1, 5000,
               0.005, 0, 0.1, -0.01, -1, 1, 500, 1, 5000, 2, 2, 30,
               0.01, 0, 1, 500, 1, 5000)
mleCondFSR_Default <- matrix("", length(mleValFSR) * (nStr + 1), 5)
colnames(mleCondFSR_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondFSR_Default[, 5] <- rep(mleValFSR, nStr + 1)
mleCondFSR_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 12 * (nStr + 1))
mleCondFSR_Default[, 3] <- rep(c("Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance", "X_value", "X_value", "X_value",
                                 "Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
mleCondFSR_Default[, 2] <- rep(c(rep("Allele", 9), rep("LUS", 9), rep("Multi-seq", 12), rep("Uniform", 6)), nStr + 1)
mleCondFSR_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValFSR)))

#MLE conditions for DSR
mleValDSR <- c(0.005, 0, 0.1, -0.01, -1, 1, 1000, 1, 5000,
               0.005, 0, 0.1, -0.01, -1, 1, 1000, 1, 5000,
               0.005, 0, 0.1, -0.01, -1, 1, 1000, 1, 5000, 2, 2, 30,
               0.01, 0, 1, 500, 1, 5000)
mleCondDSR_Default <- matrix("", length(mleValDSR) * (nStr + 1), 5)
colnames(mleCondDSR_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondDSR_Default[, 5] <- rep(mleValDSR, nStr + 1)
mleCondDSR_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 12 * (nStr + 1))
mleCondDSR_Default[, 3] <- rep(c("Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance",
                                 "Slope", "Slope", "Slope", "Intercept", "Intercept", "Intercept", "Variance", "Variance", "Variance", "X_value", "X_value", "X_value",
                                 "Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
mleCondDSR_Default[, 2] <- rep(c(rep("Allele", 9), rep("LUS", 9), rep("Multi-seq", 12), rep("Uniform", 6)), nStr + 1)
mleCondDSR_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValDSR)))

#MLE conditions for M2SR
mleValM2SR <- c(0.01, 0, 1, 500, 1, 5000)
mleCondM2SR_Default <- matrix("", length(mleValM2SR) * (nStr + 1), 5)
colnames(mleCondM2SR_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
mleCondM2SR_Default[, 5] <- rep(mleValM2SR, nStr + 1)
mleCondM2SR_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
mleCondM2SR_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
mleCondM2SR_Default[, 2] <- "Uniform"
mleCondM2SR_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValM2SR)))

usethis::use_data(kitDefault, parDefault, alCorDefault, afMethDefault, mafDefault, thetaDefault, numMCDefault, atDefault, atSingleDefault, mrDefault, degDefault,
                  stDefault, mrFltrDefault, hbFltrDefault, bsrFltrDefault, fsrFltrDefault, dsrFltrDefault, m2srFltrDefault,
                  aeMinDefault, hbMinDefault, bsrMaxDefault, fsrMaxDefault, dsrMaxDefault, m2srMaxDefault, mrDegCut, mtDefault,
                  strMar, candAE_Default, candHb_Default, candBSR_Default, candFSR_Default, candDSR_Default, candM2SR_Default,
                  mleCondAE_Default, mleCondHb_Default, mleCondBSR_Default, mleCondFSR_Default, mleCondDSR_Default, mleCondM2SR_Default,
                  pg_uu_false, pg_uu_true,
                  internal = TRUE, overwrite = TRUE)

#########################################
# Example data for checking calculation #
#########################################

pg_uu_false <- read.csv("secret/pg_uu_false.csv", header = TRUE)
pg_uu_false <- as.matrix(pg_uu_false)
pg_uu_true <- read.csv("secret/pg_uu_true.csv", header = TRUE)
pg_uu_true <- as.matrix(pg_uu_true)

usethis::use_data(pg_uu_false, pg_uu_true,
                  internal = TRUE, overwrite = TRUE)
