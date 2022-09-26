# Pick up allele sizes (testthat)
pickAlSize <- function(sampleL, peakAllL, sizeAllL, heightAllL, mt_kitLoci, dye_kitLoci, gtOne){
  alPeak <- alSize <- alDye <- numeric(0)
  nL <- length(sampleL)
  colGt <- colnames(gtOne)
  posGtMar <- grep("Marker", colGt)
  posGtAl <- grep("Allele", colGt)
  for(i in 1:nL){
    L1 <- sampleL[i]
    posPeakOneL <- !is.element(peakAllL[i, ], c("", NA))
    peakOneL <- peakAllL[i, posPeakOneL]
    sizeOneL <- as.numeric(sizeAllL[i, posPeakOneL])
    heightOneL <- as.numeric(heightAllL[i, posPeakOneL])
    dyeOneL <- dye_kitLoci[names(dye_kitLoci) == L1]
    posUpMt <- heightOneL >= mt_kitLoci[names(mt_kitLoci) == L1]
    peakOneL <- peakOneL[posUpMt]
    sizeOneL <- sizeOneL[posUpMt]
    gtOneL <- gtOne[gtOne[, posGtMar] == L1, posGtAl]
    posAl <- which(is.element(peakOneL, gtOneL) == TRUE)
    alPeak <- c(alPeak, peakOneL[posAl])
    alSize <- c(alSize, sizeOneL[posAl])
    alDye <- c(alDye, rep(dyeOneL, length(posAl)))
  }
  alSizeInfo <- rbind(alSize, alDye)
  colnames(alSizeInfo) <- alPeak
  return(alSizeInfo)
}

# Remove two or more peaks which are located in the same bin (testthat)
removeSameBin <- function(calOne){
  peakCount <- table(calOne[1, ])
  posCutPeak <- which(peakCount != 1)
  if(length(posCutPeak) != 0){
    calOne <- calOne[, - which(is.element(calOne[1, ], names(peakCount)[posCutPeak]) == TRUE), drop = FALSE]
  }
  return(calOne)
}

# Remove pull-up peaks (testthat)
removePU <- function(calOne, dyeOneL, alSize, alDye, gtOneL){
  peakOneL2 <- calOne[1, ]
  sizeOneL2 <- calOne[2, ]
  heightOneL2 <- calOne[3, ]
  posAl2 <- which(is.element(peakOneL2, gtOneL) == TRUE)
  puDyePos <- which(alDye != dyeOneL)
  puSize <- alSize[puDyePos]
  posPu <- numeric(0)
  for(k in 1:length(sizeOneL2)){
    if(!is.element(k, posAl2)){
      size2One <- sizeOneL2[k]
      if(length(which(abs(size2One - puSize) <= 1)) != 0){
        posPu <- c(posPu, k)
      }
    }
  }
  if(length(posPu) != 0){
    calOne <- calOne[, - posPu, drop = FALSE]
  }
  return(calOne)
}

# Arrange experimental data
arrangeExData <- function(exData, gtData, parLoci, sexMar, mt_kitLoci, dye_kitLoci, srConsider){
  colEx <- colnames(exData)
  posSN <- intersect(grep("Sample", colEx), grep("Name", colEx))
  sampleNames <- unique(exData[, posSN])
  posMar <- grep("Marker", colEx)
#  posDye <- grep("Dye", colCal)
  posPeak <- grep("Allele", colEx)
  posSize <- grep("Size", colEx)
  posHeight <- grep("Height", colEx)

  colGt <- colnames(gtData)
  posGtSN <- intersect(grep("Sample", colGt), grep("Name", colGt))
  posGtMar <- grep("Marker", colGt)
  posGtAl <- grep("Allele", colGt)

  sampleOne <- exData[exData[, posSN] == sampleNames[1], ]
  sampleL <- sampleOne[, posMar]
#  dyeAllL <- sampleOne[, posDye]

  exData2 <- gtData2 <- list()
  for(i in 1:length(sampleNames)){
    sampleOne <- exData[exData[, posSN] == sampleNames[i], ]
    sampleL <- sampleOne[, posMar]
    peakAllL <- sampleOne[, posPeak]
    sizeAllL <- sampleOne[, posSize]
    heightAllL <- sampleOne[, posHeight]
    sampleName <- sampleOne[1, posSN]
    gtOne <- gtData[gtData[, posGtSN] == sampleName, ]

    alSizeInfo <- pickAlSize(sampleL, peakAllL, sizeAllL, heightAllL, mt_kitLoci, dye_kitLoci, gtOne)
    alSize <- as.numeric(alSizeInfo[1, ])
    alDye <- alSizeInfo[2, ]

    exData2SampleOne <- list()
    nL2 <- length(parLoci)
    gtOne2 <- matrix(NA, nrow = nL2, ncol = 2)
    for(j in 1:nL2){
      L2 <- parLoci[j]
      posL2 <- grep(L2, sampleOne[, posMar])
      posPeakOneL <- !is.element(peakAllL[posL2, ], c("", NA))
      peakOneL <- peakAllL[posL2, posPeakOneL]
      options(warn = -1)
      peakOneL <- as.numeric(peakOneL)
      options(warn = 0)
      sizeOneL <- as.numeric(sizeAllL[posL2, posPeakOneL])
      heightOneL <- as.numeric(heightAllL[posL2, posPeakOneL])
      dyeOneL <- dye_kitLoci[names(dye_kitLoci) == L2]
      posUpMt <- heightOneL >= mt_kitLoci[names(mt_kitLoci) == L2]
      peakOneL <- peakOneL[posUpMt]
      sizeOneL <- sizeOneL[posUpMt]
      heightOneL <- heightOneL[posUpMt]

      gtOneL <- gtOne[gtOne[, posGtMar] == L2, posGtAl]
      gtOneL <- as.numeric(gtOneL)
      gtOne2[j, ] <- gtOneL
      gtOneL <- gtOneL[!is.na(gtOneL)]

      candPeaks <- makeCandPeak(gtOneL, c(srConsider[j, ]))
      posUse <- which(is.element(peakOneL, candPeaks) == TRUE)

      if(length(posUse) != 0){
        peakOneL2 <- peakOneL[posUse]
        sizeOneL2 <- as.numeric(sizeOneL[posUse])
        heightOneL2 <- as.numeric(heightOneL[posUse])
        calOne <- rbind(peakOneL2, sizeOneL2, heightOneL2)
        calOne <- removeSameBin(calOne)
        exData2SampleOne[[j]] <- removePU(calOne, dyeOneL, alSize, alDye, gtOneL)
      }else{
        exData2SampleOne[[j]] <- 0
      }
    }
    exData2[[i]] <- exData2SampleOne
    gtData2[[i]] <- gtOne2
  }
  return(list(exData2, gtData2))
}

# Get data of locus-specific amplification efficiency (testthat)
getAEData <- function(exData2){
  nS <- length(exData2)
  nL <- length(exData2[[1]])
  aeHeightSum <- matrix(0, nS, nL)
  useJudge <- rep(TRUE, nS)
  for(i in 1:nS){
    exData2One <- exData2[[i]]
    for(j in 1:nL){
      exData2OneL <- exData2One[[j]]
      if(is.matrix(exData2OneL)){
        aeHeightSum[i, j] <- sum(exData2OneL[3, ]) / 2
      }else{
        useJudge[i] <- FALSE
      }
    }
  }
  aeData <- matrix(0, nS, nL)
  for(i in 1:nS){
    aeData[i, ] <- aeHeightSum[i, ] / mean(aeHeightSum[i, ])
  }
  aeData <- aeData[useJudge, , drop = FALSE]
  aeHeightData <- aeHeightSum[useJudge, , drop = FALSE]
  return(list(aeData, aeHeightData))
}

# Calculate a heterozygote balance and the peak height that substitutes for DNA amount (testthat)
calcHb <- function(peak, height, gtOneL, srConsiderOneL){
  gtOneL <- sort(unique(gtOneL[!is.na(gtOneL)]))
  hb <- hbHeight <- 0
  # heterozygote && other than one repeat difference
  if((length(gtOneL) == 2) && ((abs(round(gtOneL[1] - gtOneL[2], 1)) != 1))){
    # no allelic drop-out
    if(all(is.element(gtOneL, peak))){
      h <- rep(0, 2)
      for(k in 1:2){
        alOne <- gtOneL[k]
        alTwo <- gtOneL[3 - k]
        shareAlSt <- alTwo
        if(srConsiderOneL[1]){
          shareAlSt <- c(shareAlSt, alTwo - 1)
        }
        if(srConsiderOneL[2]){
          shareAlSt <- c(shareAlSt, alTwo + 1)
        }
        if(srConsiderOneL[3]){
          shareAlSt <- c(shareAlSt, alTwo - 2)
        }
        if(srConsiderOneL[4]){
          if(alTwo %% 1 == 0){
            shareAlSt <- c(shareAlSt, alTwo - 0.8)
          }else{
            shareAlSt <- c(shareAlSt, alTwo - 0.2)
          }
        }
        shareAlSt <- round(sort(unique(shareAlSt)), 1)
        heightSum <- height[which(peak == alOne)]

        # Considering the effect of back stutter
        if(srConsiderOneL[1]){
          bs <- round(alOne - 1, 1)
          if(is.element(bs, peak)){
            heightSum <- heightSum + height[which(peak == bs)]
          }
        }

        # Considering the effect of forward stutter
        if(srConsiderOneL[2]){
          fs <- round(alOne + 1, 1)
          if((is.element(fs, peak)) && !is.element(fs, shareAlSt)){
            heightSum <- heightSum + height[which(peak == fs)]
          }
        }

        # Considering the effect of double-back stutter
        if(srConsiderOneL[3]){
          ds <- round(alOne - 2, 1)
          if((is.element(ds, peak)) && !is.element(ds, shareAlSt)){
            heightSum <- heightSum + height[which(peak == ds)]
          }
        }

        # Considering the effect of minus 2-nt stutter
        if(srConsiderOneL[4]){
          if(alOne %% 1 < 0.15){
            m2s <- round(alOne - 0.8, 1)
          }else{
            m2s <- round(alOne - 0.2, 1)
          }
          if((is.element(m2s, peak)) && !is.element(m2s, shareAlSt)){
            heightSum <- heightSum + height[which(peak == m2s)]
          }
        }
        h[k] <- heightSum
      }
      hb <- h[2] / h[1]
      hbHeight <- sum(height) / 2
    }
  }
  hbH <- c(hb, hbHeight)
  names(hbH) <- c("hb", "hbHeight")
  return(hbH)
}

# Get data of heterozygote balance
getHbData <- function(exData2, gtData2, srConsider){
  nS <- length(exData2)
  nL <- length(exData2[[1]])
  hbData <- hbHeightData <- matrix(0, nS, nL)
  for(i in 1:nS){
    exData2One <- exData2[[i]]
    gtAnsOne <- gtData2[[i]]
    for(j in 1:nL){
      exData2OneL <- exData2One[[j]]
      if(is.matrix(exData2OneL)){
        peak <- exData2OneL[1, ]
        height <- exData2OneL[3, ]
        gtOneL <- gtAnsOne[j, ]
        gtOneL <- sort(unique(gtOneL[!is.na(gtOneL)]))
        srConsiderOneL <- srConsider[j, ]

        hbH <- calcHb(peak, height, gtOneL, srConsiderOneL)
        hbData[i, j] <- hbH[1]
        hbHeightData[i, j] <- hbH[2]
      }
    }
  }
  return(list(hbData, hbHeightData))
}

# Calculate stutter ratios and the peak height that substitutes for DNA amount (testthat)
calcSr <- function(peak, height, gtOneL, srConsiderOneL){
  gtOneL <- sort(unique(gtOneL[!is.na(gtOneL)]))
  bsr <- fsr <- dsr <- m2sr <- al <- srH <- rep(0, 2)
  # homozygote
  if(length(gtOneL) == 1){
    al[1] <- gtOneL

    # no allelic drop-out
    if(is.element(gtOneL, peak)){
      srH[1] <- sum(height)
      heightAl <- height[which(peak == gtOneL)]

      # back stutter
      if(srConsiderOneL[1]){
        if(is.element(round(gtOneL - 1, 1), peak)){
          bsr[1] <- height[which(peak == round(gtOneL - 1, 1))] / heightAl
        }
      }

      # forward stutter
      if(srConsiderOneL[2]){
        if(is.element(round(gtOneL + 1, 1), peak)){
          fsr[1] <- height[which(peak == round(gtOneL + 1, 1))] / heightAl
        }
      }

      # double-back stutter
      if(srConsiderOneL[3]){
        if(is.element(round(gtOneL - 2, 1), peak)){
          dsr[1] <- height[which(peak == round(gtOneL - 2, 1))] / heightAl
        }
      }
      # minus 2-nt stutter
      if(srConsiderOneL[4]){
        # allele x or x.1
        if(gtOneL %% 1 < 0.15){
          if(is.element(round(gtOneL - 0.8, 1), peak)){
            m2sr[1] <- height[which(peak == round(gtOneL - 0.8, 1))] / heightAl
          }
          # allele x.2, x.3, ...
        }else{
          if(is.element(round(gtOneL - 0.2, 1), peak)){
            m2sr[1] <- height[which(peak == round(gtOneL - 0.2, 1))] / heightAl
          }
        }
      }
    }
    # heterozygote && other than one repeat difference
  }else if(abs(round(gtOneL[1] - gtOneL[2], 1)) != 1){
    for(k in 1:2){
      alOne <- gtOneL[k]
      # no allelic drop-out
      if(length(which(peak == alOne)) != 0){
        alTwo <- gtOneL[3 - k]
        shareAlSt <- alTwo
        if(srConsiderOneL[1]){
          shareAlSt <- c(shareAlSt, alTwo - 1)
        }
        if(srConsiderOneL[2]){
          shareAlSt <- c(shareAlSt, alTwo + 1)
        }
        if(srConsiderOneL[3]){
          shareAlSt <- c(shareAlSt, alTwo - 2)
        }
        if(srConsiderOneL[4]){
          if(alTwo %% 1 < 0.15){  #allele x or x.1
            shareAlSt <- c(shareAlSt, alTwo - 0.8)
          }else{  #allele x.2, x.3, ...
            shareAlSt <- c(shareAlSt, alTwo - 0.2)
          }
        }
        shareAlSt <- round(sort(unique(shareAlSt)), 1)
        al[k] <- alOne
        heightSum <- heightAl <- height[which(peak == alOne)]

        dsPos <- bsPos <- fsPos <- m2sPos <- numeric(0)

        # back stutter
        if(srConsiderOneL[1]){
          bs <- round(alOne - 1, 1)
          if(is.element(bs, peak)){
            heightSum <- heightSum + height[which(peak == bs)]
            if(!is.element(bs, shareAlSt)){
              bsPos <- which(peak == bs)
              bsr[k] <- height[bsPos] / heightAl
            }
          }
        }

        # forward stutter
        if(srConsiderOneL[2]){
          fs <- round(alOne + 1, 1)
          if((is.element(fs, peak)) && !is.element(fs, shareAlSt)){
            fsPos <- which(peak == fs)
            fsr[k] <- height[fsPos] / heightAl
            heightSum <- heightSum + height[fsPos]
          }
        }

        # double-back stutter
        if(srConsiderOneL[3]){
          ds <- round(alOne - 2, 1)
          if((is.element(ds, peak)) && !is.element(ds, shareAlSt)){
            dsPos <- which(peak == ds)
            dsr[k] <- height[dsPos] / heightAl
            heightSum <- heightSum + height[dsPos]
          }
        }

        # minus 2-nt stutter
        if(srConsiderOneL[4]){
          # allele x or x.1
          if(alOne %% 1 < 0.15){
            m2s <- round(alOne - 0.8, 1)
            # allele x.2, x.3, ...
          }else{
            m2s <- round(alOne - 0.2, 1)
          }
          if((is.element(m2s, peak)) && !is.element(m2s, shareAlSt)){
            m2sPos <- which(peak == m2s)
            m2sr[k] <- height[m2sPos] / heightAl
            heightSum <- heightSum + height[m2sPos]
          }
        }
        srH[k] <- heightSum
      }
    }
  }
  return(list(bsr, fsr, dsr, m2sr, al, srH))
}

# Get data of stutter ratios
getSRData <- function(exData2, gtData2, srConsider){
  numSample <- length(exData2)
  nL <- length(exData2[[1]])
  bsrData <- fsrData <- dsrData <- m2srData <- srAlData <- srHeightData <- matrix(0, 2 * numSample, nL)
  for(i in 1:numSample){
    exData2One <- exData2[[i]]
    gtAnsOne <- gtData2[[i]]
    for(j in 1:nL){
      exData2OneL <- exData2One[[j]]
      if(is.matrix(exData2OneL)){
        peak <- exData2OneL[1, ]
        height <- exData2OneL[3, ]
        gtOneL <- gtAnsOne[j, ]
        gtOneL <- sort(unique(gtOneL[!is.na(gtOneL)]))
        srConsiderOneL <- srConsider[j, ]

        srData <- calcSr(peak, height, gtOneL, srConsiderOneL)
        bsrData[c(2 * i - 1, 2 * i), j] <- srData[[1]]
        fsrData[c(2 * i - 1, 2 * i), j] <- srData[[2]]
        dsrData[c(2 * i - 1, 2 * i), j] <- srData[[3]]
        m2srData[c(2 * i - 1, 2 * i), j] <- srData[[4]]
        srAlData[c(2 * i - 1, 2 * i), j] <- srData[[5]]
        srHeightData[c(2 * i - 1, 2 * i), j] <- srData[[6]]
      }
    }
  }
  return(list(bsrData, fsrData, dsrData, m2srData, srAlData, srHeightData))
}

# Number of repeats in each motif (for LUS and multi-seq model) (testthat)
pickRep <- function(repSeq){
  motifSeq <- strsplit(repSeq, ' ')[[1]]
  motifRep <- numeric(0)
  for(i in 1:length(motifSeq)){
    motifOne <- motifSeq[i]
    posBracket <- regexpr("]", motifOne)[1]
    if(posBracket != -1){
      motifRep <- c(motifRep, as.numeric(strsplit(motifOne, ']')[[1]][2]))
    }
  }
  return(motifRep)
}

# Calculate a weighted LUS value (testthat)
calcWlus <- function(lusOneAl, countOneAl){
  return(sum(lusOneAl * countOneAl / sum(countOneAl)))
}

# Analyze sequence data
analyzeSeq <- function(seqData, parLoci){
  lusData <- list()
  motifLengthList <- list()
  seqAlList <- list()
  seqCountList <- list()
  for(i in 1:length(parLoci)){
    posL <- which(seqData[, which(colnames(seqData) == "Marker")] == parLoci[i])
    seqAlList[[i]] <- alleles <- as.numeric(seqData[posL, which(colnames(seqData) == "Allele")])
    alNames <- sort(unique(alleles))
    seqCountList[[i]] <- count <- as.numeric(seqData[posL, which(colnames(seqData) == "Count")])

    repSeqOneL <- seqData[posL, intersect(grep("Repeat", colnames(seqData)), grep("Region", colnames(seqData)))]
    motifLengthList[[i]] <- lapply(repSeqOneL, pickRep)

    lus <- sapply(motifLengthList[[i]], max)
    lusOneL <- rep(0, length(alNames))
    for(j in 1:length(alNames)){
      alPos <- which(alleles == alNames[j])
      lusOneL[j] <- calcWlus(lus[alPos], count[alPos])
    }
    names(lusOneL) <- alNames
    lusData[[i]] <- lusOneL
  }
  names(lusData) <- parLoci
  names(motifLengthList) <- parLoci
  names(seqAlList) <- parLoci
  names(seqCountList) <- parLoci
  return(list(lusData, motifLengthList, seqAlList, seqCountList))
}

# Calculate AIC (testthat)
calcAIC <- function(logLike, nP){
  return(-2 * logLike + 2 * nP)
}

# likelihood function of Log-normal model (testthat)
# par(mean, variance)
aeLognormLike <- function(par, ae, aeHeight, minAE){
  dens <- dtruncnorm(log(ae), a = log(minAE), b = log(1 / minAE), mean = log(par[1]), sd = sqrt(par[2] / aeHeight))
  return(-sum(log(dens)))
}

# Modeling locus-specific amplification efficiency
aeModeling <- function(modelName, ae, aeHeight, minAE, mleCondAE_oneL){
  aeMleData <- list()

  #Log-normal model
  posModel <- which(modelName == "Log-normal")
  if(length(posModel) == 1){
    posCond <- mleCondAE_oneL[, "Model"] == "Log-normal"
    mleCondAE_oneM <- mleCondAE_oneL[posCond, , drop = FALSE]
    posInt <- mleCondAE_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondAE_oneM[posInt, "Value"])
    posLow <- mleCondAE_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondAE_oneM[posLow, "Value"])
    posUp <- mleCondAE_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondAE_oneM[posUp, "Value"])
    options(warn = -1)
    aeLognormFit <- GenSA(par = initialVal, fn = aeLognormLike, ae = ae, aeHeight = aeHeight, minAE = minAE, lower = lowerVal, upper = upperVal)
    options(warn = 0)
    logLike <- - aeLognormFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- aeLognormFit[[2]]
    names(estPar) <- c("Mean", "Variance")
    aeMleData[[posModel]] <- list(modelInfo, estPar, "Log-normal")
  }
  return(aeMleData)
}

# likelihood function of Log-normal model (testthat)
# par(mean, variance)
hbLognormLike <- function(par, hb, hbHeight, minHb){
  dens <- dtruncnorm(log(hb), a = log(minHb), b = log(1 / minHb), mean = log(par[1]), sd = sqrt(par[2] / hbHeight))
  return(-sum(log(dens)))
}

# Modeling heterozygote balance
hbModeling <- function(modelName, hb, hbHeight, minHb, mleCondHb_oneL){
  hbMleData <- list()

  #Log-normal model
  posModel <- which(modelName == "Log-normal")
  if(length(posModel) == 1){
    posCond <- mleCondHb_oneL[, "Model"] == "Log-normal"
    mleCondHb_oneM <- mleCondHb_oneL[posCond, , drop = FALSE]
    posInt <- mleCondHb_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondHb_oneM[posInt, "Value"])
    posLow <- mleCondHb_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondHb_oneM[posLow, "Value"])
    posUp <- mleCondHb_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondHb_oneM[posUp, "Value"])
    options(warn = -1)
    hbLognormFit <- GenSA(par = initialVal, fn = hbLognormLike, hb = hb, hbHeight = hbHeight, minHb = minHb, lower = lowerVal, upper = upperVal)
    options(warn = 0)
    logLike <- - hbLognormFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- hbLognormFit[[2]]
    names(estPar) <- c("Mean", "Variance")
    hbMleData[[posModel]] <- list(modelInfo, estPar, "Log-normal")
  }
  return(hbMleData)
}

# Make usable SR data when considering each locus separately
makeSrUse <- function(sr, srAl, srHeight, lusOneL, srMax){
  posNonZero <- which(sr != 0)
  sr <- sr[posNonZero]
  srAl <- srAl[posNonZero]
  srHeight <- srHeight[posNonZero]
  posSrAdopt <- which(sr <= srMax)
  sr <- sr[posSrAdopt]
  srAl <- srAl[posSrAdopt]
  srHeight <- srHeight[posSrAdopt]

  alNames <- as.numeric(names(lusOneL))
  srLus <- rep(-100, length(sr))
  for(j in 1:length(lusOneL)){
    posLUS <- which(srAl == alNames[j])
    if(length(posLUS) != 0){
      srLus[posLUS] <- lusOneL[j]
    }
  }

  posNoSeq <- which(srLus == -100)
  if(length(posNoSeq) != 0){
    sr <- sr[-posNoSeq]
    srAl <- srAl[-posNoSeq]
    srHeight <- srHeight[-posNoSeq]
    srLus <- srLus[-posNoSeq]
  }
  srUseList <- list(sr, srAl, srHeight, srLus)
  names(srUseList) <- c("stutter ratio", "parent allele", "total allelic product", "LUS of parent allele")
  return(srUseList)
}

# Make SR data for considering multiple loci together
makeSrGenData <- function(posSrGen, srData, srAlData, srHeightData, lusData, motifLengthList, seqAlList, seqCountList, srMax){
  srGen <- srAlGen <- srHeightGen <- srLusGen <- motifLengthGen <- seqAlGen <- seqCountGen <- list()
  for(i in 1:length(posSrGen)){
    posSr <- posSrGen[i]
    srUseData <- makeSrUse(srData[, posSr], srAlData[, posSr], srHeightData[, posSr], lusData[[posSr]], srMax)
    srGen[[i]] <- srUseData[[1]]
    srAlGen[[i]] <- srUseData[[2]]
    srHeightGen[[i]] <- srUseData[[3]]
    srLusGen[[i]] <- srUseData[[4]]
    motifLengthGen[[i]] <- motifLengthList[[posSr]]
    seqAlGen[[i]] <- seqAlList[[posSr]]
    seqCountGen[[i]] <- seqCountList[[posSr]]
  }
  srGenList <- list(srGen, srAlGen, srHeightGen, srLusGen, motifLengthGen, seqAlGen, seqCountGen)
  names(srGenList) <- c("stutter ratio", "parent allele", "total allelic product", "LUS of parent allele", "motif length", "alleles of sequence data", "counts of each sequence")
  return(srGenList)
}

# likelihood function of uniform model (testthat)
# par(mean, variance)
srUniformLike <- function(par, sr, srHeight, srMax){
  dens <- dtruncnorm(log(sr), b = log(srMax), mean = log(par[1]), sd = sqrt(par[2] / srHeight))
  return(-sum(log(dens)))
}

# likelihood function of Allele model (testthat)
# par(slope, intercept, variance)
srAlLike <- function(par, sr, srAl, srHeight, srMax){
  meanVal <- par[1] * srAl + par[2]
  dens <- dtruncnorm(log(sr), b = log(srMax), mean = log(meanVal), sd = sqrt(par[3] / srHeight))
  return(-sum(log(dens)))
}

# likelihood function of LUS model (testthat)
# par(slope, intercept, variance)
srLusLike <- function(par, sr, srLus, srHeight, srMax){
  meanVal <- par[1] * srLus + par[2]
  dens <- dtruncnorm(log(sr), b = log(srMax), mean = log(meanVal), sd = sqrt(par[3] / srHeight))
  return(-sum(log(dens)))
}

# Calculate corrected allele numbers (testthat)
calcCA <- function(minUS, motifLengthOneAl, seqCountOneAl){
  nSeqAl <- length(motifLengthOneAl)
  lenCorSumOneAl <- rep(0, nSeqAl)
  for(i in 1:nSeqAl){
    lenCor <- motifLengthOneAl[[i]] - minUS
    lenCorSumOneAl[i] <- sum(lenCor[which(lenCor > 0)])
  }
  return(sum(lenCorSumOneAl * seqCountOneAl / sum(seqCountOneAl)))
}

# Make data of corrected allele numbers (testthat)
makeCAData <- function(minUS, motifLength, seqAl, seqCount, alPar){
  caPar <- rep(0, length(alPar))
  alNames <- sort(unique(seqAl))
  nAlN <- length(alNames)
  for(i in 1:nAlN){
    posSeqAl <- which(seqAl == alNames[i])
    caPar[which(alPar == alNames[i])] <- calcCA(minUS, motifLength[posSeqAl], seqCount[posSeqAl])
  }
  return(caPar)
}

# likelihood function of multi-sequence model (modeling multiple loci together)
# par(slope, intercept, variance, x)
srMultiLikeGen <- function(par, sr, srAlList, srHeight, srMax, motifLengthList, seqAlList, seqCountList){
  lenCorMeanAll <- rep(0, sum(sapply(srAlList, length)))
  count <- 0
  for(i in 1:length(motifLengthList)){
    minUS <- round(par[4], 0)
    motifLength <- motifLengthList[[i]]
    seqAl <- seqAlList[[i]]
    seqCount <- seqCountList[[i]]
    srAl <- srAlList[[i]]
    lenCorMeanOneL <- makeCAData(minUS, motifLength, seqAl, seqCount, srAl)
    count2 <- length(lenCorMeanOneL)
    if(count2 > 0){
      lenCorMeanAll[(count + 1):(count + count2)] <- lenCorMeanOneL
      count <- count + count2
    }
  }
  meanVal <- par[1] * lenCorMeanAll + par[2]
  dens <- dtruncnorm(log(sr), b = log(srMax), mean = log(meanVal), sd = sqrt(par[3] / srHeight))
  return(-sum(log(dens)))
}

# likelihood function of multi-sequence model (locus specific)
# par(slope, intercept, variance, x)
srMultiLikeSp <- function(par, sr, srAl, srHeight, srMax, motifLength, seqAl, seqCount){
  minUS <- round(par[4], 0)
  lenCorMeanOneL <- makeCAData(minUS, motifLength, seqAl, seqCount, srAl)
  meanVal <- par[1] * lenCorMeanOneL + par[2]
  dens <- dtruncnorm(log(sr), b = log(srMax), mean = log(meanVal), sd = sqrt(par[3] / srHeight))
  return(-sum(log(dens)))
}

# Modeling stutter ratio. When modeling multiple loci together, each object is the list of each locus data.
srModeling <- function(modelName, sr, srAl, srHeight, srLus = NULL, motifLength = NULL, seqAl = NULL, seqCount = NULL, srMax, mleCondSR_oneL){
  if(is.list(sr)){
    multiL <- TRUE
    srAlList <- srAl
    sr <- unlist(sr)
    srAl <- unlist(srAl)
    srHeight <- unlist(srHeight)
    srLus <- unlist(srLus)
  }else{
    multiL <- FALSE
  }

  srMleData <- list()

  #Allele model
  posModel <- which(modelName == "Allele")
  if(length(posModel) == 1){
    posCond <- mleCondSR_oneL[, "Model"] == "Allele"
    mleCondSR_oneM <- mleCondSR_oneL[posCond, , drop = FALSE]
    posInt <- mleCondSR_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondSR_oneM[posInt, "Value"])
    posLow <- mleCondSR_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondSR_oneM[posLow, "Value"])
    posUp <- mleCondSR_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondSR_oneM[posUp, "Value"])
    options(warn = -1)
    srAlleleFit <- GenSA(par = initialVal, fn = srAlLike, sr = sr, srAl = srAl, srHeight = srHeight, srMax = srMax, lower = lowerVal, upper = upperVal)
    options(warn = 0)
    logLike <- - srAlleleFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- srAlleleFit[[2]]
    names(estPar) <- c("Slope", "Intercept", "Variance")
    srMleData[[posModel]] <- list(modelInfo, estPar, "Allele")
  }

  #LUS model
  posModel <- which(modelName == "LUS")
  if(length(posModel) == 1){
    posCond <- mleCondSR_oneL[, "Model"] == "LUS"
    mleCondSR_oneM <- mleCondSR_oneL[posCond, , drop = FALSE]
    posInt <- mleCondSR_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondSR_oneM[posInt, "Value"])
    posLow <- mleCondSR_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondSR_oneM[posLow, "Value"])
    posUp <- mleCondSR_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondSR_oneM[posUp, "Value"])
    options(warn = -1)
    srLusFit <- GenSA(par = initialVal, fn = srLusLike, sr = sr, srLus = srLus, srHeight = srHeight, srMax = srMax, lower = lowerVal, upper = upperVal)
    options(warn = 0)
    logLike <- - srLusFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- srLusFit[[2]]
    names(estPar) <- c("Slope", "Intercept", "Variance")
    srMleData[[posModel]] <- list(modelInfo, estPar, "LUS")
  }

  #Multi-seq model
  posModel <- which(modelName == "Multi-seq")
  if(length(posModel) == 1){
    posCond <- mleCondSR_oneL[, "Model"] == "Multi-seq"
    mleCondSR_oneM <- mleCondSR_oneL[posCond, , drop = FALSE]
    posInt <- mleCondSR_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondSR_oneM[posInt, "Value"])
    posLow <- mleCondSR_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondSR_oneM[posLow, "Value"])
    posUp <- mleCondSR_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondSR_oneM[posUp, "Value"])
    if(multiL){
      options(warn = -1)
      srMultiFit <- GenSA(par = initialVal, fn = srMultiLikeGen, sr = sr, srAlList = srAlList, srHeight = srHeight, srMax = srMax, motifLengthList = motifLength, seqAl = seqAl, seqCount = seqCount, lower = lowerVal, upper = upperVal)
      options(warn = 0)
    }else{
      options(warn = -1)
      srMultiFit <- GenSA(par = initialVal, fn = srMultiLikeSp, sr = sr, srAl = srAl, srHeight = srHeight, srMax = srMax, motifLength = motifLength, seqAl = seqAl, seqCount = seqCount, lower = lowerVal, upper = upperVal)
      options(warn = 0)
    }
    logLike <- - srMultiFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- srMultiFit[[2]]
    estPar[4] <- round(estPar[4], 0)
    names(estPar) <- c("Slope", "Intercept", "Variance", "X_value")
    srMleData[[posModel]] <- list(modelInfo, estPar, "Multi-seq")
  }

  #Uniform model
  posModel <- which(modelName == "Uniform")
  if(length(posModel) == 1){
    posCond <- mleCondSR_oneL[, "Model"] == "Uniform"
    mleCondSR_oneM <- mleCondSR_oneL[posCond, , drop = FALSE]
    posInt <- mleCondSR_oneM[, "Type"] == "Initial"
    initialVal <- as.numeric(mleCondSR_oneM[posInt, "Value"])
    posLow <- mleCondSR_oneM[, "Type"] == "Lower"
    lowerVal <- as.numeric(mleCondSR_oneM[posLow, "Value"])
    posUp <- mleCondSR_oneM[, "Type"] == "Upper"
    upperVal <- as.numeric(mleCondSR_oneM[posUp, "Value"])
    options(warn = -1)
    srUniformFit <- GenSA(par = initialVal, fn = srUniformLike, sr = sr, srHeight = srHeight, srMax = srMax, lower = lowerVal, upper = upperVal)
    options(warn = 0)
    logLike <- - srUniformFit[[1]]
    aic <- calcAIC(logLike, length(initialVal))
    modelInfo <- c(logLike, aic)
    names(modelInfo) <- c("likelihood", "AIC")
    estPar <- srUniformFit[[2]]
    names(estPar) <- c("Mean", "Variance")
    srMleData[[posModel]] <- list(modelInfo, estPar, "Uniform")
  }

  names(srMleData) <- modelName
  return(srMleData)
}

# Make export data of allele repeat correction
makeExportAlCor <- function(lusData, motifLengthList, seqAlList, seqCountList,
                            bsrMleData, fsrMleData, dsrMleData){
  parLoci <- names(lusData)
  nL <- length(parLoci)
  nAl <- sapply(lusData, length)
  alCorData <- matrix("", sum(nAl), 6)
  colnames(alCorData) <- c("Marker", "Allele", "LUS", "CA_BSR", "CA_FSR", "CA_DSR")
  count <- 0
  for(i in 1:nL){
    posRow <- (count + 1):(count + nAl[i])
    count <- count + nAl[i]
    lusOneL <- lusData[[i]]

    motifLength <- motifLengthList[[i]]
    seqAl <- seqAlList[[i]]
    seqCount <- seqCountList[[i]]

    alCorData[posRow, 1] <- parLoci[i]
    alCorData[posRow, 2] <- names(lusOneL)
    alCorData[posRow, 3] <- lusOneL

    bsrMleData_oneL <- bsrMleData[[i]]
    posMulti <- which(names(bsrMleData_oneL) == "Multi-seq")
    if(length(posMulti) > 0){
      minUS <- bsrMleData_oneL[[posMulti]][[2]]["X_value"]
      alCorData[posRow, 4] <- makeCAData(minUS, motifLength, seqAl, seqCount, sort(unique(seqAl)))
    }

    fsrMleData_oneL <- fsrMleData[[i]]
    posMulti <- which(names(fsrMleData_oneL) == "Multi-seq")
    if(length(posMulti) > 0){
      minUS <- fsrMleData_oneL[[posMulti]][[2]]["X_value"]
      alCorData[posRow, 5] <- makeCAData(minUS, motifLength, seqAl, seqCount, sort(unique(seqAl)))
    }

    dsrMleData_oneL <- dsrMleData[[i]]
    posMulti <- which(names(dsrMleData_oneL) == "Multi-seq")
    if(length(posMulti) > 0){
      minUS <- dsrMleData_oneL[[posMulti]][[2]]["X_value"]
      alCorData[posRow, 6] <- makeCAData(minUS, motifLength, seqAl, seqCount, sort(unique(seqAl)))
    }
  }
  return(alCorData)
}

# Estimate parameters
mlePar <- function(exData, gtData, seqData, parLoci, sexMar, lenRU, mt_kitLoci, dye_kitLoci,
                   candAE, candHb, candBSR, candFSR, candDSR, candM2SR,
                   modelAE, modelHb, modelBSR, modelFSR, modelDSR, modelM2SR,
                   methBSR, methFSR, methDSR, methM2SR,
                   modelMultiBSR, modelMultiFSR, modelMultiDSR, modelMultiM2SR,
                   mleCondAE, mleCondHb, mleCondBSR, mleCondFSR, mleCondDSR, mleCondM2SR,
                   minAEMeth, minHbMeth, maxBSRMeth, maxFSRMeth, maxDSRMeth, maxM2SRMeth,
                   minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR
                   ){
  nL <- length(parLoci)

  srConsider <- matrix(0, nL, 4)
  srConsider[, 1] <- methBSR != "Not consider"
  srConsider[, 2] <- methFSR != "Not consider"
  srConsider[, 3] <- methDSR != "Not consider"
  srConsider[, 4] <- methM2SR != "Not consider"

  srLSp <- matrix(0, nL, 4)
  srLSp[, 1] <- methBSR == "Locus specific"
  srLSp[, 2] <- methFSR == "Locus specific"
  srLSp[, 3] <- methDSR == "Locus specific"
  srLSp[, 4] <- methM2SR == "Locus specific"

  arrangeData <- arrangeExData(exData, gtData, parLoci, sexMar, mt_kitLoci, dye_kitLoci, srConsider)
  exData2 <- arrangeData[[1]]
  gtData2 <- arrangeData[[2]]

  aeDataResult <- getAEData(exData2)
  aeData <- aeDataResult[[1]]
  aeHeightData <- aeDataResult[[2]]
  if(minAEMeth == 1){
    aeAll <- as.numeric(aeData)
    aeAll <- aeAll[which(aeAll > 0)]
    aeAll[which(aeAll > 1)] <- 1 / aeAll[which(aeAll > 1)]
    minAE <- min(aeAll)
  }

  hbDataResult <- getHbData(exData2, gtData2, srConsider)
  hbData <- hbDataResult[[1]]
  hbHeightData <- hbDataResult[[2]]
  if(minHbMeth == 1){
    hbAll <- as.numeric(hbData)
    hbAll <- hbAll[which(hbAll > 0)]
    hbAll[which(hbAll > 1)] <- 1 / hbAll[which(hbAll > 1)]
    minHb <- min(hbAll)
  }

  srDataResult <- getSRData(exData2, gtData2, srConsider)
  bsrData <- srDataResult[[1]]
  if(maxBSRMeth == 1){
    bsrAll <- as.numeric(bsrData)
    maxBSR <- max(bsrAll)
  }
  fsrData <- srDataResult[[2]]
  if(maxFSRMeth == 1){
    fsrAll <- as.numeric(fsrData)
    maxFSR <- max(fsrAll)
  }
  dsrData <- srDataResult[[3]]
  if(maxDSRMeth == 1){
    dsrAll <- as.numeric(dsrData)
    maxDSR <- max(dsrAll)
  }
  m2srData <- srDataResult[[4]]
  if(maxM2SRMeth == 1){
    m2srAll <- as.numeric(m2srData)
    maxM2SR <- max(m2srAll)
  }
  srAlleleData <- srDataResult[[5]]
  srHeightData <- srDataResult[[6]]

  resultSeq <- analyzeSeq(seqData, parLoci)
  lusData <- resultSeq[[1]]
  motifLengthList <- resultSeq[[2]]
  seqAlList <- resultSeq[[3]]
  seqCountList <- resultSeq[[4]]
  rm(exData, exData2, gtData, gtData2, arrangeData, aeDataResult, hbDataResult, srDataResult, resultSeq)

  cat("Estimation of parameters for locus-specific amplification efficiency was started.", "\n")
  aeUseData <- aeMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    posAE <- which(aeData[, i] != 0)
    ae <- aeData[posAE, i]
    aeHeight <- aeHeightData[posAE, i]
    posAdoptAE <- intersect(which(ae >= minAE), which(ae <= 1 / minAE))
    aeUseOneL <- cbind(ae[posAdoptAE], aeHeight[posAdoptAE])
    colnames(aeUseOneL) <- c("AE", "Height")
    aeUseData[[i]] <- aeUseOneL
    aeMleData[[i]] <- aeModeling(modelAE[i], ae, aeHeight, minAE, mleCondAE[mleCondAE[, "Marker"] == parLoci[i], , drop = FALSE])
    aeMleData[[i]][[length(aeMleData[[i]]) + 1]] <- "Locus specific"
  }
  names(aeUseData) <- names(aeMleData) <- parLoci
  cat("Estimation of parameters for locus-specific amplification efficiency was finished.", "\n")

  cat("Estimation of parameters for heterozygote balance was started.", "\n")
  hbUseData <- hbMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    posHb <- which(hbData[, i] != 0)
    hb <- hbData[posHb, i]
    hbHeight <- hbHeightData[posHb, i]
    posAdoptHb <- intersect(which(hb >= minHb), which(hb <= 1 / minHb))
    hbUseOneL <- cbind(hb[posAdoptHb], hbHeight[posAdoptHb])
    colnames(hbUseOneL) <- c("Hb", "Height")
    hbUseData[[i]] <- hbUseOneL
    hbMleData[[i]] <- hbModeling(modelHb[i], hb, hbHeight, minHb, mleCondHb[mleCondHb[, "Marker"] == parLoci[i], , drop = FALSE])
    hbMleData[[i]][[length(hbMleData[[i]]) + 1]] <- "Locus specific"
  }
  names(hbUseData) <- names(hbMleData) <- parLoci
  cat("Estimation of parameters for heterozygote balance was finished.", "\n")

  cat("Estimation of parameters for back stutter ratio was started.", "\n")
  options(warn = -1)
  posBsrGen <- which(apply(rbind(srConsider[, 1], !srLSp[, 1]), 2, all) == TRUE)
  options(warn = 0)
  if(length(posBsrGen) > 0){
    if(modelMultiBSR == "Best"){
      modelMultiBSR <- setdiff(candBSR[nrow(candBSR), grep("Cand", colnames(candBSR))], "Best")
    }
    bsrGenData <- makeSrGenData(posBsrGen, bsrData, srAlleleData, srHeightData, lusData, motifLengthList, seqAlList, seqCountList, maxBSR)
    bsrGenMleData <- srModeling(modelMultiBSR, bsrGenData[[1]], bsrGenData[[2]], bsrGenData[[3]], bsrGenData[[4]], bsrGenData[[5]], bsrGenData[[6]], bsrGenData[[7]], maxBSR, mleCondBSR[mleCondBSR[, "Marker"] == "Multiple loci together", , drop = FALSE])
  }else{
    bsrGenData <- integer(0)
  }
  bsrUseData <- bsrMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    modelBSR_oneL <- modelBSR[i]
    if(modelBSR_oneL == "Best"){
      modelBSR_oneL <- setdiff(candBSR[i, grep("Cand", colnames(candBSR))], "Best")
    }
    if(is.element(i, posBsrGen)){
      bsrUseData[[i]] <- bsrUseOneL <- makeSrUse(bsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxBSR)
      bsrMleData[[i]] <- bsrGenMleData
      bsrMleData[[i]][[length(bsrMleData[[i]]) + 1]] <- "Multiple loci together"
      posMultiseq <- which(modelBSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- bsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        bsrUseData[[i]][[length(bsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], bsrUseOneL[["parent allele"]])
      }else{
        bsrUseData[[i]][[length(bsrUseOneL) + 1]] <- integer(0)
      }
      names(bsrUseData[[i]])[length(bsrUseData[[i]])] <- "CA of parent allele"
    }else if(srConsider[i, 1]){
      bsrUseData[[i]] <- bsrUseOneL <- makeSrUse(bsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxBSR)
      bsrMleData[[i]] <- srModeling(modelBSR_oneL, bsrUseOneL[[1]], bsrUseOneL[[2]], bsrUseOneL[[3]], bsrUseOneL[[4]], motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], maxBSR, mleCondBSR[mleCondBSR[, "Marker"] == parLoci[i], , drop = FALSE])
      bsrMleData[[i]][[length(bsrMleData[[i]]) + 1]] <- "Locus specific"
      posMultiseq <- which(modelBSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- bsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        bsrUseData[[i]][[length(bsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], bsrUseOneL[["parent allele"]])
      }else{
        bsrUseData[[i]][[length(bsrUseOneL) + 1]] <- integer(0)
      }
      names(bsrUseData[[i]])[length(bsrUseData[[i]])] <- "CA of parent allele"
    }
  }
  names(bsrUseData) <- names(bsrMleData) <- parLoci
  cat("Estimation of parameters for back stutter ratio was finished.", "\n")

  cat("Estimation of parameters for forward stutter ratio was started.", "\n")
  options(warn = -1)
  posFsrGen <- which(apply(rbind(srConsider[, 2], !srLSp[, 2]), 2, all) == TRUE)
  options(warn = 0)
  if(length(posFsrGen) > 0){
    if(modelMultiFSR == "Best"){
      modelMultiFSR <- setdiff(candFSR[nrow(candFSR), grep("Cand", colnames(candFSR))], "Best")
    }
    fsrGenData <- makeSrGenData(posFsrGen, fsrData, srAlleleData, srHeightData, lusData, motifLengthList, seqAlList, seqCountList, maxFSR)
    fsrGenMleData <- srModeling(modelMultiFSR, fsrGenData[[1]], fsrGenData[[2]], fsrGenData[[3]], fsrGenData[[4]], fsrGenData[[5]], fsrGenData[[6]], fsrGenData[[7]], maxFSR, mleCondFSR[mleCondFSR[, "Marker"] == "Multiple loci together", , drop = FALSE])
  }else{
    fsrGenData <- integer(0)
  }
  fsrUseData <- fsrMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    modelFSR_oneL <- modelFSR[i]
    if(modelFSR_oneL == "Best"){
      modelFSR_oneL <- setdiff(candFSR[i, grep("Cand", colnames(candFSR))], "Best")
    }
    if(is.element(i, posFsrGen)){
      fsrUseData[[i]] <- fsrUseOneL <- makeSrUse(fsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxFSR)
      fsrMleData[[i]] <- fsrGenMleData
      fsrMleData[[i]][[length(fsrMleData[[i]]) + 1]] <- "Multiple loci together"
      posMultiseq <- which(modelFSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- fsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        fsrUseData[[i]][[length(fsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], fsrUseOneL[["parent allele"]])
      }else{
        fsrUseData[[i]][[length(fsrUseOneL) + 1]] <- integer(0)
      }
      names(fsrUseData[[i]])[length(fsrUseData[[i]])] <- "CA of parent allele"
    }else if(srConsider[i, 2]){
      fsrUseData[[i]] <- fsrUseOneL <- makeSrUse(fsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxFSR)
      fsrMleData[[i]] <- srModeling(modelFSR_oneL, fsrUseOneL[[1]], fsrUseOneL[[2]], fsrUseOneL[[3]], fsrUseOneL[[4]], motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], maxFSR, mleCondFSR[mleCondFSR[, "Marker"] == parLoci[i], , drop = FALSE])
      fsrMleData[[i]][[length(fsrMleData[[i]]) + 1]] <- "Locus specific"
      posMultiseq <- which(modelFSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- fsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        fsrUseData[[i]][[length(fsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], fsrUseOneL[["parent allele"]])
      }else{
        fsrUseData[[i]][[length(fsrUseOneL) + 1]] <- integer(0)
      }
      names(fsrUseData[[i]])[length(fsrUseData[[i]])] <- "CA of parent allele"
    }
  }
  names(fsrUseData) <- names(fsrMleData) <- parLoci
  cat("Estimation of parameters for forward stutter ratio was finished.", "\n")

  cat("Estimation of parameters for double-back stutter ratio was started.", "\n")
  options(warn = -1)
  posDsrGen <- which(apply(rbind(srConsider[, 3], !srLSp[, 3]), 2, all) == TRUE)
  options(warn = 0)
  if(length(posDsrGen) > 0){
    if(modelMultiDSR == "Best"){
      modelMultiDSR <- setdiff(candDSR[nrow(candDSR), grep("Cand", colnames(candDSR))], "Best")
    }
    dsrGenData <- makeSrGenData(posDsrGen, dsrData, srAlleleData, srHeightData, lusData, motifLengthList, seqAlList, seqCountList, maxDSR)
    dsrGenMleData <- srModeling(modelMultiDSR, dsrGenData[[1]], dsrGenData[[2]], dsrGenData[[3]], dsrGenData[[4]], dsrGenData[[5]], dsrGenData[[6]], dsrGenData[[7]], maxDSR, mleCondDSR[mleCondDSR[, "Marker"] == "Multiple loci together", , drop = FALSE])
  }else{
    dsrGenData <- integer(0)
  }
  dsrUseData <- dsrMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    modelDSR_oneL <- modelDSR[i]
    if(modelDSR_oneL == "Best"){
      modelDSR_oneL <- setdiff(candDSR[i, grep("Cand", colnames(candDSR))], "Best")
    }
    if(is.element(i, posDsrGen)){
      dsrUseData[[i]] <- dsrUseOneL <- makeSrUse(dsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxDSR)
      dsrMleData[[i]] <- dsrGenMleData
      dsrMleData[[i]][[length(dsrMleData[[i]]) + 1]] <- "Multiple loci together"
      posMultiseq <- which(modelDSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- dsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        dsrUseData[[i]][[length(dsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], dsrUseOneL[["parent allele"]])
      }else{
        dsrUseData[[i]][[length(dsrUseOneL) + 1]] <- integer(0)
      }
      names(dsrUseData[[i]])[length(dsrUseData[[i]])] <- "CA of parent allele"
    }else if(srConsider[i, 3]){
      dsrUseData[[i]] <- dsrUseOneL <- makeSrUse(dsrData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxDSR)
      dsrMleData[[i]] <- srModeling(modelDSR_oneL, dsrUseOneL[[1]], dsrUseOneL[[2]], dsrUseOneL[[3]], dsrUseOneL[[4]], motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], maxDSR, mleCondDSR[mleCondDSR[, "Marker"] == parLoci[i], , drop = FALSE])
      dsrMleData[[i]][[length(dsrMleData[[i]]) + 1]] <- "Locus specific"
      posMultiseq <- which(modelDSR_oneL == "Multi-seq")
      if(length(posMultiseq) == 1){
        minUS <- dsrMleData[[i]][[posMultiseq]][[2]]["X_value"]
        dsrUseData[[i]][[length(dsrUseOneL) + 1]] <- makeCAData(minUS, motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], dsrUseOneL[["parent allele"]])
      }else{
        dsrUseData[[i]][[length(dsrUseOneL) + 1]] <- integer(0)
      }
      names(dsrUseData[[i]])[length(dsrUseData[[i]])] <- "CA of parent allele"
    }
  }
  names(dsrUseData) <- names(dsrMleData) <- parLoci
  cat("Estimation of parameters for double-back stutter ratio was finished.", "\n")

  cat("Estimation of parameters for minus 2-nt stutter ratio was started.", "\n")
  options(warn = -1)
  posM2srGen <- which(apply(rbind(srConsider[, 4], !srLSp[, 4]), 2, all) == TRUE)
  options(warn = 0)
  if(length(posM2srGen) > 0){
    if(modelMultiM2SR == "Best"){
      modelMultiM2SR <- setdiff(candM2SR[nrow(candM2SR), grep("Cand", colnames(candM2SR))], "Best")
    }
    m2srGenData <- makeSrGenData(posM2srGen, m2srData, srAlleleData, srHeightData, lusData, motifLengthList, seqAlList, seqCountList, maxM2SR)
    m2srGenMleData <- srModeling(modelMultiM2SR, m2srGenData[[1]], m2srGenData[[2]], m2srGenData[[3]], m2srGenData[[4]], m2srGenData[[5]], m2srGenData[[6]], m2srGenData[[7]], maxM2SR, mleCondM2SR[mleCondM2SR[, "Marker"] == "Multiple loci together", , drop = FALSE])
  }else{
    m2srGenData <- integer(0)
  }
  m2srUseData <- m2srMleData <- mapply(rep, 1:nL, 0)
  for(i in 1:nL){
    modelM2SR_oneL <- modelM2SR[i]
    if(modelM2SR_oneL == "Best"){
      modelM2SR_oneL <- setdiff(candM2SR[i, grep("Cand", colnames(candM2SR))], "Best")
    }
    if(is.element(i, posM2srGen)){
      m2srUseData[[i]] <- m2srUseOneL <- makeSrUse(m2srData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxM2SR)
      m2srMleData[[i]] <- m2srGenMleData
      m2srMleData[[i]][[length(m2srMleData[[i]]) + 1]] <- "Multiple loci together"
      m2srUseData[[i]][[length(m2srUseOneL) + 1]] <- integer(0)
      names(m2srUseData[[i]])[length(m2srUseData[[i]])] <- "CA of parent allele"
    }else if(srConsider[i, 4]){
      m2srUseData[[i]] <- m2srUseOneL <- makeSrUse(m2srData[, i], srAlleleData[, i], srHeightData[, i], lusData[[i]], maxM2SR)
      m2srMleData[[i]] <- srModeling(modelM2SR_oneL, m2srUseOneL[[1]], m2srUseOneL[[2]], m2srUseOneL[[3]], m2srUseOneL[[4]], motifLengthList[[i]], seqAlList[[i]], seqCountList[[i]], maxM2SR, mleCondM2SR[mleCondM2SR[, "Marker"] == parLoci[i], , drop = FALSE])
      m2srMleData[[i]][[length(m2srMleData[[i]]) + 1]] <- "Locus specific"
      m2srUseData[[i]][[length(m2srUseOneL) + 1]] <- integer(0)
      names(m2srUseData[[i]])[length(m2srUseData[[i]])] <- "CA of parent allele"
    }
  }
  names(m2srUseData) <- names(m2srMleData) <- parLoci
  cat("Estimation of parameters for minus 2-nt stutter ratio was finished.", "\n")
  cat("Estimation of all parameters was finished.", "\n")

  exportAlCor <- makeExportAlCor(lusData, motifLengthList, seqAlList, seqCountList,
                                 bsrMleData, fsrMleData, dsrMleData)

  return(list(aeUseData, aeMleData,
              hbUseData, hbMleData,
              bsrGenData, bsrUseData, bsrMleData,
              fsrGenData, fsrUseData, fsrMleData,
              dsrGenData, dsrUseData, dsrMleData,
              m2srGenData, m2srUseData, m2srMleData,
              exportAlCor, motifLengthList, seqAlList, seqCountList,
              minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR
              ))
}
