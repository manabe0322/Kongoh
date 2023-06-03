# Correct repeat number of variant alleles (testthat)
corVariant <- function(peakOne, lenRUOneL){
  corVal <- round(peakOne %% 1 * 10, 0)
  peakCor <- peakOne %/% 1 + corVal / lenRUOneL
  return(peakCor)
}

# Make population frequencies (testthat)
makePopFreq <- function(afOneL, afAlOneL, afMeth, maf = numeric(0), unobsAl = numeric(0)){
  nUnobs <- length(unobsAl)
  if(nUnobs > 0){
    afAlOneL <- c(afAlOneL, unobsAl)
    if(afMeth == "Dirichlet"){
      afOneL <- c(afOneL, rep(0, nUnobs))
      afOneL <- (afOneL + 1) / (sum(afOneL) + length(afOneL))
    }else{
      afOneL <- c(afOneL, rep(maf, nUnobs))
    }
    afAlOneL2 <- afAlOneL[order(afAlOneL)]
    afOneL <- afOneL[order(afAlOneL)]
  }else{
    afAlOneL2 <- afAlOneL
    if(afMeth == "Dirichlet"){
      afOneL <- (afOneL + 1) / (sum(afOneL) + length(afOneL))
    }
  }
  names(afOneL) <- afAlOneL2
  return(afOneL)
}

# Estimate the size of a drop-out allele (testthat)
estSizeDrop <- function(dropOneAl, peakOneL, sizeOneL, lenRUOneL){
  peakOneLCor <- sapply(peakOneL, corVariant, lenRUOneL = lenRUOneL)
  dropOneAlCor <- corVariant(dropOneAl, lenRUOneL)
  distance <- abs(peakOneLCor - dropOneAlCor)
  posBasePeak <- which(distance == min(distance))
  basePeak <- peakOneLCor[posBasePeak[1]]
  baseSize <- sizeOneL[posBasePeak[1]]
  if(basePeak < dropOneAlCor){
    sizeDrop <- baseSize + lenRUOneL * min(distance)
  }else{
    sizeDrop <- baseSize - lenRUOneL * min(distance)
  }
  return(sizeDrop)
}

# Estimate size of a drop-out allele in the case of locus drop-out (testthat)
estSizeDrop2 <- function(dropOneAl, kitAl, kitSize, lenRUOneL){
  posDropAl <- which(kitAl == dropOneAl)
  if(length(posDropAl) == 1){
    return(kitSize[posDropAl])
  }else{
    kitAlCor <- sapply(kitAl, corVariant, lenRUOneL = lenRUOneL)
    dropOneAlCor <- corVariant(dropOneAl, lenRUOneL)
    distance <- abs(kitAlCor - dropOneAlCor)
    posBasePeak <- which(distance == min(distance))
    basePeak <- kitAlCor[posBasePeak[1]]
    baseSize <- kitSize[posBasePeak[1]]
    if(basePeak < dropOneAlCor){
      sizeDrop <- baseSize + lenRUOneL * min(distance)
    }else{
      sizeDrop <- baseSize - lenRUOneL * min(distance)
    }
    return(sizeDrop)
  }
}

# Add information of allele Q to peakOneL, sizeOneL, and heightOneL (testthat)
addQ <- function(peakOneL, sizeOneL, heightOneL, popFreq, lenRUOneL, kitInfoOneL){
  popAl <- as.numeric(names(popFreq))
  posAlQCand <- !is.element(popAl, peakOneL)
  alQCand <- popAl[posAlQCand]
  nQ <- length(alQCand)
  QFreq <- 0
  if(nQ > 0){
    alQOneL <- alQCand[which(popFreq[posAlQCand] == max(popFreq[posAlQCand]))[1]]
    if(length(peakOneL) > 0){
      sizeQ <- estSizeDrop(alQOneL, peakOneL, sizeOneL, lenRUOneL)
    }else{
      kitAl <- as.numeric(kitInfoOneL[, "Allele"])
      kitSize <- as.numeric(kitInfoOneL[, "Size"])
      sizeQ <- estSizeDrop2(alQOneL, kitAl, kitSize, lenRUOneL)
    }
    QFreq <- sum(popFreq[posAlQCand])
    peakOneL <- c(peakOneL, alQOneL)
    sizeOneL <- c(sizeOneL, sizeQ)
    heightOneL <- c(heightOneL, 0)
  }
  return(list(peakOneL, sizeOneL, heightOneL, QFreq))
}

# Search alleles without sequence information (test direct)
searchNoSeq <- function(peakOneL, alCorOneL, lociNameOne, srModelOneL, alQOneL){
  seqAlOneL <- as.numeric(alCorOneL[, grep("Allele", colnames(alCorOneL))])
  noSeqAl <- setdiff(peakOneL, seqAlOneL)
  nNoSeq <- length(noSeqAl)
  noSeqInfoOneL <- ""
  if(nNoSeq > 0){
    bsrSeqJudge <- is.element(srModelOneL[1], c("LUS", "Multi-seq"))
    fsrSeqJudge <- is.element(srModelOneL[2], c("LUS", "Multi-seq"))
    dsrSeqJudge <- is.element(srModelOneL[3], c("LUS", "Multi-seq"))
    if(any(c(bsrSeqJudge, fsrSeqJudge, dsrSeqJudge))){
      noSeqInfoOneL <- character(0)
      for(j in 1:nNoSeq){
        if(noSeqAl[j] == alQOneL){
          noSeqInfoOneL <- paste0(noSeqInfoOneL, lociNameOne, ": ", noSeqAl[j], " (representative of the unobserved alleles (Q))", "\n")
        }else{
          noSeqInfoOneL <- paste0(noSeqInfoOneL, lociNameOne, ": ", noSeqAl[j], "\n")
        }
      }
      noSeqInfoOneL <- substr(noSeqInfoOneL, 1, nchar(noSeqInfoOneL) - 1)
    }
  }
  return(noSeqInfoOneL)
}

# Alert a lack of information about allele repeat correction (test direct)
alertNoSeq <- function(noSeqInfo){
  noSeqMessage <- ""
  if(!all(is.element(noSeqInfo, ""))){
    noSeqInfo <- noSeqInfo[noSeqInfo != ""]
    for(i in 1:length(noSeqInfo)){
      noSeqMessage <- paste0(noSeqMessage, "\n", noSeqInfo[i])
    }
  }
  return(noSeqMessage)
}

# Make x-values for SR (testthat)
makeSrX <- function(peakOneL, modelName, srName = NULL, alCorOneL = NULL){
  if(modelName == "Allele"){
    srX <- peakOneL
  }else if(modelName == "LUS"){
    srX <- as.numeric(alCorOneL[which(as.numeric(alCorOneL[, 2]) %in% peakOneL), grep("LUS", colnames(alCorOneL))])
  }else if(modelName == "Multi-seq"){
    srX <- as.numeric(alCorOneL[which(as.numeric(alCorOneL[, 2]) %in% peakOneL), grep(srName, colnames(alCorOneL))])
  }else{
    srX <- numeric(0)
  }
  return(srX)
}

# Arrange input data for calculation
arrangeData <- function(csp, ref, af, kitInfo, atVals, lenRU, afMeth, maf, alCor = NULL, srModel = NULL){
  colCsp <- colnames(csp)

  lociName <- csp[, grep("Marker", colCsp)]

  peakAllL <- csp[, grep("Allele", colCsp)]
  peakAllL <- matrix(as.numeric(peakAllL), nrow = nrow(peakAllL))

  sizeAllL <- csp[, grep("Size", colCsp)]
  sizeAllL <- matrix(as.numeric(sizeAllL), nrow = nrow(sizeAllL))
  sizeVec <- as.numeric(sizeAllL)
  sizeVec <- sizeVec[!is.na(sizeVec)]

  heightAllL <- csp[, grep("Height", colCsp)]
  heightAllL <- matrix(as.numeric(heightAllL), nrow = nrow(heightAllL))
  heightAllL[which(is.na(heightAllL) == TRUE, arr.ind = TRUE)] <- 0
  heightVec <- as.numeric(heightAllL)
  heightVec <- heightVec[heightVec != 0]

  tempPerL <- apply(heightAllL, 1, sum)

  if(length(ref) != 0){
    colRef <- colnames(ref)
    refAllL <- ref[, - grep("Marker", colRef)]
    refAllL <- matrix(as.numeric(refAllL), nrow = nrow(refAllL))
    colnames(refAllL) <- colRef[ -grep("Marker", colRef)]
    rownames(refAllL) <- lociName
  }else{
    refAllL <- NULL
  }

  afAl <- af[, grep("Allele", colnames(af))]

  cspPeak <- cspSize <- cspHeight <- list()
  nL <- length(lociName)
  alQ <- QFreqAll <- rep(0, nL)
  popFreqList <- list()
  bsrPeak <- fsrPeak <- dsrPeak <- list()
  noSeqInfo <- rep("", nL)

  for(i in 1:nL){
    lociNameOne <- lociName[i]
    peakOneL <- peakAllL[i, !is.na(peakAllL[i, ])]
    sizeOneL <- sizeAllL[i, !is.na(sizeAllL[i, ])]
    heightOneL <- heightAllL[i, heightAllL[i, ] != 0]
    posPeak <- heightOneL >= atVals[i]
    peakOneL <- peakOneL[posPeak]
    sizeOneL <- sizeOneL[posPeak]
    heightOneL <- heightOneL[posPeak]

    if(length(refAllL) != 0){
      refOneL <- refAllL[i, ]
    }else{
      refOneL <- numeric(0)
    }

    afOneL <- af[, i + 1]
    posAfAl <- !is.na(afOneL)
    afAlOneL <- afAl[posAfAl]
    afOneL <- afOneL[posAfAl]
    unobsAl <- setdiff(unique(c(peakOneL, refOneL)), afAlOneL)

    popFreq <- makePopFreq(afOneL, afAlOneL, afMeth, maf, unobsAl)

    lenRUOneL <- lenRU[i]
    srModelOneL <- srModel[i, ]
    kitInfoOneL <- kitInfo[kitInfo[, "Marker"] == lociNameOne, , drop = FALSE]

    addQData <- addQ(peakOneL, sizeOneL, heightOneL, popFreq, lenRUOneL, kitInfoOneL)
    peakOneL <- addQData[[1]]
    sizeOneL <- addQData[[2]]
    heightOneL <- addQData[[3]]
    QFreqAll[i] <- addQData[[4]]
    alQ[i] <- peakOneL[length(peakOneL)]

    if(length(alCor) > 0){
      alCorOneL <- alCor[alCor[, grep("Marker", colnames(alCor))] == lociNameOne, ]
      noSeqInfo[i] <- searchNoSeq(peakOneL, alCorOneL, lociNameOne, srModelOneL, alQ[i])
    }else{
      alCorOneL <- NULL
    }

    if(noSeqInfo[i] == ""){
      bsrPeak[[i]] <- makeSrX(peakOneL, srModelOneL[1], "BSR", alCorOneL)
      fsrPeak[[i]] <- makeSrX(peakOneL, srModelOneL[2], "FSR", alCorOneL)
      dsrPeak[[i]] <- makeSrX(peakOneL, srModelOneL[3], "DSR", alCorOneL)
    }else{
      bsrPeak[[i]] <- fsrPeak[[i]] <- dsrPeak[[i]] <- numeric(0)
    }

    if(QFreqAll[i] > 0){
      peakOneL[length(peakOneL)] <- 99
    }

    cspPeak[[i]] <- peakOneL
    cspSize[[i]] <- sizeOneL
    cspHeight[[i]] <- heightOneL
    popFreqList[[i]] <- popFreq
  }
  names(cspPeak) <- names(cspSize) <- names(cspHeight) <- names(popFreqList) <- names(alQ) <- names(QFreqAll) <- lociName
  return(list(cspPeak, cspSize, cspHeight, refAllL, popFreqList, alQ, QFreqAll, tempPerL, bsrPeak, fsrPeak, dsrPeak, noSeqInfo))
}

# Set candidate mixture ratios (testthat)
setMR <- function(hnc, mrOne){
  if(hnc == 1){
    return(matrix(1, 1, 1))
  }else{
    mrDigit <- 5
    mrOne <- round(mrOne, mrDigit)
    mrOne <- mrOne[mrOne > 0]
    mrOne <- sort(unique(mrOne[mrOne < 0.5]))
    nMrOne <- length(mrOne)
    mrOutN <- combinations(n = nMrOne, r = hnc - 1, v = mrOne, repeats.allowed = TRUE)
    mrOutN <- mrOutN[which(apply(mrOutN, 1, sum) < 1), , drop = FALSE]
    mrN <- round(1 - apply(mrOutN, 1, sum), mrDigit)
    mr <- cbind(mrOutN, mrN)
    mrEqualOutN <- rep(round(1 / hnc, mrDigit), hnc - 1)
    mr <- rbind(mr, c(mrEqualOutN, 1 - sum(mrEqualOutN)))
    mr <- t(apply(mr, 1, sort))
    mrCutCount <- 1
    repeat{
      mrCutPos <- setdiff(which(apply(mr, 1, setequal, mr[mrCutCount, ]) == TRUE), mrCutCount)
      if(length(mrCutPos) != 0){
        mr <- mr[-mrCutPos, ]
      }
      mrCutCount <- mrCutCount + 1
      if(nrow(mr) < mrCutCount){
        break
      }
    }
    for(i in 1:(hnc - 1)){
      mr <- mr[is.element(mr[, i], c(mrOne, mrEqualOutN)), ]
    }
    return(mr)
  }
}

# Estimate candidate degradation parameters and the mean base length of PCR products (testthat)
makeDegOneC <- function(cspSize, cspHeight, tempPerL, degOne){
  lsDeg <- function(d){
    sum((exp(d * (sizeVec - sizeMean)) * tempMean / 2 - heightVec)^2)
  }

  sizeVec <- unlist(cspSize)
  heightVec <- unlist(cspHeight)
  sizeMean <- sum(sizeVec * heightVec) / sum(heightVec)
  tempMean <- mean(tempPerL)
  degError <- sapply(degOne, lsDeg)
  degOneC <- degOne[is.element(rank(degError), 1:3)]
  return(list(degOneC, sizeMean))
}

# Make detectable peaks in CSP under a genotype combination (testthat)
makeCandPeak <- function(gtCombOne, srConsiderOneL){
  candPeak <- gtCombOne
  if(srConsiderOneL[1]){
    candPeak <- c(candPeak, round(gtCombOne - 1, 1))
  }
  if(srConsiderOneL[2]){
    candPeak <- c(candPeak, round(gtCombOne + 1, 1))
  }
  if(srConsiderOneL[3]){
    candPeak <- c(candPeak, round(gtCombOne - 2, 1))
  }
  if(srConsiderOneL[4]){
    candPeak <- c(candPeak, round(gtCombOne[gtCombOne %% 1 < 0.15] - 0.8, 1), round(gtCombOne[gtCombOne %% 1 >= 0.15] - 0.2, 1))
  }
  return(sort(unique(candPeak)))
}

# Judge a genotype combination using the filters of each type of stutter ratio (testthat)
judgeGtCombSr <- function(gtCombOne, peakOneL, heightOneL, srFltr, srConsiderOneL){
  srJudge <- TRUE
  if(any(srConsiderOneL)){
    stutters <- setdiff(peakOneL[heightOneL != 0], gtCombOne)
    nSt <- length(stutters)
    if(nSt > 0){
      stHeight <- heightOneL[which(peakOneL %in% stutters)]
      stHeight <- sapply(stHeight, rep, 4)
      srFltrRep <- matrix(rep(srFltr, nSt), nrow = 4)
      alNum <- matrix(0, 4, nSt)
      alNum[1, ] <- round(stutters + 1, 1)
      alNum[2, ] <- round(stutters - 1, 1)
      alNum[3, ] <- round(stutters + 2, 1)
      alNum[4, stutters %% 1 < 0.15] <- round(stutters[stutters %% 1 < 0.15] + 0.2, 1)
      alNum[4, stutters %% 1 >= 0.15] <- round(stutters[stutters %% 1 >= 0.15] + 0.8, 1)
      alNum <- as.numeric(alNum)
      alHeight <- rep(0.01, 4 * nSt)
      alObsPos <- is.element(alNum, peakOneL)
      alHeight[alObsPos] <- heightOneL[match(alNum[alObsPos], peakOneL)]
      alHeight <- matrix(alHeight, nrow = 4)
      srJudgeMat <- stHeight / alHeight <= srFltrRep
      srJudgeMat <- srJudgeMat[srConsiderOneL, , drop = FALSE]
      if(!all(apply(srJudgeMat, 2, any))){
        srJudge <- FALSE
      }
    }
  }
  return(srJudge)
}

# Judge a genotype combination using the heterozygote balance filter in one contributor (testthat)
judgeGtCombHb <- function(gtCombOne, peakOneL, heightOneL, hbFltr, st){
  hbJudge <- TRUE
  hnc <- length(gtCombOne) / 2
  for(i in 1:hnc){
    gtOne <- gtCombOne[c(2 * i - 1, 2 * i)]
    if((!any(is.element(gtOne, gtCombOne[- c(2 * i - 1, 2 * i)]))) && (gtOne[1] != gtOne[2])){
      heights <- c(heightOneL[peakOneL == gtOne[1]], heightOneL[peakOneL == gtOne[2]])
      # Judge of Hb for one contributor
      if(all(heights >= st)){
        if((heights[1] / heights[2] < hbFltr) || (heights[1] / heights[2] > 1 / hbFltr)){
          hbJudge <- FALSE
          break
        }
      }else if(any(heights >= st) && any(heights == 0)){
        hbJudge <- FALSE
        break
      }
    }
  }
  return(hbJudge)
}

# Judge a genotype combination using the filter for mixture ratio of minor to major component (testthat)
judgeGtCombMr <- function(gtCombOne, peakOneL, heightOneL, mrFltr, st){
  mrJudge <- TRUE
  hnc <- length(gtCombOne) / 2
  uniqHeightAll <- rep(0, hnc)
  stPass <- rep(TRUE, hnc)
  for(i in 1:hnc){
    gtOne <- gtCombOne[c(2 * i - 1, 2 * i)]
    uniqPeak <- setdiff(gtOne, gtCombOne[- c(2 * i - 1, 2 * i)])
    if(length(uniqPeak) > 0){
      uniqHeight <- heightOneL[which(peakOneL %in% uniqPeak)]
      uniqHeightAll[i] <- mean(uniqHeight)
      stPass[i] <- all(uniqHeight >= st)
    }
  }
  posUniq <- which(uniqHeightAll != 0)
  nUniq <- length(posUniq)
  if(nUniq >= 2){
    uniqHeightAll <- uniqHeightAll[posUniq]
    stPass <- stPass[posUniq]
    for(i in 1:(nUniq - 1)){
      if(stPass[i]){
        uniqHeights <- uniqHeightAll[i:length(uniqHeightAll)]
        mrFalsePos <- which(uniqHeights[1] / uniqHeights[-1] >= mrFltr)
        if(length(mrFalsePos) > 0){
          mrJudge <- FALSE
          break
        }
      }
    }
  }
  return(mrJudge)
}

# Judge a genotype combination
judgeGtComb <- function(gtCombOne, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st){
  candPeak <- makeCandPeak(gtCombOne, srConsiderOneL)
  expJudge <- all(is.element(peakOneL[heightOneL != 0], candPeak))
  srJudge <- hbJudge <- mrJudge <- TRUE
  if(expJudge){
    srJudge <- judgeGtCombSr(gtCombOne, peakOneL, heightOneL, srFltr, srConsiderOneL)
    if(srJudge){
      hbJudge <- judgeGtCombHb(gtCombOne, peakOneL, heightOneL, hbFltr, st)
      if(hbJudge){
        mrJudge <- judgeGtCombMr(gtCombOne, peakOneL, heightOneL, mrFltr, st)
      }
    }
  }
  return(all(c(expJudge, hbJudge, mrJudge, srJudge)))
}

# Make genotype combinations (testthat)
makeGtComb <- function(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st){
  nAl <- length(peakOneL)
  gt <- combinations(n = nAl, r = 2, v = peakOneL, repeats.allowed = TRUE)
  nGt <- nrow(gt)
  gtCombID <- permutations(n = nGt, r = hnc, repeats.allowed = TRUE)
  nGtComb <- nrow(gtCombID)
  gtComb <- matrix(0, nGtComb, 2 * hnc)
  for(i in 1:hnc){
    gtComb[, c(2 * (i - 1) + 1, 2 * i)] <- gt[gtCombID[, i], ]
  }
  gtCombPassPos <- rep(0, nGtComb)
  for(i in 1:nGtComb){
    gtCombPassPos[i] <- judgeGtComb(gtComb[i, ], peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
  }
  gtCombPassPos <- which(gtCombPassPos == 1)
  if(length(gtCombPassPos) != 0){
    return(gtComb[gtCombPassPos, , drop = FALSE])
  }else{
    return(NULL)
  }
}

# Make conditions of mr, peak name, and d (testthat)
makeParCond <- function(mrOneC, peakOneL, degOneC){
  numMr <- length(mrOneC)
  numPeak <- length(peakOneL)
  numDeg <- length(degOneC)
  numCond <- numMr * numPeak * numDeg
  parCond <- matrix(0, 3, numCond)
  rownames(parCond) <- c("mixture ratio", "allele", "degradation")
  parCond[1, ] <- rep(mrOneC, numCond / numMr)
  parCond[2, ] <- rep(as.vector(sapply(peakOneL, rep, numMr)), numDeg)
  parCond[3, ] <- as.vector(sapply(degOneC, rep, numMr * numPeak))
  return(parCond)
}

# Cauculate Dal values (testthat)
calcDal <- function(d, sizeOneL, sizeMean){
  exp(d * (sizeOneL - sizeMean))
}

# Estimate expected peak heights (testthat)
estEPH <- function(peakOneL, sizeOneL, numMC, tempMean, mrOneC, degOneC, sizeMean, bsrPeakOneL, fsrPeakOneL, dsrPeakOneL, aeParOneL, hbParOneL, bsrParOneL, fsrParOneL, dsrParOneL, m2srParOneL, minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR){
  options(warn = -1)

  parCond <- makeParCond(mrOneC, peakOneL, degOneC)

  step2Height <- tempMean * mrOneC
  dalVal <- sapply(degOneC, calcDal, sizeOneL = sizeOneL, sizeMean = sizeMean)
  step4Height <- as.vector(matrix(step2Height / 2, ncol = 1) %*% matrix(dalVal, nrow = 1))

  nStep4 <- length(step4Height)
  step5Height <- matrix(0, numMC, nStep4)
  aeMean <- as.numeric(aeParOneL[1])
  aeVar <- as.numeric(aeParOneL[2])
  for(i in 1:nStep4){
    aeVal <- exp(rtruncnorm(numMC, a = log(minAE), b = log(1 / minAE), mean = log(aeMean), sd = sqrt(aeVar / step4Height[i])))
    step5Height[, i] <- aeVal * step4Height[i]
  }

  nStep5 <- ncol(step5Height)
  step6Height <- matrix(0, numMC, nStep5)
  hbMean <- as.numeric(hbParOneL[1])
  hbVar <- as.numeric(hbParOneL[2])
  for(i in 1:nStep5){
    hbVal <- exp(rtruncnorm(numMC, a = log(minHb), b = log(1 / minHb), mean = log(hbMean), sd = sqrt(hbVar / step5Height[, i])))
    step6Height[, i] <- 2 / (1 + hbVal) * step5Height[, i]
  }

  nMrOne <- length(mrOneC)
  nDegOne <- length(degOneC)

  nStep6 <- ncol(step6Height)
  bsrVal <- matrix(0, numMC, nStep6)
  if(length(bsrPeakOneL) != 0){
    bsrMean <- bsrParOneL[1] * bsrPeakOneL + bsrParOneL[2]
    bsrMean <- rep(as.vector(matrix(rep(bsrMean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
    posBsrAdopt <- which(bsrMean > 0)
    bsrVar <- bsrParOneL[3]
    for(i in 1:length(posBsrAdopt)){
      posBsrAdoptOne <- posBsrAdopt[i]
      bsrVal[, posBsrAdoptOne] <- exp(rtruncnorm(numMC, b = log(maxBSR), mean = log(bsrMean[posBsrAdoptOne]), sd = sqrt(bsrVar / step6Height[, posBsrAdoptOne])))
    }
  }

  fsrVal <- bsrVal * 0
  if(length(fsrPeakOneL) != 0){
    fsrMean <- fsrParOneL[1] * fsrPeakOneL + fsrParOneL[2]
    fsrMean <- rep(as.vector(matrix(rep(fsrMean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
    posFsrAdopt <- which(fsrMean > 0)
    fsrVar <- fsrParOneL[3]
    for(i in 1:length(posFsrAdopt)){
      posFsrAdoptOne <- posFsrAdopt[i]
      fsrVal[, posFsrAdoptOne] <- exp(rtruncnorm(numMC, b = log(maxFSR), mean = log(fsrMean[posFsrAdoptOne]), sd = sqrt(fsrVar / step6Height[, posFsrAdoptOne])))
    }
  }

  dsrVal <- bsrVal * 0
  if(length(dsrPeakOneL) != 0){
    dsrMean <- dsrParOneL[1] * dsrPeakOneL + dsrParOneL[2]
    dsrMean <- rep(as.vector(matrix(rep(dsrMean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
    posDsrAdopt <- which(dsrMean > 0)
    dsrVar <- dsrParOneL[3]
    for(i in 1:length(posDsrAdopt)){
      posDsrAdoptOne <- posDsrAdopt[i]
      dsrVal[, posDsrAdoptOne] <- exp(rtruncnorm(numMC, b = log(maxDSR), mean = log(dsrMean[posDsrAdoptOne]), sd = sqrt(dsrVar / step6Height[, posDsrAdoptOne])))
    }
  }

  m2sVal <- bsrVal * 0
  m2sMean <- as.numeric(m2srParOneL[1])
  m2sVar <- as.numeric(m2srParOneL[2])
  if((m2sMean > 0) && (m2sVar > 0)){
    for(i in 1:nStep6){
      m2sVal[, i] <- exp(rtruncnorm(numMC, b = log(maxM2SR), mean = log(m2sMean), sd = sqrt(m2sVar / step6Height[, i])))
    }
  }

  srSum <- 1 + bsrVal + fsrVal + dsrVal + m2sVal
  ephAl <- 1 / srSum * step6Height
  ephBs <- bsrVal / srSum * step6Height
  ephFs <- fsrVal / srSum * step6Height
  ephDs <- dsrVal / srSum * step6Height
  ephM2s <- m2sVal / srSum * step6Height

  options(warn = 0)

  return(list(ephAl, ephBs, ephFs, ephDs, ephM2s, parCond))
}

# Estimate parameters of gamma distribution (testthat)
estGamma <- function(eph){
  nCond <- ncol(eph)
  gammaParam <- matrix(0, 2, nCond)
  if(any(eph != 0)){
    m <- apply(eph, 2, mean)
    v <- apply(eph, 2, var)
    gammaParam[1, ] <- m^2 / v
    gammaParam[2, ] <- v / m
  }
  return(gammaParam)
}

#Sum gamma distributions of same allele repeat number
sumGamma <- function(gtCombOne, mr, deg, peakOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC){
  dropAl <- setdiff(gtCombOne, peakOneL)
  nDrop <- length(dropAl)

  nAl <- length(gtCombOne)
  nPeak <- length(peakOneL)
  nMrOneC <- length(mrOneC)
  nDegOneC <- length(degOneC)

  gammaSumPre1Al <- matrix(0, nAl, nPeak + nDrop)
  gammaSumPre1Bs <- gammaSumPre1Fs <- gammaSumPre1Ds <- gammaSumPre1M2s <- gammaSumPre2Al <- gammaSumPre2Bs <- gammaSumPre2Fs <- gammaSumPre2Ds <- gammaSumPre2M2s <- gammaSumPre1Al

  posPeakAl_origin <- match(gtCombOne, peakOneL)
  posPeakAl_origin[is.na(posPeakAl_origin)] <- which(peakOneL == 99)
  posPeakAl_add <- match(gtCombOne, c(peakOneL, dropAl))
  posMR <- rep(match(mr, mrOneC), times = 1, each = 2)
  posDeg <- rep(match(deg, degOneC), times = 1, each = 2)
  posCond <- nMrOneC * nPeak * (posDeg - 1) + nMrOneC * (posPeakAl_origin - 1) + posMR

  posGammaAl <- cbind(1:nAl, posPeakAl_add)
  p1 <- gammaAl[1, posCond]
  p2 <- gammaAl[2, posCond]
  gammaSumPre1Al[posGammaAl] <- p1 * p2
  gammaSumPre2Al[posGammaAl] <- p1 * (p2^2)
  gammaSumPre1Al[is.nan(gammaSumPre1Al)] <- 0
  gammaSumPre2Al[is.nan(gammaSumPre2Al)] <- 0

  posGammaSt <- matrix(0, nAl, 2)
  posGammaSt[, 1] <- 1:nAl

  posPeakBs <- match(round(gtCombOne - 1, 1), peakOneL)
  posGammaSt[, 2] <- posPeakBs
  posPeakBsObs <- !is.na(posPeakBs)
  posGammaBs <- posGammaSt[posPeakBsObs, , drop = FALSE]
  p1 <- gammaBs[1, posCond]
  p2 <- gammaBs[2, posCond]
  gammaSumPre1Bs[posGammaBs] <- (p1 * p2)[posPeakBsObs]
  gammaSumPre2Bs[posGammaBs] <- (p1 * (p2^2))[posPeakBsObs]
  gammaSumPre1Bs[is.nan(gammaSumPre1Bs)] <- 0
  gammaSumPre2Bs[is.nan(gammaSumPre2Bs)] <- 0

  posPeakFs <- match(round(gtCombOne + 1, 1), peakOneL)
  posGammaSt[, 2] <- posPeakFs
  posPeakFsObs <- !is.na(posPeakFs)
  posGammaFs <- posGammaSt[posPeakFsObs, , drop = FALSE]
  p1 <- gammaFs[1, posCond]
  p2 <- gammaFs[2, posCond]
  gammaSumPre1Fs[posGammaFs] <- (p1 * p2)[posPeakFsObs]
  gammaSumPre2Fs[posGammaFs] <- (p1 * (p2^2))[posPeakFsObs]
  gammaSumPre1Fs[is.nan(gammaSumPre1Fs)] <- 0
  gammaSumPre2Fs[is.nan(gammaSumPre2Fs)] <- 0

  posPeakDs <- match(round(gtCombOne - 2, 1), peakOneL)
  posGammaSt[, 2] <- posPeakDs
  posPeakDsObs <- !is.na(posPeakDs)
  posGammaDs <- posGammaSt[posPeakDsObs, , drop = FALSE]
  p1 <- gammaDs[1, posCond]
  p2 <- gammaDs[2, posCond]
  gammaSumPre1Ds[posGammaDs] <- (p1 * p2)[posPeakDsObs]
  gammaSumPre2Ds[posGammaDs] <- (p1 * (p2^2))[posPeakDsObs]
  gammaSumPre1Ds[is.nan(gammaSumPre1Ds)] <- 0
  gammaSumPre2Ds[is.nan(gammaSumPre2Ds)] <- 0

  posPeakM2s <- rep(0, nAl)
  posM2Al1 <- gtCombOne %% 1 < 0.15
  posM2Al2 <- gtCombOne %% 1 >= 0.15
  posPeakM2s[posM2Al1] <- match(round(gtCombOne[posM2Al1] - 0.8, 1), peakOneL)
  posPeakM2s[posM2Al2] <- match(round(gtCombOne[posM2Al2] - 0.2, 1), peakOneL)
  posGammaSt[, 2] <- posPeakM2s
  posPeakM2sObs <- !is.na(posPeakM2s)
  posGammaM2s <- posGammaSt[posPeakM2sObs, , drop = FALSE]
  p1 <- gammaM2s[1, posCond]
  p2 <- gammaM2s[2, posCond]
  gammaSumPre1M2s[posGammaM2s] <- (p1 * p2)[posPeakM2sObs]
  gammaSumPre2M2s[posGammaM2s] <- (p1 * (p2^2))[posPeakM2sObs]
  gammaSumPre1M2s[is.nan(gammaSumPre1M2s)] <- 0
  gammaSumPre2M2s[is.nan(gammaSumPre2M2s)] <- 0

  gammaSumPre1 <- apply(gammaSumPre1Al + gammaSumPre1Bs + gammaSumPre1Fs + gammaSumPre1Ds + gammaSumPre1M2s, 2, sum)
  gammaSumPre2 <- apply(gammaSumPre2Al + gammaSumPre2Bs + gammaSumPre2Fs + gammaSumPre2Ds + gammaSumPre2M2s, 2, sum)

  gammaSum <- matrix(0, 2, nPeak + nDrop)
  gammaSum[1, ] <- gammaSumPre1^2 / gammaSumPre2
  gammaSum[2, ] <- gammaSumPre2 / gammaSumPre1
  return(gammaSum)
}

# Compare observed peak heights with estimated gamma distribution
cfOphEph <- function(heightOneL, gammaSum, at){
  numH <- length(heightOneL)
  dens <- rep(0, numH)
  for(i in 1:numH){
    if(heightOneL[i] == 0){
      dens[i] <- sum(dgamma(1:(at - 1), shape = gammaSum[1, i], scale = gammaSum[2, i]))
    }else{
      dens[i] <- dgamma(heightOneL[i], shape = gammaSum[1, i], scale = gammaSum[2, i])
    }
  }
  return(prod(dens))
}

# Obtain density values of gamma distribution
densGamma <- function(gtCombOne, mrDegAll, peakOneL, heightOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC, at){
  hnc <- length(gtCombOne) / 2
  prodGtCombOne <- rep(0, nrow(mrDegAll))
  for(i in 1:nrow(mrDegAll)){
    mrDeg <- mrDegAll[i, ]
    mr <- mrDeg[1:hnc]
    deg <- mrDeg[(hnc + 1):(2 * hnc)]
    gammaSum <- sumGamma(gtCombOne, mr, deg, peakOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC)
    densPos <- which(!is.nan(gammaSum[1, ]) == TRUE)
    obsPos <- which(heightOneL != 0)
    if(length(setdiff(obsPos, densPos)) == 0){
      dens <- cfOphEph(heightOneL[densPos], gammaSum[, densPos, drop = FALSE], at)
      prodGtCombOne[i] <- dens
    }
  }
  return(prodGtCombOne)
}

# Calculate the match probability of a genotype (testthat)
calcMP <- function(gt, popFreq, theta, nObs, QFreq = 0){
  popAl <- as.numeric(names(popFreq))
  obsAl <- as.numeric(names(nObs))
  alProb <- rep(0, 2)
  for(i in 1:2){
    al <- gt[i]
    if(al == 99){
      af <- QFreq
    }else{
      af <- popFreq[popAl == al]
    }
    posAl <- obsAl == al
    nAl <- nObs[posAl]
    alProb[i] <- (nAl * theta + af * (1 - theta)) / (1 + (sum(nObs) - 1) * theta)
    nObs[posAl] <- nObs[posAl] + 1
  }
  if(gt[1] == gt[2]){
    return(prod(alProb))
  }else{
    return(2 * prod(alProb))
  }
}

# Calculate the probability of a genotype combination (testthat)
calcProbGtComb <- function(gtCombOne, hypIdOne, popFreq, theta, knownGt = numeric(0), QFreq = 0){
  hnc <- length(gtCombOne) / 2
  gtUniq <- unique(c(gtCombOne, knownGt))
  nObs <- rep(0, length(gtUniq))
  names(nObs) <- sort(gtUniq)
  nKnownAl <- length(knownGt)
  if(nKnownAl > 0){
    for(i in 1:nKnownAl){
      posKgt <- which(as.numeric(names(nObs)) == knownGt[i])
      nObs[posKgt] <- nObs[posKgt] + 1
    }
  }
  gtCombProb <- rep(0, hnc)
  for(i in 1:hnc){
    Id <- hypIdOne[i]
    gt <- gtCombOne[c(2 * i - 1, 2 * i)]
    if(Id != 0){
      if(setequal(gt, knownGt[c(2 * Id - 1, 2 * Id)])){
        gtCombProb[i] <- 1
      }
    }else{
      gtCombProb[i] <- calcMP(gt, popFreq, theta, nObs, QFreq)
      posGt <- which(is.element(as.numeric(names(nObs)), gt) == TRUE)
      if(length(posGt) == 1){
        nObs[posGt] <- nObs[posGt] + 2
      }else{
        nObs[posGt] <- nObs[posGt] + 1
      }
    }
  }
  return(prod(gtCombProb))
}

# Make all hypotheses (testthat)
makeHypAll <- function(nRef, hnc){
  personId <- 0:nRef
  hypIdComb <- combinations(n = length(personId), r = hnc, v = personId, repeats.allowed = TRUE)
  n1 <- nrow(hypIdComb)
  adoptPos <- numeric(0)
  for(i in 1:n1){
    hypIdOneCount <- table(hypIdComb[i, ])
    hypIdOneCount <- hypIdOneCount[names(hypIdOneCount) != 0]
    if((length(hypIdOneCount) == 0) || (all(is.element(hypIdOneCount, 1)))){
      adoptPos <- c(adoptPos, i)
    }
  }
  hypIdComb <- hypIdComb[adoptPos, , drop = FALSE]
  hypIdAll <- list()
  n1 <- nrow(hypIdComb)
  for(i in 1:n1){
    hypIdCombOne <- hypIdComb[i, hypIdComb[i, ] != 0]
    n2 <- length(hypIdCombOne)
    if(n2 > 0){
      kcOrder <- permutations(n = hnc, r = n2, repeats.allowed = FALSE)
      n3 <- nrow(kcOrder)
      hypId <- matrix(0, n3, hnc)
      for(j in 1:n3){
        hypId[j, kcOrder[j, ]] <- hypIdCombOne
      }
      hypIdAll[[i]] <- hypId
    }else{
      hypIdAll[[i]] <- matrix(0, 1, hnc)
    }
  }
  return(hypIdAll)
}

# Make a hypothesis
makeHypOne <- function(hnc, knownID = numeric(0)){
  if(length(knownID) > 0){
    posMat <- permutations(n = hnc, r = length(knownID), repeats.allowed = FALSE)
    nP <- nrow(posMat)
    hypMat <- matrix(0, nP, hnc)
    for(i in 1:nP){
      hypMat[i, posMat[i, ]] <- knownID
    }
  }else{
    hypMat <- matrix(0, 1, hnc)
  }
  return(hypMat)
}

# Make the best hypothesis
makeHypBest <- function(hypBestId, nameKnown){
  n1 <- length(hypBestId)
  hypBest <- rep(0, n1)
  for(i in 1:n1){
    if(hypBestId[i] == 0){
      hypBest[i] <- "U"
    }else{
      hypBest[i] <- nameKnown[hypBestId[i]]
    }
  }
  return(hypBest)
}

# Add drop-out alleles of reference profiles to gtComb
addRefDrop <- function(dropAl, pgResult, hypBestId, refOneL){
  namePGR <- colnames(pgResult)
  gtComb <- pgResult[, !is.element(namePGR, c("Weight (0-1 scale)", "Weight", "Genotype prob")), drop = FALSE]
  product <- pgResult[, "Weight"]
  gtProb <- pgResult[, "Genotype prob"]

  hnc <- ncol(gtComb) / 2
  nAl <- length(dropAl)
  dropComb1 <- combinations(n = nAl + 1, r = 1, v = c(dropAl, 99), repeats.allowed = TRUE)
  dropComb2 <- combinations(n = nAl + 1, r = 2, v = c(dropAl, 99), repeats.allowed = TRUE)
  pos99 <- which(gtComb == 99, arr.ind = TRUE)
  posRow99 <- sort(unique(pos99[, 1]))
  if((length(posRow99) > 0) && (!any(is.element(dropAl, gtComb)))){
    gtCombAdd <- gtComb[-posRow99, , drop = FALSE]
    productAdd <- product[-posRow99]
    gtProbAdd <- gtProb[-posRow99]
    overlapProbAdd <- rep(FALSE, length(gtProbAdd))
    for(i in 1:length(posRow99)){
      gts <- gtComb[posRow99[i], ]
      gtNewList <- list()
      prodOne <- product[posRow99[i]]
      gtProbOne <- gtProb[posRow99[i]]
      for(j in 1:hnc){
        gt <- gts[c(2 * j - 1, 2 * j)]
        n99 <- length(which(gt == 99))
        if(n99 == 1){
          gtNew <- cbind(dropComb1, rep(gt[which(gt != 99)], nrow(dropComb1)))
          gtNewList[[j]] <- t(apply(gtNew, 1, sort))
        }else if(n99 == 2){
          gtNewList[[j]] <- dropComb2
        }else{
          gtNewList[[j]] <- matrix(gt, nrow = 1)
        }
      }
      nGtNew <- sapply(gtNewList, nrow)
      posGtNewList <- list()
      for(j in 1:hnc){
        posGtNewList[[j]] <- 1:nGtNew[j]
      }
      posGtNew <- expand.grid(posGtNewList)
      posGtNew <- as.matrix(posGtNew)
      n <- nrow(posGtNew)
      gtCombNew <- matrix(0, n, 2 * hnc)
      productNew <- rep(0, n)
      gtProbNew <- rep(gtProbOne, n)
      overlapProbNew <- rep(FALSE, n)
      for(j in 1:n){
        pos <- posGtNew[j, ]
        for(k in 1:hnc){
          gtCombNew[j, c(2 * k - 1, 2 * k)] <- gtNewList[[k]][pos[k], ]
        }
        productNew[j] <- prodOne
      }
      
      posKgt <- 1:nrow(gtCombNew)
      for(j in 1:hnc){
        hbi <- hypBestId[j]
        if(hbi != 0){
          kgt1 <- refOneL[c(2 * hbi - 1, 2 * hbi)]
          posKgt1 <- intersect(which(gtCombNew[, 2 * j - 1] == kgt1[1]), which(gtCombNew[, 2 * j] == kgt1[2]))
          posKgt <- intersect(posKgt, posKgt1)
        }
      }
      gtCombNew <- gtCombNew[posKgt, , drop = FALSE]
      productNew <- productNew[posKgt]
      gtProbNew <- gtProbNew[posKgt]
      if(length(posKgt) >= 2){
        overlapProbNew <- rep(TRUE, length(posKgt))
      }else{
        overlapProbNew <- FALSE
      }
      
      gtCombAdd <- rbind(gtCombAdd, gtCombNew)
      productAdd <- c(productAdd, productNew)
      gtProbAdd <- c(gtProbAdd, gtProbNew)
      overlapProbAdd <- c(overlapProbAdd, overlapProbNew)
    }
  }else{
    gtCombAdd <- gtComb
    productAdd <- product
    gtProbAdd <- gtProb
    overlapProbAdd <- rep(FALSE, length(gtProbAdd))
  }
  weightAdd <- productAdd / sum(productAdd)
  pgResultAdd <- cbind(gtCombAdd, weightAdd, productAdd, gtProbAdd, overlapProbAdd)
  colnames(pgResultAdd) <- c(namePGR, "Overlap prob")
  return(pgResultAdd)
}

# Make a list of results
makeResult <- function(hypIdAll, nameKnown, mrDegAll, gammaList, gtCombList, productsList, gtProbList, log10LikeList, overallLike, lociName, calcTime, cspPeak, cspHeight, lenRU, dropAlList, refAllL, addRefDropFunc){
  resultList <- list()
  nL <- length(lociName)
  countHyp <- 0
  for(i in 1:length(hypIdAll)){
    hypIdOneAll <- hypIdAll[[i]]
    maxLikeCand <- apply(overallLike[countHyp + 1:nrow(hypIdOneAll), , drop = FALSE], 1, max)
    maxLike <- max(maxLikeCand)
    posHypBestId <- which(maxLikeCand == maxLike)[1]
    hypBestId <- hypIdOneAll[posHypBestId, ]
    hypBest <- makeHypBest(hypBestId, nameKnown)
    mrDegBestId <- which(overallLike[countHyp + posHypBestId, ] == maxLike)[1]
    mrDegBest <- mrDegAll[mrDegBestId, ]
    mrBest <- mrDegBest[1:(length(mrDegBest) / 2)]
    degBest <- mrDegBest[(length(mrDegBest) / 2 + 1):(length(mrDegBest))]
    names(mrBest) <- names(degBest) <- hypBest
    pgResultList <- list()
    likeBest <- rep(0, nL + 1)
    for(j in 1:nL){
      gtComb <- gtCombList[[j]]
      productsBest <- productsList[[j]][, mrDegBestId]
      gtProbBest <- gtProbList[[j]][countHyp + posHypBestId, ]
      likeEachComb <- productsBest * gtProbBest
      posCand <- likeEachComb != 0
      pgResult <- cbind(gtComb[posCand, , drop = FALSE], productsBest[posCand] / sum(productsBest[posCand]), productsBest[posCand], gtProbBest[posCand])
      colnames(pgResult) <- c(as.vector(sapply(hypBest, rep, 2)), "Weight (0-1 scale)", "Weight", "Genotype prob")

      dropAl <- dropAlList[[j]]
      refOneL <- refAllL[j, ]
      if(addRefDropFunc && (length(dropAl) > 0)){
        pgResult <- addRefDrop(dropAl, pgResult, hypBestId, refOneL)
      }else{
        namePGR <- colnames(pgResult)
        pgResult <- cbind(pgResult, rep(FALSE, nrow(pgResult)))
        colnames(pgResult) <- c(namePGR, "Overlap prob")
      }

      pgResultList[[j]] <- pgResult[order(pgResult[, ncol(pgResult) - 2], decreasing = TRUE), , drop = FALSE]
      likeBest[j] <- log10LikeList[[j]][countHyp + posHypBestId, mrDegBestId]
    }
    likeBest[nL + 1] <- maxLike
    names(likeBest) <- c(lociName, "total")
    names(pgResultList) <- lociName
    resultOne <- list()
    resultOne[[1]] <- hypBest
    resultOne[[2]] <- likeBest
    resultOne[[3]] <- pgResultList
    resultOne[[4]] <- mrBest
    resultOne[[5]] <- degBest
    resultOne[[6]] <- gammaList
    resultOne[[7]] <- calcTime
    resultOne[[8]] <- list(cspPeak, cspHeight, lenRU)
    names(resultOne) <- c("hypothesis", "likelihood", "probabilistic genotyping", "mixture ratio", "degradation", "gamma parameters", "calculation time", "CSP information")
    resultList[[i]] <- resultOne
    countHyp <- countHyp + nrow(hypIdOneAll)
  }
  return(resultList)
}

# Make a list of results for inappropriate hypothesis
makeResultNA <- function(hypIdAll, nameKnown, lociName, calcTime, cspPeak, cspHeight, lenRU){
  resultList <- list()
  nL <- length(lociName)
  for(i in 1:length(hypIdAll)){
    hypIdOneAll <- hypIdAll[[i]][1, ]
    hypBest <- makeHypBest(hypIdOneAll, nameKnown)
    likeBest <- rep(NA, nL + 1)
    names(likeBest) <- c(lociName, "total")
    mrBest <- degBest <- rep(NA, length(hypBest))
    names(mrBest) <- names(degBest) <- hypBest
    resultOne <- list()
    resultOne[[1]] <- hypBest
    resultOne[[2]] <- likeBest
    resultOne[[3]] <- numeric(0)
    resultOne[[4]] <- mrBest
    resultOne[[5]] <- degBest
    resultOne[[6]] <- numeric(0)
    resultOne[[7]] <- calcTime
    resultOne[[8]] <- list(cspPeak, cspHeight, lenRU)
    names(resultOne) <- c("hypothesis", "likelihood", "probabilistic genotyping", "mixture ratio", "degradation", "gamma parameters", "calculation time", "CSP information")
    resultList[[i]] <- resultOne
  }
  return(resultList)
}

# Analyze a crime stain profile
analyzeCSP <- function(csp, ref, af,
                       selectMethData, mcPar, alCor, kitInfo,
                       hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                       anaType = "LR", baseDeconvo = 0, dataDeconvo = NULL, calcAllHyp = 0, addRefDropFunc = TRUE,
                       mrDegCut){
  lociName <- csp[, grep("Marker", colnames(csp))]

  conditions <- selectMethData[, which(colnames(selectMethData) == "Conditions")]
  values <- selectMethData[, which(colnames(selectMethData) == "Values")]
  kit <- values[which(conditions == "Kit")]
  namePar <- values[which(conditions == "Parameters for Monte Carlo simulations")]
  nameAlCor <- values[which(conditions == "Allele repeat correction")]
  afMeth <- values[which(conditions == "Method of estimating allele frequencies")]
  maf <- as.numeric(values[which(conditions == "Minimum allele frequency")])
  theta <- as.numeric(values[which(conditions == "Theta")])
  numMC <- as.numeric(values[which(conditions == "Number of Monte Carlo simulations")])
  atVals <- as.numeric(values[grep("Analytical threshold", conditions)])
  lociAtPre <- strsplit(conditions[grep("Analytical threshold", conditions)], "_")
  nAt <- length(atVals)
  lociAt <- rep("", nAt)
  for(i in 1:nAt){
    lociAt[i] <- lociAtPre[[i]][2]
  }
  names(atVals) <- lociAt
  mrOne <- as.numeric(values[which(conditions == "Mixture proportion")])
  degOne <- as.numeric(values[which(conditions == "Degradation parameter")])
  st <- as.numeric(values[which(conditions == "Stochastic threshold")])
  mrFltr <- as.numeric(values[which(conditions == "Filter of mixture ratio")])
  hbFltr <- as.numeric(values[which(conditions == "Filter of heterozygote balance")])
  bsrFltr <- as.numeric(values[which(conditions == "Filter of back stutter ratio")])
  fsrFltr <- as.numeric(values[which(conditions == "Filter of forward stutter ratio")])
  dsrFltr <- as.numeric(values[which(conditions == "Filter of double-back stutter ratio")])
  m2srFltr <- as.numeric(values[which(conditions == "Filter of minus 2-nt stutter ratio")])
  srFltr <- c(bsrFltr, fsrFltr, dsrFltr, m2srFltr)

  aePar <- mcPar[mcPar[, "Biological_parameter"] == "AE", , drop = FALSE]
  aeLoci <- aePar[, "Marker"]
  aePos <- match(lociName, aeLoci)
  aePar <- aePar[aePos, ]
  aeParVal <- aePar[, c("Mean", "Variance"), drop = FALSE]
  aeParVal <- matrix(as.numeric(aeParVal), nrow = nrow(aeParVal))
  minAEAll <- as.numeric(aePar[, "Min"])

  hbPar <- mcPar[mcPar[, "Biological_parameter"] == "Hb", , drop = FALSE]
  hbLoci <- hbPar[, "Marker"]
  hbPos <- match(lociName, hbLoci)
  hbPar <- hbPar[hbPos, ]
  hbParVal <- hbPar[, c("Mean", "Variance"), drop = FALSE]
  hbParVal <- matrix(as.numeric(hbParVal), nrow = nrow(hbParVal))
  minHbAll <- as.numeric(hbPar[, "Min"])

  bsrPar <- mcPar[mcPar[, "Biological_parameter"] == "BSR", , drop = FALSE]
  bsrLoci <- bsrPar[, "Marker"]
  bsrPos <- match(lociName, bsrLoci)
  bsrPar <- bsrPar[bsrPos, ]
  bsrModel <- bsrPar[, "Model"]
  bsrParVal <- bsrPar[, c("Slope", "Intercept", "Variance"), drop = FALSE]
  bsrParVal <- matrix(as.numeric(bsrParVal), nrow = nrow(bsrParVal))
  bsrParVal[which(is.na(bsrParVal) == TRUE, arr.ind = TRUE)] <- 0
  maxBSRAll <- as.numeric(bsrPar[, "Max"])

  fsrPar <- mcPar[mcPar[, "Biological_parameter"] == "FSR", , drop = FALSE]
  fsrLoci <- fsrPar[, "Marker"]
  fsrPos <- match(lociName, fsrLoci)
  fsrPar <- fsrPar[fsrPos, ]
  fsrModel <- fsrPar[, "Model"]
  fsrParVal <- fsrPar[, c("Slope", "Intercept", "Variance"), drop = FALSE]
  fsrParVal <- matrix(as.numeric(fsrParVal), nrow = nrow(fsrParVal))
  fsrParVal[which(is.na(fsrParVal) == TRUE, arr.ind = TRUE)] <- 0
  maxFSRAll <- as.numeric(fsrPar[, "Max"])

  dsrPar <- mcPar[mcPar[, "Biological_parameter"] == "DSR", , drop = FALSE]
  dsrLoci <- dsrPar[, "Marker"]
  dsrPos <- match(lociName, dsrLoci)
  dsrPar <- dsrPar[dsrPos, ]
  dsrModel <- dsrPar[, "Model"]
  dsrParVal <- dsrPar[, c("Slope", "Intercept", "Variance"), drop = FALSE]
  dsrParVal <- matrix(as.numeric(dsrParVal), nrow = nrow(dsrParVal))
  dsrParVal[which(is.na(dsrParVal) == TRUE, arr.ind = TRUE)] <- 0
  maxDSRAll <- as.numeric(dsrPar[, "Max"])

  m2srPar <- mcPar[mcPar[, "Biological_parameter"] == "M2SR", , drop = FALSE]
  m2srLoci <- m2srPar[, "Marker"]
  m2srPos <- match(lociName, m2srLoci)
  m2srPar <- m2srPar[m2srPos, ]
  m2srModel <- m2srPar[, "Model"]
  m2srParVal <- m2srPar[, c("Mean", "Variance"), drop = FALSE]
  m2srParVal <- matrix(as.numeric(m2srParVal), nrow = nrow(m2srParVal))
  m2srParVal[which(is.na(m2srParVal) == TRUE, arr.ind = TRUE)] <- 0
  maxM2SRAll <- as.numeric(m2srPar[, "Max"])

  rownames(aeParVal) <- rownames(hbParVal) <- rownames(bsrParVal) <- rownames(fsrParVal) <- rownames(dsrParVal) <- rownames(m2srParVal) <- lociName

  srModel <- cbind(bsrModel, fsrModel, dsrModel, m2srModel)
  colnames(srModel) <- c("bsrModel", "fsrModel", "dsrModel", "m2srModel")
  rownames(srModel) <- lociName

  kitLoci <- unique(kitInfo[, "Marker"])
  nL <- length(kitLoci)
  lenRU <- rep(0, nL)
  for(i in 1:nL){
    posL <- which(kitInfo[, "Marker"] == kitLoci[i])[1]
    lenRU[i] <- kitInfo[posL, "Length_of_the_repeat_unit"]
  }
  lenRU <- lenRU[match(lociName, kitLoci)]
  lenRU <- as.numeric(lenRU)
  names(lenRU) <- lociName

  atOrder <- rep(0, nAt)
  for(i in 1:nAt){
    atOrder[i] <- atVals[which(lociAt == lociName[i])]
  }
  atVals <- atOrder
  names(atVals) <- lociName

  dataAll <- arrangeData(csp, ref, af, kitInfo, atVals, lenRU, afMeth, maf, alCor, srModel)
  cspPeak <- dataAll[[1]]
  cspSize <- dataAll[[2]]
  cspHeight <- dataAll[[3]]
  refAllL <- dataAll[[4]]
  popFreqList <- dataAll[[5]]
  alQ <- dataAll[[6]]
  QFreqAll <- dataAll[[7]]
  tempPerL <- dataAll[[8]]
  bsrPeak <- dataAll[[9]]
  fsrPeak <- dataAll[[10]]
  dsrPeak <- dataAll[[11]]
  noSeqInfo <- dataAll[[12]]

  noSeqMessage <- alertNoSeq(noSeqInfo)
  if(noSeqMessage == ""){
    lociName <- names(cspPeak)
    nL <- length(lociName)

    if(anaType == "Deconvo"){
      nRef <- 0
      nameKnown <- character(0)
      refOneL <- numeric(0)
    }else if(anaType == "LR"){
      nRef <- ncol(refAllL) / 2
      nameKnown <- colnames(refAllL)[1:(nRef * 2) %% 2 == 1]
    }

    tempMean <- mean(tempPerL)
    degData <- makeDegOneC(cspSize, cspHeight, tempPerL, degOne)
    degOneC <- degData[[1]]
    sizeMean <- degData[[2]]

    resultAllHnc <- list()
    countHnc <- 0
    errorHnc <- FALSE
    if(baseDeconvo == 0){
      dev.new(width = 6, height = 6, noRStudioGD = TRUE)
      plot(c(0, 100), c(0, 2 * (hncTo - hncFrom + 1) + 2), type = "n", xlab = "", ylab = "", axes = FALSE, mar = c(3, 3, 3, 3))
      for(i in hncFrom:hncTo){
        posBar <- 2 * (hncTo - i) + 1
        rect(0, posBar, 100, posBar + 1, col = "white")
        text(25, posBar + 1.5, paste0("Hypothesis : ", i, "-person contribution"))
      }
      par(xpd = TRUE)
      text(50, 2 * (hncTo - hncFrom + 1) + 2, "Progress Bar", cex = 1.25)

      cl <- makeCluster(4, type = "PSOCK")
      for(i in hncFrom:hncTo){
        t <- proc.time()
        countHnc <- countHnc + 1
        hnc <- i
        if((calcAllHyp == 0) && (hnc < max(c(length(knownHp), length(knownHd))))){
          errorHnc <- TRUE
          break
        }else{
          cat(paste0("Deconvolution of CSP (", hnc, "-person contribution) was started."), "\n")
          posBar <- 2 * (hncTo - hnc) + 1

          mrAll <- setMR(hnc, mrOne)
          degAll <- matrix(rep(degOneC, hnc), nrow = length(degOneC))
          mrDegAll <- cbind(matrix(as.numeric(apply(mrAll, 1, rep, nrow(degAll))), ncol = hnc, byrow = TRUE), matrix(rep(as.numeric(t(degAll)), nrow(mrAll)), ncol = hnc, byrow = TRUE))
          nMD <- nrow(mrDegAll)
          mrDegID <- 1:nMD
          mrOneC <- sort(unique(as.vector(mrAll)))

          if(anaType == "Deconvo"){
            hypIdAll <- list()
            hypIdAll[[1]] <- makeHypOne(hnc)
          }else if(anaType == "LR"){
            if(calcAllHyp == 0){
              hypIdAll <- list()
              hypIdAll[[1]] <- makeHypOne(hnc, knownHp)
              hypIdAll[[2]] <- makeHypOne(hnc, knownHd)
            }else{
              hypIdAll <- makeHypAll(nRef, hnc)
            }
          }
          nH <- sum(sapply(hypIdAll, nrow))

          gammaList <- gtCombList <- productsList <- log10LikeList <- gtProbList <- dropAlList <- list()
          overallLike <- matrix(0, nH, nMD)
          impossible <- FALSE
          for(j in 1:nL){
            peakOneL <- cspPeak[[j]]
            sizeOneL <- cspSize[[j]]
            heightOneL <- cspHeight[[j]]
            srConsiderOneL <- !is.element(srModel[j, ], "No")
            if(anaType == "LR"){
              refOneL <- refAllL[j, ]
            }
            bsrPeakOneL <- bsrPeak[[j]]
            fsrPeakOneL <- fsrPeak[[j]]
            dsrPeakOneL <- dsrPeak[[j]]
            aeParOneL <- aeParVal[j, ]
            hbParOneL <- hbParVal[j, ]
            bsrParOneL <- bsrParVal[j, ]
            fsrParOneL <- fsrParVal[j, ]
            dsrParOneL <- dsrParVal[j, ]
            m2srParOneL <- m2srParVal[j, ]
            lenRUOneL <- lenRU[j]
            popFreq <- popFreqList[[j]]
            QFreq <- QFreqAll[j]
            at <- atVals[j]
            minAE <- minAEAll[j]
            minHb <- minHbAll[j]
            maxBSR <- maxBSRAll[j]
            maxFSR <- maxFSRAll[j]
            maxDSR <- maxDSRAll[j]
            maxM2SR <- maxM2SRAll[j]

            gtCombList[[j]] <- gtComb <- makeGtComb(hnc, peakOneL, heightOneL, mrFltr, hbFltr, srFltr, srConsiderOneL, st)
            if(length(gtComb) == 0){
              impossible <- TRUE
              break
            }else{
              ephData <- estEPH(peakOneL, sizeOneL, numMC, tempMean, mrOneC, degOneC, sizeMean, bsrPeakOneL, fsrPeakOneL, dsrPeakOneL, aeParOneL, hbParOneL, bsrParOneL, fsrParOneL, dsrParOneL, m2srParOneL, minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR)
              gammaAl <- estGamma(ephData[[1]])
              gammaBs <- estGamma(ephData[[2]])
              gammaFs <- estGamma(ephData[[3]])
              gammaDs <- estGamma(ephData[[4]])
              gammaM2s <- estGamma(ephData[[5]])
              parCond <- ephData[[6]]
              gammaList[[j]] <- list(gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, parCond)

              nGC <- nrow(gtComb)
              products <- matrix(0, nGC, nMD)
              if(hnc >= 3){
                productsOneL <- parRapply(cl, gtComb, densGamma, mrDegAll[mrDegID, , drop = FALSE], peakOneL, heightOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC, at)
                products[, mrDegID] <- matrix(productsOneL, nrow = nGC, byrow = TRUE)
              }else{
                for(k in 1:nGC){
                  products[k, mrDegID] <- densGamma(gtComb[k, ], mrDegAll[mrDegID, , drop = FALSE], peakOneL, heightOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC, at)
                }
              }
              productsList[[j]] <- products

              posDrop <- !is.element(refOneL, peakOneL)
              dropAlList[[j]] <- sort(unique(refOneL[posDrop]))
              refOneL[posDrop] <- 99

              nGC <- nrow(gtComb)
              gtProb <- matrix(0, nH, nGC)
              log10Like <- mrDegAdopt <- matrix(0, nH, nMD)
              countHyp <- 0
              nHI1 <- length(hypIdAll)
              for(k in 1:nHI1){
                hypIdOneAll <- hypIdAll[[k]]
                nHI2 <- nrow(hypIdOneAll)
                for(l in 1:nHI2){
                  countHyp <- countHyp + 1
                  hypIdOne <- hypIdOneAll[l, ]
                  if(hnc >= 3){
                    gtProb[countHyp, ] <- parRapply(cl, gtComb, calcProbGtComb, hypIdOne, popFreq, theta, refOneL, QFreq)
                  }else{
                    for(m in 1:nGC){
                      gtProb[countHyp, m] <- calcProbGtComb(gtComb[m, ], hypIdOne, popFreq, theta, refOneL, QFreq)
                    }
                  }
                  log10LikeOne <- log10(matrix(gtProb[countHyp, ], nrow = 1) %*% products)
                  log10Like[countHyp, ] <- log10LikeOne
                }
                posLike <- (countHyp - nHI2 + 1):countHyp
                log10LikeOne2 <- as.numeric(log10Like[posLike, ])
                posMrDegAdopt <- intersect(which(log10LikeOne2 != -Inf), which(log10LikeOne2 >= max(log10LikeOne2) + log10(mrDegCut)))
                if(length(posMrDegAdopt) > 0){
                  mrDegAdopt2 <- rep(0, length(log10LikeOne2))
                  mrDegAdopt2[posMrDegAdopt] <- 1
                  mrDegAdopt[posLike, ] <- matrix(mrDegAdopt2, ncol = nMD)
                }
              }
              mrDegID <- which(apply(mrDegAdopt, 2, sum) != 0)
              if(length(mrDegID) == 0){
                impossible <- TRUE
                break
              }
              log10LikeList[[j]] <- log10Like
              overallLike <- overallLike + log10Like
              gtProbList[[j]] <- gtProb
              rect(0, posBar, 100 * j / nL, posBar + 1, col = "greenyellow")
            }
          }
          timeOneHnc <- proc.time() - t
          calcTime <- timeOneHnc[3]
          if(impossible){
            resultAllHnc[[countHnc]] <- makeResultNA(hypIdAll, nameKnown, lociName, calcTime, cspPeak, cspHeight, lenRU)
            rect(0, posBar, 100, posBar + 1, col = "greenyellow")
          }else{
            resultAllHnc[[countHnc]] <- makeResult(hypIdAll, nameKnown, mrDegAll, gammaList, gtCombList, productsList, gtProbList, log10LikeList, overallLike, lociName, calcTime, cspPeak, cspHeight, lenRU, dropAlList, refAllL, addRefDropFunc)
          }
          cat(paste0("Time of Deconvolution (", hnc, "-person contribution) : ", round(calcTime, 3), " sec."), "\n")
        }
      }
      stopCluster(cl)
      dev.off()
    }else{
      cl <- makeCluster(4, type = "PSOCK")
      t2 <- proc.time()
      for(i in hncFrom:hncTo){
        hnc <- i
        if((calcAllHyp == 0) && (hnc < max(c(length(knownHp), length(knownHd))))){
          errorHnc <- TRUE
          break
        }else{
          countHnc <- countHnc + 1
          dataDeconvoOne <- dataDeconvo[[countHnc]][[1]]
          pgDeconvo <- dataDeconvoOne[[3]]
          calcTime <- dataDeconvoOne[[7]]

          if(calcAllHyp == 0){
            hypIdAll <- list()
            hypIdAll[[1]] <- makeHypOne(hnc, knownHp)
            hypIdAll[[2]] <- makeHypOne(hnc, knownHd)
          }else{
            hypIdAll <- makeHypAll(nRef, hnc)
          }
          nH <- sum(sapply(hypIdAll, nrow))

          if(length(pgDeconvo) == 0){
            resultOneHnc <- makeResultNA(hypIdAll, nameKnown, lociName, calcTime, cspPeak, cspHeight, lenRU)
          }else{
            gtCombList <- productsList <- log10LikeList <- gtProbList <- dropAlList <- list()
            overallLike <- matrix(0, nH, 1)
            for(j in 1:nL){
              popFreq <- popFreqList[[j]]
              QFreq <- QFreqAll[j]

              pgOneL <- pgDeconvo[[j]]
              colPg <- colnames(pgOneL)
              gtCombList[[j]] <- gtComb <- pgOneL[, colPg == "U", drop = FALSE]
              productsList[[j]] <- products <- pgOneL[, colPg == "Weight", drop = FALSE]

              refOneL <- refAllL[j, ]
              peakOneL <- cspPeak[[j]]
              posDrop <- !is.element(refOneL, peakOneL)
              dropAlList[[j]] <- sort(unique(refOneL[posDrop]))
              refOneL[posDrop] <- 99

              nGC <- nrow(gtComb)
              gtProb <- matrix(0, nH, nGC)
              log10Like <- matrix(0, nH, 1)
              countHyp <- 0
              nHI1 <- length(hypIdAll)
              for(k in 1:nHI1){
                hypIdOneAll <- hypIdAll[[k]]
                nHI2 <- nrow(hypIdOneAll)
                for(l in 1:nHI2){
                  countHyp <- countHyp + 1
                  hypIdOne <- hypIdOneAll[l, ]
                  if(hnc >= 3){
                    gtProb[countHyp, ] <- parRapply(cl, gtComb, calcProbGtComb, hypIdOne, popFreq, theta, refOneL, QFreq)
                  }else{
                    for(m in 1:nGC){
                      gtProb[countHyp, m] <- calcProbGtComb(gtComb[m, ], hypIdOne, popFreq, theta, refOneL, QFreq)
                    }
                  }
                  log10LikeOne <- log10(matrix(gtProb[countHyp, ], nrow = 1) %*% matrix(products, ncol = 1))
                  log10Like[countHyp, ] <- log10LikeOne
                }
              }
              log10LikeList[[j]] <- log10Like
              overallLike <- overallLike + log10Like
              gtProbList[[j]] <- gtProb
            }

            mrDeg <- matrix(c(dataDeconvoOne[[4]], dataDeconvoOne[[5]]), nrow = 1)
            gammaList <- dataDeconvoOne[[6]]
            calcTime <- dataDeconvoOne[[7]]
            resultOneHnc <- makeResult(hypIdAll, nameKnown, mrDeg, gammaList, gtCombList, productsList, gtProbList, log10LikeList, overallLike, lociName, calcTime, cspPeak, cspHeight, lenRU, dropAlList, refAllL, addRefDropFunc)
          }
          resultAllHnc[[i]] <- resultOneHnc
        }
      }
      ct2 <- proc.time() - t2
      cat(paste0("Time of LR assignment : ", round(ct2[3], 3), " sec."), "\n")
      stopCluster(cl)
    }
    if(errorHnc){
      errorMessage <- "Error: The total number of contributors is smaller than the number of known contributors."
      cat(errorMessage)
      return(errorMessage)
    }else{
      names(resultAllHnc) <- paste0("hnc_", hncFrom:hncTo)
      return(resultAllHnc)
    }
  }else{
    errorMessage <- paste0("There is a lack of information about allele repeat correction of the following allele(s): ", "\n", noSeqMessage, "\n")
    cat(errorMessage)
    return(errorMessage)
  }
}
