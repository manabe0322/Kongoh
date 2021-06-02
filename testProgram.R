#Make genotype combinations
gtCombMake <- function(peakOneL, heightOneL, hnc, srConsiderOneL, hbFltr, stFltr, st){
  
  #Make possible peak of observation in CSP under a hypothesized genotype combination
  possiblePeakMake <- function(gtCombOne, srConsiderOneL){
    possiblePeak <- gtCombOne
    if(srConsiderOneL[1]){
      possiblePeak <- c(possiblePeak, round(gtCombOne - 1, 1))
    }
    if(srConsiderOneL[2]){
      possiblePeak <- c(possiblePeak, round(gtCombOne + 1, 1))
    }
    if(srConsiderOneL[3]){
      possiblePeak <- c(possiblePeak, round(gtCombOne - 2, 1))
    }
    if(srConsiderOneL[4]){
      possiblePeak <- c(possiblePeak, round(gtCombOne[gtCombOne %% 1 < 0.15] - 0.8, 1), round(gtCombOne[gtCombOne %% 1 >= 0.15] - 0.2, 1))
    }
    return(sort(unique(possiblePeak)))
  }
  
  #Judge a genotype combination
  gtCombJudge <- function(gtCombOne, srConsiderOneL, peakOneL, heightOneL, hbFltr, stFltr, st){
    possiblePeak <- possiblePeakMake(gtCombOne, srConsiderOneL)
    peakObs <- peakOneL[heightOneL != 0]
    expJudge <- all(is.element(peakObs, possiblePeak))
    hbJudge <- TRUE
    stJudge <- TRUE
    
    #Memo: whether a genotype combination can explain all observed peaks in CSP
    if(expJudge){
      
      #Memo: exclude unrealistic genotype combination based on stutter filters
      stutters <- setdiff(peakObs, gtCombOne)
      nSt <- length(stutters)
      if(nSt > 0){
        stHeight <- heightOneL[which(peakOneL %in% stutters)]
        stHeight <- sapply(stHeight, rep, 4)
        stFltrRep <- matrix(rep(stFltr, nSt), nrow = 4)
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
        stJudgeMat <- stHeight / alHeight <= stFltrRep
        stJudgeMat <- stJudgeMat[srConsiderOneL, , drop = FALSE]
        if(!all(apply(stJudgeMat, 2, any))){
          stJudge <- FALSE
        }
      }
      
      #Memo: exclude unrealistic genotype combination based on stochastic threshold and heterozygote balance
      if(stJudge){
        notShareHeightAll <- numeric(0)
        hnc <- (length(gtCombOne) / 2)
        for(i in 1:hnc){
          gtOne <- gtCombOne[c(2 * i - 1, 2 * i)]
          if((!any(is.element(gtOne, gtCombOne[- c(2 * i - 1, 2 * i)]))) && (gtOne[1] != gtOne[2])){
            heights <- c(heightOneL[peakOneL == gtOne[1]], heightOneL[peakOneL == gtOne[2]])
            #Memo: judge of Hb for one contributor
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
          notShare <- setdiff(gtOne, gtCombOne[- c(2 * i - 1, 2 * i)])
          if(length(notShare) > 0){
            notShareHeight <- heightOneL[which(peakOneL %in% notShare)]
            notShareHeightAll <- c(notShareHeightAll, sum(notShareHeight) / 2)
          }
        }
        nNotShare <- length(notShareHeightAll)
        if(hbJudge && (nNotShare >= 2)){
          for(i in 1:(nNotShare - 1)){
            #Memo: judge of Hb between each contributor
            notShareHeight2 <- notShareHeightAll[i:length(notShareHeightAll)]
            hbFalsePos <- which(notShareHeight2[1] / notShareHeight2[-1] >= 1 / hbFltr)
            if((length(hbFalsePos) > 0) && (notShareHeight2[1] >= st)){
              hbJudge <- FALSE
              break
            }
          }
        }
      }
    }
    return(all(c(expJudge, hbJudge, stJudge)))
  }
  
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
    gtCombPassPos[i] <- gtCombJudge(gtComb[i, ], srConsiderOneL, peakOneL, heightOneL, hbFltr, stFltr, st)
  }
  gtCombPassPos <- which(gtCombPassPos == 1)
  if(length(gtCombPassPos) != 0){
    return(gtComb[gtCombPassPos, , drop = FALSE])
  }else{
    return(NULL)
  }
}

gtCombCut <- function(gtComb, peakOneL, heightOneL){
  hnc <- ncol(gtComb) / 2
  sortH <- sort(heightOneL, decreasing = TRUE)
  judgeMat <- matrix(TRUE, nrow(gtComb), hnc - 1)
  if(hnc >= 2){
    for(i in 1:(hnc - 1)){
      gt1p <- gtComb[, 2 * hnc - c(2 * (i - 1) + 1, 2 * (i - 1))]
      candAl <- peakOneL[which(heightOneL %in% sortH[1:(2 * i)])]
      judge1 <- apply(gt1p, 1, is.element, candAl)
      judgeMat[, hnc - i] <- apply(judge1, 2, all)
    }
    gtCombRough <- gtComb[apply(judgeMat, 1, all), , drop = FALSE]
  }else{
    gtCombRough <- gtComb
  }
  return(gtCombRough)
}

gtCombCut2 <- function(gtComb, peakOneL, heightOneL){
  hnc <- ncol(gtComb) / 2
  sortH <- sort(heightOneL, decreasing = TRUE)
  judgeMat <- matrix(TRUE, nrow(gtComb), hnc)
  for(i in 1:hnc){
    gt1p <- gtComb[, 2 * hnc - c(2 * (i - 1) + 1, 2 * (i - 1))]
    candAl <- peakOneL[which(heightOneL %in% sortH[1:(2 * i)])]
    judge1 <- apply(gt1p, 1, is.element, candAl)
    judgeMat[, hnc - i + 1] <- apply(judge1, 2, all)
  }
  gtCombRough <- gtComb[apply(judgeMat, 1, all), , drop = FALSE]
  return(gtCombRough)
}

library(gtools)
peakOneL <- c(15:19, 99)
heightOneL <- c(1800, 262, 1494, 2067, 2035, 0)
hnc <- 2
srConsiderOneL <- c(TRUE, TRUE, TRUE, FALSE)
hbFltr <- 0.25
stFltr <- c(0.7, 0.35, 0.13, 0.12)
st <- 500
gtCombTest2 <- gtCombMake(peakOneL, heightOneL, hnc, srConsiderOneL, hbFltr, stFltr, st)
gtCombRoughTest2 <- gtCombCut(gtCombTest2, peakOneL, heightOneL)
gtCombRoughTest2_2 <- gtCombCut2(gtCombTest2, peakOneL, heightOneL)

hnc <- 3
gtCombTest3 <- gtCombMake(peakOneL, heightOneL, hnc, srConsiderOneL, hbFltr, stFltr, st)
gtCombRoughTest3 <- gtCombCut(gtCombTest3, peakOneL, heightOneL)
gtCombRoughTest3_2 <- gtCombCut2(gtCombTest3, peakOneL, heightOneL)

hnc <- 4
gtCombTest4 <- gtCombMake(peakOneL, heightOneL, hnc, srConsiderOneL, hbFltr, stFltr, st)
gtCombRoughTest4 <- gtCombCut(gtCombTest4, peakOneL, heightOneL)
gtCombRoughTest4_2 <- gtCombCut2(gtCombTest4, peakOneL, heightOneL)
