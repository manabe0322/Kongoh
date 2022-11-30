# Search kits
searchKit <- function(loci, pathPack){
  kitNames <- list.files(paste0(pathPack, "/extdata/kit"))
  kitNames <- gsub(".csv", "", kitNames)
  kit <- character(0)
  for(i in 1:length(kitNames)){
    kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", kitNames[i], ".csv"), header = TRUE)
    kitInfo <- as.matrix(kitInfo)
    kitLoci <- unique(kitInfo[, "Marker"])
    kitLoci <- gsub(" ", "", kitLoci, fixed = TRUE)
    if(setequal(kitLoci, loci)){
      kit <- c(kit, kitNames[i])
    }
  }
  return(kit)
}

# Check input files
checkFile <- function(envGUI, cspInput, refInput, afInput, meth){
  cspLoci <- cspInput[, grep("Marker", colnames(cspInput))]

  pathPack <- get("pathPack", pos = envGUI)
  exKit <- searchKit(cspLoci, pathPack)

  if(length(exKit) > 0){
    kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", exKit[1], ".csv"), header = TRUE)
    kitInfo <- as.matrix(kitInfo)
    posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
    if(length(posSexMar) > 0){
      sexMar <- unique(kitInfo[posSexMar, "Marker"])
      posCsp <- !is.element(cspLoci, sexMar)
      csp <- cspInput[posCsp, , drop = FALSE]
      cspLoci <- cspLoci[posCsp]
    }else{
      sexMar <- character(0)
    }

    if(meth == "LR"){
      refLoci <- refInput[, grep("Marker", colnames(refInput))]
      posRef <- !is.element(refLoci, sexMar)
      ref <- refInput[posRef, , drop = FALSE]
      refLoci <- refLoci[posRef]
    }else{
      ref <- NULL
      refLoci <- cspLoci
    }

    posAf <- !is.element(colnames(afInput), sexMar)
    af <- afInput[, posAf, drop = FALSE]
    afLoci <- colnames(af)[posAf]
    afLoci <- afLoci[- grep("Allele", colnames(af))]

    if(all(c(setequal(cspLoci, refLoci), setequal(cspLoci, afLoci)))){
      if(meth == "LR"){
        posRef <- match(cspLoci, refLoci)
        ref <- ref[posRef, , drop = FALSE]
      }

      posAf <- match(cspLoci, afLoci)
      af <- af[, c(1, posAf + 1), drop = FALSE]

      return(list(csp, ref, af))
    }else{
      tkmessageBox(message = "Autosomal loci of each file was not the same!", icon = "error", type = "ok")
    }
  }else{
    tkmessageBox(message = "Locus set of the crime stain profile has not been registered. Please register locus set at Tools -> Typing kit.", icon = "error", type = "ok")
  }
}

# GUI for analyzing CSP
guiAnalyze <- function(anaType, envProj, envGUI,
                       hncFrom, hncTo,
                       anaMeth, baseDeconvo = 0, calcAllHyp = 0,
                       knownHp = numeric(0), knownHd = numeric(0)){
  csp <- get("csp", pos = envProj)
  ref <- get("ref", pos = envProj)
  af <- get("af", pos = envProj)
  dataDeconvo <- get("dataDeconvo", pos = envProj)
  assign("baseDeconvo", baseDeconvo, envir = envProj)
  assign("calcAllHyp", calcAllHyp, envir = envProj)
  pathPack <- get("pathPack", pos = envGUI)
  if(anaType == "Deconvo"){
    hncMin <- get("hncMin_deconvo", pos = envProj)
    hncMax <- get("hncMax_deconvo", pos = envProj)
    assign("hncFrom_deconvo", hncFrom, envir = envProj)
    assign("hncTo_deconvo", hncTo, envir = envProj)
    assign("anaMeth_deconvo", anaMeth, envir = envProj)
  }else if(anaType == "LR"){
    hncMin <- get("hncMin_lr", pos = envProj)
    hncMax <- get("hncMax_lr", pos = envProj)
    assign("hncFrom_lr", hncFrom, envir = envProj)
    assign("hncTo_lr", hncTo, envir = envProj)
    assign("anaMeth_lr", anaMeth, envir = envProj)
    if(calcAllHyp == 1){
      knownHp <- numeric(0)
      knownHd <- numeric(0)
      assign("nameKnownHp", character(0), envir = envProj)
      assign("nameKnownHd", character(0), envir = envProj)
    }else{
      nRef <- (ncol(ref) - 1) / 2
      nameKnown <- colnames(ref)[1:(nRef * 2 + 1) %% 2 == 0]
      assign("nameKnownHp", nameKnown[knownHp], envir = envProj)
      assign("nameKnownHd", nameKnown[knownHd], envir = envProj)
    }
  }

  if(anaMeth == ""){
    tkmessageBox(message = "Select an analysis method!", icon = "error", type = "ok")
  }else if(!is.element(hncFrom, hncMin:hncMax) || !is.element(hncTo, hncMin:hncMax)){
    tkmessageBox(message = paste0("Set the number of contributors from ", hncMin, " to ", hncMax), icon = "error", type = "ok")
  }else{
    selectMethData <- read.csv(paste(pathPack, "/extdata/analysis_method/", anaMeth, ".csv", sep = ""), header = TRUE)
    selectMethData <- as.matrix(selectMethData)
    conditions <- selectMethData[, which(colnames(selectMethData) == "Conditions")]
    values <- selectMethData[, which(colnames(selectMethData) == "Values")]
    kit <- values[which(conditions == "Kit")]
    namePar <- values[which(conditions == "Parameters for Monte Carlo simulations")]
    nameAlCor <- values[which(conditions == "Allele repeat correction")]
    afMeth <- values[which(conditions == "Method of estimating allele frequencies")]
    
    lociAtPre <- strsplit(conditions[grep("Analytical threshold", conditions)], "_")
    for(i in 1:length(lociAtPre)){
      lociAtPre[[i]][2] <- gsub(" ", "", lociAtPre[[i]][2], fixed = TRUE)
    }
    selectMethData[grep("Analytical threshold", conditions), which(colnames(selectMethData) == "Conditions")] <- sapply(lociAtPre, paste0, collapse = "_")
    
    mcPar <- read.csv(paste(pathPack, "/extdata/parameters/", namePar, sep = ""), header = TRUE)
    mcPar <- as.matrix(mcPar)
    mcPar <- gsub(" ", "", mcPar, fixed = TRUE)
    alCor <- read.csv(paste(pathPack, "/extdata/repeat_correction/", nameAlCor, sep = ""), header = TRUE)
    alCor <- as.matrix(alCor)
    alCor <- gsub(" ", "", alCor, fixed = TRUE)

    kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", kit, ".csv"), header = TRUE)
    kitInfo <- as.matrix(kitInfo)
    kitInfo[, "Marker"] <- gsub(" ", "", kitInfo[, "Marker"], fixed = TRUE)
    kitLoci <- unique(kitInfo[, "Marker"])
    posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
    if(length(posSexMar) > 0){
      sexMar <- unique(kitInfo[posSexMar, "Marker"])
      kitLoci <- setdiff(kitLoci, sexMar)
    }

    #Check locus set
    cspLoci <- csp[, grep("Marker", colnames(csp))]
    if(setequal(cspLoci, kitLoci)){
      assign("cspLoci", cspLoci, envir = envProj)
      #Check allele frequencies
      afSum <- apply(af[, -1], 2, sum, na.rm = TRUE)
      afVal <- as.numeric(af[, -1])
      afVal <- afVal[!is.na(afVal)]
      inputOk <- "ok"
      if(afMeth == "Dirichlet" && length(which(afVal %% 1 != 0)) != 0){
        inputOk <- "error"
        tkmessageBox(message = "Allele frequencies must be represented as allele counts when the probabilities of each allele are determined by the Dirichlet distribution!", type = "ok", icon = "error")
      }else if(afMeth == "No correction" && any(afSum != 1)){
        inputOk <- tclvalue(tkmessageBox(message = "Sum of allele frequencies is not equal to one. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(inputOk == "ok"){
        result <- analyzeCSP(csp, ref, af,
                             selectMethData, mcPar, alCor, kitInfo,
                             hncFrom, hncTo, knownHp, knownHd,
                             anaType, baseDeconvo, dataDeconvo, calcAllHyp, TRUE,
                             0.0001)
        if(!is.list(result)){
          errorMessage <- result
          tkmessageBox(message = errorMessage, icon = "error", type = "ok")
        }else{
          if(anaType == "Deconvo"){
            assign("finDeconvo", TRUE, envir = envProj)
            assign("dataDeconvo", result, envir = envProj)
            makeSubTabResult_Deconvo(envProj, envGUI)
          }else if(anaType == "LR"){
            assign("finLR", TRUE, envir = envProj)
            assign("dataLR", result, envir = envProj)
            makeSubTabResult_LR(envProj, envGUI)
          }
        }
      }
    }else{
      tkmessageBox(message = "Locus set of the selected analysis method was not the same as that of input files!", icon = "error", type = "ok")
    }
  }
}

# Make table of PG
tablePG <- function(pg, anaType){
  n1 <- ncol(pg)
  gt <- pg[, 1:(n1 - 3), drop = FALSE]
  nC <- ncol(gt) / 2
  tableGt <- matrix(0, nrow(gt), nC)
  nameC <- rep("", nC)
  countU <- 0
  for(i in 1:nC){
    for(j in 1:nrow(gt)){
      al_1 <- gt[j, 2 * i - 1]
      if(al_1 == 99){
        al_1 <- "Q"
      }
      al_2 <- gt[j, 2 * i]
      if(al_2 == 99){
        al_2 <- "Q"
      }
      tableGt[j, i] <- paste0(al_1, ", ", al_2)
    }
    nameCOne <- colnames(gt)[2 * i - 1]
    if((nameCOne == "U") && (anaType == "Deconvo")){
      countU <- countU + 1
      nameCOne <- paste0("U", countU)
    }
    nameC[i] <- nameCOne
  }
  pgNew <- cbind(tableGt, sprintf("%.2e", pg[, "Weight (0-1 scale)"]))
  colnames(pgNew) <- c(nameC, "Weight (0-1 scale)")
  return(pgNew)
}

# Make a graph of observed peaks vs estimated gamma distributions
graphPG <- function(anaType, envProj, envName, dataResult, selectHnc, selectHyp, selectL, cspLoci){
  if(anaType == "Deconvo"){
    mlbPG <- get("mlbPG", pos = envName)
    hnc <- as.numeric(tclvalue(selectHnc))
    hyp <- rep("U", hnc)
  }else if(anaType == "LR"){
    mlbPG <- get("mlbPG", pos = envName)
    hyp <- tclvalue(selectHyp)
    hyp <- strsplit(hyp, ",")[[1]]
    hnc <- length(hyp)
  }
  if(tclvalue(tkcurselection(mlbPG)) == ""){
    tkmessageBox(message = "Select one genotype combination!", icon = "error", type = "ok")
  }else{
    hncAll <- as.numeric(gsub("hnc_", "", names(dataResult)))
    posHnc <- which(hncAll == hnc)
    dataOneHnc <- dataResult[[posHnc]]
    for(i in 1:length(dataOneHnc)){
      dataOneHyp <- dataOneHnc[[i]]
      if(setequal(dataOneHyp[[1]], hyp)){
        break
      }
    }
    posL <- which(cspLoci == tclvalue(selectL))
    gtComb <- dataOneHyp[[3]][[posL]]
    gtCombOne <- gtComb[as.numeric(tclvalue(tkcurselection(mlbPG))) + 1, 1:(2 * hnc)]

    mr <- dataOneHyp[[4]]
    deg <- dataOneHyp[[5]]

    dataGamma <- dataOneHyp[[6]][[posL]]
    gammaAl <- dataGamma[[1]]
    gammaBs <- dataGamma[[2]]
    gammaFs <- dataGamma[[3]]
    gammaDs <- dataGamma[[4]]
    gammaM2s <- dataGamma[[5]]
    parCond <- dataGamma[[6]]
    mrOneC <- sort(unique(parCond[1, ]))
    degOneC <- sort(unique(parCond[3, ]))

    dataCspInfo <- dataOneHyp[[8]]
    cspPeak <- dataCspInfo[[1]]
    peakOneL <- cspPeak[[posL]]
    cspHeight <- dataCspInfo[[2]]
    heightOneL <- cspHeight[[posL]]
    lenRUOneL <- dataCspInfo[[3]][posL]

    gammaSum <- sumGamma(gtCombOne, mr, deg, peakOneL, gammaAl, gammaBs, gammaFs, gammaDs, gammaM2s, mrOneC, degOneC)
    posDens <- !is.nan(gammaSum[1, ])
    gammaSumGraph <- gammaSum[, posDens, drop = FALSE]

    peakOneLAdd <- c(peakOneL, setdiff(gtCombOne, peakOneL))
    peakOneLGraph <- peakOneLGraph2 <- peakOneLAdd[posDens]
    if(any(is.element(peakOneLGraph, 99))){
      peakOneLGraph[which(peakOneLGraph == 99)] <- max(peakOneLGraph[which(peakOneLGraph != 99)]) + 2
    }
    heightOneLGraph <- heightOneL[posDens]

    dev.new(width = 6, height = 6, noRStudioGD = TRUE)
    yMax <- 2 * max(heightOneL)
    plot(-1, -1, xlim = c(min(peakOneLGraph) - 1, max(peakOneLGraph) + 1), ylim = c(0, yMax), xaxt = "n", xlab = "Allele repeat number", ylab = "Peak height (RFU)", las = 1)
    segments(min(peakOneLGraph) - 1, 0, max(peakOneLGraph) + 1, 0)
    for(i in 1:length(peakOneLGraph)){
      peakOne <- corVariant(peakOneLGraph[i], lenRUOneL)
      shape <- gammaSumGraph[1, i]
      scale <- gammaSumGraph[2, i]
      expect <- shape * scale
      polygon(c(peakOne - 0.1, peakOne - 0.1, peakOne + 0.1, peakOne + 0.1), c(0, expect, expect, 0), col = "mistyrose")

      segments(peakOne - 0.1, 0, peakOne, heightOneLGraph[i], col = "blue")
      segments(peakOne + 0.1, 0, peakOne, heightOneLGraph[i], col = "blue")
      if(peakOneLGraph2[i] == 99){
        xLabelOne <- "Q"
      }else{
        xLabelOne <- peakOneLGraph2[i]
      }
      par(xpd = TRUE)
      text(peakOne, - yMax / 12, xLabelOne)
      par(xpd = FALSE)
    }
  }
}

# Make a sub-tab of proposition for Deconvolution
makeSubTabProp_Deconvo <- function(envProj, envGUI){
  cspFn <- get("cspFn", pos = envProj)
  refFn <- get("refFn", pos = envProj)
  afFn <- get("afFn", pos = envProj)
  hncFrom_deconvo <- get("hncFrom_deconvo", pos = envProj)
  hncFromVar_deconvo <- tclVar(hncFrom_deconvo)
  hncTo_deconvo <- get("hncTo_deconvo", pos = envProj)
  hncToVar_deconvo <- tclVar(hncTo_deconvo)
  anaMeth_deconvo <- get("anaMeth_deconvo", pos = envProj)
  anaMethVar_deconvo <- tclVar(anaMeth_deconvo)

  pathPack <- get("pathPack", pos = envGUI)
  subTabProp_Deconvo <- get("subTabProp_Deconvo", pos = envGUI)
  frameProp_Deconvo <- get("frameProp_Deconvo", pos = envGUI)

  nameMeth <- list.files(paste0(pathPack, "/extdata/analysis_method"))
  nameMeth <- gsub(".csv", "", nameMeth)

  tkdestroy(frameProp_Deconvo)
  frameProp_Deconvo <- tkframe(subTabProp_Deconvo)
  frameProp1 <- tkframe(frameProp_Deconvo)
  frameProp2 <- tkframe(frameProp_Deconvo)

  frameProp_noc <- tkframe(frameProp1, relief = "groove", borderwidth = 2)
  frameProp_noc1 <- tkframe(frameProp_noc)
  frameProp_noc2 <- tkframe(frameProp_noc)
  tkgrid(tklabel(frameProp_noc1, text = "Number of contributors", font = "Helvetica 10 bold"), sticky = "w")
  hncFromSpin <- tkspinbox(frameProp_noc2, from = 1, to = 4, increment = 1, textvariable = hncFromVar_deconvo, width = 6, highlightthickness = 1, justify = "center", background = "white", state = "normal")
  hncToSpin <- tkspinbox(frameProp_noc2, from = 1, to = 4, increment = 1, textvariable = hncToVar_deconvo, width = 6, highlightthickness = 1, justify = "center", background = "white", state = "normal")
  tkgrid(tklabel(frameProp_noc2, text = "    From"), hncFromSpin, padx = 10, sticky = "w")
  tkgrid(tklabel(frameProp_noc2, text = "    To"),  hncToSpin, padx = 10, sticky = "w")
  tkgrid(frameProp_noc1, padx = 20)
  tkgrid(frameProp_noc2, padx = 20)

  tkgrid(frameProp_noc, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameProp1, sticky = "w")

  frameSetting <- tkframe(frameProp2, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(frameSetting, text = "Setting", font = "Helvetica 10 bold"), sticky = "w")
  labelMeth <- tklabel(frameSetting, text = "Analysis method")
  comboMeth <- ttkcombobox(frameSetting, values = nameMeth, textvariable = anaMethVar_deconvo, width = 25, state = "readonly")
  tkgrid(labelMeth, comboMeth, padx = 10, pady = 5, sticky = "w")

  frameFiles <- tkframe(frameProp2, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(frameFiles, text = "Files", font = "Helvetica 10 bold"), sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Crime Stain Profile"), tklabel(frameFiles, text = cspFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Reference Profile"), tklabel(frameFiles, text = refFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Allele Frequencies"), tklabel(frameFiles, text = afFn), padx = 10, sticky = "w")
  tkgrid(frameSetting, frameFiles, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameProp2, sticky = "w")

  tkgrid(tkbutton(frameProp_Deconvo, text = "    Analyze    ", cursor = "hand2",
                  command = function() guiAnalyze("Deconvo", envProj, envGUI, as.numeric(tclvalue(hncFromVar_deconvo)), as.numeric(tclvalue(hncToVar_deconvo)), tclvalue(anaMethVar_deconvo))), pady = 5)

  tkgrid(frameProp_Deconvo, padx = 20, pady = 10)
  assign("frameProp_Deconvo", frameProp_Deconvo, envir = envGUI)
}

# Make a report for Deconvolution
makeReport_Deconvo <- function(envProj, envGUI, envDeconvo){
  saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".csv")
    }
    options(warn = -1)

    softVer <- get("softVer", pos = envGUI)
    cspFn <- get("cspFn", pos = envProj)
    afFn <- get("afFn", pos = envProj)
    anaMeth_deconvo <- get("anaMeth_deconvo", pos = envProj)
    likeAll <- get("likeAll", pos = envDeconvo)
    parAll <- get("parAll", pos = envDeconvo)
    calcTimes <- get("calcTimes", pos = envDeconvo)

    write.table("======== Software version ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE)
    write.table(softVer, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Input files ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Crime stain profile : ", cspFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Allele frequencies : ", afFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Analysis method ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(anaMeth_deconvo, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Log10 (Likelihood) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(likeAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Estimated parameters ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(parAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Calculation time for deconvolution (sec) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(calcTimes, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

    options(warn = 0)
  }
}

# Proceed to likelihood ratio
proceedLR <- function(envProj, envGUI){
  refInput <- get("refInput", pos = envProj)
  if(length(refInput) == 0){
    refFn <- get("refFn", pos = envProj)
    refFnVar <- tclVar(refFn)
    labelRefName <- get("labelRefName", pos = envGUI)
    buttViewRef <- get("buttViewRef", pos = envGUI)

    tfRef <- tktoplevel()
    tkwm.title(tfRef, "Load reference profiles")
    frameRef1 <- tkframe(tfRef)
    frameRef2 <- tkframe(tfRef)

    tkgrid(tklabel(frameRef1, text = "Input File", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
    labelRef <- tklabel(frameRef1, text = "Reference Profiles")
    labelRefName_new <- tklabel(frameRef1, textvariable = refFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    buttLoadRef <- tkbutton(frameRef1, text = "    Load    ", cursor = "hand2", command = function() openFile(envProj, envGUI, "ref", labelRefName, buttViewRef, FALSE, labelRefName_new))
    tkconfigure(labelRefName_new, textvariable = refFnVar)
    tkgrid(labelRef, labelRefName_new, buttLoadRef, padx = 10, pady = 5, sticky = "w")
    tkgrid(frameRef1, padx = 10, pady = 5, sticky = "w")
    tkgrid(tkbutton(frameRef2, text = "    Next    ", cursor = "hand2", command = function() makeTabLR(envProj, envGUI, tfRef)),
           padx = 10, pady = 5, sticky = "w")
    tkgrid(frameRef2, padx = 10, pady = 5, sticky = "w")
  }else{
    makeTabLR(envProj, envGUI)
  }
}

# Make a sub-tab of results for Deconvolution
makeSubTabResult_Deconvo <- function(envProj, envGUI){
  finDeconvo <- get("finDeconvo", pos = envProj)
  subTabResult_Deconvo <- get("subTabResult_Deconvo", pos = envGUI)
  frameResult_Deconvo <- get("frameResult_Deconvo", pos = envGUI)
  tkdestroy(frameResult_Deconvo)
  frameResult_Deconvo <- tkframe(subTabResult_Deconvo)
  if(finDeconvo){
    showPG <- function(hnc, lociID){
      mlbPG <- get("mlbPG", pos = envDeconvo)
      scrY_PG <- get("scrY_PG", pos = envDeconvo)
      tkdestroy(mlbPG)
      tkdestroy(scrY_PG)
      dataOneHnc <- dataDeconvo[[hnc - hncFrom + 1]]
      likeOneHnc <- dataOneHnc[[1]][[2]]
      pgOneHnc <- dataOneHnc[[1]][[3]]
      if(length(pgOneHnc) == 0){
        scrY_PG <- tklabel(framePG_3, text = "")
        mlbPG <- tklabel(framePG_3, text = "Inappropriate hypothesis")
        tkgrid(mlbPG, scrY_PG)
        tkconfigure(buttGraph, state = "disable", cursor = "arrow")
        tkconfigure(buttExport, state = "disable", cursor = "arrow")
      }else{
        pgOneL <- tablePG(pgOneHnc[[lociID]], "Deconvo")
        scrY_PG <- tkscrollbar(framePG_3, repeatinterval = 5, command = function(...) tkyview(mlbPG, ...))
        mlbPG <- tk2mclistbox(framePG_3, width = 20 * ncol(pgOneL), height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_PG, ...))
        for(i in 1:ncol(pgOneL)){
          tk2column(mlbPG, "add", label = colnames(pgOneL)[i], width = 20)
        }
        tkgrid(mlbPG, scrY_PG)
        tk2insert.multi(mlbPG, "end", pgOneL)
        tkgrid.configure(scrY_PG, rowspan = 25, sticky = "nsw")
        tkconfigure(buttGraph, state = "normal", cursor = "hand2")
        tkconfigure(buttExport, state = "normal", cursor = "hand2")
      }
      assign("scrY_PG", scrY_PG, envir = envDeconvo)
      assign("mlbPG", mlbPG, envir = envDeconvo)
    }

    exportPG <- function(hnc, lociID){
      saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
      if(tclvalue(saveAs) != ""){
        if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
          fileName <- tclvalue(saveAs)
        }else{
          fileName <- paste0(tclvalue(saveAs), ".csv")
        }
        dataOneHnc <- dataDeconvo[[hnc - hncFrom + 1]]
        pgOneL <- tablePG(dataOneHnc[[1]][[3]][[lociID]], "Deconvo")
        write.csv(pgOneL, file = fileName, row.names = FALSE)
      }
    }

    exportPGAll <- function(){
      savePG <- function(){
        if(tclvalue(pgFolderName) == ""){
          tkmessageBox(message = "Enter the folder name for saving files!", icon = "error", type = "ok")
        }else{
          folderPath <- choose.dir()
          if(!is.na(folderPath)){
            folderPath <- paste0(folderPath, "/", tclvalue(pgFolderName))
            dir.create(path = folderPath)
            nHnc <- length(dataDeconvo)
            for(i in 1:nHnc){
              dataOneHnc <- dataDeconvo[[i]]
              pgOneHnc <- dataOneHnc[[1]][[3]]
              if(length(pgOneHnc) != 0){
                for(j in 1:length(cspLoci)){
                  pgOneL <- tablePG(pgOneHnc[[j]], "Deconvo")
                  write.csv(pgOneL, paste0(folderPath, "/pg_", i, "-contributors_", cspLoci[j], ".csv"), row.names = FALSE)
                }
              }
            }
            tkmessageBox(message = paste0("Results of probabilistic genotyping have been saved successfully in the folder of ", tclvalue(pgFolderName), "."), icon = "info", type = "ok")
          }
        }
      }
      tfExportAll <- tktoplevel()
      tkwm.title(tfExportAll, "Export results of probabilistic genotyping")
      tkgrid(tklabel(tfExportAll, text = "Enter the folder name for saving files."), padx = 110, pady = 5)
      pgFolderName <- tclVar("")
      tkgrid(tkentry(tfExportAll, textvariable = pgFolderName, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 120, pady = 5)
      tkgrid(tkbutton(tfExportAll, text = "    Save    ", cursor = "hand2", command = function() savePG()), padx = 120, pady = 20)
    }

    showLike <- function(){
      nH <- hncTo - hncFrom + 1
      nL <- length(cspLoci)

      scrY_L <- get("scrY_L", pos = envDeconvo)
      mlbLike <- get("mlbLike", pos = envDeconvo)
      tkdestroy(mlbLike)
      tkdestroy(scrY_L)
      scrY_L <- tkscrollbar(frameL_2, repeatinterval = 5, command = function(...) tkyview(mlbLike, ...))
      mlbLike <- tk2mclistbox(frameL_2, width = 18 + 12 * nH, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_L, ...))
      tk2column(mlbLike, "add", label = "Marker", width = 18)

      mlbPar <- get("mlbPar", pos = envDeconvo)
      tkdestroy(mlbPar)
      mlbPar <- tk2mclistbox(frameL_4, width = 18 + 12 * nH, height = 2 * hncTo, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbPar, "add", label = "Parameter", width = 18)

      likeAll <- likeAll_display <- matrix("", length(cspLoci) + 1, nH + 1)
      colLike <- rep("", nH + 1)
      colLike[1] <- "Marker"
      likeAll[, 1] <- likeAll_display[, 1] <- c(cspLoci, "total")
      parAll <- parAll_display <- matrix("", 2 * hncTo, nH + 1)
      colPar <- rep("", nH + 1)
      colPar[1] <- "Parameter"
      parAll[, 1] <- parAll_display[, 1] <- c(paste0("Mix prop. (U", 1:hncTo, ")"), paste0("Degradation (U", 1:hncTo, ")"))
      calcTimes <- matrix("", 1, nH)
      hncLabel <- rep("", nH)
      for(i in 1:nH){
        hnc <- hncFrom + i - 1
        hncLabel[i] <- paste0(hnc, "-person")
        colLike[i + 1] <- hncLabel[i]
        tk2column(mlbLike, "add", label = hncLabel[i], width = 12)
        colPar[i + 1] <- hncLabel[i]
        tk2column(mlbPar, "add", label = hncLabel[i], width = 12)
        dataOneHnc <- dataDeconvo[[i]]
        if(length(dataOneHnc) > 0){
          like <- dataOneHnc[[1]][[2]]
          likeAll[, i + 1] <- like
          likeAll_display[, i + 1] <- signif(like, 3)
          mp <- dataOneHnc[[1]][[4]]
          parAll[1:hnc, i + 1] <- mp
          parAll_display[1:hnc, i + 1] <- signif(mp, 3)
          deg <- dataOneHnc[[1]][[5]]
          parAll[hncTo + 1:hnc, i + 1] <- deg
          parAll_display[hncTo + 1:hnc, i + 1] <- signif(deg, 3)
          calcTimes[1, i] <- dataOneHnc[[1]][[7]]
        }
      }
      tkgrid(mlbLike, scrY_L)
      tk2insert.multi(mlbLike, "end", likeAll_display)
      tkgrid.configure(scrY_L, rowspan = 25, sticky = "nsw")
      assign("mlbLike", mlbLike, envir = envDeconvo)
      assign("scrY_L", scrY_L, envir = envDeconvo)
      colnames(likeAll) <- colLike
      assign("likeAll", likeAll, envir = envDeconvo)

      tkgrid(mlbPar)
      tk2insert.multi(mlbPar, "end", parAll_display)
      assign("mlbPar", mlbPar, envir = envDeconvo)
      colnames(parAll) <- colPar
      assign("parAll", parAll, envir = envDeconvo)

      colnames(calcTimes) <- hncLabel
      assign("calcTimes", calcTimes, envir = envDeconvo)
    }

    subTabs_Deconvo <- get("subTabs_Deconvo", pos = envGUI)
    dataDeconvo <- get("dataDeconvo", pos = envProj)
    cspLoci <- get("cspLoci", pos = envProj)

    envDeconvo <- new.env(parent = globalenv())
    assign("scrY_PG", tkscrollbar(frameResult_Deconvo), envir = envDeconvo)
    assign("mlbPG", tk2mclistbox(frameResult_Deconvo), envir = envDeconvo)
    assign("scrY_L", tkscrollbar(frameResult_Deconvo), envir = envDeconvo)
    assign("mlbLike", tk2mclistbox(frameResult_Deconvo), envir = envDeconvo)
    assign("mlbPar", tk2mclistbox(frameResult_Deconvo), envir = envDeconvo)

    framePG <- tkframe(frameResult_Deconvo, relief = "groove", borderwidth = 2)
    frameL <- tkframe(frameResult_Deconvo, relief = "groove", borderwidth = 2)
    framePG_1 <- tkframe(framePG)
    framePG_2 <- tkframe(framePG)
    framePG_3 <- tkframe(framePG)
    framePG_4 <- tkframe(framePG)
    frameL_1 <- tkframe(frameL)
    frameL_2 <- tkframe(frameL)
    frameL_3 <- tkframe(frameL)
    frameL_4 <- tkframe(frameL)
    frameL_5 <- tkframe(frameL)

    tkgrid(tklabel(framePG_1, text = "Probabilistic genotyping", font = "Helvetica 10 bold"), pady = 5)

    hncAll <- as.numeric(gsub("hnc_", "", names(dataDeconvo)))
    hncFrom <- min(hncAll)
    hncTo <- max(hncAll)
    selectHnc <- tclVar(hncAll[1])
    comboHnc <- ttkcombobox(framePG_2, values = hncAll, textvariable = selectHnc, state = "readonly", width = 20)
    tkgrid(tklabel(framePG_2, text = "Number of contributors : "), comboHnc, sticky = "w")

    selectL <- tclVar(cspLoci[1])
    comboLoci <- ttkcombobox(framePG_2, values = cspLoci, textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(framePG_2, text = "Locus : "), comboLoci, sticky = "w")

    tkbind(comboHnc, "<<ComboboxSelected>>", function(){
      hnc <- as.numeric(tclvalue(selectHnc))
      lociID <- which(cspLoci == tclvalue(selectL))
      showPG(hnc, lociID)
    })

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      hnc <- as.numeric(tclvalue(selectHnc))
      lociID <- which(cspLoci == tclvalue(selectL))
      showPG(hnc, lociID)
    })

    buttGraph <- tkbutton(framePG_4, text = "    Graph    ", cursor = "hand2", command = function() graphPG("Deconvo", envProj, envDeconvo, dataDeconvo, selectHnc, NULL, selectL, cspLoci))
    buttExport <- tkbutton(framePG_4, text = "    Export displayed data    ", cursor = "hand2", command = function() exportPG(as.numeric(tclvalue(selectHnc)), which(cspLoci == tclvalue(selectL))))
    buttExportAll <- tkbutton(framePG_4, text = "    Export all    ", cursor = "hand2", command = function() exportPGAll())
    tkgrid(buttGraph, buttExport, buttExportAll, padx = 5, sticky = "w")
    showPG(as.numeric(tclvalue(selectHnc)), 1)

    tkgrid(framePG_1, pady = 5, sticky = "w")
    tkgrid(framePG_2, pady = 5, sticky = "w")
    tkgrid(framePG_3, pady = 5, sticky = "w")
    tkgrid(framePG_4, pady = 5, sticky = "w")

    tkgrid(tklabel(frameL_1, text = "Log10 (Likelihood)", font = "Helvetica 10 bold"), pady = 5)
    tkgrid(tklabel(frameL_3, text = "Estimated parameters", font = "Helvetica 10 bold"), pady = 5)
    buttReport <- tkbutton(frameL_5, text = "    Report    ", cursor = "hand2", command = function() makeReport_Deconvo(envProj, envGUI, envDeconvo))
    buttProceed <- tkbutton(frameL_5, text = "    Proceed to likelihood ratio    ", cursor = "hand2", command = function() proceedLR(envProj, envGUI))
    showLike()
    tkgrid(buttReport, buttProceed, padx = 5, pady = 5)
    tkgrid(frameL_1, sticky = "w")
    tkgrid(frameL_2, sticky = "w")
    tkgrid(frameL_3, sticky = "w")
    tkgrid(frameL_4, sticky = "w")
    tkgrid(frameL_5, sticky = "w")
    tkgrid(frameL_5)

    tkgrid(framePG, frameL, padx = 5, pady = 5, sticky = "nw")

    tk2notetab.select(subTabs_Deconvo, "Result")
  }
  tkgrid(frameResult_Deconvo)
  assign("frameResult_Deconvo", frameResult_Deconvo, envir = envGUI)
}

# Make a tab for Deconvolution
makeTabDeconvo <- function(envProj, envGUI){
  cspInput <- get("cspInput", pos = envProj)
  refInput <- get("refInput", pos = envProj)
  afInput <- get("afInput", pos = envProj)
  if(any(c(length(cspInput) == 0, length(afInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    inputData <- checkFile(envGUI, cspInput, refInput, afInput, meth = "Deconvo")
    if(length(inputData) == 3){
      tabs <- get("tabs", pos = envGUI)
      frameDeconvo <- get("frameDeconvo", pos = envGUI)
      subTabs_Deconvo <- get("subTabs_Deconvo", pos = envGUI)
      tkpack(subTabs_Deconvo, fill = "both")
      tkpack(frameDeconvo, fill = "both", padx = 10, pady = 10)

      csp <- inputData[[1]]
      assign("csp", csp, envir = envProj)
      ref <- inputData[[2]]
      assign("ref", ref, envir = envProj)
      af <- inputData[[3]]
      assign("af", af, envir = envProj)

      makeSubTabProp_Deconvo(envProj, envGUI)
      makeSubTabResult_Deconvo(envProj, envGUI)
      tk2notetab.select(tabs, "Deconvolution")
    }
  }
}

# Make a sub-tab of proposition for LR
makeSubTabProp_LR <- function(envProj, envGUI){
  changeHncAnaMeth <- function(){
    if(tclvalue(baseDeconvoVar) == "1"){
      tclvalue(anaMethVar_lr) <- get("anaMeth_deconvo", pos = envProj)
      tclvalue(anaMethStateVar) <- "disabled"
      tclvalue(hncStateVar_lr) <- "disabled"
      tclvalue(hncFromVar_lr) <- hncFrom_deconvo
      tclvalue(hncToVar_lr) <- hncTo_deconvo
      hncMin_lr <- hncFrom_deconvo
      hncMax_lr <- hncTo_deconvo
      assign("hncState_lr", tclvalue(hncStateVar_lr), envir = envProj)
      assign("hncMin_lr", hncMin_lr, envir = envProj)
      assign("hncMax_lr", hncMax_lr, envir = envProj)
    }else{
      tclvalue(anaMethVar_lr) <- get("anaMeth_lr", pos = envProj)
      tclvalue(anaMethStateVar) <- "normal"
      tclvalue(hncStateVar_lr) <- "normal"
      tclvalue(hncFromVar_lr) <- 1
      tclvalue(hncToVar_lr) <- 4
      hncMin_lr <- 1
      hncMax_lr <- 4
      assign("hncState_lr", tclvalue(hncStateVar_lr), envir = envProj)
      assign("hncMin_lr", hncMin_lr, envir = envProj)
      assign("hncMax_lr", hncMax_lr, envir = envProj)
    }
    tkconfigure(comboMeth, state = tclvalue(anaMethStateVar))
    tkconfigure(hncFromSpin, from = hncMin_lr, to = hncMax_lr, state = tclvalue(hncStateVar_lr))
    tkconfigure(hncToSpin, from = hncMin_lr, to = hncMax_lr, state = tclvalue(hncStateVar_lr))
  }

  changeHypState <- function(){
    if(tclvalue(calcAllHypVar) == 1){
      tclvalue(hypStateVar) <- "disabled"
    }else{
      tclvalue(hypStateVar) <- "normal"
    }
    for(i in 1:nK){
      tkconfigure(ckbuttHp[[i]], state = tclvalue(hypStateVar))
      tkconfigure(ckbuttHd[[i]], state = tclvalue(hypStateVar))
    }
    assign("hypState", tclvalue(hypStateVar), envir = envProj)
  }

  cspFn <- get("cspFn", pos = envProj)
  refFn <- get("refFn", pos = envProj)
  afFn <- get("afFn", pos = envProj)
  ref <- get("ref", pos = envProj)
  hncFrom_deconvo <- get("hncFrom_deconvo", pos = envProj)
  hncTo_deconvo <- get("hncTo_deconvo", pos = envProj)
  hypState <- get("hypState", pos = envProj)
  hypStateVar <- tclVar(hypState)
  hncState_lr <- get("hncState_lr", pos = envProj)
  hncStateVar_lr <- tclVar(hncState_lr)
  hncFrom_lr <- get("hncFrom_lr", pos = envProj)
  hncFromVar_lr <- tclVar(hncFrom_lr)
  hncTo_lr <- get("hncTo_lr", pos = envProj)
  hncToVar_lr <- tclVar(hncTo_lr)
  anaMeth_lr <- get("anaMeth_lr", pos = envProj)
  anaMethVar_lr <- tclVar(anaMeth_lr)
  baseDeconvo <- get("baseDeconvo", pos = envProj)
  baseDeconvoVar <- tclVar(baseDeconvo)
  baseDeconvoState <- get("baseDeconvoState", pos = envProj)
  baseDeconvoStateVar <- tclVar(baseDeconvoState)
  calcAllHyp <- get("calcAllHyp", pos = envProj)
  calcAllHypVar <- tclVar(calcAllHyp)
  calcAllHypState <- get("calcAllHypState", pos = envProj)
  calcAllHypStateVar <- tclVar(calcAllHypState)
  finDeconvo <- get("finDeconvo", pos = envProj)

  pathPack <- get("pathPack", pos = envGUI)
  subTabProp_LR <- get("subTabProp_LR", pos = envGUI)
  frameProp_LR <- get("frameProp_LR", pos = envGUI)

  nameMeth <- list.files(paste0(pathPack, "/extdata/analysis_method"))
  nameMeth <- gsub(".csv", "", nameMeth)


  if(finDeconvo){
    tclvalue(baseDeconvoVar) <- 1
    tclvalue(baseDeconvoStateVar) <- "normal"
    tclvalue(hncStateVar_lr) <- "disabled"
    tclvalue(hncFromVar_lr) <- hncFrom_deconvo
    tclvalue(hncToVar_lr) <- hncTo_deconvo
    tclvalue(anaMethVar_lr) <- get("anaMeth_deconvo", pos = envProj)
    anaMethStateVar <- tclVar("disabled")
    hncMin_lr <- hncFrom_deconvo
    hncMax_lr <- hncTo_deconvo
    assign("hncMin_lr", hncMin_lr, envir = envProj)
    assign("hncMax_lr", hncMax_lr, envir = envProj)
  }else{
    anaMethStateVar <- tclVar("readonly")
    hncMin_lr <- get("hncMin_lr", pos = envProj)
    hncMax_lr <- get("hncMax_lr", pos = envProj)
  }

  tkdestroy(frameProp_LR)
  frameProp_LR <- tkframe(subTabProp_LR)
  frameProp1 <- tkframe(frameProp_LR)
  frameProp2 <- tkframe(frameProp_LR)
  frameProp3 <- tkframe(frameProp_LR)

  frameProp_Hp <- tkframe(frameProp1, relief = "groove", borderwidth = 2)
  frameProp_Hd <- tkframe(frameProp1, relief = "groove", borderwidth = 2)
  labelHp <- tklabel(frameProp_Hp, text = "Prosecutor hypothesis", font = "Helvetica 10 bold")
  labelHd <- tklabel(frameProp_Hd, text = "Defense hypothesis", font = "Helvetica 10 bold")
  frameHpKnown <- tkframe(frameProp_Hp)
  frameHdKnown <- tkframe(frameProp_Hd)
  nK <- (ncol(ref) - 1) / 2
  nameKnown <- colnames(ref[, 2:(nK * 2 + 1)])[2:(nK * 2 + 1) %% 2 == 0]
  ckbuttHp <- ckbuttHd <- list()
  hpVars <- hdVars <- list()
  for(i in 1:nK){
    if(length(hpVars) != nK){
      hpVars[[i]] <- tclVar("0")
    }
    if(length(hdVars) != nK){
      hdVars[[i]] <- tclVar("0")
    }
    ckbuttHp[[i]] <- tkcheckbutton(frameHpKnown, text = nameKnown[i], variable = hpVars[[i]], state = tclvalue(hypStateVar))
    tkgrid(ckbuttHp[[i]], sticky = "w")
    ckbuttHd[[i]] <- tkcheckbutton(frameHdKnown, text = nameKnown[i], variable = hdVars[[i]], state = tclvalue(hypStateVar))
    tkgrid(ckbuttHd[[i]], sticky = "w")
  }
  tkgrid(labelHp, padx = 20)
  tkgrid(labelHd, padx = 20)
  tkgrid(frameHpKnown, padx = 20)
  tkgrid(frameHdKnown, padx = 20)
  tkgrid(tklabel(frameProp_Hp, text = "(+ unknown contributor(s))"))
  tkgrid(tklabel(frameProp_Hd, text = "(+ unknown contributor(s))"))

  frameProp_noc <- tkframe(frameProp1, relief = "groove", borderwidth = 2)
  frameProp_noc1 <- tkframe(frameProp_noc)
  frameProp_noc2 <- tkframe(frameProp_noc)
  tkgrid(tklabel(frameProp_noc1, text = "Number of contributors", font = "Helvetica 10 bold"), sticky = "w")
  hncFromSpin <- tkspinbox(frameProp_noc2, from = hncMin_lr, to = hncMax_lr, increment = 1, textvariable = hncFromVar_lr, width = 6, highlightthickness = 1, justify = "center", background = "white", state = tclvalue(hncStateVar_lr))
  hncToSpin <- tkspinbox(frameProp_noc2, from = hncMin_lr, to = hncMax_lr, increment = 1, textvariable = hncToVar_lr, width = 6, highlightthickness = 1, justify = "center", background = "white", state = tclvalue(hncStateVar_lr))
  tkgrid(tklabel(frameProp_noc2, text = "    From"), hncFromSpin, padx = 10, sticky = "w")
  tkgrid(tklabel(frameProp_noc2, text = "    To"),  hncToSpin, padx = 10, sticky = "w")
  tkgrid(frameProp_noc1, padx = 20)
  tkgrid(frameProp_noc2, padx = 20)

  tkgrid(frameProp_Hp, frameProp_Hd, frameProp_noc, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameProp1, sticky = "w")

  frameSetting <- tkframe(frameProp2, relief = "groove", borderwidth = 2)
  frameSetting1 <- tkframe(frameSetting)
  frameSetting2 <- tkframe(frameSetting)
  tkgrid(tklabel(frameSetting1, text = "Setting", font = "Helvetica 10 bold"), sticky = "w")
  labelMeth <- tklabel(frameSetting1, text = "Analysis method")
  comboMeth <- ttkcombobox(frameSetting1, values = nameMeth, textvariable = anaMethVar_lr, width = 25, state = tclvalue(anaMethStateVar))
  tkgrid(labelMeth, comboMeth, padx = 10, pady = 5, sticky = "w")
  ckbuttBaseDeconvo <- tkcheckbutton(frameSetting2, text = "Based on the result of deconvolution", variable = baseDeconvoVar, state = tclvalue(baseDeconvoStateVar), command = function() changeHncAnaMeth())
  tkgrid(ckbuttBaseDeconvo, padx = 10, pady = 5, sticky = "w")
  ckbuttCalcAllHyp <- tkcheckbutton(frameSetting2, text = "Calculate the likelihood of all assumed hypotheses", variable = calcAllHypVar, state = tclvalue(calcAllHypStateVar), command = function() changeHypState())
  tkgrid(ckbuttCalcAllHyp, padx = 10, pady = 5, sticky = "w")
  tkgrid(frameSetting1, sticky = "w")
  tkgrid(frameSetting2, sticky = "w")

  tkbind(comboMeth)

  frameFiles <- tkframe(frameProp2, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(frameFiles, text = "Files", font = "Helvetica 10 bold"), sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Crime Stain Profile"), tklabel(frameFiles, text = cspFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Reference Profile"), tklabel(frameFiles, text = refFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameFiles, text = "Allele Frequencies"), tklabel(frameFiles, text = afFn), padx = 10, sticky = "w")
  tkgrid(frameSetting, frameFiles, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameProp2, sticky = "w")

  buttAnalyze <- tkbutton(frameProp3, text = "    Analyze    ", cursor = "hand2",
                          command = function() guiAnalyze("LR", envProj, envGUI,
                                                          as.numeric(tclvalue(hncFromVar_lr)), as.numeric(tclvalue(hncToVar_lr)),
                                                          tclvalue(anaMethVar_lr), as.numeric(tclvalue(baseDeconvoVar)), as.numeric(tclvalue(calcAllHypVar)),
                                                          which(as.numeric(sapply(hpVars, tclvalue)) != 0), which(as.numeric(sapply(hdVars, tclvalue)) != 0)))
  tkgrid(buttAnalyze, padx = 10, pady = 5)
  tkgrid(frameProp3, pady = 5)

  tkgrid(frameProp_LR, padx = 20, pady = 10)
  assign("frameProp_LR", frameProp_LR, envir = envGUI)
}

# List up hypotheses
listupHyp <- function(dataLR, calcAllHyp, nameKnownHp, nameKnownHd){
  hyps <- hypsHp <- hypsHd <- character(0)
  for(i in 1:length(dataLR)){
    dataOneHnc <- dataLR[[i]]
    if(length(dataOneHnc) > 0){
      for(j in 1:length(dataOneHnc)){
        oneHyp <- dataOneHnc[[j]][[1]]
        judgeHp <- judgeHd <- FALSE
        if(calcAllHyp == 1){
          judgeHp <- judgeHd <- TRUE
        }else{
          if(length(nameKnownHp) == 0){
            if(all(is.element(oneHyp, "U"))){
              judgeHp <- TRUE
            }
          }else if(setequal(setdiff(oneHyp, "U"), nameKnownHp)){
            judgeHp <- TRUE
          }
          if(length(nameKnownHd) == 0){
            if(all(is.element(oneHyp, "U"))){
              judgeHd <- TRUE
            }
          }else if(setequal(setdiff(oneHyp, "U"), nameKnownHd)){
            judgeHd <- TRUE
          }
        }
        if(judgeHp || judgeHd){
          known <- oneHyp[!is.element(oneHyp, "U")]
          unknown <- oneHyp[is.element(oneHyp, "U")]
          oneHyp <- paste(c(known, unknown), collapse = ",")
          if(judgeHp){
            hyps <- c(hyps, oneHyp)
            hypsHp <- c(hypsHp, oneHyp)
          }
          if(judgeHd){
            hyps <- c(hyps, oneHyp)
            hypsHd <- c(hypsHd, oneHyp)
          }
        }
      }
    }
  }
  hyps <- unique(hyps)
  return(list(hyps, hypsHp, hypsHd))
}

# Make a report for LR
makeReport_LR <- function(envProj, envGUI, envLR, hp, hd){
  saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".csv")
    }
    options(warn = -1)

    softVer <- get("softVer", pos = envGUI)
    cspFn <- get("cspFn", pos = envProj)
    refFn <- get("refFn", pos = envProj)
    afFn <- get("afFn", pos = envProj)
    anaMeth_lr <- get("anaMeth_lr", pos = envProj)
    selectLR <- get("selectLR", pos = envLR)
    selectParHp <- get("selectParHp", pos = envLR)
    selectParHd <- get("selectParHd", pos = envLR)
    calcTimeHp <- get("calcTimeHp", pos = envLR)
    calcTimeHd <- get("calcTimeHd", pos = envLR)
    hncHp <- length(strsplit(hp, ",")[[1]])
    hncHd <- length(strsplit(hd, ",")[[1]])

    write.table("======== Software version ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE)
    write.table(softVer, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Input files ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Crime stain profile : ", cspFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Reference profile : ", refFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Allele frequencies : ", afFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Analysis method ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(anaMeth_lr, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Hypothesis ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Prosecutor hypothesis (Hp) : ", hp), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Defense hypothesis (Hd) : ", hd), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Likelihood ratio ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(selectLR, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Estimated parameters (Hp) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(selectParHp, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Estimated parameters (Hd) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(selectParHd, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Calculation time for deconvolution (sec) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(matrix(c(paste0("Hp (", hncHp, "-person)"), calcTimeHp), nrow = 1), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(matrix(c(paste0("Hd (", hncHd, "-person)"), calcTimeHd), nrow = 1), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    options(warn = 0)
  }
}

exportLikeAll <- function(envProj, envGUI, cspLoci, hyps){
  saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".csv")
    }

    softVer <- get("softVer", pos = envGUI)
    cspFn <- get("cspFn", pos = envProj)
    refFn <- get("refFn", pos = envProj)
    afFn <- get("afFn", pos = envProj)
    anaMeth_lr <- get("anaMeth_lr", pos = envProj)
    dataLR <- get("dataLR", pos = envProj)

    nL <- length(cspLoci)
    nH <- length(hyps)
    matLike <- matrix(0, nL + 1, nH)
    count <- 0
    for(i in 1:length(dataLR)){
      dataOneHnc <- dataLR[[i]]
      for(j in 1:length(dataOneHnc)){
        count <- count + 1
        dataOneHyp <- dataOneHnc[[j]]
        matLike[, count] <- dataOneHyp[[2]]
      }
    }

    matLR <- matrix(0, nH, nH)
    for(i in 1:nH){
      likeHp <- matLike[nL + 1, i]
      for(j in 1:nH){
        likeHd <- matLike[nL + 1, j]
        matLR[i, j] <- 10^(likeHp - likeHd)
      }
    }
    
    matLike <- cbind(c(cspLoci, "Total"), matLike)
    colnames(matLike) <- c("Marker", hyps)
    matLR <- cbind(hyps, matLR)
    colnames(matLR) <- c("", hyps)

    options(warn = -1)

    write.table("======== Software version ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE)
    write.table(softVer, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Input files ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Crime stain profile : ", cspFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Reference profile : ", refFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(paste0("Allele frequencies : ", afFn), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Analysis method ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(anaMeth_lr, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Hypothesis ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    for(i in 1:nH){
      write.table(hyps[i], file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Log10 (Likelihood) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(matLike, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    write.table("======== Likelihood ratio ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("Row: Hp", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("Column: Hd", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(matLR, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    options(warn = 0)
  }
}

# Make a sub-tab of results for LR
makeSubTabResult_LR <- function(envProj, envGUI){
  finLR <- get("finLR", pos = envProj)
  dataLR <- get("dataLR", pos = envProj)
  cspLoci <- get("cspLoci", pos = envProj)
  calcAllHyp <- get("calcAllHyp", pos = envProj)
  nameKnownHp <- get("nameKnownHp", pos = envProj)
  nameKnownHd <- get("nameKnownHd", pos = envProj)
  subTabs_LR <- get("subTabs_LR", pos = envGUI)
  subTabResult_LR <- get("subTabResult_LR", pos = envGUI)
  frameResult_LR <- get("frameResult_LR", pos = envGUI)
  tkdestroy(frameResult_LR)
  frameResult_LR <- tkframe(subTabResult_LR)
  if(finLR){
    showPG_LR <- function(hyp, lociID){
      mlbPG <- get("mlbPG", pos = envLR)
      scrY_PG <- get("scrY_PG", pos = envLR)
      tkdestroy(mlbPG)
      tkdestroy(scrY_PG)
      hyp <- strsplit(hyp, ",")[[1]]
      hnc <- length(hyp)
      dataOneHnc <- dataLR[[hnc - hncFrom + 1]]
      for(i in 1:length(dataOneHnc)){
        if(setequal(dataOneHnc[[i]][[1]], hyp)){
          pos <- i
          break
        }
      }
      dataOneHyp <- dataOneHnc[[pos]]
      likeOneHyp <- dataOneHyp[[2]]
      pgOneHyp <- dataOneHyp[[3]]
      if(length(pgOneHyp) == 0){
        scrY_PG <- tklabel(framePG_3, text = "")
        mlbPG <- tklabel(framePG_3, text = "Inappropriate hypothesis")
        tkgrid(mlbPG)
        tkconfigure(buttGraph, state = "disable", cursor = "arrow")
        tkconfigure(buttExport, state = "disable", cursor = "arrow")
      }else if(likeOneHyp[lociID] == -Inf){
        scrY_PG <- tklabel(framePG_3, text = "")
        mlbPG <- tklabel(framePG_3, text = "Inappropriate hypothesis")
        tkgrid(mlbPG)
        tkconfigure(buttGraph, state = "disable", cursor = "arrow")
        tkconfigure(buttExport, state = "disable", cursor = "arrow")
      }else{
        pgOneL <- tablePG(dataOneHyp[[3]][[lociID]], "LR")
        scrY_PG <- tkscrollbar(framePG_3, repeatinterval = 5, command = function(...) tkyview(mlbPG, ...))
        mlbPG <- tk2mclistbox(framePG_3, width = 20 * ncol(pgOneL), height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_PG, ...))
        for(i in 1:ncol(pgOneL)){
          tk2column(mlbPG, "add", label = colnames(pgOneL)[i], width = 20)
        }
        tkgrid(mlbPG, scrY_PG)
        tk2insert.multi(mlbPG, "end", pgOneL)
        tkgrid.configure(scrY_PG, rowspan = 25, sticky = "nsw")
        tkconfigure(buttGraph, state = "normal", cursor = "hand2")
        tkconfigure(buttExport, state = "normal", cursor = "hand2")
      }
      assign("scrY_PG", scrY_PG, envir = envLR)
      assign("mlbPG", mlbPG, envir = envLR)
    }

    exportPG_LR <- function(hyp, lociID){
      saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
      if(tclvalue(saveAs) != ""){
        if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
          fileName <- tclvalue(saveAs)
        }else{
          fileName <- paste0(tclvalue(saveAs), ".csv")
        }
        hyp <- strsplit(hyp, ",")[[1]]
        hnc <- length(hyp)
        dataOneHnc <- dataLR[[hnc - hncFrom + 1]]
        for(i in 1:length(dataOneHnc)){
          if(setequal(dataOneHnc[[i]][[1]], hyp)){
            pos <- i
            break
          }
        }
        dataOneHyp <- dataOneHnc[[pos]]
        pgOneL <- tablePG(dataOneHyp[[3]][[lociID]], "LR")
        write.csv(pgOneL, file = fileName, row.names = FALSE)
      }
    }

    exportPGAll_LR <- function(){
      savePG <- function(){
        if(tclvalue(pgFolderName) == ""){
          tkmessageBox(message = "Enter the folder name for saving files!", icon = "error", type = "ok")
        }else{
          folderPath <- choose.dir()
          if(!is.na(folderPath)){
            folderPath <- paste0(folderPath, "/", tclvalue(pgFolderName))
            dir.create(path = folderPath)
            nHnc <- length(dataLR)
            for(i in 1:nHnc){
              dataOneHnc <- dataLR[[i]]
              for(j in 1:length(dataOneHnc)){
                dataOneHyp <- dataOneHnc[[j]]
                likeOneHyp <- dataOneHyp[[2]]
                pgOneHyp <- dataOneHyp[[3]]
                if(length(pgOneHyp) > 0){
                  oneHyp <- dataOneHyp[[1]]
                  known <- oneHyp[!is.element(oneHyp, "U")]
                  unknown <- oneHyp[is.element(oneHyp, "U")]
                  oneHyp <- paste(c(sort(known), unknown), collapse = ",")
                  for(k in 1:length(cspLoci)){
                    if(likeOneHyp[k] != -Inf){
                      pgOneL <- tablePG(pgOneHyp[[k]], "LR")
                      write.csv(pgOneL, paste0(folderPath, "/pg_", oneHyp, "_", cspLoci[k], ".csv"), row.names = FALSE)
                    }
                  }
                }
              }
            }
            tkmessageBox(message = paste0("Results of probabilistic genotyping have been saved successfully in the folder of ", tclvalue(pgFolderName), "."), icon = "info", type = "ok")
          }
        }
      }
      tfExportAll <- tktoplevel()
      tkwm.title(tfExportAll, "Export results of probabilistic genotyping")
      tkgrid(tklabel(tfExportAll, text = "Enter the folder name for saving files."), padx = 110, pady = 5)
      pgFolderName <- tclVar("")
      tkgrid(tkentry(tfExportAll, textvariable = pgFolderName, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 120, pady = 5)
      tkgrid(tkbutton(tfExportAll, text = "    Save    ", cursor = "hand2", command = function() savePG()), padx = 120, pady = 20)
    }

    showLR <- function(hp, hd){
      hp <- strsplit(hp, ",")[[1]]
      hncHp <- length(hp)
      dataOneHnc <- dataLR[[hncHp - hncFrom + 1]]
      for(i in 1:length(dataOneHnc)){
        if(setequal(dataOneHnc[[i]][[1]], hp)){
          posHp <- i
          break
        }
      }
      dataOneHp <- dataOneHnc[[posHp]]
      likeHp <- dataOneHp[[2]]
      mpHp <- dataOneHp[[4]]
      degHp <- dataOneHp[[5]]
      assign("calcTimeHp", dataOneHp[[7]], envir = envLR)

      hd <- strsplit(hd, ",")[[1]]
      hncHd <- length(hd)
      dataOneHnc <- dataLR[[hncHd - hncFrom + 1]]
      for(i in 1:length(dataOneHnc)){
        if(setequal(dataOneHnc[[i]][[1]], hd)){
          posHd <- i
          break
        }
      }
      dataOneHd <- dataOneHnc[[posHd]]
      likeHd <- dataOneHd[[2]]
      mpHd <- dataOneHd[[4]]
      degHd <- dataOneHd[[5]]
      assign("calcTimeHd", dataOneHd[[7]], envir = envLR)

      nL <- length(cspLoci)
      selectLR <- matrix(0, nL + 1, 5)
      colnames(selectLR) <- c("Marker", "Log10 (Likelihood (Hp))", "Log10 (Likelihood (Hd))", "Log10 (LR)", "LR")
      selectLR[, 1] <- names(likeHp)
      selectLR[, 2] <- likeHp
      selectLR[, 3] <- likeHd
      selectLR[, 4] <- likeHp - likeHd
      selectLR[, 5] <- 10^(likeHp - likeHd)
      assign("selectLR", selectLR, envir = envLR)

      selectLR_display <- matrix(0, nL + 1, 5)
      colnames(selectLR_display) <- c("Marker", "Log10 (Likelihood (Hp))", "Log10 (Likelihood (Hd))", "Log10 (LR)", "LR")
      selectLR_display[, 1] <- names(likeHp)
      selectLR_display[, 2] <- signif(likeHp, 3)
      selectLR_display[, 3] <- signif(likeHd, 3)
      selectLR_display[, 4] <- signif(likeHp - likeHd, 3)
      selectLR_display[, 5] <- signif(10^(likeHp - likeHd), 3)

      selectParHp <- matrix(0, hncHp, 3)
      colnames(selectParHp) <- c("Contributor", "Mix prop. (Hp)", "Degradation (Hp)")
      selectParHp[, 1] <- names(mpHp)
      selectParHp[, 2] <- mpHp
      selectParHp[, 3] <- degHp
      assign("selectParHp", selectParHp, envir = envLR)

      selectParHp_display <- matrix(0, hncHp, 3)
      colnames(selectParHp_display) <- c("Contributor", "Mix prop. (Hp)", "Degradation (Hp)")
      selectParHp_display[, 1] <- names(mpHp)
      selectParHp_display[, 2] <- signif(mpHp, 3)
      selectParHp_display[, 3] <- signif(degHp, 3)

      selectParHd <- matrix(0, hncHd, 3)
      colnames(selectParHd) <- c("Contributor", "Mix prop. (Hd)", "Degradation (Hd)")
      selectParHd[, 1] <- names(mpHd)
      selectParHd[, 2] <- mpHd
      selectParHd[, 3] <- degHd
      assign("selectParHd", selectParHd, envir = envLR)

      selectParHd_display <- matrix(0, hncHd, 3)
      colnames(selectParHd_display) <- c("Contributor", "Mix prop. (Hd)", "Degradation (Hd)")
      selectParHd_display[, 1] <- names(mpHd)
      selectParHd_display[, 2] <- signif(mpHd, 3)
      selectParHd_display[, 3] <- signif(degHd, 3)
      assign("selectParHd", selectParHd_display, envir = envLR)

      scrY_LR <- get("scrY_LR", pos = envLR)
      mlbLR <- get("mlbLR", pos = envLR)
      mlbPar_Hp <- get("mlbPar_Hp", pos = envLR)
      mlbPar_Hd <- get("mlbPar_Hd", pos = envLR)
      tkdestroy(mlbLR)
      tkdestroy(mlbPar_Hp)
      tkdestroy(mlbPar_Hd)
      tkdestroy(scrY_LR)

      scrY_LR <- tkscrollbar(frameL_3, repeatinterval = 5, command = function(...) tkyview(mlbLR, ...))
      mlbLR <- tk2mclistbox(frameL_3, width = 90, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_LR, ...))
      tk2column(mlbLR, "add", label = "Marker", width = 10)
      tk2column(mlbLR, "add", label = "Log10 (Likelihood (Hp))", width = 25)
      tk2column(mlbLR, "add", label = "Log10 (Likelihood (Hd))", width = 25)
      tk2column(mlbLR, "add", label = "Log10 (LR)", width = 15)
      tk2column(mlbLR, "add", label = "LR", width = 15)
      tkgrid(mlbLR, scrY_LR)
      tk2insert.multi(mlbLR, "end", selectLR_display)
      tkgrid.configure(scrY_LR, rowspan = 25, sticky = "nsw")
      assign("mlbLR", mlbLR, envir = envLR)
      assign("scrY_LR", scrY_LR, envir = envLR)

      mlbPar_Hp <- tk2mclistbox(frameL_4, width = 45, height = hncHp, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbPar_Hp, "add", label = "Contributor", width = 12)
      tk2column(mlbPar_Hp, "add", label = "Mix prop. (Hp)", width = 15)
      tk2column(mlbPar_Hp, "add", label = "Degradation (Hp)", width = 18)
      tk2insert.multi(mlbPar_Hp, "end", selectParHp_display)
      assign("mlbPar_Hp", mlbPar_Hp, envir = envLR)

      mlbPar_Hd <- tk2mclistbox(frameL_4, width = 45, height = hncHd, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbPar_Hd, "add", label = "Contributor", width = 12)
      tk2column(mlbPar_Hd, "add", label = "Mix prop. (Hd)", width = 15)
      tk2column(mlbPar_Hd, "add", label = "Degradation (Hd)", width = 18)
      tk2insert.multi(mlbPar_Hd, "end", selectParHd_display)
      assign("mlbPar_Hd", mlbPar_Hd, envir = envLR)

      tkgrid(mlbPar_Hp, mlbPar_Hd, padx = 5, pady = 5, sticky = "n")
    }

    envLR <- new.env(parent = globalenv())
    assign("scrY_PG", tkscrollbar(frameResult_LR), envir = envLR)
    assign("mlbPG", tk2mclistbox(frameResult_LR), envir = envLR)
    assign("scrY_LR", tkscrollbar(frameResult_LR), envir = envLR)
    assign("mlbLR", tk2mclistbox(frameResult_LR), envir = envLR)
    assign("mlbPar_Hp", tk2mclistbox(frameResult_LR), envir = envLR)
    assign("mlbPar_Hd", tk2mclistbox(frameResult_LR), envir = envLR)

    hncAll <- as.numeric(gsub("hnc_", "", names(dataLR)))
    hncFrom <- min(hncAll)
    hncTo <- max(hncAll)

    hypsList <- listupHyp(dataLR, calcAllHyp, nameKnownHp, nameKnownHd)
    hyps <- hypsList[[1]]
    hypsHp <- hypsList[[2]]
    hypsHd <- hypsList[[3]]

    framePG <- tkframe(frameResult_LR, relief = "groove", borderwidth = 2)
    frameL <- tkframe(frameResult_LR, relief = "groove", borderwidth = 2)
    framePG_1 <- tkframe(framePG)
    framePG_2 <- tkframe(framePG)
    framePG_3 <- tkframe(framePG)
    framePG_4 <- tkframe(framePG)
    frameL_1 <- tkframe(frameL)
    frameL_2 <- tkframe(frameL)
    frameL_3 <- tkframe(frameL)
    frameL_4 <- tkframe(frameL)
    frameL_5 <- tkframe(frameL)

    tkgrid(tklabel(framePG_1, text = "Probabilistic genotyping", font = "Helvetica 10 bold"), pady = 5)

    selectHyp <- tclVar(hyps[1])
    comboHyp <- ttkcombobox(framePG_2, values = hyps, textvariable = selectHyp, state = "readonly", width = 35)
    tkgrid(tklabel(framePG_2, text = "Hypothesis : "), comboHyp, sticky = "w")

    selectL <- tclVar(cspLoci[1])
    comboLoci <- ttkcombobox(framePG_2, values = cspLoci, textvariable = selectL, state = "readonly", width = 35)
    tkgrid(tklabel(framePG_2, text = "Locus : "), comboLoci, sticky = "w")

    tkbind(comboHyp, "<<ComboboxSelected>>", function(){
      hyp <- tclvalue(selectHyp)
      lociID <- which(cspLoci == tclvalue(selectL))
      showPG_LR(hyp, lociID)
    })

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      hyp <- tclvalue(selectHyp)
      lociID <- which(cspLoci == tclvalue(selectL))
      showPG_LR(hyp, lociID)
    })

    buttGraph <- tkbutton(framePG_4, text = "    Graph    ", cursor = "hand2", command = function() graphPG("LR", envProj, envLR, dataLR, NULL, selectHyp, selectL, cspLoci))
    buttExport <- tkbutton(framePG_4, text = "    Export displayed data    ", cursor = "hand2", command = function() exportPG_LR(tclvalue(selectHyp), which(cspLoci == tclvalue(selectL))))
    buttExportAll <- tkbutton(framePG_4, text = "    Export all    ", cursor = "hand2", command = function() exportPGAll_LR())
    tkgrid(buttGraph, buttExport, buttExportAll, padx = 5, sticky = "w")
    showPG_LR(hyps[1], 1)

    tkgrid(framePG_1, pady = 5, sticky = "w")
    tkgrid(framePG_2, pady = 5, sticky = "w")
    tkgrid(framePG_3, pady = 5, sticky = "w")
    tkgrid(framePG_4, pady = 5, sticky = "w")

    tkgrid(tklabel(frameL_1, text = "Likelihood ratio", font = "Helvetica 10 bold"), pady = 5)
    selectHp <- tclVar(hypsHp[1])
    comboHp <- ttkcombobox(frameL_2, values = hypsHp, textvariable = selectHp, state = "readonly", width = 35)
    tkgrid(tklabel(frameL_2, text = "Prosecutor hypothesis (Hp) : "), comboHp, sticky = "w")
    selectHd <- tclVar(hypsHd[1])
    comboHd <- ttkcombobox(frameL_2, values = hypsHd, textvariable = selectHd, state = "readonly", width = 35)
    tkgrid(tklabel(frameL_2, text = "Defense hypothesis (Hd) : "), comboHd, sticky = "w")

    tkbind(comboHp, "<<ComboboxSelected>>", function(){
      hp <- tclvalue(selectHp)
      hd <- tclvalue(selectHd)
      showLR(hp, hd)
    })

    tkbind(comboHd, "<<ComboboxSelected>>", function(){
      hp <- tclvalue(selectHp)
      hd <- tclvalue(selectHd)
      showLR(hp, hd)
    })

    hp <- tclvalue(selectHp)
    hd <- tclvalue(selectHd)
    showLR(hp, hd)

    buttReport <- tkbutton(frameL_5, text = "    Report (displayed data)    ", cursor = "hand2", command = function() makeReport_LR(envProj, envGUI, envLR, tclvalue(selectHp), tclvalue(selectHd)))
    buttExportLikeAll <- tkbutton(frameL_5, text = "    Export all likelihoods    ", cursor = "hand2", command = function() exportLikeAll(envProj, envGUI, cspLoci, hyps))
    tkgrid(buttReport, buttExportLikeAll, padx = 5, pady = 5)

    tkgrid(frameL_1, pady = 5, sticky = "w")
    tkgrid(frameL_2, pady = 5, sticky = "w")
    tkgrid(frameL_3, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameL_4, pady = 5, sticky = "w")
    tkgrid(frameL_5)

    tkgrid(framePG, frameL, padx = 5, pady = 5, sticky = "nw")

    tk2notetab.select(subTabs_LR, "Result")
  }
  tkgrid(frameResult_LR)
  assign("frameResult_LR", frameResult_LR, envir = envGUI)
}

# Make a tab for LR
makeTabLR <- function(envProj, envGUI, tfRef = tktoplevel()){
  tkdestroy(tfRef)
  cspInput <- get("cspInput", pos = envProj)
  refInput <- get("refInput", pos = envProj)
  afInput <- get("afInput", pos = envProj)
  if(any(c(length(cspInput) == 0, length(refInput) == 0, length(afInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    inputData <- checkFile(envGUI, cspInput, refInput, afInput, meth = "LR")
    if(length(inputData) == 3){
      tabs <- get("tabs", pos = envGUI)
      frameLR <- get("frameLR", pos = envGUI)
      subTabs_LR <- get("subTabs_LR", pos = envGUI)
      tkpack(subTabs_LR, fill = "both")
      tkpack(frameLR, fill = "both", padx = 10, pady = 10)

      csp <- inputData[[1]]
      assign("csp", csp, envir = envProj)
      ref <- inputData[[2]]
      assign("ref", ref, envir = envProj)
      af <- inputData[[3]]
      assign("af", af, envir = envProj)

      makeSubTabProp_LR(envProj, envGUI)
      makeSubTabResult_LR(envProj, envGUI)
      tk2notetab.select(tabs, "Likelihood ratio")
    }
  }
}
