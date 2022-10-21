# Make a Files tab
makeTab1Par <- function(envParProj, envParGUI){
  openFilePar <- function(type){
    inputOk <- "ok"
    finEstPar <- get("finEstPar", pos = envParProj)
    if(finEstPar){
      inputOk <- tclvalue(tkmessageBox(message = "Estimated parameters will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      assign("finEstPar", FALSE, envir = envParProj)
      tab2Par <- get("tab2Par", pos = envParGUI)
      frameTab2Par <- get("frameTab2Par", pos = envParGUI)
      tkdestroy(frameTab2Par)
      assign("frameTab2Par", tkframe(tab2Par), envir = envParGUI)
      tab3Par <- get("tab3Par", pos = envParGUI)
      frameTab3Par <- get("frameTab3Par", pos = envParGUI)
      tkdestroy(frameTab3Par)
      assign("frameTab3Par", tkframe(tab3Par), envir = envParGUI)

      if(type == "ex"){
        fpVar <- exFpVar
        fnVar <- exFnVar
      }else if(type == "gt"){
        fpVar <- gtFpVar
        fnVar <- gtFnVar
      }else if(type == "seq"){
        fpVar <- seqFpVar
        fnVar <- seqFnVar
      }
      fileName <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
      if(!nchar(fileName)){
        tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
      }else{
        tmp <- sub("\\}", fileName, replacement = "")
        tmp2 <- sub("\\{", tmp, replacement = "")
        tclvalue(fpVar) <- tmp2
        foo3 <- strsplit(tmp2, "/")[[1]]
        tclvalue(fnVar) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
        if(type == "ex"){
          exData <- read.csv(tclvalue(fpVar), header = TRUE)
          exData <- as.matrix(exData)
          exData <- gsub(" ", "", exData, fixed = TRUE)
          assign("exData", exData, envir = envParProj)
          assign("exFp", tclvalue(fpVar), envir = envParProj)
          assign("exFn", tclvalue(fnVar), envir = envParProj)
        }else if(type == "gt"){
          gtData <- read.csv(tclvalue(fpVar), header = TRUE)
          gtData <- as.matrix(gtData)
          gtData <- gsub(" ", "", gtData, fixed = TRUE)
          assign("gtData", gtData, envir = envParProj)
          assign("gtFp", tclvalue(fpVar), envir = envParProj)
          assign("gtFn", tclvalue(fnVar), envir = envParProj)
        }else if(type == "seq"){
          seqData <- read.csv(tclvalue(fpVar), header = TRUE)
          seqData <- as.matrix(seqData)
          seqData[, "Marker"] <- gsub(" ", "", seqData[, "Marker"], fixed = TRUE)
          assign("seqData", seqData, envir = envParProj)
          assign("seqFp", tclvalue(fpVar), envir = envParProj)
          assign("seqFn", tclvalue(fnVar), envir = envParProj)
        }
      }
    }
  }

  tab1Par <- get("tab1Par", pos = envParGUI)
  frameTab1Par <- get("frameTab1Par", pos = envParGUI)
  tkdestroy(frameTab1Par)
  frameTab1Par <- tkframe(tab1Par)

  exFp <- get("exFp", pos = envParProj)
  exFpVar <- tclVar(exFp)
  exFn <- get("exFn", pos = envParProj)
  exFnVar <- tclVar(exFn)

  gtFp <- get("gtFp", pos = envParProj)
  gtFpVar <- tclVar(gtFp)
  gtFn <- get("gtFn", pos = envParProj)
  gtFnVar <- tclVar(gtFn)

  seqFp <- get("seqFp", pos = envParProj)
  seqFpVar <- tclVar(seqFp)
  seqFn <- get("seqFn", pos = envParProj)
  seqFnVar <- tclVar(seqFn)

  loadProjButt <- tkbutton(frameTab1Par, text = "    Load project    ", cursor = "hand2", command = function() loadProjPar(envParProj, envParGUI))

  labelEx <- tklabel(frameTab1Par, text = "Experimental Data")
  labelExFn <- tklabel(frameTab1Par, textvariable = exFnVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttEx <- tkbutton(frameTab1Par, text = "    Load    ", cursor = "hand2", command = function() openFilePar("ex"))

  labelGt <- tklabel(frameTab1Par, text = "Genotype Data")
  labelGtFn <- tklabel(frameTab1Par, textvariable = gtFnVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttGt <- tkbutton(frameTab1Par, text = "    Load    ", cursor = "hand2", command = function() openFilePar("gt"))

  labelSeq <- tklabel(frameTab1Par, text = "Sequence Data")
  labelSeqFn <- tklabel(frameTab1Par, textvariable = seqFnVar, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttSeq <- tkbutton(frameTab1Par, text = "    Load    ", cursor = "hand2", command = function() openFilePar("seq"))

  nextButt <- tkbutton(frameTab1Par, text = "    Next    ", cursor = "hand2", command = function() checkFilePar(envParProj, envParGUI))

  tkgrid(loadProjButt, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelEx, labelExFn, buttEx, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelGt, labelGtFn, buttGt, padx = 10, pady = 5, sticky = "w")
  tkgrid(labelSeq, labelSeqFn, buttSeq, padx = 10, pady = 5, sticky = "w")
  tkgrid(nextButt, padx = 10, pady = 5, sticky = "w")
  tkgrid(frameTab1Par)

  assign("frameTab1Par", frameTab1Par, envir = envParGUI)
}

# Check loci of an input file
checkInputLoci <- function(inputData){
  posSn <- intersect(grep("Sample", colnames(inputData)), grep("Name", colnames(inputData)))
  snAll <- unique(inputData[, posSn])
  sn <- snAll[1]
  inputLoci <- inputData[inputData[, posSn] == sn, "Marker"]
  outSn <- character(0)
  for(i in 2:length(snAll)){
    sn <- snAll[i]
    inputLoci2 <- inputData[inputData[, posSn] == sn, "Marker"]
    if(!setequal(inputLoci, inputLoci2)){
      outSn <- sn
      break
    }
  }
  return(outSn)
}

# Check files for estimating parameters
checkFilePar <- function(envParProj, envParGUI){
  exData <- get("exData", pos = envParProj)
  gtData <- get("gtData", pos = envParProj)
  seqData <- get("seqData", pos = envParProj)
  if(any(c(length(exData) == 0, length(gtData) == 0, length(seqData) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    outSn <- checkInputLoci(exData)
    if(length(outSn) == 0){
      exLoci <- unique(exData[, grep("Marker", colnames(exData))])
      pathPack <- get("pathPack", pos = envParGUI)
      exKit <- searchKit(exLoci, pathPack)
      if(length(exKit) > 0){
        kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", exKit[1], ".csv"), header = TRUE)
        kitInfo <- as.matrix(kitInfo)
        kitLoci <- unique(kitInfo[, "Marker"])
        kitLoci <- gsub(" ", "", kitLoci, fixed = TRUE)
        posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
        if(length(posSexMar) > 0){
          sexMar <- unique(kitInfo[posSexMar, "Marker"])
          parLoci <- setdiff(kitLoci, sexMar)
        }else{
          parLoci <- kitLoci
          sexMar <- character(0)
        }
        outSn <- checkInputLoci(gtData)
        if(length(outSn) == 0){
          gtLoci <- unique(gtData[, grep("Marker", colnames(gtData))])
          if(setequal(exLoci, gtLoci)){
            seqLoci <- unique(seqData[, which(colnames(seqData) == "Marker")])
            if(all(is.element(parLoci, seqLoci))){
              exSampleName <- unique(exData[, intersect(grep("Sample", colnames(exData)), grep("Name", colnames(exData)))])
              gtSampleName <- unique(gtData[, intersect(grep("Sample", colnames(gtData)), grep("Name", colnames(gtData)))])
              if(setequal(exSampleName, gtSampleName)){
                assign("exKit", exKit, envir = envParProj)
                assign("kitLoci", kitLoci, envir = envParProj)
                assign("parLoci", parLoci, envir = envParProj)
                assign("sexMar", sexMar, envir = envParProj)
                makeTab2Par(envParProj, envParGUI)
                tabsPar <- get("tabsPar", pos = envParGUI)
                tk2notetab.select(tabsPar, "Setting")
                fincheckFilePar <- TRUE
              }else{
                tkmessageBox(message = "sample names of the experimental data and the genotype data are not the same!", icon = "error", type = "ok")
              }
            }else{
              tkmessageBox(message = "Some loci of the experimental data are not included in the sequence data!", icon = "error", type = "ok")
            }
          }else{
            tkmessageBox(message = "Locus set of the experimental data is not equal to that of genotype data!", icon = "error", type = "ok")
          }
        }else{
          tkmessageBox(message = paste0("Locus set of the genotype data is not the same between samples. Please check the sample '", outSn, "'."), icon = "error", type = "ok")
        }
      }else{
        tkmessageBox(message = "Locus set of the experimental data has not been registered. Please register locus set at Tools -> Typing kit.", icon = "error", type = "ok")
      }
    }else{
      tkmessageBox(message = paste0("Locus set of the experimental data is not the same between samples. Please check the sample '", outSn, "'."), icon = "error", type = "ok")
    }
  }
}

# Make a Setting tab
makeTab2Par <- function(envParProj, envParGUI){
  #Link a comboMethSR and a comboModelSR
  comboBind <- function(comboMethSR, comboModelSR, methSRVar, modelSRVar){
    tkbind(comboMethSR, "<<ComboboxSelected>>", function(){
      if(tclvalue(methSRVar) == "Locus specific"){
        tkconfigure(comboModelSR, textvariable = modelSRVar, state = "readonly")
      }else{
        tkconfigure(comboModelSR, textvariable = tclVar(""), state = "disable")
      }
    })
  }

  setMLE <- function(nameMleCondVar, nameL, nameParentF, nameChildF){
    mleCondVar <- get(nameMleCondVar, pos = envParGUI)
    parentF <- get(nameParentF, pos = envParGUI)
    childF <- get(nameChildF, pos = envParGUI)
    tkdestroy(childF)
    childF <- tkframe(parentF)
    tkgrid(tklabel(childF, text = "Model"), row = 0, column = 0, padx = 10, pady = 2)
    tkgrid(tklabel(childF, text = "Parameter"), row = 0, column = 1, padx = 10, pady = 2)
    tkgrid(tklabel(childF, text = "Initial value"), row = 0, column = 2, padx = 10, pady = 2)
    tkgrid(tklabel(childF, text = "Lower value"), row = 0, column = 3, padx = 10, pady = 2)
    tkgrid(tklabel(childF, text = "Upper value"), row = 0, column = 4, padx = 10, pady = 2)

    mleCondVar_oneL <- mleCondVar[[which(names(mleCondVar) == nameL)]]
    models <- names(mleCondVar_oneL)
    countRow <- 1
    for(i in 1:length(models)){
      mleCondVar_oneM <- mleCondVar_oneL[[i]]
      params <- names(mleCondVar_oneM)
      tkgrid(tklabel(childF, text = models[i]), row = countRow, column = 0, padx = 10, pady = 1)
      for(j in 1:length(params)){
        mleCondVar_oneP <- mleCondVar_oneM[[j]]
        tkgrid(tklabel(childF, text = params[j]), row = countRow, column = 1, padx = 10, pady = 1)
        entryInitial <- tkentry(childF, textvariable = mleCondVar_oneP[[1]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryInitial, row = countRow, column = 2, padx = 10, pady = 1)
        entryLower <- tkentry(childF, textvariable = mleCondVar_oneP[[2]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryLower, row = countRow, column = 3, padx = 10, pady = 1)
        entryUpper <- tkentry(childF, textvariable = mleCondVar_oneP[[3]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryUpper, row = countRow, column = 4, padx = 10, pady = 1)
        countRow <- countRow + 1
      }
    }
    tkgrid(childF, sticky = "w")
    assign(nameChildF, childF, envir = envParGUI)
  }

  setGen <- function(){
    frameGen_1 <- tkframe(subTabGen, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameGen_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameGen_1, text = "Min. threshold", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)

    nL <- length(kitLoci)
    mt_kitLoci <- get("mt_kitLoci", pos = envParProj)
    mtDefault <- get("mtDefault", pos = envParProj)
    if(length(mt_kitLoci) == 0){
      mt_kitLoci <- rep(mtDefault, nL)
    }

    mtVar <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameGen_1, text = kitLoci[i])
      mtVar[[i]] <- tclVar(mt_kitLoci[i])
      entryMT <- tkentry(frameGen_1, textvariable = mtVar[[i]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      tkgrid(labelLocus, entryMT, padx = 5, pady = 1)
    }

    frameGen_right <- tkframe(subTabGen)
    frameGen_2 <- tkframe(frameGen_right, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameGen_2, text = "Kit", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    selectKit <- get("selectKit", pos = envParProj)
    if(length(selectKit) == 0){
      selectKit <- exKit[1]
    }
    kitParVar <- tclVar(selectKit)
    tkgrid(ttkcombobox(frameGen_2, values = exKit, textvariable = kitParVar, width = 20, state = "readonly"), padx = 5, pady = 1)

    frameGen_3 <- tkframe(frameGen_right, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameGen_3, text = "Sex chromosomal markers", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    tkgrid(tklabel(frameGen_3, text = paste(sexMar, collapse = ", ")), padx = 5, pady = 1)

    tkgrid(frameGen_2, padx = 5, sticky = "w")
    tkgrid(frameGen_3, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameGen_1, frameGen_right, padx = 5, pady = 5, sticky = "n")

    assign("mtVar", mtVar, envir = envParGUI)
    assign("kitParVar", kitParVar, envir = envParGUI)
  }

  setAE <- function(){
    changeMinAE <- function(){
      if(tclvalue(minAEMethVar) == "1"){
        tkconfigure(entryMinAE, state = "disabled")
      }else{
        tkconfigure(entryMinAE, state = "normal")
      }
    }

    frameAE_1 <- tkframe(subTabAE, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameAE_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameAE_1, text = "Model", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candAE[, grep("Cand", colnames(candAE)), drop = FALSE]
    modelDefault <- candAE[, colnames(candAE) == "Default"]
    modelAE <- get("modelAE", pos = envParProj)
    if(length(modelAE) == 0){
      modelAE <- modelDefault
    }
    modelAEVar <- list()
    for(i in 1:nL){
      nameL <- parLoci[i]
      labelLocus <- tklabel(frameAE_1, text = nameL)
      modelAEVar[[i]] <- tclVar(modelAE[i])
      comboModel <- ttkcombobox(frameAE_1, values = modelCand[i, ], textvariable = modelAEVar[[i]], width = 12, state = "readonly")
      tkgrid(labelLocus, comboModel, padx = 5, pady = 1)
    }

    frameAE_right <- tkframe(subTabAE)

    frameAE_2 <- tkframe(frameAE_right, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameAE_2, text = "Min. AE", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    minAEMeth <- get("minAEMeth", pos = envParProj)
    minAEMethVar <- tclVar(minAEMeth)
    ckMinAE <- tkcheckbutton(frameAE_2, text = "Based on experimental data", variable = minAEMethVar, command = function() changeMinAE())
    tkgrid(ckMinAE, padx = 5, pady = 1)
    minAE <- get("minAE", pos = envParProj)
    minAEVar <- tclVar(minAE)
    entryMinAE <- tkentry(frameAE_2, textvariable = minAEVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMinAE()
    tkgrid(entryMinAE, padx = 5, pady = 1)

    frameAE_3 <- tkframe(frameAE_right, relief = "groove", borderwidth = 2)
    frameAE_3_1 <- tkframe(frameAE_3)
    frameAE_3_2 <- tkframe(frameAE_3)
    assign("frameAE_3", frameAE_3, envir = envParGUI)
    assign("frameAE_3_2", frameAE_3_2, envir = envParGUI)

    tkgrid(tklabel(frameAE_3, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameAE_3_1, values = parLoci, textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameAE_3_1, text = "Locus : "), comboLoci, padx = 5, pady = 4, sticky = "w")
    tkgrid(frameAE_3_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondAEVar", tclvalue(selectL), "frameAE_3", "frameAE_3_2")
    })
    setMLE("mleCondAEVar", tclvalue(selectL), "frameAE_3", "frameAE_3_2")

    tkgrid(frameAE_2, sticky = "w")
    tkgrid(frameAE_3, pady = 5, sticky = "w")
    tkgrid(frameAE_1, frameAE_right, padx = 5, pady = 5, sticky = "n")

    assign("modelAEVar", modelAEVar, envir = envParGUI)
    assign("minAEMethVar", minAEMethVar, envir = envParGUI)
    assign("minAEVar", minAEVar, envir = envParGUI)
  }

  setHb <- function(){
    changeMinHb <- function(){
      if(tclvalue(minHbMethVar) == "1"){
        tkconfigure(entryMinHb, state = "disabled")
      }else{
        tkconfigure(entryMinHb, state = "normal")
      }
    }

    frameHb_1 <- tkframe(subTabHb, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameHb_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameHb_1, text = "Model", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candHb[, grep("Cand", colnames(candHb)), drop = FALSE]
    modelDefault <- candHb[, colnames(candHb) == "Default"]
    modelHb <- get("modelHb", pos = envParProj)
    if(length(modelHb) == 0){
      modelHb <- modelDefault
    }
    modelHbVar <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameHb_1, text = parLoci[i])
      modelHbVar[[i]] <- tclVar(modelHb[i])
      comboModel <- ttkcombobox(frameHb_1, values = modelCand[i, ], textvariable = modelHbVar[[i]], width = 12, state = "readonly")
      tkgrid(labelLocus, comboModel, padx = 5, pady = 1)
    }

    frameHb_right <- tkframe(subTabHb)

    frameHb_2 <- tkframe(frameHb_right, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameHb_2, text = "Min. Hb", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    minHbMeth <- get("minHbMeth", pos = envParProj)
    minHbMethVar <- tclVar(minHbMeth)
    ckMinHb <- tkcheckbutton(frameHb_2, text = "Based on experimental data", variable = minHbMethVar, command = function() changeMinHb())
    tkgrid(ckMinHb, padx = 5, pady = 1)
    minHb <- get("minHb", pos = envParProj)
    minHbVar <- tclVar(minHb)
    entryMinHb <- tkentry(frameHb_2, textvariable = minHbVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMinHb()
    tkgrid(entryMinHb, padx = 5, pady = 1)

    frameHb_3 <- tkframe(frameHb_right, relief = "groove", borderwidth = 2)
    frameHb_3_1 <- tkframe(frameHb_3)
    frameHb_3_2 <- tkframe(frameHb_3)
    assign("frameHb_3", frameHb_3, envir = envParGUI)
    assign("frameHb_3_2", frameHb_3_2, envir = envParGUI)

    tkgrid(tklabel(frameHb_3, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameHb_3_1, values = parLoci, textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameHb_3_1, text = "Locus : "), comboLoci, padx = 5, pady = 2, sticky = "w")
    tkgrid(frameHb_3_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondHbVar", tclvalue(selectL), "frameHb_3", "frameHb_3_2")
    })
    setMLE("mleCondHbVar", tclvalue(selectL), "frameHb_3", "frameHb_3_2")

    tkgrid(frameHb_2, sticky = "w")
    tkgrid(frameHb_3, pady = 5, sticky = "w")
    tkgrid(frameHb_1, frameHb_right, padx = 5, pady = 5, sticky = "n")

    assign("modelHbVar", modelHbVar, envir = envParGUI)
    assign("minHbMethVar", minHbMethVar, envir = envParGUI)
    assign("minHbVar", minHbVar, envir = envParGUI)
  }

  setBSR <- function(){
    changeMaxBSR <- function(){
      if(tclvalue(maxBSRMethVar) == "1"){
        tkconfigure(entryMaxBSR, state = "disabled")
      }else{
        tkconfigure(entryMaxBSR, state = "normal")
      }
    }

    frameBSR_1 <- tkframe(subTabBSR, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameBSR_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameBSR_1, text = "Model", font = "Helvetica 10 bold"),
           tklabel(frameBSR_1, text = "Method", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candBSR[, grep("Cand", colnames(candBSR)), drop = FALSE]
    modelDefault <- candBSR[, colnames(candBSR) == "Default"]
    methDefault <- candBSR[, colnames(candBSR) == "Meth"]
    modelBSR <- get("modelBSR", pos = envParProj)
    if(length(modelBSR) == 0){
      modelBSR <- modelDefault
    }
    methBSR <- get("methBSR", pos = envParProj)
    if(length(methBSR) == 0){
      methBSR <- methDefault
    }
    modelBSRVar <- comboModelBSR <- methBSRVar <- comboMethBSR <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameBSR_1, text = parLoci[i])
      methBSRVar[[i]] <- tclVar(methBSR[i])
      comboMethBSR[[i]] <- ttkcombobox(frameBSR_1, values = c("Locus specific", "Multiple loci together", "Not consider"), textvariable = methBSRVar[[i]], width = 20, state = "readonly")
      modelBSRVar[[i]] <- tclVar(modelBSR[i])
      if(tclvalue(methBSRVar[[i]]) == "Locus specific"){
        comboModelBSR[[i]] <- ttkcombobox(frameBSR_1, values = modelCand[i, ], textvariable = modelBSRVar[[i]], width = 12, state = "readonly")
      }else{
        comboModelBSR[[i]] <- ttkcombobox(frameBSR_1, values = modelCand[i, ], textvariable = tclVar(""), width = 12, state = "disable")
      }
      tkgrid(labelLocus, comboModelBSR[[i]], comboMethBSR[[i]], padx = 5, pady = 1)
    }

    frameBSR_right <- tkframe(subTabBSR)
    frameBSR_23 <- tkframe(frameBSR_right)

    frameBSR_2 <- tkframe(frameBSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameBSR_2, text = "Model when considering multiple loci together", font = "Helvetica 10 bold"), pady = 2)
    modelMultiBSR <- get("modelMultiBSR", pos = envParProj)
    if(length(modelMultiBSR) == 0){
      modelMultiBSR <- modelDefault[nL + 1]
    }
    modelMultiBSRVar <- tclVar(modelMultiBSR)
    tkgrid(ttkcombobox(frameBSR_2, values = modelCand[nL + 1, ], textvariable = modelMultiBSRVar, width = 12, state = "readonly"), pady = 1)

    frameBSR_3 <- tkframe(frameBSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameBSR_3, text = "Max. BSR", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    maxBSRMeth <- get("maxBSRMeth", pos = envParProj)
    maxBSRMethVar <- tclVar(maxBSRMeth)
    ckMaxBSR <- tkcheckbutton(frameBSR_3, text = "Based on experimental data", variable = maxBSRMethVar, command = function() changeMaxBSR())
    tkgrid(ckMaxBSR, padx = 5, pady = 1)
    maxBSR <- get("maxBSR", pos = envParProj)
    maxBSRVar <- tclVar(maxBSR)
    entryMaxBSR <- tkentry(frameBSR_3, textvariable = maxBSRVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMaxBSR()
    tkgrid(entryMaxBSR, padx = 5, pady = 1)

    frameBSR_4 <- tkframe(frameBSR_right, relief = "groove", borderwidth = 2)
    frameBSR_4_1 <- tkframe(frameBSR_4)
    frameBSR_4_2 <- tkframe(frameBSR_4)
    assign("frameBSR_4", frameBSR_4, envir = envParGUI)
    assign("frameBSR_4_2", frameBSR_4_2, envir = envParGUI)

    tkgrid(tklabel(frameBSR_4, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameBSR_4_1, values = c(parLoci, "Multiple loci together"), textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameBSR_4_1, text = "Locus : "), comboLoci, padx = 5, pady = 2, sticky = "w")
    tkgrid(frameBSR_4_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondBSRVar", tclvalue(selectL), "frameBSR_4", "frameBSR_4_2")
    })
    setMLE("mleCondBSRVar", tclvalue(selectL), "frameBSR_4", "frameBSR_4_2")

    tkgrid(frameBSR_2, frameBSR_3, padx = 5, sticky = "nw")
    tkgrid(frameBSR_23, sticky = "w")
    tkgrid(frameBSR_4, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameBSR_1, frameBSR_right, padx = 5, pady = 5, sticky = "n")

    assign("modelBSRVar", modelBSRVar, envir = envParGUI)
    assign("comboModelBSR", comboModelBSR, envir = envParGUI)
    assign("methBSRVar", methBSRVar, envir = envParGUI)
    assign("comboMethBSR", comboMethBSR, envir = envParGUI)
    assign("modelMultiBSRVar", modelMultiBSRVar, envir = envParGUI)
    assign("maxBSRMethVar", maxBSRMethVar, envir = envParGUI)
    assign("maxBSRVar", maxBSRVar, envir = envParGUI)
  }

  setFSR <- function(){
    changeMaxFSR <- function(){
      if(tclvalue(maxFSRMethVar) == "1"){
        tkconfigure(entryMaxFSR, state = "disabled")
      }else{
        tkconfigure(entryMaxFSR, state = "normal")
      }
    }

    frameFSR_1 <- tkframe(subTabFSR, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameFSR_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameFSR_1, text = "Model", font = "Helvetica 10 bold"),
           tklabel(frameFSR_1, text = "Method", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candFSR[, grep("Cand", colnames(candFSR)), drop = FALSE]
    modelDefault <- candFSR[, colnames(candFSR) == "Default"]
    methDefault <- candFSR[, colnames(candFSR) == "Meth"]
    modelFSR <- get("modelFSR", pos = envParProj)
    if(length(modelFSR) == 0){
      modelFSR <- modelDefault
    }
    methFSR <- get("methFSR", pos = envParProj)
    if(length(methFSR) == 0){
      methFSR <- methDefault
    }
    modelFSRVar <- comboModelFSR <- methFSRVar <- comboMethFSR <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameFSR_1, text = parLoci[i])
      methFSRVar[[i]] <- tclVar(methFSR[i])
      comboMethFSR[[i]] <- ttkcombobox(frameFSR_1, values = c("Locus specific", "Multiple loci together", "Not consider"), textvariable = methFSRVar[[i]], width = 20, state = "readonly")
      modelFSRVar[[i]] <- tclVar(modelFSR[i])
      if(tclvalue(methFSRVar[[i]]) == "Locus specific"){
        comboModelFSR[[i]] <- ttkcombobox(frameFSR_1, values = modelCand[i, ], textvariable = modelFSRVar[[i]], width = 12, state = "readonly")
      }else{
        comboModelFSR[[i]] <- ttkcombobox(frameFSR_1, values = modelCand[i, ], textvariable = tclVar(""), width = 12, state = "disable")
      }
      tkgrid(labelLocus, comboModelFSR[[i]], comboMethFSR[[i]], padx = 5, pady = 1)
    }

    frameFSR_right <- tkframe(subTabFSR)
    frameFSR_23 <- tkframe(frameFSR_right)

    frameFSR_2 <- tkframe(frameFSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameFSR_2, text = "Model when considering multiple loci together", font = "Helvetica 10 bold"), pady = 2)
    modelMultiFSR <- get("modelMultiFSR", pos = envParProj)
    if(length(modelMultiFSR) == 0){
      modelMultiFSR <- modelDefault[nL + 1]
    }
    modelMultiFSRVar <- tclVar(modelMultiFSR)
    tkgrid(ttkcombobox(frameFSR_2, values = modelCand[nL + 1, ], textvariable = modelMultiFSRVar, width = 12, state = "readonly"), pady = 1)

    frameFSR_3 <- tkframe(frameFSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameFSR_3, text = "Max. FSR", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    maxFSRMeth <- get("maxFSRMeth", pos = envParProj)
    maxFSRMethVar <- tclVar(maxFSRMeth)
    ckMaxFSR <- tkcheckbutton(frameFSR_3, text = "Based on experimental data", variable = maxFSRMethVar, command = function() changeMaxFSR())
    tkgrid(ckMaxFSR, padx = 5, pady = 1)
    maxFSR <- get("maxFSR", pos = envParProj)
    maxFSRVar <- tclVar(maxFSR)
    entryMaxFSR <- tkentry(frameFSR_3, textvariable = maxFSRVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMaxFSR()
    tkgrid(entryMaxFSR, padx = 5, pady = 1)

    frameFSR_4 <- tkframe(frameFSR_right, relief = "groove", borderwidth = 2)
    frameFSR_4_1 <- tkframe(frameFSR_4)
    frameFSR_4_2 <- tkframe(frameFSR_4)
    assign("frameFSR_4", frameFSR_4, envir = envParGUI)
    assign("frameFSR_4_2", frameFSR_4_2, envir = envParGUI)

    tkgrid(tklabel(frameFSR_4, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameFSR_4_1, values = c(parLoci, "Multiple loci together"), textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameFSR_4_1, text = "Locus : "), comboLoci, padx = 5, pady = 2, sticky = "w")
    tkgrid(frameFSR_4_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondFSRVar", tclvalue(selectL), "frameFSR_4", "frameFSR_4_2")
    })
    setMLE("mleCondFSRVar", tclvalue(selectL), "frameFSR_4", "frameFSR_4_2")

    tkgrid(frameFSR_2, frameFSR_3, padx = 5, sticky = "w")
    tkgrid(frameFSR_23, sticky = "w")
    tkgrid(frameFSR_4, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameFSR_1, frameFSR_right, padx = 5, pady = 5, sticky = "n")

    assign("modelFSRVar", modelFSRVar, envir = envParGUI)
    assign("comboModelFSR", comboModelFSR, envir = envParGUI)
    assign("methFSRVar", methFSRVar, envir = envParGUI)
    assign("comboMethFSR", comboMethFSR, envir = envParGUI)
    assign("modelMultiFSRVar", modelMultiFSRVar, envir = envParGUI)
    assign("maxFSRMethVar", maxFSRMethVar, envir = envParGUI)
    assign("maxFSRVar", maxFSRVar, envir = envParGUI)
  }

  setDSR <- function(){
    changeMaxDSR <- function(){
      if(tclvalue(maxDSRMethVar) == "1"){
        tkconfigure(entryMaxDSR, state = "disabled")
      }else{
        tkconfigure(entryMaxDSR, state = "normal")
      }
    }

    frameDSR_1 <- tkframe(subTabDSR, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameDSR_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameDSR_1, text = "Model", font = "Helvetica 10 bold"),
           tklabel(frameDSR_1, text = "Method", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candDSR[, grep("Cand", colnames(candDSR)), drop = FALSE]
    modelDefault <- candDSR[, colnames(candDSR) == "Default"]
    methDefault <- candDSR[, colnames(candDSR) == "Meth"]
    modelDSR <- get("modelDSR", pos = envParProj)
    if(length(modelDSR) == 0){
      modelDSR <- modelDefault
    }
    methDSR <- get("methDSR", pos = envParProj)
    if(length(methDSR) == 0){
      methDSR <- methDefault
    }
    modelDSRVar <- comboModelDSR <- methDSRVar <- comboMethDSR <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameDSR_1, text = parLoci[i])
      methDSRVar[[i]] <- tclVar(methDSR[i])
      comboMethDSR[[i]] <- ttkcombobox(frameDSR_1, values = c("Locus specific", "Multiple loci together", "Not consider"), textvariable = methDSRVar[[i]], width = 20, state = "readonly")
      modelDSRVar[[i]] <- tclVar(modelDSR[i])
      if(tclvalue(methDSRVar[[i]]) == "Locus specific"){
        comboModelDSR[[i]] <- ttkcombobox(frameDSR_1, values = modelCand[i, ], textvariable = modelDSRVar[[i]], width = 12, state = "readonly")
      }else{
        comboModelDSR[[i]] <- ttkcombobox(frameDSR_1, values = modelCand[i, ], textvariable = tclVar(""), width = 12, state = "disable")
      }
      tkgrid(labelLocus, comboModelDSR[[i]], comboMethDSR[[i]], padx = 5, pady = 1)
    }

    frameDSR_right <- tkframe(subTabDSR)
    frameDSR_23 <- tkframe(frameDSR_right)

    frameDSR_2 <- tkframe(frameDSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameDSR_2, text = "Model when considering multiple loci together", font = "Helvetica 10 bold"), pady = 2)
    modelMultiDSR <- get("modelMultiDSR", pos = envParProj)
    if(length(modelMultiDSR) == 0){
      modelMultiDSR <- modelDefault[nL + 1]
    }
    modelMultiDSRVar <- tclVar(modelMultiDSR)
    tkgrid(ttkcombobox(frameDSR_2, values = modelCand[nL + 1, ], textvariable = modelMultiDSRVar, width = 12, state = "readonly"), pady = 1)

    frameDSR_3 <- tkframe(frameDSR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameDSR_3, text = "Max. DSR", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    maxDSRMeth <- get("maxDSRMeth", pos = envParProj)
    maxDSRMethVar <- tclVar(maxDSRMeth)
    ckMaxDSR <- tkcheckbutton(frameDSR_3, text = "Based on experimental data", variable = maxDSRMethVar, command = function() changeMaxDSR())
    tkgrid(ckMaxDSR, padx = 5, pady = 1)
    maxDSR <- get("maxDSR", pos = envParProj)
    maxDSRVar <- tclVar(maxDSR)
    entryMaxDSR <- tkentry(frameDSR_3, textvariable = maxDSRVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMaxDSR()
    tkgrid(entryMaxDSR, padx = 5, pady = 1)

    frameDSR_4 <- tkframe(frameDSR_right, relief = "groove", borderwidth = 2)
    frameDSR_4_1 <- tkframe(frameDSR_4)
    frameDSR_4_2 <- tkframe(frameDSR_4)
    assign("frameDSR_4", frameDSR_4, envir = envParGUI)
    assign("frameDSR_4_2", frameDSR_4_2, envir = envParGUI)

    tkgrid(tklabel(frameDSR_4, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameDSR_4_1, values = c(parLoci, "Multiple loci together"), textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameDSR_4_1, text = "Locus : "), comboLoci, padx = 5, pady = 2, sticky = "w")
    tkgrid(frameDSR_4_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondDSRVar", tclvalue(selectL), "frameDSR_4", "frameDSR_4_2")
    })
    setMLE("mleCondDSRVar", tclvalue(selectL), "frameDSR_4", "frameDSR_4_2")

    tkgrid(frameDSR_2, frameDSR_3, padx = 5, sticky = "w")
    tkgrid(frameDSR_23, sticky = "w")
    tkgrid(frameDSR_4, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameDSR_1, frameDSR_right, padx = 5, pady = 5, sticky = "n")

    assign("modelDSRVar", modelDSRVar, envir = envParGUI)
    assign("comboModelDSR", comboModelDSR, envir = envParGUI)
    assign("methDSRVar", methDSRVar, envir = envParGUI)
    assign("comboMethDSR", comboMethDSR, envir = envParGUI)
    assign("modelMultiDSRVar", modelMultiDSRVar, envir = envParGUI)
    assign("maxDSRMethVar", maxDSRMethVar, envir = envParGUI)
    assign("maxDSRVar", maxDSRVar, envir = envParGUI)
  }

  setM2SR <- function(){
    changeMaxM2SR <- function(){
      if(tclvalue(maxM2SRMethVar) == "1"){
        tkconfigure(entryMaxM2SR, state = "disabled")
      }else{
        tkconfigure(entryMaxM2SR, state = "normal")
      }
    }

    frameM2SR_1 <- tkframe(subTabM2SR, relief = "groove", borderwidth = 2)
    nL <- length(parLoci)
    tkgrid(tklabel(frameM2SR_1, text = "Marker", font = "Helvetica 10 bold"),
           tklabel(frameM2SR_1, text = "Model", font = "Helvetica 10 bold"),
           tklabel(frameM2SR_1, text = "Method", font = "Helvetica 10 bold"),
           padx = 5, pady = 2)
    modelCand <- candM2SR[, grep("Cand", colnames(candM2SR)), drop = FALSE]
    modelDefault <- candM2SR[, colnames(candM2SR) == "Default"]
    methDefault <- candM2SR[, colnames(candM2SR) == "Meth"]
    modelM2SR <- get("modelM2SR", pos = envParProj)
    if(length(modelM2SR) == 0){
      modelM2SR <- modelDefault
    }
    methM2SR <- get("methM2SR", pos = envParProj)
    if(length(methM2SR) == 0){
      methM2SR <- methDefault
    }
    modelM2SRVar <- comboModelM2SR <- methM2SRVar <- comboMethM2SR <- list()
    for(i in 1:nL){
      labelLocus <- tklabel(frameM2SR_1, text = parLoci[i])
      methM2SRVar[[i]] <- tclVar(methM2SR[i])
      comboMethM2SR[[i]] <- ttkcombobox(frameM2SR_1, values = c("Locus specific", "Multiple loci together", "Not consider"), textvariable = methM2SRVar[[i]], width = 20, state = "readonly")
      modelM2SRVar[[i]] <- tclVar(modelM2SR[i])
      if(tclvalue(methM2SRVar[[i]]) == "Locus specific"){
        comboModelM2SR[[i]] <- ttkcombobox(frameM2SR_1, values = modelCand[i, ], textvariable = modelM2SRVar[[i]], width = 12, state = "readonly")
      }else{
        comboModelM2SR[[i]] <- ttkcombobox(frameM2SR_1, values = modelCand[i, ], textvariable = tclVar(""), width = 12, state = "disable")
      }
      tkgrid(labelLocus, comboModelM2SR[[i]], comboMethM2SR[[i]], padx = 5, pady = 1)
    }

    frameM2SR_right <- tkframe(subTabM2SR)
    frameM2SR_23 <- tkframe(frameM2SR_right)

    frameM2SR_2 <- tkframe(frameM2SR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameM2SR_2, text = "Model when considering multiple loci together", font = "Helvetica 10 bold"), pady = 2)
    modelMultiM2SR <- get("modelMultiM2SR", pos = envParProj)
    if(length(modelMultiM2SR) == 0){
      modelMultiM2SR <- modelDefault[nL + 1]
    }
    modelMultiM2SRVar <- tclVar(modelMultiM2SR)
    tkgrid(ttkcombobox(frameM2SR_2, values = modelCand[nL + 1, ], textvariable = modelMultiM2SRVar, width = 12, state = "readonly"), pady = 1)

    frameM2SR_3 <- tkframe(frameM2SR_23, relief = "groove", borderwidth = 2)
    tkgrid(tklabel(frameM2SR_3, text = "Max. M2SR", font = "Helvetica 10 bold"), padx = 5, pady = 2)
    maxM2SRMeth <- get("maxM2SRMeth", pos = envParProj)
    maxM2SRMethVar <- tclVar(maxM2SRMeth)
    ckMaxM2SR <- tkcheckbutton(frameM2SR_3, text = "Based on experimental data", variable = maxM2SRMethVar, command = function() changeMaxM2SR())
    tkgrid(ckMaxM2SR, padx = 5, pady = 1)
    maxM2SR <- get("maxM2SR", pos = envParProj)
    maxM2SRVar <- tclVar(maxM2SR)
    entryMaxM2SR <- tkentry(frameM2SR_3, textvariable = maxM2SRVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disabled")
    changeMaxM2SR()
    tkgrid(entryMaxM2SR, padx = 5, pady = 1)

    frameM2SR_4 <- tkframe(frameM2SR_right, relief = "groove", borderwidth = 2)
    frameM2SR_4_1 <- tkframe(frameM2SR_4)
    frameM2SR_4_2 <- tkframe(frameM2SR_4)
    assign("frameM2SR_4", frameM2SR_4, envir = envParGUI)
    assign("frameM2SR_4_2", frameM2SR_4_2, envir = envParGUI)

    tkgrid(tklabel(frameM2SR_4, text = "Conditions of MLE", font = "Helvetica 10 bold"), pady = 2)

    selectL <- tclVar(parLoci[1])
    comboLoci <- ttkcombobox(frameM2SR_4_1, values = c(parLoci, "Multiple loci together"), textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameM2SR_4_1, text = "Locus : "), comboLoci, padx = 5, pady = 2, sticky = "w")
    tkgrid(frameM2SR_4_1, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      setMLE("mleCondM2SRVar", tclvalue(selectL), "frameM2SR_4", "frameM2SR_4_2")
    })
    setMLE("mleCondM2SRVar", tclvalue(selectL), "frameM2SR_4", "frameM2SR_4_2")

    tkgrid(frameM2SR_2, frameM2SR_3, padx = 5, sticky = "w")
    tkgrid(frameM2SR_23, sticky = "w")
    tkgrid(frameM2SR_4, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameM2SR_1, frameM2SR_right, padx = 5, pady = 5, sticky = "n")

    assign("modelM2SRVar", modelM2SRVar, envir = envParGUI)
    assign("comboModelM2SR", comboModelM2SR, envir = envParGUI)
    assign("methM2SRVar", methM2SRVar, envir = envParGUI)
    assign("comboMethM2SR", comboMethM2SR, envir = envParGUI)
    assign("modelMultiM2SRVar", modelMultiM2SRVar, envir = envParGUI)
    assign("maxM2SRMethVar", maxM2SRMethVar, envir = envParGUI)
    assign("maxM2SRVar", maxM2SRVar, envir = envParGUI)
  }

  extCand <- function(candDefault, parLoci, multi = FALSE){
    strDefault <- rownames(candDefault)
    posGen <- which(strDefault == "General")
    nL <- length(parLoci)
    nCol <- ncol(candDefault)
    if(multi){
      cand <- matrix("", nL + 1, nCol)
      rownames(cand) <- c(parLoci, "Multiple loci")
      cand[nL + 1, ] <- candDefault[posGen, ]
    }else{
      cand <- matrix("", nL, nCol)
      rownames(cand) <- parLoci
    }
    colnames(cand) <- colnames(candDefault)
    for(i in 1:nL){
      posL <- which(strDefault == parLoci[i])
      if(length(posL) == 1){
        cand[i, ] <- candDefault[posL, ]
      }else{
        cand[i, ] <- candDefault[posGen, ]
      }
    }
    return(cand)
  }

  extMleCond <- function(mleCondDefault, parLoci){
    mleCond <- NULL
    for(i in 1:length(parLoci)){
      posL <- which(mleCondDefault[, 1] == parLoci[i])
      if(length(posL) == 0){
        posL <- which(mleCondDefault[, 1] == "General")
        bindData <- mleCondDefault[posL, ]
        bindData[, 1] <- parLoci[i]
      }else{
        bindData <- mleCondDefault[posL, ]
      }
      mleCond <- rbind(mleCond, bindData)
    }
    posGen <- which(mleCondDefault[, 1] == "General")
    bindData <- mleCondDefault[posGen, ]
    bindData[, 1] <- "Multiple loci together"
    mleCond <- rbind(mleCond, bindData)

    colnames(mleCond) <- colnames(mleCondDefault)
    return(mleCond)
  }

  tclMleCond <- function(mleCond){
    tclObj <- list()
    nameLoci <- unique(mleCond[, 1])
    for(i in 1:length(nameLoci)){
      tclObj_oneL <- list()
      mleCondOneL <- mleCond[mleCond[, 1] == nameLoci[i], , drop = FALSE]
      nameM <- unique(mleCondOneL[, 2])
      for(j in 1:length(nameM)){
        tclObj_oneM <- list()
        mleCondOneM <- mleCondOneL[mleCondOneL[, 2] == nameM[j], , drop = FALSE]
        nameP <- unique(mleCondOneM[, 3])
        for(k in 1:length(nameP)){
          tclObj_oneP <- list()
          mleCondOneP <- mleCondOneM[mleCondOneM[, 3] == nameP[k], , drop = FALSE]
          for(l in 1:nrow(mleCondOneP)){
            tclObj_oneP[[l]] <- tclVar(mleCondOneP[l, 5])
          }
          names(tclObj_oneP) <- mleCondOneP[, 4]
          tclObj_oneM[[k]] <- tclObj_oneP
        }
        names(tclObj_oneM) <- nameP
        tclObj_oneL[[j]] <- tclObj_oneM
      }
      names(tclObj_oneL) <- nameM
      tclObj[[i]] <- tclObj_oneL
    }
    names(tclObj) <- nameLoci
    return(tclObj)
  }

  tab2Par <- get("tab2Par", pos = envParGUI)
  frameTab2Par <- get("frameTab2Par", pos = envParGUI)
  tkdestroy(frameTab2Par)
  frameTab2Par <- tkframe(tab2Par)

  subTabs_parSet <- tk2notebook(frameTab2Par, tabs = c("General", "Amplification efficiency (AE)", "Heterozygote balance (Hb)", "Back stutter ratio (BSR)", "Forward stutter ratio (FSR)", "Double-back stutter ratio (DSR)", "Minus 2-nt stutter ratio (M2SR)"))
  tkpack(subTabs_parSet, fill = "both", expand = 1)
  subTabGen <- tk2notetab(subTabs_parSet, "General")
  subTabAE <- tk2notetab(subTabs_parSet, "Amplification efficiency (AE)")
  subTabHb <- tk2notetab(subTabs_parSet, "Heterozygote balance (Hb)")
  subTabBSR <- tk2notetab(subTabs_parSet, "Back stutter ratio (BSR)")
  subTabFSR <- tk2notetab(subTabs_parSet, "Forward stutter ratio (FSR)")
  subTabDSR <- tk2notetab(subTabs_parSet, "Double-back stutter ratio (DSR)")
  subTabM2SR <- tk2notetab(subTabs_parSet, "Minus 2-nt stutter ratio (M2SR)")

  exKit <- get("exKit", pos = envParProj)
  kitLoci <- get("kitLoci", pos = envParProj)
  parLoci <- get("parLoci", pos = envParProj)
  sexMar <- get("sexMar", pos = envParProj)

  candAE <- get("candAE", pos = envParProj)
  if(length(candAE) == 0){
    candAE_Default <- get("candAE_Default", pos = envParProj)
    candAE <- extCand(candAE_Default, parLoci)
  }
  assign("candAE", candAE, envir = envParProj)
  candHb <- get("candHb", pos = envParProj)
  if(length(candHb) == 0){
    candHb_Default <- get("candHb_Default", pos = envParProj)
    candHb <- extCand(candHb_Default, parLoci)
  }
  assign("candHb", candHb, envir = envParProj)
  candBSR <- get("candBSR", pos = envParProj)
  if(length(candBSR) == 0){
    candBSR_Default <- get("candBSR_Default", pos = envParProj)
    candBSR <- extCand(candBSR_Default, parLoci, multi = TRUE)
  }
  assign("candBSR", candBSR, envir = envParProj)
  candFSR <- get("candFSR", pos = envParProj)
  if(length(candFSR) == 0){
    candFSR_Default <- get("candFSR_Default", pos = envParProj)
    candFSR <- extCand(candFSR_Default, parLoci, multi = TRUE)
  }
  assign("candFSR", candFSR, envir = envParProj)
  candDSR <- get("candDSR", pos = envParProj)
  if(length(candDSR) == 0){
    candDSR_Default <- get("candDSR_Default", pos = envParProj)
    candDSR <- extCand(candDSR_Default, parLoci, multi = TRUE)
  }
  assign("candDSR", candDSR, envir = envParProj)
  candM2SR <- get("candM2SR", pos = envParProj)
  if(length(candM2SR) == 0){
    candM2SR_Default <- get("candM2SR_Default", pos = envParProj)
    candM2SR <- extCand(candM2SR_Default, parLoci, multi = TRUE)
  }
  assign("candM2SR", candM2SR, envir = envParProj)

  mleCondAE <- get("mleCondAE", pos = envParProj)
  if(length(mleCondAE) == 0){
    mleCondAE_Default <- get("mleCondAE_Default", pos = envParProj)
    mleCondAE <- extMleCond(mleCondAE_Default, parLoci)
  }
  mleCondHb <- get("mleCondHb", pos = envParProj)
  if(length(mleCondHb) == 0){
    mleCondHb_Default <- get("mleCondHb_Default", pos = envParProj)
    mleCondHb <- extMleCond(mleCondHb_Default, parLoci)
  }
  mleCondBSR <- get("mleCondBSR", pos = envParProj)
  if(length(mleCondBSR) == 0){
    mleCondBSR_Default <- get("mleCondBSR_Default", pos = envParProj)
    mleCondBSR <- extMleCond(mleCondBSR_Default, parLoci)
  }
  mleCondFSR <- get("mleCondFSR", pos = envParProj)
  if(length(mleCondFSR) == 0){
    mleCondFSR_Default <- get("mleCondFSR_Default", pos = envParProj)
    mleCondFSR <- extMleCond(mleCondFSR_Default, parLoci)
  }
  mleCondDSR <- get("mleCondDSR", pos = envParProj)
  if(length(mleCondDSR) == 0){
    mleCondDSR_Default <- get("mleCondDSR_Default", pos = envParProj)
    mleCondDSR <- extMleCond(mleCondDSR_Default, parLoci)
  }
  mleCondM2SR <- get("mleCondM2SR", pos = envParProj)
  if(length(mleCondM2SR) == 0){
    mleCondM2SR_Default <- get("mleCondM2SR_Default", pos = envParProj)
    mleCondM2SR <- extMleCond(mleCondM2SR_Default, parLoci)
  }

  mleCondAEVar <- tclMleCond(mleCondAE)
  assign("mleCondAEVar", mleCondAEVar, envir = envParGUI)
  mleCondHbVar <- tclMleCond(mleCondHb)
  assign("mleCondHbVar", mleCondHbVar, envir = envParGUI)
  mleCondBSRVar <- tclMleCond(mleCondBSR)
  assign("mleCondBSRVar", mleCondBSRVar, envir = envParGUI)
  mleCondFSRVar <- tclMleCond(mleCondFSR)
  assign("mleCondFSRVar", mleCondFSRVar, envir = envParGUI)
  mleCondDSRVar <- tclMleCond(mleCondDSR)
  assign("mleCondDSRVar", mleCondDSRVar, envir = envParGUI)
  mleCondM2SRVar <- tclMleCond(mleCondM2SR)
  assign("mleCondM2SRVar", mleCondM2SRVar, envir = envParGUI)

  setGen()
  setAE()
  setHb()
  setBSR()
  setFSR()
  setDSR()
  setM2SR()

  comboMethBSR <- get("comboMethBSR", pos = envParGUI)
  comboModelBSR <- get("comboModelBSR", pos = envParGUI)
  methBSRVar <- get("methBSRVar", pos = envParGUI)
  modelBSRVar <- get("modelBSRVar", pos = envParGUI)
  mapply(comboBind, comboMethBSR, comboModelBSR, methBSRVar, modelBSRVar)

  comboMethFSR <- get("comboMethFSR", pos = envParGUI)
  comboModelFSR <- get("comboModelFSR", pos = envParGUI)
  methFSRVar <- get("methFSRVar", pos = envParGUI)
  modelFSRVar <- get("modelFSRVar", pos = envParGUI)
  mapply(comboBind, comboMethFSR, comboModelFSR, methFSRVar, modelFSRVar)

  comboMethDSR <- get("comboMethDSR", pos = envParGUI)
  comboModelDSR <- get("comboModelDSR", pos = envParGUI)
  methDSRVar <- get("methDSRVar", pos = envParGUI)
  modelDSRVar <- get("modelDSRVar", pos = envParGUI)
  mapply(comboBind, comboMethDSR, comboModelDSR, methDSRVar, modelDSRVar)

  comboMethM2SR <- get("comboMethM2SR", pos = envParGUI)
  comboModelM2SR <- get("comboModelM2SR", pos = envParGUI)
  methM2SRVar <- get("methM2SRVar", pos = envParGUI)
  modelM2SRVar <- get("modelM2SRVar", pos = envParGUI)
  mapply(comboBind, comboMethM2SR, comboModelM2SR, methM2SRVar, modelM2SRVar)

  tkpack(tkbutton(frameTab2Par, text = "    Estimate parameters    ", cursor = "hand2", command = function() guiMlePar(envParProj, envParGUI)), pady = 5)

  tkpack(frameTab2Par, fill = "both", padx = 10, pady = 10)
  assign("frameTab2Par", frameTab2Par, envir = envParGUI)
}

# Make conditions of maximum likelihood estimation
makeMleCond <- function(mleCondVar){
  mleCond <- NULL
  nameL <- names(mleCondVar)
  nL <- length(nameL)
  for(i in 1:nL){
    mleCondVar_oneL <- mleCondVar[[i]]
    nameM <- names(mleCondVar_oneL)
    nM <- length(nameM)
    for(j in 1:nM){
      mleCondVar_oneM <- mleCondVar_oneL[[j]]
      nameP <- names(mleCondVar_oneM)
      nP <- length(nameP)
      mleCond_oneM <- matrix(0, 3 * nP, 5)
      mleCond_oneM[, 1] <- nameL[i]
      mleCond_oneM[, 2] <- nameM[j]
      for(k in 1:nP){
        mleCondVar_oneP <- mleCondVar_oneM[[k]]
        mleCond_oneM[(3 * k - 2):(3 * k), 3] <- nameP[k]
        mleCond_oneM[(3 * k - 2):(3 * k), 4] <- names(mleCondVar_oneP)
        mleCond_oneM[(3 * k - 2):(3 * k), 5] <- sapply(mleCondVar_oneP, tclvalue)
      }
      mleCond <- rbind(mleCond, mleCond_oneM)
    }
  }
  colnames(mleCond) <- c("Marker", "Model", "Parameter", "Type", "Value")
  return(mleCond)
}

# GUI for estimating parameters
guiMlePar <- function(envParProj, envParGUI){
  exData <- get("exData", pos = envParProj)
  gtData <- get("gtData", pos = envParProj)
  seqData <- get("seqData", pos = envParProj)
  kitLoci <- get("kitLoci", pos = envParProj)
  parLoci <- get("parLoci", pos = envParProj)
  mtVar <- get("mtVar", pos = envParGUI)
  mt_kitLoci <- as.numeric(sapply(mtVar, tclvalue))
  names(mt_kitLoci) <- kitLoci
  assign("mt_kitLoci", mt_kitLoci, envir = envParProj)

  selectKit <- tclvalue(get("kitParVar", pos = envParGUI))
  assign("selectKit", selectKit, envir = envParProj)
  pathPack <- get("pathPack", pos = envParGUI)
  kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", selectKit, ".csv"), header = TRUE)
  kitInfo <- as.matrix(kitInfo)
  kitLoci <- unique(kitInfo[, "Marker"])
  nL <- length(kitLoci)
  dye_kitLoci <- rep("", nL)
  names(dye_kitLoci) <- kitLoci
  lenRU <- rep(0, nL)
  for(i in 1:nL){
    posL <- which(kitInfo[, "Marker"] == kitLoci[i])[1]
    dye_kitLoci[i] <- kitInfo[posL, "Dye"]
    lenRU[i] <- kitInfo[posL, "Length_of_the_repeat_unit"]
  }
  lenRU <- lenRU[match(parLoci, kitLoci)]
  lenRU <- as.numeric(lenRU)
  names(lenRU) <- parLoci

  modelAEVar <- get("modelAEVar", pos = envParGUI)
  modelAE <- sapply(modelAEVar, tclvalue)
  names(modelAE) <- parLoci
  assign("modelAE", modelAE, envir = envParProj)
  modelHbVar <- get("modelHbVar", pos = envParGUI)
  modelHb <- sapply(modelHbVar, tclvalue)
  names(modelHb) <- parLoci
  assign("modelHb", modelHb, envir = envParProj)
  modelBSRVar <- get("modelBSRVar", pos = envParGUI)
  modelBSR <- sapply(modelBSRVar, tclvalue)
  names(modelBSR) <- parLoci
  assign("modelBSR", modelBSR, envir = envParProj)
  modelFSRVar <- get("modelFSRVar", pos = envParGUI)
  modelFSR <- sapply(modelFSRVar, tclvalue)
  names(modelFSR) <- parLoci
  assign("modelFSR", modelFSR, envir = envParProj)
  modelDSRVar <- get("modelDSRVar", pos = envParGUI)
  modelDSR <- sapply(modelDSRVar, tclvalue)
  names(modelDSR) <- parLoci
  assign("modelDSR", modelDSR, envir = envParProj)
  modelM2SRVar <- get("modelM2SRVar", pos = envParGUI)
  modelM2SR <- sapply(modelM2SRVar, tclvalue)
  names(modelM2SR) <- parLoci
  assign("modelM2SR", modelM2SR, envir = envParProj)

  methBSRVar <- get("methBSRVar", pos = envParGUI)
  methBSR <- sapply(methBSRVar, tclvalue)
  names(methBSR) <- parLoci
  assign("methBSR", methBSR, envir = envParProj)
  methFSRVar <- get("methFSRVar", pos = envParGUI)
  methFSR <- sapply(methFSRVar, tclvalue)
  names(methFSR) <- parLoci
  assign("methFSR", methFSR, envir = envParProj)
  methDSRVar <- get("methDSRVar", pos = envParGUI)
  methDSR <- sapply(methDSRVar, tclvalue)
  names(methDSR) <- parLoci
  assign("methDSR", methDSR, envir = envParProj)
  methM2SRVar <- get("methM2SRVar", pos = envParGUI)
  methM2SR <- sapply(methM2SRVar, tclvalue)
  names(methM2SR) <- parLoci
  assign("methM2SR", methM2SR, envir = envParProj)

  modelMultiBSR <- tclvalue(get("modelMultiBSRVar", pos = envParGUI))
  assign("modelMultiBSR", modelMultiBSR, envir = envParProj)
  modelMultiFSR <- tclvalue(get("modelMultiFSRVar", pos = envParGUI))
  assign("modelMultiFSR", modelMultiFSR, envir = envParProj)
  modelMultiDSR <- tclvalue(get("modelMultiDSRVar", pos = envParGUI))
  assign("modelMultiDSR", modelMultiDSR, envir = envParProj)
  modelMultiM2SR <- tclvalue(get("modelMultiM2SRVar", pos = envParGUI))
  assign("modelMultiM2SR", modelMultiM2SR, envir = envParProj)

  minAEMeth <- as.numeric(tclvalue(get("minAEMethVar", pos = envParGUI)))
  assign("minAEMeth", minAEMeth, envir = envParProj)
  minHbMeth <- as.numeric(tclvalue(get("minHbMethVar", pos = envParGUI)))
  assign("minHbMeth", minHbMeth, envir = envParProj)
  maxBSRMeth <- as.numeric(tclvalue(get("maxBSRMethVar", pos = envParGUI)))
  assign("maxBSRMeth", maxBSRMeth, envir = envParProj)
  maxFSRMeth <- as.numeric(tclvalue(get("maxFSRMethVar", pos = envParGUI)))
  assign("maxFSRMeth", maxFSRMeth, envir = envParProj)
  maxDSRMeth <- as.numeric(tclvalue(get("maxDSRMethVar", pos = envParGUI)))
  assign("maxDSRMeth", maxDSRMeth, envir = envParProj)
  maxM2SRMeth <- as.numeric(tclvalue(get("maxM2SRMethVar", pos = envParGUI)))
  assign("maxM2SRMeth", maxM2SRMeth, envir = envParProj)

  minAE <- as.numeric(tclvalue(get("minAEVar", pos = envParGUI)))
  minHb <- as.numeric(tclvalue(get("minHbVar", pos = envParGUI)))
  maxBSR <- as.numeric(tclvalue(get("maxBSRVar", pos = envParGUI)))
  maxFSR <- as.numeric(tclvalue(get("maxFSRVar", pos = envParGUI)))
  maxDSR <- as.numeric(tclvalue(get("maxDSRVar", pos = envParGUI)))
  maxM2SR <- as.numeric(tclvalue(get("maxM2SRVar", pos = envParGUI)))

  candAE <- get("candAE", pos = envParProj)
  candHb <- get("candHb", pos = envParProj)
  candBSR <- get("candBSR", pos = envParProj)
  candFSR <- get("candFSR", pos = envParProj)
  candDSR <- get("candDSR", pos = envParProj)
  candM2SR <- get("candM2SR", pos = envParProj)

  mleCondAEVar <- get("mleCondAEVar", pos = envParGUI)
  mleCondAE <- makeMleCond(mleCondAEVar)
  assign("mleCondAE", mleCondAE, envir = envParProj)
  mleCondHbVar <- get("mleCondHbVar", pos = envParGUI)
  mleCondHb <- makeMleCond(mleCondHbVar)
  assign("mleCondHb", mleCondHb, envir = envParProj)
  mleCondBSRVar <- get("mleCondBSRVar", pos = envParGUI)
  mleCondBSR <- makeMleCond(mleCondBSRVar)
  assign("mleCondBSR", mleCondBSR, envir = envParProj)
  mleCondFSRVar <- get("mleCondFSRVar", pos = envParGUI)
  mleCondFSR <- makeMleCond(mleCondFSRVar)
  assign("mleCondFSR", mleCondFSR, envir = envParProj)
  mleCondDSRVar <- get("mleCondDSRVar", pos = envParGUI)
  mleCondDSR <- makeMleCond(mleCondDSRVar)
  assign("mleCondDSR", mleCondDSR, envir = envParProj)
  mleCondM2SRVar <- get("mleCondM2SRVar", pos = envParGUI)
  mleCondM2SR <- makeMleCond(mleCondM2SRVar)
  assign("mleCondM2SR", mleCondM2SR, envir = envParProj)

  assign("dye_kitLoci", dye_kitLoci, envir = envParProj)
  assign("lenRU", lenRU, envir = envParProj)

  resultMLE <- mlePar(exData, gtData, seqData, parLoci, sexMar, lenRU, mt_kitLoci, dye_kitLoci,
                      candAE, candHb, candBSR, candFSR, candDSR, candM2SR,
                      modelAE, modelHb, modelBSR, modelFSR, modelDSR, modelM2SR,
                      methBSR, methFSR, methDSR, methM2SR,
                      modelMultiBSR, modelMultiFSR, modelMultiDSR, modelMultiM2SR,
                      mleCondAE, mleCondHb, mleCondBSR, mleCondFSR, mleCondDSR, mleCondM2SR,
                      minAEMeth, minHbMeth, maxBSRMeth, maxFSRMeth, maxDSRMeth, maxM2SRMeth,
                      minAE, minHb, maxBSR, maxFSR, maxDSR, maxM2SR)

  assign("aeUseData", resultMLE[[1]], envir = envParProj)
  assign("aeMleData", resultMLE[[2]], envir = envParProj)
  assign("hbUseData", resultMLE[[3]], envir = envParProj)
  assign("hbMleData", resultMLE[[4]], envir = envParProj)
  assign("bsrGenData", resultMLE[[5]], envir = envParProj)
  assign("bsrUseData", resultMLE[[6]], envir = envParProj)
  assign("bsrMleData", resultMLE[[7]], envir = envParProj)
  assign("fsrGenData", resultMLE[[8]], envir = envParProj)
  assign("fsrUseData", resultMLE[[9]], envir = envParProj)
  assign("fsrMleData", resultMLE[[10]], envir = envParProj)
  assign("dsrGenData", resultMLE[[11]], envir = envParProj)
  assign("dsrUseData", resultMLE[[12]], envir = envParProj)
  assign("dsrMleData", resultMLE[[13]], envir = envParProj)
  assign("m2srGenData", resultMLE[[14]], envir = envParProj)
  assign("m2srUseData", resultMLE[[15]], envir = envParProj)
  assign("m2srMleData", resultMLE[[16]], envir = envParProj)
  assign("exportAlCor", resultMLE[[17]], envir = envParProj)
  assign("motifLengthList", resultMLE[[18]], envir = envParProj)
  assign("seqAlList", resultMLE[[19]], envir = envParProj)
  assign("seqCountList", resultMLE[[20]], envir = envParProj)
  assign("minAE", resultMLE[[21]], envir = envParProj)
  assign("minHb", resultMLE[[22]], envir = envParProj)
  assign("maxBSR", resultMLE[[23]], envir = envParProj)
  assign("maxFSR", resultMLE[[24]], envir = envParProj)
  assign("maxDSR", resultMLE[[25]], envir = envParProj)
  assign("maxM2SR", resultMLE[[26]], envir = envParProj)

  assign("displayAE", character(0), envir = envParProj)
  assign("exportAE", character(0), envir = envParProj)
  assign("displayHb", character(0), envir = envParProj)
  assign("exportHb", character(0), envir = envParProj)
  assign("displayBSR", character(0), envir = envParProj)
  assign("exportBSR", character(0), envir = envParProj)
  assign("displayFSR", character(0), envir = envParProj)
  assign("exportFSR", character(0), envir = envParProj)
  assign("displayDSR", character(0), envir = envParProj)
  assign("exportDSR", character(0), envir = envParProj)
  assign("displayM2SR", character(0), envir = envParProj)
  assign("exportM2SR", character(0), envir = envParProj)

  assign("finEstPar", TRUE, envir = envParProj)
  makeTab3Par(envParProj, envParGUI)
  tk2notetab.select(get("tabsPar", pos = envParGUI), "Result")
}

# Re-estimate parameters
reestRun <- function(envParProj, envParGUI, tfReest, selectLocus, selectModel, nameModel, bpName){
  tkdestroy(tfReest)
  tab3Par <- get("tab3Par", pos = envParGUI)
  frameTab3Par <- get("frameTab3Par", pos = envParGUI)
  tkdestroy(frameTab3Par)
  frameTab3Par <- tkframe(tab3Par)
  assign("frameTab3Par", frameTab3Par, envir = envParGUI)

  mleCondVar <- get("mleCondVar", pos = envParGUI)
  posM <- which(nameModel == selectModel)
  if(bpName == "AE"){
    cat("Reestimation of parameters was started.", "\n")
    mleCond <- get("mleCondAE", pos = envParProj)
    mleCond_oneL <- mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE]
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("aeMleData", pos = envParProj)
    useData <- get("aeUseData", pos = envParProj)
    useOneL <- useData[[posL]]
    minMax <- get("minAE", pos = envParProj)
    mleData[[posL]][[posM]] <- aeModeling(selectModel, useOneL[, 1], useOneL[, 2], minMax, mleCond_oneL)[[1]]
    assign("aeMleData", mleData, envir = envParProj)
    assign("mleCondAE", mleCond, envir = envParProj)
  }else if(bpName == "Hb"){
    cat("Reestimation of parameters was started.", "\n")
    mleCond <- get("mleCondHb", pos = envParProj)
    mleCond_oneL <- mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE]
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("hbMleData", pos = envParProj)
    useData <- get("hbUseData", pos = envParProj)
    useOneL <- useData[[posL]]
    minMax <- get("minHb", pos = envParProj)
    mleData[[posL]][[posM]] <- hbModeling(selectModel, useOneL[, 1], useOneL[, 2], minMax, mleCond_oneL)[[1]]
    assign("hbMleData", mleData, envir = envParProj)
    assign("mleCondHb", mleCond, envir = envParProj)
  }else if(bpName == "BSR"){
    mleCond <- get("mleCondBSR", pos = envParProj)
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("bsrMleData", pos = envParProj)
    mleData_oneL <- mleData[[posL]]
    minMax <- get("maxBSR", pos = envParProj)
    inputOk <- "ok"
    if(mleData_oneL[[length(mleData_oneL)]] == "Multiple loci together"){
      inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  selectLocus, " is 'multiple loci together'. The result of re-estimation will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
      if(inputOk == "ok"){
        cat("Reestimation of parameters was started.", "\n")
        methBSR <- get("methBSR", pos = envParProj)
        posBsrGen <- which(methBSR == "Multiple loci together")
        genData <- get("bsrGenData", pos = envParProj)
        bsrGenData <- srModeling(selectModel, genData[[1]], genData[[2]], genData[[3]], genData[[4]], genData[[5]], genData[[6]], genData[[7]], minMax, mleCond[mleCond[, "Marker"] == "Multiple loci together", , drop = FALSE])
        for(i in 1:length(posBsrGen)){
          mleData[[posBsrGen[i]]][[posM]] <- bsrGenData[[1]]
        }
      }
    }else{
      cat("Reestimation of parameters was started.", "\n")
      useData <- get("bsrUseData", pos = envParProj)
      useOneL <- useData[[posL]]
      motifLengthList <- get("motifLengthList", pos = envParProj)
      seqAlList <- get("seqAlList", pos = envParProj)
      seqCountList <- get("seqCountList", pos = envParProj)
      mleData[[posL]][[posM]] <- srModeling(selectModel, useOneL[[1]], useOneL[[2]], useOneL[[3]], useOneL[[4]], motifLengthList[[posL]], seqAlList[[posL]], seqCountList[[posL]], minMax, mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE])[[1]]
    }
    assign("bsrMleData", mleData, envir = envParProj)
    assign("mleCondBSR", mleCond, envir = envParProj)
  }else if(bpName == "FSR"){
    mleCond <- get("mleCondFSR", pos = envParProj)
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("fsrMleData", pos = envParProj)
    mleData_oneL <- mleData[[posL]]
    minMax <- get("maxFSR", pos = envParProj)
    inputOk <- "ok"
    if(mleData_oneL[[length(mleData_oneL)]] == "Multiple loci together"){
      inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  selectLocus, " is 'multiple loci together'. The result of re-estimation will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
      if(inputOk == "ok"){
        cat("Reestimation of parameters was started.", "\n")
        methFSR <- get("methFSR", pos = envParProj)
        posFSRGen <- which(methFSR == "Multiple loci together")
        genData <- get("fsrGenData", pos = envParProj)
        fsrGenData <- srModeling(selectModel, genData[[1]], genData[[2]], genData[[3]], genData[[4]], genData[[5]], genData[[6]], genData[[7]], minMax, mleCond[mleCond[, "Marker"] == "Multiple loci together", , drop = FALSE])
        for(i in 1:length(posFSRGen)){
          mleData[[posFSRGen[i]]][[posM]] <- fsrGenData[[1]]
        }
      }
    }else{
      cat("Reestimation of parameters was started.", "\n")
      useData <- get("fsrUseData", pos = envParProj)
      useOneL <- useData[[posL]]
      motifLengthList <- get("motifLengthList", pos = envParProj)
      seqAlList <- get("seqAlList", pos = envParProj)
      seqCountList <- get("seqCountList", pos = envParProj)
      mleData[[posL]][[posM]] <- srModeling(selectModel, useOneL[[1]], useOneL[[2]], useOneL[[3]], useOneL[[4]], motifLengthList[[posL]], seqAlList[[posL]], seqCountList[[posL]], minMax, mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE])[[1]]
    }
    assign("fsrMleData", mleData, envir = envParProj)
    assign("mleCondFSR", mleCond, envir = envParProj)
  }else if(bpName == "DSR"){
    mleCond <- get("mleCondDSR", pos = envParProj)
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("dsrMleData", pos = envParProj)
    mleData_oneL <- mleData[[posL]]
    minMax <- get("maxDSR", pos = envParProj)
    inputOk <- "ok"
    if(mleData_oneL[[length(mleData_oneL)]] == "Multiple loci together"){
      inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  selectLocus, " is 'multiple loci together'. The result of re-estimation will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
      if(inputOk == "ok"){
        cat("Reestimation of parameters was started.", "\n")
        methDSR <- get("methDSR", pos = envParProj)
        posDSRGen <- which(methDSR == "Multiple loci together")
        genData <- get("dsrGenData", pos = envParProj)
        dsrGenData <- srModeling(selectModel, genData[[1]], genData[[2]], genData[[3]], genData[[4]], genData[[5]], genData[[6]], genData[[7]], minMax, mleCond[mleCond[, "Marker"] == "Multiple loci together", , drop = FALSE])
        for(i in 1:length(posDSRGen)){
          mleData[[posDSRGen[i]]][[posM]] <- dsrGenData[[1]]
        }
      }
    }else{
      cat("Reestimation of parameters was started.", "\n")
      useData <- get("dsrUseData", pos = envParProj)
      useOneL <- useData[[posL]]
      motifLengthList <- get("motifLengthList", pos = envParProj)
      seqAlList <- get("seqAlList", pos = envParProj)
      seqCountList <- get("seqCountList", pos = envParProj)
      mleData[[posL]][[posM]] <- srModeling(selectModel, useOneL[[1]], useOneL[[2]], useOneL[[3]], useOneL[[4]], motifLengthList[[posL]], seqAlList[[posL]], seqCountList[[posL]], minMax, mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE])[[1]]
    }
    assign("dsrMleData", mleData, envir = envParProj)
    assign("mleCondDSR", mleCond, envir = envParProj)
  }else if(bpName == "M2SR"){
    mleCond <- get("mleCondM2SRVar", pos = envParProj)
    posL <- which(names(mleCondVar) == selectLocus)
    mleData <- get("m2srMleData", pos = envParProj)
    mleData_oneL <- mleData[[posL]]
    minMax <- get("maxM2SR", pos = envParProj)
    inputOk <- "ok"
    if(mleData_oneL[[length(mleData_oneL)]] == "Multiple loci together"){
      inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  selectLocus, " is 'multiple loci together'. The result of re-estimation will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
      if(inputOk == "ok"){
        cat("Reestimation of parameters was started.", "\n")
        methM2SR <- get("methM2SR", pos = envParProj)
        posM2SRGen <- which(methM2SR == "Multiple loci together")
        genData <- get("m2srGenData", pos = envParProj)
        m2srGenData <- srModeling(selectModel, genData[[1]], genData[[2]], genData[[3]], genData[[4]], genData[[5]], genData[[6]], genData[[7]], minMax, mleCond[mleCond[, "Marker"] == "Multiple loci together", , drop = FALSE])
        for(i in 1:length(posM2SRGen)){
          mleData[[posM2SRGen[i]]][[posM]] <- m2srGenData[[1]]
        }
      }
    }else{
      cat("Reestimation of parameters was started.", "\n")
      useData <- get("m2srUseData", pos = envParProj)
      useOneL <- useData[[posL]]
      motifLengthList <- get("motifLengthList", pos = envParProj)
      seqAlList <- get("seqAlList", pos = envParProj)
      seqCountList <- get("seqCountList", pos = envParProj)
      mleData[[posL]][[posM]] <- srModeling(selectModel, useOneL[[1]], useOneL[[2]], useOneL[[3]], useOneL[[4]], motifLengthList[[posL]], seqAlList[[posL]], seqCountList[[posL]], minMax, mleCond[mleCond[, "Marker"] == selectLocus, , drop = FALSE])[[1]]
    }
    assign("m2srMleData", mleData, envir = envParProj)
    assign("mleCondM2SR", mleCond, envir = envParProj)
  }
  cat("Reestimation of parameters was finished.", "\n")
  makeTab3Par(envParProj, envParGUI)
  tk2notetab.select(get("tabsPar", pos = envParGUI), "Result")
}

# Make a Result tab
makeTab3Par <- function(envParProj, envParGUI){
  extAdoptModel <- function(mleData_oneL, adoptMeth = "MLE"){
    nM <- length(mleData_oneL) - 1
    if(adoptMeth == "MLE"){
      aics <- rep(0, nM)
      for(i in 1:nM){
        aics[i] <- mleData_oneL[[i]][[1]]["AIC"]
      }
      posAdopt <- which.min(aics)
    }else{
      for(i in 1:nM){
        if(mleData_oneL[[i]][[3]] == adoptMeth){
          posAdopt <- i
          break
        }
      }
    }
    return(mleData_oneL[[posAdopt]])
  }

  graphVar <- function(selectLocus, bpName, selectModel, mleData_oneL, bpData, hData, bpMin){
    selectMleData <- extAdoptModel(mleData_oneL, selectModel)
    estPar <- selectMleData[[2]]
    if(bpName == "AE"){
      yLab <- "Locus-specific amplification efficiency"
    }else if(bpName == "Hb"){
      yLab <- "Heterozygote balance"
    }
    xMin <- 0
    xMax <- round(max(hData) + 1, 0)
    yMin <- min(bpData)
    yMax <- max(bpData)
    dev.new(width = 6, height = 6, noRStudioGD = TRUE)
    plot(hData, bpData, xlim = c(xMin, xMax), ylim = c(yMin, yMax), xlab = "Average heterozygous peak height (RFU)", ylab = yLab, main = selectLocus)
    myu <- estPar["Mean"]
    sigma <- estPar["Variance"]
    Q0.95 <- qtruncnorm(0.95, a = log(bpMin), b = log(1 / bpMin), mean = log(myu), sd = sqrt(sigma / (100:xMax)))
    Q0.05 <- qtruncnorm(0.05, a = log(bpMin), b = log(1 / bpMin), mean = log(myu), sd = sqrt(sigma / (100:xMax)))
    par(new = TRUE)
    plot(100:xMax, exp(Q0.95), type = "l", xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         xlab = "", ylab = "", axes = FALSE, col = "deeppink")
    par(new = TRUE)
    plot(100:xMax, exp(Q0.05), type = "l", xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         xlab = "", ylab = "", axes = FALSE, col = "mediumorchid3")
    legend(xMax * 0.65, yMax, lty = c(1, 1), col = c("deeppink", "mediumorchid3"), c("95% quantile", "5% quantile"))
  }

  graphSrMean <- function(selectLocus, bpName, selectModel, mleData_oneL, srUseData, methSR){
    if(bpName == "BSR"){
      yLab = "back stutter ratio"
    }else if(bpName == "FSR"){
      yLab = "forward stutter ratio"
    }else if(bpName == "DSR"){
      yLab = "double-back stutter ratio"
    }else if(bpName == "M2SR"){
      yLab = "minus 2-nt stutter ratio"
    }

    selectMleData <- extAdoptModel(mleData_oneL, selectModel)
    estPar <- selectMleData[[2]]
    slope <- estPar["Slope"]
    intercept <- estPar["Intercept"]

    if(selectModel == "Allele"){
      posModel <- 2
      xLab <- "Allele"
    }else if(selectModel == "LUS"){
      posModel <- 4
      xLab <- "LUS"
    }else if(selectModel == "Multi-seq"){
      posModel <- 5
      xLab <- "Corrected allele number"
    }else if(selectModel == "Uniform"){
      posModel <- 2
      xLab <- "Allele"
      slope <- 0
      intercept <- estPar["Mean"]
    }

    posL <- which(names(methSR) == selectLocus)
    srOther <- xValOther <- numeric(0)
    colSelectL <- "black"
    if(methSR[posL] == "Multiple loci together"){
      posMulti <- which(methSR == "Multiple loci together")
      colSelectL <- "blue"
      for(i in 1:length(posMulti)){
        posMultiOne <- posMulti[i]
        srUseOneL <- srUseData[[posMultiOne]]
        if(posMultiOne == posL){
          sr <- srUseOneL[[1]]
          xVal <- srUseOneL[[posModel]]
        }else{
          srOther <- c(srOther, srUseOneL[[1]])
          xValOther <- c(xValOther, srUseOneL[[posModel]])
        }
      }
    }else{
      srUseOneL <- srUseData[[posL]]
      sr <- srUseOneL[[1]]
      xVal <- srUseOneL[[posModel]]
    }

    xMin <- round(min(c(xVal, xValOther)) - 0.5, 0)
    xMax <- round(max(c(xVal, xValOther)) + 0.5, 0)
    yMin <- min(c(sr, srOther))
    yMax <- max(c(sr, srOther))

    dev.new(width = 6, height = 6, noRStudioGD = TRUE)
    plot(xValOther, srOther, xlim = c(xMin, xMax), ylim = c(yMin, yMax), xlab = xLab, ylab = yLab, xaxt = "n", main = selectLocus)
    par(new = TRUE)
    plot(xVal, sr, xlim = c(xMin, xMax), ylim = c(yMin, yMax), xlab = "", ylab = "", axes = FALSE, main = "", col = colSelectL)
    axis(side = 1, at = seq(xMin, xMax, 1))
    lineX <- seq(round(xMin - 1, 1), round(xMax + 1, 1), 0.1)
    lineY <- slope * lineX + intercept
    par(new = TRUE)
    plot(lineX, lineY, type = "l", xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         xlab = "", ylab = "", axes = FALSE)
    if(methSR[posL] == "Multiple loci together"){
      legend(xMin, yMax, pch = c(1, 1), col = c("blue", "black"), c(selectLocus, "Other loci"))
    }
  }

  graphSrVar <- function(selectLocus, bpName, selectModel, mleData_oneL, srUseData, srMax, methSR){
    if(bpName == "BSR"){
      yLab = "back stutter ratio"
    }else if(bpName == "FSR"){
      yLab = "forward stutter ratio"
    }else if(bpName == "DSR"){
      yLab = "double-back stutter ratio"
    }else if(bpName == "M2SR"){
      yLab = "minus 2-nt stutter ratio"
    }

    selectMleData <- extAdoptModel(mleData_oneL, selectModel)
    estPar <- selectMleData[[2]]
    sigma <- estPar["Variance"]

    if(selectModel == "Allele"){
      posModel <- 2
    }else if(selectModel == "LUS"){
      posModel <- 4
    }else if(selectModel == "Multi-seq"){
      posModel <- 5
    }else if(selectModel == "Uniform"){
      posModel <- 2
      myu <- estPar["Mean"]
    }

    posL <- which(names(methSR) == selectLocus)
    srOther <- hOther <- alOther <- numeric(0)
    colSelectL <- "black"
    if(methSR[posL] == "Multiple loci together"){
      posMulti <- which(methSR == "Multiple loci together")
      colSelectL <- "blue"
      for(i in 1:length(posMulti)){
        posMultiOne <- posMulti[i]
        srUseOneL <- srUseData[[posMultiOne]]
        if(posMultiOne == posL){
          sr <- srUseOneL[[1]]
          h <- srUseOneL[[3]]
          al <- srUseOneL[[posModel]]
        }else{
          srOther <- c(srOther, srUseOneL[[1]])
          hOther <- c(hOther, srUseOneL[[3]])
          alOther <- c(alOther, srUseOneL[[posModel]])
        }
      }
    }else{
      srUseOneL <- srUseData[[posL]]
      sr <- srUseOneL[[1]]
      h <- srUseOneL[[3]]
      al <- srUseOneL[[posModel]]
    }

    if(selectModel != "Uniform"){
      meanAl <- mean(c(al, alOther))
      slope <- estPar["Slope"]
      intercept <- estPar["Intercept"]
      myu <- slope * meanAl + intercept
      srMean <- slope * al + intercept
      srCor <- srMean - myu
      sr <- sr - srCor
      srMeanOther <- slope * alOther + intercept
      srCorOther <- srMeanOther - myu
      srOther <- srOther - srCorOther
      yLab <- paste0(yLab, " (corrected)")
    }

    xMin <- 0
    xMax <- round(max(c(h, hOther)) + 1, 0)
    yMin <- min(c(sr, srOther))
    yMax <- max(c(sr, srOther))
    dev.new(width = 6, height = 6, noRStudioGD = TRUE)
    plot(hOther, srOther, xlim = c(xMin, xMax), ylim = c(yMin, yMax), xlab = "Total allelic product (RFU)", ylab = yLab, main = selectLocus)
    par(new = TRUE)
    plot(h, sr, xlim = c(xMin, xMax), ylim = c(yMin, yMax), xlab = "", ylab = "", axes = FALSE, main = "", col = colSelectL)
    Q0.95 <- qtruncnorm(0.95, b = log(srMax), mean = log(myu), sd = sqrt(sigma / (100:xMax)))
    Q0.05 <- qtruncnorm(0.05, b = log(srMax), mean = log(myu), sd = sqrt(sigma / (100:xMax)))
    par(new = TRUE)
    plot(100:xMax, exp(Q0.95), type = "l", xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         xlab = "", ylab = "", axes = FALSE, col = "deeppink")
    par(new = TRUE)
    plot(100:xMax, exp(Q0.05), type = "l", xlim = c(xMin, xMax), ylim = c(yMin, yMax),
         xlab = "", ylab = "", axes = FALSE, col = "mediumorchid3")
    legend(xMax * 0.65, yMax, lty = c(1, 1), col = c("deeppink", "mediumorchid3"), c("95% quantile", "5% quantile"))
    if(methSR[posL] == "Multiple loci together"){
      legend(xMax * 0.65, yMax * 0.825, pch = c(1, 1), col = c("blue", "black"), c(selectLocus, "Other loci"))
    }
  }

  reestPar <- function(selectLocus, nameModel, bpName){
    resetCond <- function(selectModel){
      frameReest_1_2 <- get("frameReest_1_2", pos = envParGUI)
      tkdestroy(frameReest_1_2)
      frameReest_1_2 <- tkframe(frameReest_1)
      tkgrid(tklabel(frameReest_1_2, text = "Parameter"), row = 0, column = 0, padx = 10, pady = 2)
      tkgrid(tklabel(frameReest_1_2, text = "Initial value"), row = 0, column = 1, padx = 10, pady = 2)
      tkgrid(tklabel(frameReest_1_2, text = "Lower value"), row = 0, column = 2, padx = 10, pady = 2)
      tkgrid(tklabel(frameReest_1_2, text = "Upper value"), row = 0, column = 3, padx = 10, pady = 2)

      if(bpName == "AE"){
        mleCondVar <- get("mleCondAEVar", pos = envParGUI)
      }else if(bpName == "Hb"){
        mleCondVar <- get("mleCondHbVar", pos = envParGUI)
      }else if(bpName == "BSR"){
        mleCondVar <- get("mleCondBSRVar", pos = envParGUI)
      }else if(bpName == "FSR"){
        mleCondVar <- get("mleCondFSRVar", pos = envParGUI)
      }else if(bpName == "DSR"){
        mleCondVar <- get("mleCondDSRVar", pos = envParGUI)
      }else if(bpName == "M2SR"){
        mleCondVar <- get("mleCondM2SRVar", pos = envParGUI)
      }
      assign("mleCondVar", mleCondVar, envir = envParGUI)
      mleCondVar_oneL <- mleCondVar[[which(names(mleCondVar) == selectLocus)]]
      mleCondVar_oneM <- mleCondVar_oneL[[which(nameModel == selectModel)]]
      params <- names(mleCondVar_oneM)
      for(i in 1:length(params)){
        mleCondVar_oneP <- mleCondVar_oneM[[i]]
        tkgrid(tklabel(frameReest_1_2, text = params[i]), row = i, column = 0, padx = 10, pady = 1)
        entryInitial <- tkentry(frameReest_1_2, textvariable = mleCondVar_oneP[[1]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryInitial, row = i, column = 1, padx = 10, pady = 1)
        entryLower <- tkentry(frameReest_1_2, textvariable = mleCondVar_oneP[[2]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryLower, row = i, column = 2, padx = 10, pady = 1)
        entryUpper <- tkentry(frameReest_1_2, textvariable = mleCondVar_oneP[[3]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
        tkgrid(entryUpper, row = i, column = 3, padx = 10, pady = 1)
      }
      tkgrid(frameReest_1_2, sticky = "w")
      assign("frameReest_1_2", frameReest_1_2, envir = envParGUI)
    }

    tfReest <- tktoplevel()
    tkwm.title(tfReest, "Re-estimate parameters")
    frameReest_1 <- tkframe(tfReest)
    frameReest_1_1 <- tkframe(frameReest_1)
    frameReest_1_2 <- tkframe(frameReest_1)
    assign("frameReest_1_2", frameReest_1_2, envir = envParGUI)
    frameReest_2 <- tkframe(tfReest)
    tkgrid(tklabel(frameReest_1_1, text = "Locus : "), row = 0, column = 0, padx = 5, pady = 5, sticky = "w")
    tkgrid(tklabel(frameReest_1_1, text = selectLocus), row = 0, column = 1, padx = 5, pady = 5, sticky = "w")
    tkgrid(tklabel(frameReest_1_1, text = "Select a model : "), row = 1, column = 0, padx = 5, pady = 5, sticky = "w")
    selectModelVar <- tclVar(nameModel[1])
    comboModel <- ttkcombobox(frameReest_1_1, values = nameModel, textvariable = selectModelVar, state = "readonly")
    tkgrid(comboModel, row = 1, column = 1, padx = 5, pady = 5, sticky = "w")
    tkgrid(frameReest_1_1, sticky = "w")

    tkbind(comboModel, "<<ComboboxSelected>>", function(){
      resetCond(tclvalue(selectModelVar))
    })
    resetCond(tclvalue(selectModelVar))
    tkgrid(frameReest_1)
    tkgrid(tkbutton(frameReest_2, text = "    Estimate    ", cursor = "hand2", command = function() reestRun(envParProj, envParGUI, tfReest, selectLocus, tclvalue(selectModelVar), nameModel, bpName)), pady = 5)
    tkgrid(frameReest_2)
  }

  resultAE <- function(){
    showDetail_AE <- function(){
      posL <- tclvalue(tkcurselection(mlbAE))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameAE_2_1 <- get("frameAE_2_1", pos = envParGUI)
        frameAE_2_2 <- get("frameAE_2_2", pos = envParGUI)
        frameAE_2_3 <- get("frameAE_2_3", pos = envParGUI)
        frameAE_2_4 <- get("frameAE_2_4", pos = envParGUI)
        tkdestroy(frameAE_2_1)
        tkdestroy(frameAE_2_2)
        tkdestroy(frameAE_2_3)
        tkdestroy(frameAE_2_4)
        frameAE_2_1 <- tkframe(frameAE_2)
        frameAE_2_2 <- tkframe(frameAE_2, relief = "groove", borderwidth = 2)
        frameAE_2_3 <- tkframe(frameAE_2, relief = "groove", borderwidth = 2)
        frameAE_2_4 <- tkframe(frameAE_2)

        posL <- as.numeric(posL) + 1
        aeMleData_oneL <- aeMleData[[posL]]
        selectLocus <- nameL[posL]
        tkgrid(tklabel(frameAE_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(aeMleData_oneL) > 0){
          tkgrid(tklabel(frameAE_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbAE2 <- tk2mclistbox(frameAE_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbAE2, "add", label = "Model", width = 15)
          tk2column(mlbAE2, "add", label = "AIC", width = 15)
          nM <- length(aeMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            aeMleData_oneM <- aeMleData_oneL[[i]]
            displayAIC[i, 1] <- aeMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(aeMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbAE2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbAE2, "end", displayAIC)

          tkgrid(tklabel(frameAE_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameAE_2_3_1 <- tkframe(frameAE_2_3)
          tkgrid(tklabel(frameAE_2_3_1, text = "Model : "),
                 ttkcombobox(frameAE_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameAE_2_3_1)
          tkgrid(tkbutton(frameAE_2_3, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphVar(selectLocus, "AE", tclvalue(selectModelVar), aeMleData_oneL, aeUseData[[posL]][, 1], aeUseData[[posL]][, 2], minAE)), padx = 5, pady = 5, sticky = "w")

          tkgrid(tkbutton(frameAE_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "AE")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameAE_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameAE_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameAE_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameAE_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameAE_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameAE_2_1", frameAE_2_1, envir = envParGUI)
        assign("frameAE_2_2", frameAE_2_2, envir = envParGUI)
        assign("frameAE_2_3", frameAE_2_3, envir = envParGUI)
        assign("frameAE_2_4", frameAE_2_4, envir = envParGUI)
      }
    }

    frameAE_1 <- tkframe(subTabAE)
    frameAE_2 <- tkframe(subTabAE, relief = "groove", borderwidth = 2)
    assign("frameAE_2_1", tkframe(frameAE_2), envir = envParGUI)
    assign("frameAE_2_2", tkframe(frameAE_2), envir = envParGUI)
    assign("frameAE_2_3", tkframe(frameAE_2), envir = envParGUI)
    assign("frameAE_2_4", tkframe(frameAE_2), envir = envParGUI)
    nL <- length(aeMleData)
    nameL <- names(aeMleData)
    nameP <- unique(mleCondAE[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "Min. AE", nameP)
    displayAE <- get("displayAE", pos = envParProj)
    exportAE <- get("exportAE", pos = envParProj)
    if((length(displayAE) == 0) || (length(exportAE) == 0)){
      displayAE <- matrix("", nL, nP + 3)
      displayAE[, 1] <- nameL
      exportAE <- displayAE
      colnames(exportAE) <- colDisplay
      for(i in 1:nL){
        aeMleData_oneL <- aeMleData[[i]]
        if(length(aeMleData_oneL) > 0){
          adoptData <- extAdoptModel(aeMleData_oneL)
          exportAE[i, 2] <- adoptData[[3]]
          displayAE[i, 2] <- adoptData[[3]]
          exportAE[i, 3] <- minAE
          displayAE[i, 3] <- signif(minAE, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportAE[i, posPar] <- estPar
          displayAE[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_AE <- tkscrollbar(frameAE_1, repeatinterval = 5, command = function(...) tkyview(mlbAE, ...))
    mlbAE <- tk2mclistbox(frameAE_1, width = 35 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_AE, ...))
    tk2column(mlbAE, "add", label = colDisplay[1], width = 10)
    tk2column(mlbAE, "add", label = colDisplay[2], width = 15)
    tk2column(mlbAE, "add", label = colDisplay[3], width = 10)
    for(i in 1:nP){
      tk2column(mlbAE, "add", label = colDisplay[i + 3], width = 12)
    }
    tkgrid(mlbAE, scrY_AE)
    tk2insert.multi(mlbAE, "end", displayAE)
    tkgrid.configure(scrY_AE, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameAE_1, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_AE()), padx = 5, pady = 5)
    tkgrid(frameAE_1, frameAE_2, padx = 10, pady = 5, sticky = "n")

    assign("displayAE", displayAE, envir = envParProj)
    assign("exportAE", exportAE, envir = envParProj)
  }

  resultHb <- function(){
    showDetail_Hb <- function(){
      posL <- tclvalue(tkcurselection(mlbHb))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameHb_2_1 <- get("frameHb_2_1", pos = envParGUI)
        frameHb_2_2 <- get("frameHb_2_2", pos = envParGUI)
        frameHb_2_3 <- get("frameHb_2_3", pos = envParGUI)
        frameHb_2_4 <- get("frameHb_2_4", pos = envParGUI)
        tkdestroy(frameHb_2_1)
        tkdestroy(frameHb_2_2)
        tkdestroy(frameHb_2_3)
        tkdestroy(frameHb_2_4)
        frameHb_2_1 <- tkframe(frameHb_2)
        frameHb_2_2 <- tkframe(frameHb_2, relief = "groove", borderwidth = 2)
        frameHb_2_3 <- tkframe(frameHb_2, relief = "groove", borderwidth = 2)
        frameHb_2_4 <- tkframe(frameHb_2)

        posL <- as.numeric(posL) + 1
        selectLocus <- nameL[posL]
        hbMleData_oneL <- hbMleData[[posL]]
        tkgrid(tklabel(frameHb_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(hbMleData_oneL) > 0){
          tkgrid(tklabel(frameHb_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbHb2 <- tk2mclistbox(frameHb_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbHb2, "add", label = "Model", width = 15)
          tk2column(mlbHb2, "add", label = "AIC", width = 15)
          nM <- length(hbMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            hbMleData_oneM <- hbMleData_oneL[[i]]
            displayAIC[i, 1] <- hbMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(hbMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbHb2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbHb2, "end", displayAIC)

          tkgrid(tklabel(frameHb_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameHb_2_3_1 <- tkframe(frameHb_2_3)
          tkgrid(tklabel(frameHb_2_3_1, text = "Model : "),
                 ttkcombobox(frameHb_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameHb_2_3_1)
          tkgrid(tkbutton(frameHb_2_3, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphVar(selectLocus, "Hb", tclvalue(selectModelVar), hbMleData_oneL, hbUseData[[posL]][, 1], hbUseData[[posL]][, 2], minHb)), padx = 5, pady = 5, sticky = "w")

          tkgrid(tkbutton(frameHb_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "Hb")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameHb_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameHb_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameHb_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameHb_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameHb_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameHb_2_1", frameHb_2_1, envir = envParGUI)
        assign("frameHb_2_2", frameHb_2_2, envir = envParGUI)
        assign("frameHb_2_3", frameHb_2_3, envir = envParGUI)
        assign("frameHb_2_4", frameHb_2_4, envir = envParGUI)
      }
    }

    frameHb_1 <- tkframe(subTabHb)
    frameHb_2 <- tkframe(subTabHb, relief = "groove", borderwidth = 2)
    assign("frameHb_2_1", tkframe(frameHb_2), envir = envParGUI)
    assign("frameHb_2_2", tkframe(frameHb_2), envir = envParGUI)
    assign("frameHb_2_3", tkframe(frameHb_2), envir = envParGUI)
    assign("frameHb_2_4", tkframe(frameHb_2), envir = envParGUI)
    nL <- length(hbMleData)
    nameL <- names(hbMleData)
    nameP <- unique(mleCondHb[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "Min. Hb", nameP)
    displayHb <- get("displayHb", pos = envParProj)
    exportHb <- get("exportHb", pos = envParProj)
    if((length(displayHb) == 0) || (length(exportHb) == 0)){
      displayHb <- matrix("", nL, nP + 3)
      displayHb[, 1] <- names(hbMleData)
      exportHb <- displayHb
      colnames(exportHb) <- colDisplay
      for(i in 1:nL){
        hbMleData_oneL <- hbMleData[[i]]
        if(length(hbMleData_oneL) > 0){
          adoptData <- extAdoptModel(hbMleData_oneL)
          exportHb[i, 2] <- adoptData[[3]]
          displayHb[i, 2] <- adoptData[[3]]
          exportHb[i, 3] <- minHb
          displayHb[i, 3] <- signif(minHb, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportHb[i, posPar] <- estPar
          displayHb[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_Hb <- tkscrollbar(frameHb_1, repeatinterval = 5, command = function(...) tkyview(mlbHb, ...))
    mlbHb <- tk2mclistbox(frameHb_1, width = 35 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_Hb, ...))
    tk2column(mlbHb, "add", label = colDisplay[1], width = 10)
    tk2column(mlbHb, "add", label = colDisplay[2], width = 15)
    tk2column(mlbHb, "add", label = colDisplay[3], width = 10)
    for(i in 1:nP){
      tk2column(mlbHb, "add", label = colDisplay[i + 3], width = 12)
    }
    tkgrid(mlbHb, scrY_Hb)
    tk2insert.multi(mlbHb, "end", displayHb)
    tkgrid.configure(scrY_Hb, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameHb_1, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_Hb()), padx = 5, pady = 5)
    tkgrid(frameHb_1, frameHb_2, padx = 10, pady = 5, sticky = "n")

    assign("displayHb", displayHb, envir = envParProj)
    assign("exportHb", exportHb, envir = envParProj)
  }

  resultBSR <- function(){
    changeModelBSR <- function(nameModel, posL){
      adoptModel <- function(selectModel){
        exportBSR <- get("exportBSR", pos = envParProj)
        displayBSR <- get("displayBSR", pos = envParProj)
        methOneL <- displayBSR[posL, 4]
        inputOk <- "ok"
        if(methOneL == "Multiple loci together"){
          inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  nameL[posL], " is 'multiple loci together'. The selected model will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
        }
        if(inputOk == "ok"){
          frameBSR_1_1 <- get("frameBSR_1_1", pos = envParGUI)
          frameBSR_1_2 <- get("frameBSR_1_2", pos = envParGUI)
          mlbBSR <- get("mlbBSR", pos = envParGUI)
          tkdestroy(frameBSR_1_1)
          tkdestroy(frameBSR_1_2)
          tkdestroy(mlbBSR)
          frameBSR_1_1 <- tkframe(frameBSR_1)
          frameBSR_1_2 <- tkframe(frameBSR_1)
          mlbBSR <- tk2mclistbox(frameBSR_1_1, width = 72 + 12 * nP, height = 27, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbBSR, "add", label = colDisplay[1], width = 10)
          tk2column(mlbBSR, "add", label = colDisplay[2], width = 15)
          tk2column(mlbBSR, "add", label = colDisplay[3], width = 15)
          tk2column(mlbBSR, "add", label = colDisplay[4], width = 22)
          tk2column(mlbBSR, "add", label = colDisplay[5], width = 10)
          for(i in 1:nP){
            tk2column(mlbBSR, "add", label = colDisplay[i + 5], width = 12)
          }
          if(methOneL == "Multiple loci together"){
            for(i in 1:nrow(displayBSR)){
              bsrMleData_oneL <- bsrMleData[[i]]
              if(bsrMleData_oneL[[length(bsrMleData_oneL)]] == "Multiple loci together"){
                adoptData <- extAdoptModel(bsrMleData_oneL, selectModel)
                exportBSR[i, 2] <- adoptData[[3]]
                displayBSR[i, 2] <- adoptData[[3]]
                exportBSR[i, 3] <- "Yes"
                displayBSR[i, 3] <- "Yes"
                estPar <- adoptData[[2]]
                posPar <- match(names(estPar), colDisplay)
                exportBSR[i, 6:(nP + 5)] <- ""
                exportBSR[i, posPar] <- estPar
                displayBSR[i, 6:(nP + 5)] <- ""
                displayBSR[i, posPar] <- signif(estPar, 3)
              }
            }
          }else{
            bsrMleData_oneL <- bsrMleData[[posL]]
            adoptData <- extAdoptModel(bsrMleData_oneL, selectModel)
            exportBSR[posL, 2] <- adoptData[[3]]
            displayBSR[posL, 2] <- adoptData[[3]]
            exportBSR[posL, 3] <- "Yes"
            displayBSR[posL, 3] <- "Yes"
            estPar <- adoptData[[2]]
            posPar <- match(names(estPar), colDisplay)
            exportBSR[posL, 6:(nP + 5)] <- ""
            exportBSR[posL, posPar] <- estPar
            displayBSR[posL, 6:(nP + 5)] <- ""
            displayBSR[posL, posPar] <- signif(estPar, 3)
          }
          tkgrid(mlbBSR, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbBSR, "end", displayBSR)
          tkgrid(tkbutton(frameBSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_BSR()), padx = 5, pady = 5)
          tkgrid(frameBSR_1_1)
          tkgrid(frameBSR_1_2)

          assign("frameBSR_1_1", frameBSR_1_1, envir = envParGUI)
          assign("frameBSR_1_2", frameBSR_1_2, envir = envParGUI)
          assign("mlbBSR", mlbBSR, envir = envParGUI)
          assign("exportBSR", exportBSR, envir = envParProj)
          assign("displayBSR", displayBSR, envir = envParProj)
          tkdestroy(tfChange)
        }
      }

      tfChange <- tktoplevel()
      tkwm.title(tfChange, "Change the adopted model")
      tkgrid(tklabel(tfChange, text = "Select a model : "), row = 0, column = 0, padx = 5, pady = 5)
      selectModelVar <- tclVar(nameModel[1])
      tkgrid(ttkcombobox(tfChange, values = nameModel, textvariable = selectModelVar, state = "readonly"), row = 0, column = 1, padx = 5, pady = 5)
      tkgrid(tkbutton(tfChange, text = "    Adopt    ", cursor = "hand2", command = function() adoptModel(tclvalue(selectModelVar))), row = 0, column = 2, padx = 5, pady = 5)
    }

    showDetail_BSR <- function(){
      mlbBSR <- get("mlbBSR", pos = envParGUI)
      posL <- tclvalue(tkcurselection(mlbBSR))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameBSR_2_1 <- get("frameBSR_2_1", pos = envParGUI)
        frameBSR_2_2 <- get("frameBSR_2_2", pos = envParGUI)
        frameBSR_2_3 <- get("frameBSR_2_3", pos = envParGUI)
        frameBSR_2_4 <- get("frameBSR_2_4", pos = envParGUI)
        tkdestroy(frameBSR_2_1)
        tkdestroy(frameBSR_2_2)
        tkdestroy(frameBSR_2_3)
        tkdestroy(frameBSR_2_4)
        frameBSR_2_1 <- tkframe(frameBSR_2)
        frameBSR_2_2 <- tkframe(frameBSR_2, relief = "groove", borderwidth = 2)
        frameBSR_2_3 <- tkframe(frameBSR_2, relief = "groove", borderwidth = 2)
        frameBSR_2_4 <- tkframe(frameBSR_2)

        posL <- as.numeric(posL) + 1
        bsrMleData_oneL <- bsrMleData[[posL]]
        selectLocus <- nameL[posL]
        tkgrid(tklabel(frameBSR_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(bsrMleData_oneL) > 0){
          tkgrid(tklabel(frameBSR_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbBSR2 <- tk2mclistbox(frameBSR_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbBSR2, "add", label = "Model", width = 15)
          tk2column(mlbBSR2, "add", label = "AIC", width = 15)
          nM <- length(bsrMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            bsrMleData_oneM <- bsrMleData_oneL[[i]]
            displayAIC[i, 1] <- bsrMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(bsrMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbBSR2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbBSR2, "end", displayAIC)

          tkgrid(tklabel(frameBSR_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameBSR_2_3_1 <- tkframe(frameBSR_2_3)
          tkgrid(tklabel(frameBSR_2_3_1, text = "Model : "),
                 ttkcombobox(frameBSR_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameBSR_2_3_1)
          frameBSR_2_3_2 <- tkframe(frameBSR_2_3)
          tkgrid(tkbutton(frameBSR_2_3_2, text = "    Mean    ", cursor = "hand2", command = function() graphSrMean(selectLocus, "BSR", tclvalue(selectModelVar), bsrMleData_oneL, bsrUseData, methBSR)),
                 tkbutton(frameBSR_2_3_2, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphSrVar(selectLocus, "BSR", tclvalue(selectModelVar), bsrMleData_oneL, bsrUseData, maxBSR, methBSR)),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameBSR_2_3_2)
          tkgrid(tkbutton(frameBSR_2_4, text = "    Change the adopted model    ", cursor = "hand2", command = function() changeModelBSR(nameModel, posL)), padx = 5, pady = 5, sticky = "w")
          tkgrid(tkbutton(frameBSR_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "BSR")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameBSR_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameBSR_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameBSR_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameBSR_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameBSR_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameBSR_2_1", frameBSR_2_1, envir = envParGUI)
        assign("frameBSR_2_2", frameBSR_2_2, envir = envParGUI)
        assign("frameBSR_2_3", frameBSR_2_3, envir = envParGUI)
        assign("frameBSR_2_4", frameBSR_2_4, envir = envParGUI)
      }
    }

    frameBSR_1 <- tkframe(subTabBSR)
    frameBSR_2 <- tkframe(subTabBSR, relief = "groove", borderwidth = 2)
    frameBSR_1_1 <- tkframe(frameBSR_1)
    frameBSR_1_2 <- tkframe(frameBSR_1)
    assign("frameBSR_1_1", frameBSR_1_1, envir = envParGUI)
    assign("frameBSR_1_2", frameBSR_1_2, envir = envParGUI)
    assign("frameBSR_2_1", tkframe(frameBSR_2), envir = envParGUI)
    assign("frameBSR_2_2", tkframe(frameBSR_2), envir = envParGUI)
    assign("frameBSR_2_3", tkframe(frameBSR_2), envir = envParGUI)
    assign("frameBSR_2_4", tkframe(frameBSR_2), envir = envParGUI)
    nL <- length(bsrMleData)
    nameL <- names(bsrMleData)
    nameP <- unique(mleCondBSR[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "User-selected", "Method", "Max. BSR", nameP)
    displayBSR <- get("displayBSR", pos = envParProj)
    exportBSR <- get("exportBSR", pos = envParProj)
    if((length(displayBSR) == 0) || (length(exportBSR) == 0)){
      displayBSR <- matrix("", nL, nP + 5)
      displayBSR[, 1] <- nameL
      exportBSR <- displayBSR
      colnames(exportBSR) <- colDisplay
      for(i in 1:nL){
        bsrMleData_oneL <- bsrMleData[[i]]
        nMle <- length(bsrMleData_oneL)
        if(nMle > 0){
          adoptData <- extAdoptModel(bsrMleData_oneL)
          exportBSR[i, 2] <- adoptData[[3]]
          displayBSR[i, 2] <- adoptData[[3]]
          exportBSR[i, 3] <- "No"
          displayBSR[i, 3] <- "No"
          exportBSR[i, 4] <- bsrMleData_oneL[[nMle]]
          displayBSR[i, 4] <- bsrMleData_oneL[[nMle]]
          exportBSR[i, 5] <- maxBSR
          displayBSR[i, 5] <- signif(maxBSR, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportBSR[i, posPar] <- estPar
          displayBSR[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_BSR <- tkscrollbar(frameBSR_1_1, repeatinterval = 5, command = function(...) tkyview(mlbBSR, ...))
    mlbBSR <- tk2mclistbox(frameBSR_1_1, width = 72 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_BSR, ...))
    tk2column(mlbBSR, "add", label = colDisplay[1], width = 10)
    tk2column(mlbBSR, "add", label = colDisplay[2], width = 15)
    tk2column(mlbBSR, "add", label = colDisplay[3], width = 15)
    tk2column(mlbBSR, "add", label = colDisplay[4], width = 22)
    tk2column(mlbBSR, "add", label = colDisplay[5], width = 10)
    for(i in 1:nP){
      tk2column(mlbBSR, "add", label = colDisplay[i + 5], width = 12)
    }
    tkgrid(mlbBSR, scrY_BSR)
    tk2insert.multi(mlbBSR, "end", displayBSR)
    tkgrid.configure(scrY_BSR, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameBSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_BSR()), padx = 5, pady = 5)
    tkgrid(frameBSR_1_1)
    tkgrid(frameBSR_1_2)
    tkgrid(frameBSR_1, frameBSR_2, padx = 10, pady = 5, sticky = "n")

    assign("mlbBSR", mlbBSR, envir = envParGUI)
    assign("exportBSR", exportBSR, envir = envParProj)
    assign("displayBSR", displayBSR, envir = envParProj)
  }

  resultFSR <- function(){
    changeModelFSR <- function(nameModel, posL){
      adoptModel <- function(selectModel){
        exportFSR <- get("exportFSR", pos = envParProj)
        displayFSR <- get("displayFSR", pos = envParProj)
        methOneL <- displayFSR[posL, 4]
        inputOk <- "ok"
        if(methOneL == "Multiple loci together"){
          inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  nameL[posL], " is 'multiple loci together'. The selected model will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
        }
        if(inputOk == "ok"){
          frameFSR_1_1 <- get("frameFSR_1_1", pos = envParGUI)
          frameFSR_1_2 <- get("frameFSR_1_2", pos = envParGUI)
          mlbFSR <- get("mlbFSR", pos = envParGUI)
          tkdestroy(frameFSR_1_1)
          tkdestroy(frameFSR_1_2)
          tkdestroy(mlbFSR)
          frameFSR_1_1 <- tkframe(frameFSR_1)
          frameFSR_1_2 <- tkframe(frameFSR_1)
          mlbFSR <- tk2mclistbox(frameFSR_1_1, width = 72 + 12 * nP, height = 27, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbFSR, "add", label = colDisplay[1], width = 10)
          tk2column(mlbFSR, "add", label = colDisplay[2], width = 15)
          tk2column(mlbFSR, "add", label = colDisplay[3], width = 15)
          tk2column(mlbFSR, "add", label = colDisplay[4], width = 22)
          tk2column(mlbFSR, "add", label = colDisplay[5], width = 10)
          for(i in 1:nP){
            tk2column(mlbFSR, "add", label = colDisplay[i + 5], width = 12)
          }
          if(methOneL == "Multiple loci together"){
            for(i in 1:nrow(displayFSR)){
              fsrMleData_oneL <- fsrMleData[[i]]
              if(fsrMleData_oneL[[length(fsrMleData_oneL)]] == "Multiple loci together"){
                adoptData <- extAdoptModel(fsrMleData_oneL, selectModel)
                exportFSR[i, 2] <- adoptData[[3]]
                displayFSR[i, 2] <- adoptData[[3]]
                exportFSR[i, 3] <- "Yes"
                displayFSR[i, 3] <- "Yes"
                estPar <- adoptData[[2]]
                posPar <- match(names(estPar), colDisplay)
                exportFSR[i, 6:(nP + 5)] <- ""
                exportFSR[i, posPar] <- estPar
                displayFSR[i, 6:(nP + 5)] <- ""
                displayFSR[i, posPar] <- signif(estPar, 3)
              }
            }
          }else{
            fsrMleData_oneL <- fsrMleData[[posL]]
            adoptData <- extAdoptModel(fsrMleData_oneL, selectModel)
            exportFSR[posL, 2] <- adoptData[[3]]
            displayFSR[posL, 2] <- adoptData[[3]]
            exportFSR[posL, 3] <- "Yes"
            displayFSR[posL, 3] <- "Yes"
            estPar <- adoptData[[2]]
            posPar <- match(names(estPar), colDisplay)
            exportFSR[posL, 6:(nP + 5)] <- ""
            exportFSR[posL, posPar] <- estPar
            displayFSR[posL, 6:(nP + 5)] <- ""
            displayFSR[posL, posPar] <- signif(estPar, 3)
          }
          tkgrid(mlbFSR, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbFSR, "end", displayFSR)
          tkgrid(tkbutton(frameFSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_FSR()), padx = 5, pady = 5)
          tkgrid(frameFSR_1_1)
          tkgrid(frameFSR_1_2)

          assign("frameFSR_1_1", frameFSR_1_1, envir = envParGUI)
          assign("frameFSR_1_2", frameFSR_1_2, envir = envParGUI)
          assign("mlbFSR", mlbFSR, envir = envParGUI)
          assign("exportFSR", exportFSR, envir = envParProj)
          assign("displayFSR", displayFSR, envir = envParProj)
          tkdestroy(tfChange)
        }
      }

      tfChange <- tktoplevel()
      tkwm.title(tfChange, "Change the adopted model")
      tkgrid(tklabel(tfChange, text = "Select a model : "), row = 0, column = 0, padx = 5, pady = 5)
      selectModelVar <- tclVar(nameModel[1])
      tkgrid(ttkcombobox(tfChange, values = nameModel, textvariable = selectModelVar, state = "readonly"), row = 0, column = 1, padx = 5, pady = 5)
      tkgrid(tkbutton(tfChange, text = "    Adopt    ", cursor = "hand2", command = function() adoptModel(tclvalue(selectModelVar))), row = 0, column = 2, padx = 5, pady = 5)
    }

    showDetail_FSR <- function(){
      mlbFSR <- get("mlbFSR", pos = envParGUI)
      posL <- tclvalue(tkcurselection(mlbFSR))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameFSR_2_1 <- get("frameFSR_2_1", pos = envParGUI)
        frameFSR_2_2 <- get("frameFSR_2_2", pos = envParGUI)
        frameFSR_2_3 <- get("frameFSR_2_3", pos = envParGUI)
        frameFSR_2_4 <- get("frameFSR_2_4", pos = envParGUI)
        tkdestroy(frameFSR_2_1)
        tkdestroy(frameFSR_2_2)
        tkdestroy(frameFSR_2_3)
        tkdestroy(frameFSR_2_4)
        frameFSR_2_1 <- tkframe(frameFSR_2)
        frameFSR_2_2 <- tkframe(frameFSR_2, relief = "groove", borderwidth = 2)
        frameFSR_2_3 <- tkframe(frameFSR_2, relief = "groove", borderwidth = 2)
        frameFSR_2_4 <- tkframe(frameFSR_2)

        posL <- as.numeric(posL) + 1
        selectLocus <- nameL[posL]
        fsrMleData_oneL <- fsrMleData[[posL]]
        tkgrid(tklabel(frameFSR_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(fsrMleData_oneL) > 0){
          tkgrid(tklabel(frameFSR_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbFSR2 <- tk2mclistbox(frameFSR_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbFSR2, "add", label = "Model", width = 15)
          tk2column(mlbFSR2, "add", label = "AIC", width = 15)
          nM <- length(fsrMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            fsrMleData_oneM <- fsrMleData_oneL[[i]]
            displayAIC[i, 1] <- fsrMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(fsrMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbFSR2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbFSR2, "end", displayAIC)

          tkgrid(tklabel(frameFSR_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameFSR_2_3_1 <- tkframe(frameFSR_2_3)
          tkgrid(tklabel(frameFSR_2_3_1, text = "Model : "),
                 ttkcombobox(frameFSR_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameFSR_2_3_1)
          frameFSR_2_3_2 <- tkframe(frameFSR_2_3)
          tkgrid(tkbutton(frameFSR_2_3_2, text = "    Mean    ", cursor = "hand2", command = function() graphSrMean(selectLocus, "FSR", tclvalue(selectModelVar), fsrMleData_oneL, fsrUseData, methFSR)),
                 tkbutton(frameFSR_2_3_2, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphSrVar(selectLocus, "FSR", tclvalue(selectModelVar), fsrMleData_oneL, fsrUseData, maxFSR, methFSR)),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameFSR_2_3_2)
          tkgrid(tkbutton(frameFSR_2_4, text = "    Change the adopted model    ", cursor = "hand2", command = function() changeModelFSR(nameModel, posL)), padx = 5, pady = 5, sticky = "w")
          tkgrid(tkbutton(frameFSR_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "FSR")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameFSR_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameFSR_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameFSR_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameFSR_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameFSR_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameFSR_2_1", frameFSR_2_1, envir = envParGUI)
        assign("frameFSR_2_2", frameFSR_2_2, envir = envParGUI)
        assign("frameFSR_2_3", frameFSR_2_3, envir = envParGUI)
        assign("frameFSR_2_4", frameFSR_2_4, envir = envParGUI)
      }
    }

    frameFSR_1 <- tkframe(subTabFSR)
    frameFSR_2 <- tkframe(subTabFSR, relief = "groove", borderwidth = 2)
    frameFSR_1_1 <- tkframe(frameFSR_1)
    frameFSR_1_2 <- tkframe(frameFSR_1)
    assign("frameFSR_1_1", frameFSR_1_1, envir = envParGUI)
    assign("frameFSR_1_2", frameFSR_1_2, envir = envParGUI)
    assign("frameFSR_2_1", tkframe(frameFSR_2), envir = envParGUI)
    assign("frameFSR_2_2", tkframe(frameFSR_2), envir = envParGUI)
    assign("frameFSR_2_3", tkframe(frameFSR_2), envir = envParGUI)
    assign("frameFSR_2_4", tkframe(frameFSR_2), envir = envParGUI)
    nL <- length(fsrMleData)
    nameL <- names(fsrMleData)
    nameP <- unique(mleCondFSR[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "User-selected", "Method", "Max. FSR", nameP)
    displayFSR <- get("displayFSR", pos = envParProj)
    exportFSR <- get("exportFSR", pos = envParProj)
    if((length(displayFSR) == 0) || (length(exportFSR) == 0)){
      displayFSR <- matrix("", nL, nP + 5)
      displayFSR[, 1] <- nameL
      exportFSR <- displayFSR
      colnames(exportFSR) <- colDisplay
      for(i in 1:nL){
        fsrMleData_oneL <- fsrMleData[[i]]
        nMle <- length(fsrMleData_oneL)
        if(nMle > 0){
          adoptData <- extAdoptModel(fsrMleData_oneL)
          exportFSR[i, 2] <- adoptData[[3]]
          displayFSR[i, 2] <- adoptData[[3]]
          exportFSR[i, 3] <- "No"
          displayFSR[i, 3] <- "No"
          exportFSR[i, 4] <- fsrMleData_oneL[[nMle]]
          displayFSR[i, 4] <- fsrMleData_oneL[[nMle]]
          exportFSR[i, 5] <- maxFSR
          displayFSR[i, 5] <- signif(maxFSR, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportFSR[i, posPar] <- estPar
          displayFSR[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_FSR <- tkscrollbar(frameFSR_1_1, repeatinterval = 5, command = function(...) tkyview(mlbFSR, ...))
    mlbFSR <- tk2mclistbox(frameFSR_1_1, width = 72 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_FSR, ...))
    tk2column(mlbFSR, "add", label = colDisplay[1], width = 10)
    tk2column(mlbFSR, "add", label = colDisplay[2], width = 15)
    tk2column(mlbFSR, "add", label = colDisplay[3], width = 15)
    tk2column(mlbFSR, "add", label = colDisplay[4], width = 22)
    tk2column(mlbFSR, "add", label = colDisplay[5], width = 10)
    for(i in 1:nP){
      tk2column(mlbFSR, "add", label = colDisplay[i + 5], width = 12)
    }
    tkgrid(mlbFSR, scrY_FSR)
    tk2insert.multi(mlbFSR, "end", displayFSR)
    tkgrid.configure(scrY_FSR, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameFSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_FSR()), padx = 5, pady = 5)
    tkgrid(frameFSR_1_1)
    tkgrid(frameFSR_1_2)
    tkgrid(frameFSR_1, frameFSR_2, padx = 10, pady = 5, sticky = "n")

    assign("mlbFSR", mlbFSR, envir = envParGUI)
    assign("exportFSR", exportFSR, envir = envParProj)
    assign("displayFSR", displayFSR, envir = envParProj)
  }

  resultDSR <- function(){
    changeModelDSR <- function(nameModel, posL){
      adoptModel <- function(selectModel){
        exportDSR <- get("exportDSR", pos = envParProj)
        displayDSR <- get("displayDSR", pos = envParProj)
        methOneL <- displayDSR[posL, 4]
        inputOk <- "ok"
        if(methOneL == "Multiple loci together"){
          inputOk <- tclvalue(tkmessageBox(message = paste0("The modeling method of ",  nameL[posL], " is 'multiple loci together'. The selected model will be also reflected in other loci with the method 'multiple loci together'. Do you want to continue?"), type = "okcancel", icon = "warning"))
        }
        if(inputOk == "ok"){
          frameDSR_1_1 <- get("frameDSR_1_1", pos = envParGUI)
          frameDSR_1_2 <- get("frameDSR_1_2", pos = envParGUI)
          mlbDSR <- get("mlbDSR", pos = envParGUI)
          tkdestroy(frameDSR_1_1)
          tkdestroy(frameDSR_1_2)
          tkdestroy(mlbDSR)
          frameDSR_1_1 <- tkframe(frameDSR_1)
          frameDSR_1_2 <- tkframe(frameDSR_1)
          mlbDSR <- tk2mclistbox(frameDSR_1_1, width = 72 + 12 * nP, height = 27, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbDSR, "add", label = colDisplay[1], width = 10)
          tk2column(mlbDSR, "add", label = colDisplay[2], width = 15)
          tk2column(mlbDSR, "add", label = colDisplay[3], width = 15)
          tk2column(mlbDSR, "add", label = colDisplay[4], width = 22)
          tk2column(mlbDSR, "add", label = colDisplay[5], width = 10)
          for(i in 1:nP){
            tk2column(mlbDSR, "add", label = colDisplay[i + 5], width = 12)
          }
          if(methOneL == "Multiple loci together"){
            for(i in 1:nrow(displayDSR)){
              dsrMleData_oneL <- dsrMleData[[i]]
              if(dsrMleData_oneL[[length(dsrMleData_oneL)]] == "Multiple loci together"){
                adoptData <- extAdoptModel(dsrMleData_oneL, selectModel)
                exportDSR[i, 2] <- adoptData[[3]]
                displayDSR[i, 2] <- adoptData[[3]]
                exportDSR[i, 3] <- "Yes"
                displayDSR[i, 3] <- "Yes"
                estPar <- adoptData[[2]]
                posPar <- match(names(estPar), colDisplay)
                exportDSR[i, 6:(nP + 5)] <- ""
                exportDSR[i, posPar] <- estPar
                displayDSR[i, 6:(nP + 5)] <- ""
                displayDSR[i, posPar] <- signif(estPar, 3)
              }
            }
          }else{
            dsrMleData_oneL <- dsrMleData[[posL]]
            adoptData <- extAdoptModel(dsrMleData_oneL, selectModel)
            exportDSR[posL, 2] <- adoptData[[3]]
            displayDSR[posL, 2] <- adoptData[[3]]
            exportDSR[posL, 3] <- "Yes"
            displayDSR[posL, 3] <- "Yes"
            estPar <- adoptData[[2]]
            posPar <- match(names(estPar), colDisplay)
            exportDSR[posL, 6:(nP + 5)] <- ""
            exportDSR[posL, posPar] <- estPar
            displayDSR[posL, 6:(nP + 5)] <- ""
            displayDSR[posL, posPar] <- signif(estPar, 3)
          }
          tkgrid(mlbDSR, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbDSR, "end", displayDSR)
          tkgrid(tkbutton(frameDSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_DSR()), padx = 5, pady = 5)
          tkgrid(frameDSR_1_1)
          tkgrid(frameDSR_1_2)

          assign("frameDSR_1_1", frameDSR_1_1, envir = envParGUI)
          assign("frameDSR_1_2", frameDSR_1_2, envir = envParGUI)
          assign("mlbDSR", mlbDSR, envir = envParGUI)
          assign("exportDSR", exportDSR, envir = envParProj)
          assign("displayDSR", displayDSR, envir = envParProj)
          tkdestroy(tfChange)
        }
      }

      tfChange <- tktoplevel()
      tkwm.title(tfChange, "Change the adopted model")
      tkgrid(tklabel(tfChange, text = "Select a model : "), row = 0, column = 0, padx = 5, pady = 5)
      selectModelVar <- tclVar(nameModel[1])
      tkgrid(ttkcombobox(tfChange, values = nameModel, textvariable = selectModelVar, state = "readonly"), row = 0, column = 1, padx = 5, pady = 5)
      tkgrid(tkbutton(tfChange, text = "    Adopt    ", cursor = "hand2", command = function() adoptModel(tclvalue(selectModelVar))), row = 0, column = 2, padx = 5, pady = 5)
    }

    showDetail_DSR <- function(){
      mlbDSR <- get("mlbDSR", pos = envParGUI)
      posL <- tclvalue(tkcurselection(mlbDSR))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameDSR_2_1 <- get("frameDSR_2_1", pos = envParGUI)
        frameDSR_2_2 <- get("frameDSR_2_2", pos = envParGUI)
        frameDSR_2_3 <- get("frameDSR_2_3", pos = envParGUI)
        frameDSR_2_4 <- get("frameDSR_2_4", pos = envParGUI)
        tkdestroy(frameDSR_2_1)
        tkdestroy(frameDSR_2_2)
        tkdestroy(frameDSR_2_3)
        tkdestroy(frameDSR_2_4)
        frameDSR_2_1 <- tkframe(frameDSR_2)
        frameDSR_2_2 <- tkframe(frameDSR_2, relief = "groove", borderwidth = 2)
        frameDSR_2_3 <- tkframe(frameDSR_2, relief = "groove", borderwidth = 2)
        frameDSR_2_4 <- tkframe(frameDSR_2)

        posL <- as.numeric(posL) + 1
        dsrMleData_oneL <- dsrMleData[[posL]]
        selectLocus <- nameL[posL]
        tkgrid(tklabel(frameDSR_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(dsrMleData_oneL) > 0){
          tkgrid(tklabel(frameDSR_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbDSR2 <- tk2mclistbox(frameDSR_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbDSR2, "add", label = "Model", width = 15)
          tk2column(mlbDSR2, "add", label = "AIC", width = 15)
          nM <- length(dsrMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            dsrMleData_oneM <- dsrMleData_oneL[[i]]
            displayAIC[i, 1] <- dsrMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(dsrMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbDSR2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbDSR2, "end", displayAIC)

          tkgrid(tklabel(frameDSR_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameDSR_2_3_1 <- tkframe(frameDSR_2_3)
          tkgrid(tklabel(frameDSR_2_3_1, text = "Model : "),
                 ttkcombobox(frameDSR_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameDSR_2_3_1)
          frameDSR_2_3_2 <- tkframe(frameDSR_2_3)
          tkgrid(tkbutton(frameDSR_2_3_2, text = "    Mean    ", cursor = "hand2", command = function() graphSrMean(selectLocus, "DSR", tclvalue(selectModelVar), dsrMleData_oneL, dsrUseData, methDSR)),
                 tkbutton(frameDSR_2_3_2, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphSrVar(selectLocus, "DSR", tclvalue(selectModelVar), dsrMleData_oneL, dsrUseData, maxDSR, methDSR)),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameDSR_2_3_2)
          tkgrid(tkbutton(frameDSR_2_4, text = "    Change the adopted model    ", cursor = "hand2", command = function() changeModelDSR(nameModel, posL)), padx = 5, pady = 5, sticky = "w")
          tkgrid(tkbutton(frameDSR_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "DSR")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameDSR_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameDSR_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameDSR_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameDSR_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameDSR_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameDSR_2_1", frameDSR_2_1, envir = envParGUI)
        assign("frameDSR_2_2", frameDSR_2_2, envir = envParGUI)
        assign("frameDSR_2_3", frameDSR_2_3, envir = envParGUI)
        assign("frameDSR_2_4", frameDSR_2_4, envir = envParGUI)
      }
    }

    frameDSR_1 <- tkframe(subTabDSR)
    frameDSR_2 <- tkframe(subTabDSR, relief = "groove", borderwidth = 2)
    frameDSR_1_1 <- tkframe(frameDSR_1)
    frameDSR_1_2 <- tkframe(frameDSR_1)
    assign("frameDSR_1_1", frameDSR_1_1, envir = envParGUI)
    assign("frameDSR_1_2", frameDSR_1_2, envir = envParGUI)
    assign("frameDSR_2_1", tkframe(frameDSR_2), envir = envParGUI)
    assign("frameDSR_2_2", tkframe(frameDSR_2), envir = envParGUI)
    assign("frameDSR_2_3", tkframe(frameDSR_2), envir = envParGUI)
    assign("frameDSR_2_4", tkframe(frameDSR_2), envir = envParGUI)
    nL <- length(dsrMleData)
    nameL <- names(dsrMleData)
    nameP <- unique(mleCondDSR[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "User-selected", "Method", "Max. DSR", nameP)
    displayDSR <- get("displayDSR", pos = envParProj)
    exportDSR <- get("exportDSR", pos = envParProj)
    if((length(displayDSR) == 0) || (length(exportDSR) == 0)){
      displayDSR <- matrix("", nL, nP + 5)
      displayDSR[, 1] <- nameL
      exportDSR <- displayDSR
      colnames(exportDSR) <- colDisplay
      for(i in 1:nL){
        dsrMleData_oneL <- dsrMleData[[i]]
        nMle <- length(dsrMleData_oneL)
        if(nMle > 0){
          adoptData <- extAdoptModel(dsrMleData_oneL)
          exportDSR[i, 2] <- adoptData[[3]]
          displayDSR[i, 2] <- adoptData[[3]]
          exportDSR[i, 3] <- "No"
          displayDSR[i, 3] <- "No"
          exportDSR[i, 4] <- dsrMleData_oneL[[nMle]]
          displayDSR[i, 4] <- dsrMleData_oneL[[nMle]]
          exportDSR[i, 5] <- maxDSR
          displayDSR[i, 5] <- signif(maxDSR, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportDSR[i, posPar] <- estPar
          displayDSR[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_DSR <- tkscrollbar(frameDSR_1_1, repeatinterval = 5, command = function(...) tkyview(mlbDSR, ...))
    mlbDSR <- tk2mclistbox(frameDSR_1_1, width = 72 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_DSR, ...))
    tk2column(mlbDSR, "add", label = colDisplay[1], width = 10)
    tk2column(mlbDSR, "add", label = colDisplay[2], width = 15)
    tk2column(mlbDSR, "add", label = colDisplay[3], width = 15)
    tk2column(mlbDSR, "add", label = colDisplay[4], width = 22)
    tk2column(mlbDSR, "add", label = colDisplay[5], width = 10)
    for(i in 1:nP){
      tk2column(mlbDSR, "add", label = colDisplay[i + 5], width = 12)
    }
    tkgrid(mlbDSR, scrY_DSR)
    tk2insert.multi(mlbDSR, "end", displayDSR)
    tkgrid.configure(scrY_DSR, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameDSR_1_2, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_DSR()), padx = 5, pady = 5)
    tkgrid(frameDSR_1_1)
    tkgrid(frameDSR_1_2)
    tkgrid(frameDSR_1, frameDSR_2, padx = 10, pady = 5, sticky = "n")

    assign("mlbDSR", mlbDSR, envir = envParGUI)
    assign("exportDSR", exportDSR, envir = envParProj)
    assign("displayDSR", displayDSR, envir = envParProj)
  }

  resultM2SR <- function(){
    showDetail_M2SR <- function(){
      posL <- tclvalue(tkcurselection(mlbM2SR))
      if(posL == ""){
        tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
      }else{
        frameM2SR_2_1 <- get("frameM2SR_2_1", pos = envParGUI)
        frameM2SR_2_2 <- get("frameM2SR_2_2", pos = envParGUI)
        frameM2SR_2_3 <- get("frameM2SR_2_3", pos = envParGUI)
        frameM2SR_2_4 <- get("frameM2SR_2_4", pos = envParGUI)
        tkdestroy(frameM2SR_2_1)
        tkdestroy(frameM2SR_2_2)
        tkdestroy(frameM2SR_2_3)
        tkdestroy(frameM2SR_2_4)
        frameM2SR_2_1 <- tkframe(frameM2SR_2)
        frameM2SR_2_2 <- tkframe(frameM2SR_2, relief = "groove", borderwidth = 2)
        frameM2SR_2_3 <- tkframe(frameM2SR_2, relief = "groove", borderwidth = 2)
        frameM2SR_2_4 <- tkframe(frameM2SR_2)

        posL <- as.numeric(posL) + 1
        m2srMleData_oneL <- m2srMleData[[posL]]
        selectLocus <- nameL[posL]
        tkgrid(tklabel(frameM2SR_2_1, text = paste0("Locus : ", selectLocus)))
        if(length(m2srMleData_oneL) > 0){
          tkgrid(tklabel(frameM2SR_2_2, text = "Comparison of each model", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          mlbM2SR2 <- tk2mclistbox(frameM2SR_2_2, width = 30, height = 4, resizablecolumns = TRUE, selectmode = "single")
          tk2column(mlbM2SR2, "add", label = "Model", width = 15)
          tk2column(mlbM2SR2, "add", label = "AIC", width = 15)
          nM <- length(m2srMleData_oneL) - 1
          displayAIC <- matrix("", nM, 2)
          for(i in 1:nM){
            m2srMleData_oneM <- m2srMleData_oneL[[i]]
            displayAIC[i, 1] <- m2srMleData_oneM[[3]]
            displayAIC[i, 2] <- signif(m2srMleData_oneM[[1]]["AIC"], 3)
          }
          tkgrid(mlbM2SR2, padx = 5, pady = 5, sticky = "w")
          tk2insert.multi(mlbM2SR2, "end", displayAIC)

          tkgrid(tklabel(frameM2SR_2_3, text = "Graph", font = "Helvetica 10 bold"), padx = 5, pady = 5, sticky = "w")
          nameModel <- displayAIC[, 1]
          selectModelVar <- tclVar(nameModel[1])
          frameM2SR_2_3_1 <- tkframe(frameM2SR_2_3)
          tkgrid(tklabel(frameM2SR_2_3_1, text = "Model : "),
                 ttkcombobox(frameM2SR_2_3_1, values = nameModel, textvariable = selectModelVar, state = "readonly"),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameM2SR_2_3_1)
          frameM2SR_2_3_2 <- tkframe(frameM2SR_2_3)
          tkgrid(tkbutton(frameM2SR_2_3_2, text = "    Mean    ", cursor = "hand2", command = function() graphSrMean(selectLocus, "M2SR", tclvalue(selectModelVar), m2srMleData_oneL, m2srUseData, methM2SR)),
                 tkbutton(frameM2SR_2_3_2, text = "    5% and 95% quantiles    ", cursor = "hand2", command = function() graphSrVar(selectLocus, "M2SR", tclvalue(selectModelVar), m2srMleData_oneL, m2srUseData, maxM2SR, methM2SR)),
                 padx = 5, pady = 5, sticky = "w")
          tkgrid(frameM2SR_2_3_2)
          tkgrid(tkbutton(frameM2SR_2_4, text = "    Re-estimate parameters    ", cursor = "hand2", command = function() reestPar(selectLocus, nameModel, "M2SR")), padx = 5, pady = 5, sticky = "w")
        }else{
          tkgrid(tklabel(frameM2SR_2_1, text = "Not consider"), padx = 5, pady = 5, sticky = "w")
        }
        tkgrid(frameM2SR_2_1, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameM2SR_2_2, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameM2SR_2_3, padx = 5, pady = 5, sticky = "w")
        tkgrid(frameM2SR_2_4, padx = 5, pady = 5, sticky = "w")
        assign("frameM2SR_2_1", frameM2SR_2_1, envir = envParGUI)
        assign("frameM2SR_2_2", frameM2SR_2_2, envir = envParGUI)
        assign("frameM2SR_2_3", frameM2SR_2_3, envir = envParGUI)
        assign("frameM2SR_2_4", frameM2SR_2_4, envir = envParGUI)
      }
    }

    frameM2SR_1 <- tkframe(subTabM2SR)
    frameM2SR_2 <- tkframe(subTabM2SR, relief = "groove", borderwidth = 2)
    assign("frameM2SR_2_1", tkframe(frameM2SR_2), envir = envParGUI)
    assign("frameM2SR_2_2", tkframe(frameM2SR_2), envir = envParGUI)
    assign("frameM2SR_2_3", tkframe(frameM2SR_2), envir = envParGUI)
    assign("frameM2SR_2_4", tkframe(frameM2SR_2), envir = envParGUI)
    nL <- length(m2srMleData)
    nameL <- names(m2srMleData)
    nameP <- unique(mleCondM2SR[, "Parameter"])
    nP <- length(nameP)
    colDisplay <- c("Marker", "Adopted model", "Method", "Max. M2SR", nameP)
    displayM2SR <- get("displayM2SR", pos = envParProj)
    exportM2SR <- get("exportM2SR", pos = envParProj)
    if((length(displayM2SR) == 0) || (length(exportM2SR) == 0)){
      displayM2SR <- matrix("", nL, nP + 4)
      displayM2SR[, 1] <- nameL
      exportM2SR <- displayM2SR
      colnames(exportM2SR) <- colDisplay
      for(i in 1:nL){
        m2srMleData_oneL <- m2srMleData[[i]]
        nMle <- length(m2srMleData_oneL)
        if(nMle > 0){
          adoptData <- extAdoptModel(m2srMleData_oneL)
          exportM2SR[i, 2] <- adoptData[[3]]
          displayM2SR[i, 2] <- adoptData[[3]]
          exportM2SR[i, 3] <- m2srMleData_oneL[[nMle]]
          displayM2SR[i, 3] <- m2srMleData_oneL[[nMle]]
          exportM2SR[i, 4] <- maxM2SR
          displayM2SR[i, 4] <- signif(maxM2SR, 3)
          estPar <- adoptData[[2]]
          posPar <- match(names(estPar), colDisplay)
          exportM2SR[i, posPar] <- estPar
          displayM2SR[i, posPar] <- signif(estPar, 3)
        }
      }
    }
    scrY_M2SR <- tkscrollbar(frameM2SR_1, repeatinterval = 5, command = function(...) tkyview(mlbM2SR, ...))
    mlbM2SR <- tk2mclistbox(frameM2SR_1, width = 59 + 12 * nP, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_M2SR, ...))
    tk2column(mlbM2SR, "add", label = colDisplay[1], width = 10)
    tk2column(mlbM2SR, "add", label = colDisplay[2], width = 15)
    tk2column(mlbM2SR, "add", label = colDisplay[3], width = 22)
    tk2column(mlbM2SR, "add", label = colDisplay[4], width = 12)
    for(i in 1:nP){
      tk2column(mlbM2SR, "add", label = colDisplay[i + 4], width = 12)
    }
    tkgrid(mlbM2SR, scrY_M2SR)
    tk2insert.multi(mlbM2SR, "end", displayM2SR)
    tkgrid.configure(scrY_M2SR, rowspan = 25, sticky = "nsw")
    tkgrid(tkbutton(frameM2SR_1, text = "    Show detail    ", cursor = "hand2", command = function() showDetail_M2SR()), padx = 5, pady = 5)
    tkgrid(frameM2SR_1, frameM2SR_2, padx = 10, pady = 5, sticky = "n")

    assign("exportM2SR", exportM2SR, envir = envParProj)
  }

  makeExportPar <- function(){
    exportAE <- get("exportAE", pos = envParProj)
    exportHb <- get("exportHb", pos = envParProj)
    exportBSR <- get("exportBSR", pos = envParProj)
    exportFSR <- get("exportFSR", pos = envParProj)
    exportDSR <- get("exportDSR", pos = envParProj)
    exportM2SR <- get("exportM2SR", pos = envParProj)
    uniCol <- unique(c(colnames(exportAE), colnames(exportHb), colnames(exportBSR), colnames(exportFSR), colnames(exportDSR), colnames(exportM2SR)))
    parCol <- setdiff(uniCol, c("Marker", "Adopted model", "User-selected", "Method", "Min. AE", "Min. Hb", "Max. BSR", "Max. FSR", "Max. DSR", "Max. M2SR"))

    nL <- length(parLoci)

    exportPar <- matrix("", 6 * nL, length(parCol) + 7)
    colExport <- c("Biological_parameter", "Kit", "Marker", "Model", "Locus_specific", "Min", "Max", parCol)
    colnames(exportPar) <- colExport

    exportPar[1:nL, 1] <- "AE"
    exportPar[(nL + 1):(2 * nL), 1] <- "Hb"
    exportPar[(2 * nL + 1):(3 * nL), 1] <- "BSR"
    exportPar[(3 * nL + 1):(4 * nL), 1] <- "FSR"
    exportPar[(4 * nL + 1):(5 * nL), 1] <- "DSR"
    exportPar[(5 * nL + 1):(6 * nL), 1] <- "M2SR"
    exportPar[, 2] <- selectKit
    exportPar[, 3] <- rep(parLoci, 6)
    exportPar[1:nL, 4] <- exportAE[, "Adopted model"]
    exportPar[(nL + 1):(2 * nL), 4] <- exportHb[, "Adopted model"]
    exportPar[(2 * nL + 1):(3 * nL), 4] <- exportBSR[, "Adopted model"]
    exportPar[(3 * nL + 1):(4 * nL), 4] <- exportFSR[, "Adopted model"]
    exportPar[(4 * nL + 1):(5 * nL), 4] <- exportDSR[, "Adopted model"]
    exportPar[(5 * nL + 1):(6 * nL), 4] <- exportM2SR[, "Adopted model"]
    exportPar[, 5] <- TRUE
    posMulti <- which(exportBSR[, "Method"] == "Multiple loci together")
    exportPar[2 * nL + posMulti, 5] <- FALSE
    posNot <- which(exportBSR[, "Method"] == "")
    exportPar[2 * nL + posNot, 5] <- ""
    posMulti <- which(exportFSR[, "Method"] == "Multiple loci together")
    exportPar[3 * nL + posMulti, 5] <- FALSE
    posNot <- which(exportFSR[, "Method"] == "")
    exportPar[3 * nL + posNot, 5] <- ""
    posMulti <- which(exportDSR[, "Method"] == "Multiple loci together")
    exportPar[4 * nL + posMulti, 5] <- FALSE
    posNot <- which(exportDSR[, "Method"] == "")
    exportPar[4 * nL + posNot, 5] <- ""
    posMulti <- which(exportM2SR[, "Method"] == "Multiple loci together")
    exportPar[5 * nL + posMulti, 5] <- FALSE
    posNot <- which(exportM2SR[, "Method"] == "")
    exportPar[5 * nL + posNot, 5] <- ""
    exportPar[1:nL, "Min"] <- exportAE[, "Min. AE"]
    exportPar[(nL + 1):(2 * nL), "Min"] <- exportHb[, "Min. Hb"]
    exportPar[(2 * nL + 1):(3 * nL), "Max"] <- exportBSR[, "Max. BSR"]
    exportPar[(3 * nL + 1):(4 * nL), "Max"] <- exportFSR[, "Max. FSR"]
    exportPar[(4 * nL + 1):(5 * nL), "Max"] <- exportDSR[, "Max. DSR"]
    exportPar[(5 * nL + 1):(6 * nL), "Max"] <- exportM2SR[, "Max. M2SR"]
    aeCol <- colnames(exportAE)
    parAE <- aeCol[is.element(aeCol, parCol)]
    exportPar[1:nL, match(parAE, colExport)] <- exportAE[, match(parAE, aeCol)]
    hbCol <- colnames(exportHb)
    parHb <- hbCol[is.element(hbCol, parCol)]
    exportPar[(nL + 1):(2 * nL), match(parHb, colExport)] <- exportHb[, match(parHb, hbCol)]
    bsrCol <- colnames(exportBSR)
    parBSR <- bsrCol[is.element(bsrCol, parCol)]
    exportPar[(2 * nL + 1):(3 * nL), match(parBSR, colExport)] <- exportBSR[, match(parBSR, bsrCol)]
    fsrCol <- colnames(exportFSR)
    parFSR <- fsrCol[is.element(fsrCol, parCol)]
    exportPar[(3 * nL + 1):(4 * nL), match(parFSR, colExport)] <- exportFSR[, match(parFSR, fsrCol)]
    dsrCol <- colnames(exportDSR)
    parDSR <- dsrCol[is.element(dsrCol, parCol)]
    exportPar[(4 * nL + 1):(5 * nL), match(parDSR, colExport)] <- exportDSR[, match(parDSR, dsrCol)]
    m2srCol <- colnames(exportM2SR)
    parM2SR <- m2srCol[is.element(m2srCol, parCol)]
    exportPar[(5 * nL + 1):(6 * nL), match(parM2SR, colExport)] <- exportM2SR[, match(parM2SR, m2srCol)]

    return(exportPar)
  }

  arrangeExportAlCor <- function(){
    exportBSR <- get("exportBSR", pos = envParProj)
    exportFSR <- get("exportFSR", pos = envParProj)
    exportDSR <- get("exportDSR", pos = envParProj)
    nL <- length(parLoci)
    exportAlCorFinal <- exportAlCor
    for(i in 1:nL){
      posL <- which(exportAlCorFinal[, "Marker"] == parLoci[i])
      modelBSR <- exportBSR[i, "Adopted model"]
      modelFSR <- exportFSR[i, "Adopted model"]
      modelDSR <- exportDSR[i, "Adopted model"]
      if(modelBSR != "Multi-seq"){
        exportAlCorFinal[posL, "CA_BSR"] <- ""
      }
      if(modelFSR != "Multi-seq"){
        exportAlCorFinal[posL, "CA_FSR"] <- ""
      }
      if(modelDSR != "Multi-seq"){
        exportAlCorFinal[posL, "CA_DSR"] <- ""
      }
    }
    return(exportAlCorFinal)
  }

  exportParData <- function(){
    savePar <- function(){
      if((tclvalue(fnParVar) == "") || (tclvalue(fnAlCorVar) == "")){
        tkmessageBox(message = "Enter the folder name for saving files!", icon = "error", type = "ok")
      }else{
        options(warn = -1)
        pathPack <- get("pathPack", pos = envParGUI)
        write.csv(exportPar, paste0(pathPack, "/extdata/parameters/", tclvalue(fnParVar), ".csv"), row.names = FALSE, col.names = TRUE)
        write.csv(exportAlCorFinal, paste0(pathPack, "/extdata/repeat_correction/", tclvalue(fnAlCorVar), ".csv"), row.names = FALSE, col.names = TRUE)
        options(warn = 0)
        tkdestroy(tfExportPar)
        tkmessageBox(message = paste0("Results of parameter estimation have been saved successfully at '", pathPack, "/extdata/parameters/'. ", "\n",
                                      "Results of allele repeat correction have been saved successfully at '", pathPack, "/extdata/repeat_correction/'."),
                     icon = "info", type = "ok")
      }
    }
    exportPar <- makeExportPar()
    exportAlCorFinal <- arrangeExportAlCor()

    tfExportPar <- tktoplevel()
    tkwm.title(tfExportPar, "Export results of parameter estimation")
    tkgrid(tklabel(tfExportPar, text = "Enter the file name of parameters for Monte Carlo simulation."), padx = 20, pady = 5, sticky = "w")
    fnParVar <- tclVar("")
    tkgrid(tkentry(tfExportPar, textvariable = fnParVar, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 40, pady = 5, sticky = "w")
    tkgrid(tklabel(tfExportPar, text = "Enter the file name of allele repeat correction."), padx = 20, pady = 5, sticky = "w")
    fnAlCorVar <- tclVar("")
    tkgrid(tkentry(tfExportPar, textvariable = fnAlCorVar, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 40, pady = 5, sticky = "w")
    tkgrid(tkbutton(tfExportPar, text = "    Save    ", cursor = "hand2", command = function() savePar()), padx = 40, pady = 20, sticky = "w")
  }

  tab3Par <- get("tab3Par", pos = envParGUI)
  frameTab3Par <- get("frameTab3Par", pos = envParGUI)
  tkdestroy(frameTab3Par)
  frameTab3Par <- tkframe(tab3Par)

  subTabs_parResult <- tk2notebook(frameTab3Par, tabs = c("Amplification efficiency (AE)", "Heterozygote balance (Hb)", "Back stutter ratio (BSR)", "Forward stutter ratio (FSR)", "Double-back stutter ratio (DSR)", "Minus 2-nt stutter ratio (M2SR)"))
  tkpack(subTabs_parResult, fill = "both", expand = 1)
  subTabAE <- tk2notetab(subTabs_parResult, "Amplification efficiency (AE)")
  subTabHb <- tk2notetab(subTabs_parResult, "Heterozygote balance (Hb)")
  subTabBSR <- tk2notetab(subTabs_parResult, "Back stutter ratio (BSR)")
  subTabFSR <- tk2notetab(subTabs_parResult, "Forward stutter ratio (FSR)")
  subTabDSR <- tk2notetab(subTabs_parResult, "Double-back stutter ratio (DSR)")
  subTabM2SR <- tk2notetab(subTabs_parResult, "Minus 2-nt stutter ratio (M2SR)")

  selectKit <- get("selectKit", pos = envParProj)
  parLoci <- get("parLoci", pos = envParProj)

  aeUseData <- get("aeUseData", pos = envParProj)
  aeMleData <- get("aeMleData", pos = envParProj)
  hbUseData <- get("hbUseData", pos = envParProj)
  hbMleData <- get("hbMleData", pos = envParProj)
  bsrUseData <- get("bsrUseData", pos = envParProj)
  bsrMleData <- get("bsrMleData", pos = envParProj)
  fsrUseData <- get("fsrUseData", pos = envParProj)
  fsrMleData <- get("fsrMleData", pos = envParProj)
  dsrUseData <- get("dsrUseData", pos = envParProj)
  dsrMleData <- get("dsrMleData", pos = envParProj)
  m2srUseData <- get("m2srUseData", pos = envParProj)
  m2srMleData <- get("m2srMleData", pos = envParProj)
  exportAlCor <- get("exportAlCor", pos = envParProj)

  minAE <- get("minAE", pos = envParProj)
  minHb <- get("minHb", pos = envParProj)
  maxBSR <- get("maxBSR", pos = envParProj)
  maxFSR <- get("maxFSR", pos = envParProj)
  maxDSR <- get("maxDSR", pos = envParProj)
  maxM2SR <- get("maxM2SR", pos = envParProj)

  mleCondAE <- get("mleCondAE", pos = envParProj)
  mleCondHb <- get("mleCondHb", pos = envParProj)
  mleCondBSR <- get("mleCondBSR", pos = envParProj)
  mleCondFSR <- get("mleCondFSR", pos = envParProj)
  mleCondDSR <- get("mleCondDSR", pos = envParProj)
  mleCondM2SR <- get("mleCondM2SR", pos = envParProj)

  methBSR <- get("methBSR", pos = envParProj)
  methFSR <- get("methFSR", pos = envParProj)
  methDSR <- get("methDSR", pos = envParProj)
  methM2SR <- get("methM2SR", pos = envParProj)

  resultAE()
  resultHb()
  resultBSR()
  resultFSR()
  resultDSR()
  resultM2SR()

  tkpack(tkbutton(frameTab3Par, text = "    Export    ", cursor = "hand2", command = function() exportParData()),
         tkbutton(frameTab3Par, text = "    Save project    ", cursor = "hand2", command = function() saveProjPar(envParProj)), pady = 10)
  tkpack(frameTab3Par, fill = "both", padx = 10, pady = 10)
  assign("frameTab3Par", frameTab3Par, envir = envParGUI)
}

loadProjPar <- function(envParProj_old, envParGUI){
  inputOk <- "ok"
  finEstPar <- get("finEstPar", pos = envParProj_old)
  if(finEstPar){
    inputOk <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(inputOk == "ok"){
    tfPar <- get("tfPar", pos = envParGUI)
    fpVar <- tclVar("")
    fileName <- tclvalue(tkgetOpenFile(parent = tfPar, initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{R Data Files} {.Rdata}}"))
    posPar1 <- regexpr("[{]", fileName)[[1]]
    posPar2 <- regexpr("[}]", fileName)[[1]]
    if((posPar1 == 1) && (posPar2 == nchar(fileName))){
      fileName <- gsub("[{]", "", fileName)
      fileName <- gsub("[}]", "", fileName)
    }
    if(!nchar(fileName)){
      tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
    }else{
      load(fileName)
      for(i in ls(envParProj, all.names = TRUE)){
        assign(i, get(i, envParProj), envParProj_old)
      }
      makeTab1Par(envParProj_old, envParGUI)
      makeTab2Par(envParProj_old, envParGUI)
      makeTab3Par(envParProj_old, envParGUI)
      tabsPar <- get("tabsPar", pos = envParGUI)
      tk2notetab.select(tabsPar, "Result")
    }
  }
}

saveProjPar <- function(envParProj){
  saveAs <- tkgetSaveFile(filetypes = "{{R Data Files} {.RData}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 5, nchar(tclvalue(saveAs))) == ".RData"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".RData")
    }
    save(envParProj, file = reportName)
  }
}

setDefaultPar <- function(envParProj){
  #Minimum threshold
  assign("mtDefault", 30, envir = envParProj)

  #Autosomal STR markers
  strMar <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX", "D8S1179", "D21S11", "D18S51", "D2S441", "D19S433", "TH01", "FGA", "D22S1045", "D5S818", "D13S317", "D7S820", "SE33", "D10S1248", "D1S1656", "D12S391", "D2S1338", "D6S1043", "Penta D", "Penta E")
  nStr <- length(strMar)

  #Candidate model of AE
  candAE_Default <- matrix("", nStr + 1, 2)
  rownames(candAE_Default) <- c(strMar, "General")
  colnames(candAE_Default) <- c("Cand_1", "Default")
  candAE_Default[, 1] <- "Log-normal"
  candAE_Default[, 2] <- "Log-normal"
  assign("candAE_Default", candAE_Default, envir = envParProj)

  #Candidate model of Hb
  candHb_Default <- matrix("", nStr + 1, 2)
  rownames(candHb_Default) <- c(strMar, "General")
  colnames(candHb_Default) <- c("Cand_1", "Default")
  candHb_Default[, 1] <- "Log-normal"
  candHb_Default[, 2] <- "Log-normal"
  assign("candHb_Default", candHb_Default, envir = envParProj)

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
  assign("candBSR_Default", candBSR_Default, envir = envParProj)

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
  assign("candFSR_Default", candFSR_Default, envir = envParProj)

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
  assign("candDSR_Default", candDSR_Default, envir = envParProj)

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
  assign("candM2SR_Default", candM2SR_Default, envir = envParProj)

  #MLE conditions for AE
  mleValAE <- c(1, -0.5, 2, 50, 1, 500)
  mleCondAE_Default <- matrix("", length(mleValAE) * (nStr + 1), 5)
  colnames(mleCondAE_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
  mleCondAE_Default[, 5] <- rep(mleValAE, nStr + 1)
  mleCondAE_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
  mleCondAE_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
  mleCondAE_Default[, 2] <- "Log-normal"
  mleCondAE_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValAE)))
  assign("mleCondAE_Default", mleCondAE_Default, envir = envParProj)

  #MLE conditions for Hb
  mleValHb <- c(1, 0.9, 1.1, 50, 1, 500)
  mleCondHb_Default <- matrix("", length(mleValHb) * (nStr + 1), 5)
  colnames(mleCondHb_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
  mleCondHb_Default[, 5] <- rep(mleValHb, nStr + 1)
  mleCondHb_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
  mleCondHb_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
  mleCondHb_Default[, 2] <- "Log-normal"
  mleCondHb_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValHb)))
  assign("mleCondHb_Default", mleCondHb_Default, envir = envParProj)

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
  assign("mleCondBSR_Default", mleCondBSR_Default, envir = envParProj)

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
  assign("mleCondFSR_Default", mleCondFSR_Default, envir = envParProj)

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
  assign("mleCondDSR_Default", mleCondDSR_Default, envir = envParProj)

  #MLE conditions for M2SR
  mleValM2SR <- c(0.01, 0, 1, 500, 1, 5000)
  mleCondM2SR_Default <- matrix("", length(mleValM2SR) * (nStr + 1), 5)
  colnames(mleCondM2SR_Default) <- c("Locus", "Model", "Parameter", "Type", "Value")
  mleCondM2SR_Default[, 5] <- rep(mleValM2SR, nStr + 1)
  mleCondM2SR_Default[, 4] <- rep(c("Initial", "Lower", "Upper"), 2 * (nStr + 1))
  mleCondM2SR_Default[, 3] <- rep(c("Mean", "Mean", "Mean", "Variance", "Variance", "Variance"), nStr + 1)
  mleCondM2SR_Default[, 2] <- "Uniform"
  mleCondM2SR_Default[, 1] <- as.character(sapply(c(strMar, "General"), rep, length(mleValM2SR)))
  assign("mleCondM2SR_Default", mleCondM2SR_Default, envir = envParProj)

  assign("mt_kitLoci", numeric(0), envir = envParProj)
  assign("selectKit", character(0), envir = envParProj)

  assign("modelAE", character(0), envir = envParProj)
  assign("minAEMeth", 1, envir = envParProj)
  assign("minAE", 0.058, envir = envParProj)
  assign("candAE", character(0), envir = envParProj)
  assign("mleCondAE", character(0), envir = envParProj)

  assign("modelHb", character(0), envir = envParProj)
  assign("minHbMeth", 1, envir = envParProj)
  assign("minHb", 0.046, envir = envParProj)
  assign("candHb", character(0), envir = envParProj)
  assign("mleCondHb", character(0), envir = envParProj)

  assign("methBSR", character(0), envir = envParProj)
  assign("modelBSR", character(0), envir = envParProj)
  assign("modelMultiBSR", character(0), envir = envParProj)
  assign("maxBSRMeth", 1, envir = envParProj)
  assign("maxBSR", 0.7, envir = envParProj)
  assign("candBSR", character(0), envir = envParProj)
  assign("mleCondBSR", character(0), envir = envParProj)

  assign("methFSR", character(0), envir = envParProj)
  assign("modelFSR", character(0), envir = envParProj)
  assign("modelMultiFSR", character(0), envir = envParProj)
  assign("maxFSRMeth", 1, envir = envParProj)
  assign("maxFSR", 0.35, envir = envParProj)
  assign("candFSR", character(0), envir = envParProj)
  assign("mleCondFSR", character(0), envir = envParProj)

  assign("methDSR", character(0), envir = envParProj)
  assign("modelDSR", character(0), envir = envParProj)
  assign("modelMultiDSR", character(0), envir = envParProj)
  assign("maxDSRMeth", 1, envir = envParProj)
  assign("maxDSR", 0.13, envir = envParProj)
  assign("candDSR", character(0), envir = envParProj)
  assign("mleCondDSR", character(0), envir = envParProj)

  assign("methM2SR", character(0), envir = envParProj)
  assign("modelM2SR", character(0), envir = envParProj)
  assign("modelMultiM2SR", character(0), envir = envParProj)
  assign("maxM2SRMeth", 1, envir = envParProj)
  assign("maxM2SR", 0.12, envir = envParProj)
  assign("candM2SR", character(0), envir = envParProj)
  assign("mleCondM2SR", character(0), envir = envParProj)

  assign("displayAE", character(0), envir = envParProj)
  assign("exportAE", character(0), envir = envParProj)
  assign("displayHb", character(0), envir = envParProj)
  assign("exportHb", character(0), envir = envParProj)
  assign("displayBSR", character(0), envir = envParProj)
  assign("exportBSR", character(0), envir = envParProj)
  assign("displayFSR", character(0), envir = envParProj)
  assign("exportFSR", character(0), envir = envParProj)
  assign("displayDSR", character(0), envir = envParProj)
  assign("exportDSR", character(0), envir = envParProj)
  assign("displayM2SR", character(0), envir = envParProj)
  assign("exportM2SR", character(0), envir = envParProj)
}

# Make a window for estimating parameters
windowParEst <- function(envGUI){
  envParProj <- new.env(parent = globalenv())
  envParGUI <- new.env(parent = globalenv())
  tfPar <- tktoplevel()
  assign("tfPar", tfPar, envir = envParGUI)
  tkwm.title(tfPar, "Estimate parameters")

  pathPack <- get("pathPack", pos = envGUI)
  assign("pathPack", pathPack, envir = envParGUI)

  tabsPar <- tk2notebook(tfPar, tabs = c("Files", "Setting", "Result"))
  tkpack(tabsPar, fill = "both", expand = 1)
  tab1Par <- tk2notetab(tabsPar, "Files")
  tab2Par <- tk2notetab(tabsPar, "Setting")
  tab3Par <- tk2notetab(tabsPar, "Result")
  assign("tabsPar", tabsPar, envir = envParGUI)
  assign("tab1Par", tab1Par, envir = envParGUI)
  assign("tab2Par", tab2Par, envir = envParGUI)
  assign("tab3Par", tab3Par, envir = envParGUI)

  frameTab1Par <- tkframe(tab1Par)
  assign("frameTab1Par", frameTab1Par, envir = envParGUI)
  frameTab2Par <- tkframe(tab2Par)
  assign("frameTab2Par", frameTab2Par, envir = envParGUI)
  frameTab3Par <- tkframe(tab3Par)
  assign("frameTab3Par", frameTab3Par, envir = envParGUI)

  assign("exData", NULL, envir = envParProj)
  assign("exFp", character(0), envir = envParProj)
  assign("exFn", character(0), envir = envParProj)
  assign("gtData", NULL, envir = envParProj)
  assign("gtFp", character(0), envir = envParProj)
  assign("gtFn", character(0), envir = envParProj)
  assign("seqData", NULL, envir = envParProj)
  assign("seqFp", character(0), envir = envParProj)
  assign("seqFn", character(0), envir = envParProj)
  assign("finEstPar", FALSE, envir = envParProj)

  setDefaultPar(envParProj)

  makeTab1Par(envParProj, envParGUI)
}
