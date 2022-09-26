# Set an analysis method
setAnaMeth <- function(action, envAnaMeth){
  # Customize conditions
  customCond <- function(){
    stateEntryMaf <- function(){
      if(tclvalue(afMethVar) == "Dirichlet"){
        tkconfigure(entryMaf, state = "disable")
      }else{
        tkconfigure(entryMaf, state = "normal")
      }
    }

    nameParVar <- get("nameParVar", pos = envAnaMeth)
    nameAlCorVar <- get("nameAlCorVar", pos = envAnaMeth)
    afMethVar <- get("afMethVar", pos = envAnaMeth)
    mafVar <- get("mafVar", pos = envAnaMeth)
    thetaVar <- get("thetaVar", pos = envAnaMeth)
    numMCVar <- get("numMCVar", pos = envAnaMeth)

    frameCond <- tkframe(tabCond)
    labelParMC <- tklabel(frameCond, text = "Parameters for Monte Carlo simulation")
    fileNameParMC <- list.files(paste0(pathPack, "/extdata/parameters"))
    comboParMC <- ttkcombobox(frameCond, values = fileNameParMC, textvariable = nameParVar, width = 40, state = "readonly")
    tkgrid(labelParMC, comboParMC, padx = 10, pady = 5, sticky = "w")
    labelAlCor <- tklabel(frameCond, text = "Allele repeat correction")
    fileNameAlCor <- list.files(paste0(pathPack, "/extdata/repeat_correction"))
    comboAlCor <- ttkcombobox(frameCond, values = fileNameAlCor, textvariable = nameAlCorVar, width = 40, state = "readonly")
    tkgrid(labelAlCor, comboAlCor, padx = 10, pady = 5, sticky = "w")

    labelAfMeth <- tklabel(frameCond, text = "Method of estimating allele frequencies")
    nameAfMeth <- c("Dirichlet", "No correction")
    comboAfMeth <- ttkcombobox(frameCond, values = nameAfMeth, textvariable = afMethVar, width = 40, state = "readonly")
    tkgrid(labelAfMeth, comboAfMeth, padx = 10, pady = 5, sticky = "w")

    labelMaf <- tklabel(frameCond, text = "Minimum allele frequency")
    entryMaf <- tkentry(frameCond, textvariable = mafVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = "disable")
    tkgrid(labelMaf, entryMaf, padx = 10, pady = 5, sticky = "w")

    tkbind(comboAfMeth, "<<ComboboxSelected>>", function(){
      stateEntryMaf()
    })
    stateEntryMaf()

    labelTheta <- tklabel(frameCond, text = "Theta")
    entryTheta <- tkentry(frameCond, textvariable = thetaVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    tkgrid(labelTheta, entryTheta, padx = 10, pady = 5, sticky = "w")

    labelNumMC <- tklabel(frameCond, text = "Number of Monte Carlo simulation")
    entryNumMC <- tkentry(frameCond, textvariable = numMCVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    tkgrid(labelNumMC, entryNumMC, padx = 10, pady = 5, sticky = "w")

    tkgrid(frameCond)
  }

  # Customize analytical thresholds
  customAT <- function(){
    kitVar <- get("kitVar", pos = envAnaMeth)
    atVars <- get("atVars", pos = envAnaMeth)
    lociAt <- names(atVars)
    kitNames <- list.files(paste0(pathPack, "/extdata/kit"))
    kitNames <- gsub(".csv", "", kitNames)

    frameKit <- tkframe(tabAt)
    labelKit <- tklabel(frameKit, text = "Kit")
    comboKit <- ttkcombobox(frameKit, values = kitNames, textvariable = kitVar, width = 40, state = "readonly")
    tkgrid(labelKit, comboKit, padx = 10, pady = 5, sticky = "w")
    tkgrid(frameKit)

    tkbind(comboKit, "<<ComboboxSelected>>", function(){
      frameAt <- get("frameAt", pos = envAt)
      tkdestroy(frameAt)

      kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", tclvalue(kitVar), ".csv"), header = TRUE)
      kitInfo <- as.matrix(kitInfo)
      selectLoci <- unique(kitInfo[, "Marker"])
      posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
      if(length(posSexMar) > 0){
        sexMar <- unique(kitInfo[posSexMar, "Marker"])
        selectLoci <- setdiff(selectLoci, sexMar)
      }
      nAt <- length(selectLoci)
      atVars <- list()
      for(i in 1:nAt){
        atVars[[i]] <- tclVar(atSingleDefault)
      }
      names(atVars) <- selectLoci
      assign("atVars", atVars, envir = envAnaMeth)

      frameAt <- tkframe(tabAt)
      assign("frameAt", frameAt, envir = envAt)
      for(i in 1:nAt){
        tkgrid(tklabel(frameAt, text = selectLoci[i]),
               tkentry(frameAt, textvariable = atVars[[i]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
               padx = 10, pady = 1, sticky = "w")
      }
      tkgrid(frameAt)
    })

    envAt <- new.env(parent = globalenv())
    frameAt <- tkframe(tabAt)
    assign("frameAt", frameAt, envir = envAt)
    for(i in 1:length(atVars)){
      tkgrid(tklabel(frameAt, text = lociAt[i]),
             tkentry(frameAt, textvariable = atVars[[i]], width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 10, pady = 1, sticky = "w")
    }
    tkgrid(frameAt)
  }

  # Customize mixture proportions of one contributor
  customMR <- function(){
    mrSort <- function(){
      mrList <- tclvalue(mrListVar)
      mrList <- unlist(strsplit(mrList, " "))
      mrList <- sort(as.numeric(mrList))
      nMrOne <- length(mrList)
      tkdelete(mrListbox, 0, size(mrListbox) - 1)
      for(i in 1:nMrOne){
        tkinsert(mrListbox, "end", mrList[i])
      }
    }

    mrRestore <- function(){
      tkdelete(mrListbox, 0, size(mrListbox) - 1)
      nMrOne <- length(mrOne)
      for(i in 1:nMrOne){
        tkinsert(mrListbox, "end", mrOne[i])
      }
    }

    mrAdd <- function(){
      mrNew <- tclvalue(mrNewVar)
      if(mrNew != ""){
        tkinsert(mrListbox, "end", mrNew)
      }
    }

    mrDelete <- function(){
      selectPos <- tkcurselection(mrListbox)
      selectPos <- tclvalue(selectPos)
      if(selectPos != ""){
        tkdelete(mrListbox, selectPos)
      }
    }

    mrListVar <- get("mrListVar", pos = envAnaMeth)
    mrNewVar <- tclVar("")
    frameMr <- tkframe(tabMr)
    mrListFrame <- tkframe(frameMr)
    mrListbox <- tk2listbox(mrListFrame, listvariable = mrListVar, height = 20, justify = "center", selectmode = "single")
    tkgrid(tklabel(mrListFrame, text = "Mixture proportions per contributor"), padx = 20, sticky = "w")
    tkgrid(mrListbox, padx = 20, sticky = "w")
    nMrOne <- length(mrOne)
    for(i in 1:nMrOne){
      tkinsert(mrListbox, "end", mrOne[i])
    }
    mrButtFrame <- tkframe(mrListFrame)
    mrSortButt <- tkbutton(mrButtFrame, text = " Sort ", command = function() mrSort())
    mrRestoreButt <- tkbutton(mrButtFrame, text = " Restore ", command = function() mrRestore())
    tkgrid(mrSortButt, mrRestoreButt, padx = 10)
    tkgrid(mrButtFrame, pady = 10)

    mrSetFrame <- tkframe(frameMr)
    mrAddFrame <- tkframe(mrSetFrame)
    mrAddEntry <- tkentry(mrAddFrame, textvariable = mrNewVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    mrAddButt <- tkbutton(mrAddFrame, text = " Add ", command = function() mrAdd())
    mrDeleteFrame <- tkframe(mrSetFrame)
    mrDeleteButt <- tkbutton(mrDeleteFrame, text = " Delete ", command = function() mrDelete())
    mrSaveFrame <- tkframe(mrSetFrame)
    mrSaveButt <- tkbutton(mrSaveFrame, text = "Save", command = function() mrSave())
    tkgrid(tklabel(mrSetFrame, text = "Add an entered value to the list of mixture proportions"), padx = 20, sticky = "w")
    tkgrid(mrAddEntry, mrAddButt, padx = 20)
    tkgrid(mrAddFrame, padx = 20, sticky = "w")
    tkgrid(tklabel(mrSetFrame, text = ""), padx = 20, sticky = "w")
    tkgrid(tklabel(mrSetFrame, text = "Delete a selected mixture proportion"), padx = 20, sticky = "w")
    tkgrid(tklabel(mrDeleteFrame, text = "                    "), mrDeleteButt, padx = 20)
    tkgrid(mrDeleteFrame, padx = 20, sticky = "w")
    tkgrid(mrListFrame, mrSetFrame, pady = 10)
    tkgrid(frameMr)
  }

  # Customize degradation parameters
  customDeg <- function(){
    degSort <- function(){
      degList <- tclvalue(degListVar)
      degList <- unlist(strsplit(degList, " "))
      degList <- sort(as.numeric(degList))
      ndegOne <- length(degList)
      tkdelete(degListbox, 0, size(degListbox) - 1)
      for(i in 1:ndegOne){
        tkinsert(degListbox, "end", degList[i])
      }
    }

    degRestore <- function(){
      tkdelete(degListbox, 0, size(degListbox) - 1)
      ndegOne <- length(degOne)
      for(i in 1:ndegOne){
        tkinsert(degListbox, "end", degOne[i])
      }
    }

    degAdd <- function(){
      degNew <- tclvalue(degNewVar)
      if(degNew != ""){
        tkinsert(degListbox, "end", degNew)
      }
    }

    degDelete <- function(){
      selectPos <- tkcurselection(degListbox)
      selectPos <- tclvalue(selectPos)
      if(selectPos != ""){
        tkdelete(degListbox, selectPos)
      }
    }

    degListVar <- get("degListVar", pos = envAnaMeth)
    degNewVar <- tclVar("")
    frameDeg <- tkframe(tabDeg)
    degListFrame <- tkframe(frameDeg)
    degListbox <- tk2listbox(degListFrame, listvariable = degListVar, height = 20, justify = "center", selectmode = "single")
    tkgrid(tklabel(degListFrame, text = "Degradation parameters"), padx = 20, sticky = "w")
    tkgrid(degListbox, padx = 20, sticky = "w")
    ndegOne <- length(degOne)
    for(i in 1:ndegOne){
      tkinsert(degListbox, "end", degOne[i])
    }
    degButtFrame <- tkframe(degListFrame)
    degSortButt <- tkbutton(degButtFrame, text = " Sort ", command = function() degSort())
    degRestoreButt <- tkbutton(degButtFrame, text = " Restore ", command = function() degRestore())
    tkgrid(degSortButt, degRestoreButt, padx = 10)
    tkgrid(degButtFrame, pady = 10)

    degSetFrame <- tkframe(frameDeg)
    degAddFrame <- tkframe(degSetFrame)
    degAddEntry <- tkentry(degAddFrame, textvariable = degNewVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    degAddButt <- tkbutton(degAddFrame, text = " Add ", command = function() degAdd())
    degDeleteFrame <- tkframe(degSetFrame)
    degDeleteButt <- tkbutton(degDeleteFrame, text = " Delete ", command = function() degDelete())
    degSaveFrame <- tkframe(degSetFrame)
    degSaveButt <- tkbutton(degSaveFrame, text = "Save", command = function() degSave())
    tkgrid(tklabel(degSetFrame, text = "Add an entered value to the list of degradation parameters"), padx = 20, sticky = "w")
    tkgrid(degAddEntry, degAddButt, padx = 20)
    tkgrid(degAddFrame, padx = 20, sticky = "w")
    tkgrid(tklabel(degSetFrame, text = ""), padx = 20, sticky = "w")
    tkgrid(tklabel(degSetFrame, text = "Delete a selected degradation parameter"), padx = 20, sticky = "w")
    tkgrid(tklabel(degDeleteFrame, text = "                    "), degDeleteButt, padx = 20)
    tkgrid(degDeleteFrame, padx = 20, sticky = "w")
    tkgrid(degListFrame, degSetFrame, pady = 10)
    tkgrid(frameDeg)
  }

  # Customize conditions for excluding unrealistic genotype combinations
  customCutGt <- function(){
    stVar <- get("stVar", pos = envAnaMeth)
    mrFltrVar <- get("mrFltrVar", pos = envAnaMeth)
    hbFltrVar <- get("hbFltrVar", pos = envAnaMeth)
    bsrFltrVar <- get("bsrFltrVar", pos = envAnaMeth)
    fsrFltrVar <- get("fsrFltrVar", pos = envAnaMeth)
    dsrFltrVar <- get("dsrFltrVar", pos = envAnaMeth)
    m2srFltrVar <- get("m2srFltrVar", pos = envAnaMeth)

    frameCutGt <- tkframe(tabCutGt)
    labelSt <- tklabel(frameCutGt, text = "Stochastic threshold")
    entrySt <- tkentry(frameCutGt, textvariable = stVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelMrFltr <- tklabel(frameCutGt, text = "Mixture ratio filter")
    entryMrFltr <- tkentry(frameCutGt, textvariable = mrFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelHbFltr <- tklabel(frameCutGt, text = "Heterozygote balance filter")
    entryHbFltr <- tkentry(frameCutGt, textvariable = hbFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelBsrFltr <- tklabel(frameCutGt, text = "Back stutter filter")
    entryBsrFltr <- tkentry(frameCutGt, textvariable = bsrFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelFsrFltr <- tklabel(frameCutGt, text = "Forward stutter filter")
    entryFsrFltr <- tkentry(frameCutGt, textvariable = fsrFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelDsrFltr <- tklabel(frameCutGt, text = "Double-back stutter filter")
    entryDsrFltr <- tkentry(frameCutGt, textvariable = dsrFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    labelM2srFltr <- tklabel(frameCutGt, text = "Minus 2-nt stutter filter")
    entryM2srFltr <- tkentry(frameCutGt, textvariable = m2srFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

    tkgrid(labelSt, entrySt, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelMrFltr, entryMrFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelHbFltr, entryHbFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelBsrFltr, entryBsrFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelFsrFltr, entryFsrFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelDsrFltr, entryDsrFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(labelM2srFltr, entryM2srFltr, padx = 20, pady = 1, sticky = "w")
    tkgrid(frameCutGt, pady = 10)
  }

  # Save method data
  saveMeth <- function(saveAs){
    checkMCPar <- function(){
      errorMCPar <- FALSE
      mcPar <- read.csv(paste0(pathPack, "/extdata/parameters/", namePar), header = TRUE)
      mcPar <- as.matrix(mcPar)
      kitPar <- unique(mcPar[, colnames(mcPar) == "Kit"])
      if(length(kitPar) != 1 || kit != kitPar){
        errorMCPar <- TRUE
        tclvalue(tkmessageBox(message = "Locus set of analytical thresholds is different from that of Monte Carlo parameters!", icon = "error", type = "ok"))
      }
      return(errorMCPar)
    }

    checkAlCor <- function(){
      errorAlCor <- FALSE
      alCor <- read.csv(paste0(pathPack, "/extdata/repeat_correction/", nameAlCor), header = TRUE)
      alCor <- as.matrix(alCor)
      lociAlCor <- unique(alCor[, grep("Marker", colnames(alCor))])
      lociAt <- names(atVars)
      judgeLoci <- !is.element(lociAt, lociAlCor)
      if(any(judgeLoci)){
        errorAlCor <- TRUE
        noAlCorLoci <- lociAt[judgeLoci]
        noAlCorMessage <- ""
        for(i in 1:length(noAlCorLoci)){
          noAlCorMessage <- paste0(noAlCorMessage, "\n", noAlCorLoci[i])
        }
        tkmessageBox(message = paste0("There is no information of allele repeat correction for the following loci:", "\n", noAlCorMessage), icon = "error", type = "ok")
      }
      return(errorAlCor)
    }

    nameMeth <- get("nameMeth", pos = envAnaMeth)
    selectMeth <- get("selectMeth", pos = envAnaMeth)
    kit <- tclvalue(get("kitVar", pos = envAnaMeth))
    namePar <- tclvalue(get("nameParVar", pos = envAnaMeth))
    nameAlCor <- tclvalue(get("nameAlCorVar", pos = envAnaMeth))
    afMeth <- tclvalue(get("afMethVar", pos = envAnaMeth))
    maf <- tclvalue(get("mafVar", pos = envAnaMeth))
    theta <- tclvalue(get("thetaVar", pos = envAnaMeth))
    numMC <- tclvalue(get("numMCVar", pos = envAnaMeth))
    atVars <- get("atVars", pos = envAnaMeth)
    mrListVar <- get("mrListVar", pos = envAnaMeth)
    degListVar <- get("degListVar", pos = envAnaMeth)
    st <- tclvalue(get("stVar", pos = envAnaMeth))
    mrFltr <- tclvalue(get("mrFltrVar", pos = envAnaMeth))
    hbFltr <- tclvalue(get("hbFltrVar", pos = envAnaMeth))
    bsrFltr <- tclvalue(get("bsrFltrVar", pos = envAnaMeth))
    fsrFltr <- tclvalue(get("fsrFltrVar", pos = envAnaMeth))
    dsrFltr <- tclvalue(get("dsrFltrVar", pos = envAnaMeth))
    m2srFltr <- tclvalue(get("m2srFltrVar", pos = envAnaMeth))

    errorMCPar <- checkMCPar()
    errorAlCor <- checkAlCor()

    if(!errorMCPar && !errorAlCor){
      mrList <- tclvalue(mrListVar)
      mrList <- unlist(strsplit(mrList, " "))

      degList <- tclvalue(degListVar)
      degList <- unlist(strsplit(degList, " "))

      nAt <- length(atVars)
      nMr <- length(mrList)
      nDeg <- length(degList)
      methData <- matrix(0, 14 + nAt + nMr + nDeg, 2)
      colnames(methData) <- c("Conditions", "Values")
      methData[, 1] <- c("Kit", "Parameters for Monte Carlo simulations", "Allele repeat correction",
                         "Method of estimating allele frequencies", "Minimum allele frequency", "Theta", "Number of Monte Carlo simulations",
                         paste0("Analytical threshold_", names(atVars)), rep("Mixture proportion", nMr), rep("Degradation parameter", nDeg),
                         "Stochastic threshold", "Filter of mixture ratio", "Filter of heterozygote balance",
                         "Filter of back stutter ratio", "Filter of forward stutter ratio", "Filter of double-back stutter ratio", "Filter of minus 2-nt stutter ratio")
      methData[, 2] <- c(kit, namePar, nameAlCor,
                         afMeth, maf, theta, numMC,
                         sapply(atVars, tclvalue), mrList, degList,
                         st, mrFltr, hbFltr,
                         bsrFltr, fsrFltr, dsrFltr, m2srFltr)

      if(saveAs){
        tfSave <- tktoplevel()
        tkwm.title(tfSave, "Save the analysis method")
        tkgrid(tklabel(tfSave, text = "Enter the name of the analysis method"), padx = 60, pady = 5)
        saveMethVar <- tclVar("")
        tkgrid(tkentry(tfSave, textvariable = saveMethVar, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 60, pady = 5)
        tkgrid(tkbutton(tfSave, text = "    Save    ", cursor = "hand2", command = function(){
          newMeth <- tclvalue(saveMethVar)
          write.csv(methData, paste0(pathPack, "/extdata/analysis_method/", newMeth, ".csv"), row.names = FALSE)
          assign("nameMeth", c(nameMeth, newMeth), envir = envAnaMeth)
          tkinsert(listMeth, "end", newMeth)
          tkdestroy(tfSave)
          tkdestroy(tfSetting)
        }), padx = 20, pady = 20)
      }else{
        inputOk <- tclvalue(tkmessageBox(message = paste0(selectMeth, " already exists. Do you want to replace it?"), type = "okcancel", icon = "warning"))
        if(inputOk == "ok"){
          write.csv(methData, paste0(pathPack, "/extdata/analysis_method/", selectMeth, ".csv"), row.names = FALSE)
          tkdestroy(tfSetting)
        }
      }
    }
  }

  pathPack <- get("pathPack", pos = envAnaMeth)
  listMeth <- get("listMeth", pos = envAnaMeth)
  error <- FALSE
  if(action == "new"){
    selectMeth <- ""
    kit <- "GlobalFiler"
    namePar <-  ""
    nameAlCor <- ""
    afMeth <- "Dirichlet"
    maf <- 0.001
    theta <- 0
    numMC <- 1000
    atVals <- rep(100, 21)
    names(atVals) <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                       "D8S1179", "D21S11", "D18S51",
                       "D2S441", "D19S433", "TH01", "FGA",
                       "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                       "D10S1248", "D1S1656", "D12S391", "D2S1338")
    nAt <- length(atVals)
    lociAt <- names(atVals)
    mrOne <- c(0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)
    degOne <- round(seq(-0.05, 0, 0.0025), 4)
    st <- 500
    mrFltr <- 4
    hbFltr <- 0.25
    bsrFltr <- 0.7
    fsrFltr <- 0.35
    dsrFltr <- 0.13
    m2srFltr <- 0.12
    enterName <- TRUE
  }else if(action == "edit"){
    if(tclvalue(tkcurselection(listMeth)) == ""){
      errorOk <- tclvalue(tkmessageBox(message = "Select one method!", icon = "error", type = "ok"))
      if(errorOk == "ok"){
        error <- TRUE
      }
    }else{
      nameMeth <- get("nameMeth", pos = envAnaMeth)
      selectMeth <- nameMeth[as.numeric(tclvalue(tkcurselection(listMeth))) + 1]
      selectMethData <- read.csv(paste0(pathPack, "/extdata/analysis_method/", selectMeth, ".csv"), header = TRUE)
      selectMethData <- as.matrix(selectMethData)
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
      enterName <- FALSE
    }
  }

  if(!error){
    assign("selectMeth", selectMeth, envir = envAnaMeth)
    assign("kitVar", tclVar(kit), envir = envAnaMeth)
    assign("nameParVar", tclVar(namePar), envir = envAnaMeth)
    assign("nameAlCorVar", tclVar(nameAlCor), envir = envAnaMeth)
    assign("afMethVar", tclVar(afMeth), envir = envAnaMeth)
    assign("mafVar", tclVar(maf), envir = envAnaMeth)
    assign("thetaVar", tclVar(theta), envir = envAnaMeth)
    assign("numMCVar", tclVar(numMC), envir = envAnaMeth)
    atVars <- list()
    for(i in 1:nAt){
      atVars[[i]] <- tclVar(atVals[i])
    }
    names(atVars) <- names(atVals)
    assign("atVars", atVars, envir = envAnaMeth)
    assign("mrListVar", tclVar(""), envir = envAnaMeth)
    assign("degListVar", tclVar(""), envir = envAnaMeth)
    assign("stVar", tclVar(st), envir = envAnaMeth)
    assign("mrFltrVar", tclVar(mrFltr), envir = envAnaMeth)
    assign("hbFltrVar", tclVar(hbFltr), envir = envAnaMeth)
    assign("bsrFltrVar", tclVar(bsrFltr), envir = envAnaMeth)
    assign("fsrFltrVar", tclVar(fsrFltr), envir = envAnaMeth)
    assign("dsrFltrVar", tclVar(dsrFltr), envir = envAnaMeth)
    assign("m2srFltrVar", tclVar(m2srFltr), envir = envAnaMeth)

    tfSetting <- tktoplevel()
    tkwm.title(tfSetting, "Set an analysis method")
    frameSet1 <- tkframe(tfSetting)
    tabs <- tk2notebook(frameSet1, tabs = c("Conditions", "Analytical threshold", "Mixture proportion", "Degradation", "Exclude unrealistic genotype combinations"))
    tkpack(tabs, fill = "both", expand = 1)
    tabCond <- tk2notetab(tabs, "Conditions")
    tabAt <- tk2notetab(tabs, "Analytical threshold")
    tabMr <- tk2notetab(tabs, "Mixture proportion")
    tabDeg <- tk2notetab(tabs, "Degradation")
    tabCutGt <- tk2notetab(tabs, "Exclude unrealistic genotype combinations")
    customCond()
    customAT()
    customMR()
    customDeg()
    customCutGt()
    tkgrid(frameSet1)

    frameSet2 <- tkframe(tfSetting)
    buttSave <- tkbutton(frameSet2, text = "    Save    ", cursor = "hand2", command = function() saveMeth(saveAs = enterName))
    buttSaveAs <- tkbutton(frameSet2, text = "    Save as   ", cursor = "hand2", command = function() saveMeth(saveAs = TRUE))
    tkgrid(buttSave, buttSaveAs, padx = 10, pady = 5)
    tkgrid(frameSet2)
  }
}

# Delete an analysis method
deleteAnaMeth <- function(envAnaMeth){
  listMeth <- get("listMeth", pos = envAnaMeth)
  posDelete <- tclvalue(tkcurselection(listMeth))
  if(posDelete == ""){
    tkmessageBox(message = "Select one method!", icon = "error", type = "ok")
  }else{
    deleteOk <- tclvalue(tkmessageBox(message = "The selected method will be deleted. Do you want to continue?", icon = "warning", type = "ok"))
    if(deleteOk == "ok"){
      pathPack <- get("pathPack", pos = envAnaMeth)
      frame1 <- get("frame1", pos = envAnaMeth)
      nameMeth <- get("nameMeth", pos = envAnaMeth)
      posDelete <- as.numeric(posDelete) + 1
      deleteMeth <- nameMeth[posDelete]
      file.remove(paste0(pathPack, "/extdata/analysis_method/", deleteMeth, ".csv"), recursive = FALSE)
      nameMeth <- nameMeth[-posDelete]
      tkdestroy(listMeth)
      listMeth <- tk2listbox(frame1, height = 10, width = 60, selectmode = "single")
      tkgrid(listMeth, padx = 10, pady = 5)
      for(i in 1:length(nameMeth)){
        tkinsert(listMeth, "end", nameMeth[i])
      }
      assign("listMeth", listMeth, envir = envAnaMeth)
      assign("nameMeth", nameMeth, envir = envAnaMeth)
    }
  }
}

# Make a window for analysis methods
windowAnaMeth <- function(envGUI){
  pathPack <- get("pathPack", pos = envGUI)
  envAnaMeth <- new.env(parent = globalenv())
  nameMeth <- list.files(paste0(pathPack, "/extdata/analysis_method"))
  nameMeth <- gsub(".csv", "", nameMeth)
  assign("pathPack", pathPack, envir = envAnaMeth)
  assign("nameMeth", nameMeth, envir = envAnaMeth)

  tf <- tktoplevel()
  tkwm.title(tf, "Analysis method")
  frame1 <- tkframe(tf)
  frame2 <- tkframe(tf)

  listMeth <- tk2listbox(frame1, height = 10, width = 60, selectmode = "single")
  tkgrid(listMeth, padx = 10, pady = 5)
  for(i in 1:length(nameMeth)){
    tkinsert(listMeth, "end", nameMeth[i])
  }
  assign("frame1", frame1, envir = envAnaMeth)
  assign("listMeth", listMeth, envir = envAnaMeth)
  tkgrid(frame1)

  buttNew <- tkbutton(frame2, text = "    New    ", cursor = "hand2", command = function() setAnaMeth(action = "new", envAnaMeth))
  buttEdit <- tkbutton(frame2, text = "    Edit    ", cursor = "hand2", command = function() setAnaMeth(action = "edit", envAnaMeth))
  buttDelete <- tkbutton(frame2, text = "    Delete    ", cursor = "hand2", command = function() deleteAnaMeth(envAnaMeth))
  tkgrid(buttNew, buttEdit, buttDelete, padx = 10, pady = 5)
  tkgrid(frame2)
}
