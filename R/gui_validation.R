windowValid <- function(envGUI){
  envValidData <- new.env(parent = globalenv())
  envValidGUI <- new.env(parent = globalenv())
  setEnvValidData(envValidData, TRUE)
  pathPack <- get("pathPack", pos = envGUI)
  assign("pathPack", pathPack, envir = envValidGUI)

  tfValid <- tktoplevel()
  tkwm.title(tfValid, "Validation mode")

  tabsValid <- tk2notebook(tfValid, tabs = c("Files", "Setting", "Results"))
  tkpack(tabsValid, fill = "both", expand = 1)
  tabVF <- tk2notetab(tabsValid, "Files")
  frameVF <- tkframe(tabVF)
  tabVS <- tk2notetab(tabsValid, "Setting")
  tabVR <- tk2notetab(tabsValid, "Results")
  assign("tabsValid", tabsValid, envir = envValidGUI)
  assign("tabVF", tabVF, envir = envValidGUI)
  assign("frameVF", frameVF, envir = envValidGUI)
  assign("tabVS", tabVS, envir = envValidGUI)
  assign("frameVS", tkframe(tabVS), envir = envValidGUI)
  assign("tabVR", tabVR, envir = envValidGUI)

  makeTabVF(envValidData, envValidGUI)
}

setEnvValidData <- function(envValidData, setfiles){
  if(setfiles){
    assign("infoValidInput", NULL, envir = envValidData)
    assign("infoValidFp", character(0), envir = envValidData)
    assign("infoValidFn", character(0), envir = envValidData)
    assign("cspValidInput", NULL, envir = envValidData)
    assign("cspValidFp", character(0), envir = envValidData)
    assign("cspValidFn", character(0), envir = envValidData)
    assign("refValidInput", NULL, envir = envValidData)
    assign("refValidFp", character(0), envir = envValidData)
    assign("refValidFn", character(0), envir = envValidData)
    assign("afValidInput", NULL, envir = envValidData)
    assign("afValidFp", character(0), envir = envValidData)
    assign("afValidFn", character(0), envir = envValidData)
    assign("randValidInput", NULL, envir = envValidData)
    assign("randValidFp", character(0), envir = envValidData)
    assign("randValidFn", character(0), envir = envValidData)
  }
  assign("hncMin_valid", 1, envir = envValidData)
  assign("hncMax_valid", 4, envir = envValidData)
  assign("hncFrom_valid", 1, envir = envValidData)
  assign("hncTo_valid", 4, envir = envValidData)
  assign("anaMeth_valid", "", envir = envValidData)
#  assign("numHdTest", 100, envir = envValidData)
  assign("finValid", FALSE, envir = envValidData)
}

makeTabVF <- function(envValidData, envValidGUI){
  openFileValid <- function(type){
    inputOk <- "ok"
    finValid <- get("finValid", pos = envValidData)
    if(finValid){
      inputOk <- tclvalue(tkmessageBox(message = "Validation results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      setEnvValidData(envValidData, FALSE)

      if(type == "info"){
        fpVar <- infoValidFpVar
        fnVar <- infoValidFnVar
      }else if(type == "csp"){
        fpVar <- cspValidFpVar
        fnVar <- cspValidFnVar
      }else if(type == "ref"){
        fpVar <- refValidFpVar
        fnVar <- refValidFnVar
      }else if(type == "af"){
        fpVar <- afValidFpVar
        fnVar <- afValidFnVar
      }else if(type == "rand"){
        fpVar <- randValidFpVar
        fnVar <- randValidFnVar
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
        if(type == "info"){
          infoValidInput <- read.csv(tclvalue(fpVar), header = TRUE)
          infoValidInput <- as.matrix(infoValidInput)
          assign("infoValidInput", infoValidInput, envir = envValidData)
          assign("infoValidFp", tclvalue(fpVar), envir = envValidData)
          assign("infoValidFn", tclvalue(fnVar), envir = envValidData)
        }else if(type == "csp"){
          cspValidInput <- read.csv(tclvalue(fpVar), header = TRUE)
          cspValidInput <- as.matrix(cspValidInput)
          cspValidInput <- gsub(" ", "", cspValidInput, fixed = TRUE)
          assign("cspValidInput", cspValidInput, envir = envValidData)
          assign("cspValidFp", tclvalue(fpVar), envir = envValidData)
          assign("cspValidFn", tclvalue(fnVar), envir = envValidData)
        }else if(type == "ref"){
          refValidInput <- read.csv(tclvalue(fpVar), header = TRUE)
          refValidInput <- as.matrix(refValidInput)
          refValidInput <- gsub(" ", "", refValidInput, fixed = TRUE)
          assign("refValidInput", refValidInput, envir = envValidData)
          assign("refValidFp", tclvalue(fpVar), envir = envValidData)
          assign("refValidFn", tclvalue(fnVar), envir = envValidData)
        }else if(type == "af"){
          afValidInput <- read.csv(tclvalue(fpVar), header = TRUE)
          afValidInput <- as.matrix(afValidInput)
          assign("afValidInput", afValidInput, envir = envValidData)
          assign("afValidFp", tclvalue(fpVar), envir = envValidData)
          assign("afValidFn", tclvalue(fnVar), envir = envValidData)
        }else if(type == "rand"){
          randValidInput <- read.csv(tclvalue(fpVar), header = TRUE)
          randValidInput <- as.matrix(randValidInput)
          assign("randValidInput", randValidInput, envir = envValidData)
          assign("randValidFp", tclvalue(fpVar), envir = envValidData)
          assign("randValidFn", tclvalue(fnVar), envir = envValidData)
        }
      }
    }
  }

  infoValidFp <- get("infoValidFp", pos = envValidData)
  infoValidFpVar <- tclVar(infoValidFp)
  infoValidFn <- get("infoValidFn", pos = envValidData)
  infoValidFnVar <- tclVar(infoValidFn)
  cspValidFp <- get("cspValidFp", pos = envValidData)
  cspValidFpVar <- tclVar(cspValidFp)
  cspValidFn <- get("cspValidFn", pos = envValidData)
  cspValidFnVar <- tclVar(cspValidFn)
  refValidFp <- get("refValidFp", pos = envValidData)
  refValidFpVar <- tclVar(refValidFp)
  refValidFn <- get("refValidFn", pos = envValidData)
  refValidFnVar <- tclVar(refValidFn)
  afValidFp <- get("afValidFp", pos = envValidData)
  afValidFpVar <- tclVar(afValidFp)
  afValidFn <- get("afValidFn", pos = envValidData)
  afValidFnVar <- tclVar(afValidFn)
  randValidFp <- get("randValidFp", pos = envValidData)
  randValidFpVar <- tclVar(randValidFp)
  randValidFn <- get("randValidFn", pos = envValidData)
  randValidFnVar <- tclVar(randValidFn)
  tabVF <- get("tabVF", pos = envValidGUI)
  frameVF <- get("frameVF", pos = envValidGUI)

  tkdestroy(frameVF)
  frameVF <- tkframe(tabVF)

  frameVF_1 <- tkframe(frameVF)
  tkgrid(tklabel(frameVF_1, text = "Input File", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  
  labelInfo <- tklabel(frameVF_1, text = "Information on Crime Stain Profiles")
  labelInfoName <- tklabel(frameVF_1, textvariable = infoValidFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadInfo <- tkbutton(frameVF_1, text = "    Load    ", cursor = "hand2", command = function() openFileValid("info"))
  tkgrid(labelInfo, labelInfoName, buttLoadInfo, padx = 10, pady = 5, sticky = "w")
  
  labelCsp <- tklabel(frameVF_1, text = "Crime Stain Profiles")
  labelCspName <- tklabel(frameVF_1, textvariable = cspValidFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadCsp <- tkbutton(frameVF_1, text = "    Load    ", cursor = "hand2", command = function() openFileValid("csp"))
  tkgrid(labelCsp, labelCspName, buttLoadCsp, padx = 10, pady = 5, sticky = "w")

  labelRef <- tklabel(frameVF_1, text = "Reference Profiles")
  labelRefName <-tklabel(frameVF_1, textvariable = refValidFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadRef <- tkbutton(frameVF_1, text = "    Load    ", cursor = "hand2", command = function() openFileValid("ref"))
  tkgrid(labelRef, labelRefName, buttLoadRef, padx = 10, pady = 5, sticky = "w")

  labelAf <- tklabel(frameVF_1, text = "Allele Frequencies")
  labelAfName <- tklabel(frameVF_1, textvariable = afValidFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadAf <- tkbutton(frameVF_1, text = "    Load    ", cursor = "hand2", command = function() openFileValid("af"))
  tkgrid(labelAf, labelAfName, buttLoadAf, padx = 10, pady = 5, sticky = "w")
  
  labelRand <- tklabel(frameVF_1, text = "Non-contributor profiles")
  labelRandName <- tklabel(frameVF_1, textvariable = randValidFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadRand <- tkbutton(frameVF_1, text = "    Load    ", cursor = "hand2", command = function() openFileValid("rand"))
  tkgrid(labelRand, labelRandName, buttLoadRand, padx = 10, pady = 5, sticky = "w")

  tkgrid(frameVF_1, padx = 10, pady = 5, sticky = "w")

  frameVF_2 <- tkframe(frameVF)
  buttNext <- tkbutton(frameVF_2, text = "    Next    ", cursor = "hand2", command = function() checkValidFile(envValidData, envValidGUI))
  tkgrid(buttNext, padx = 10, pady = 5, sticky = "w")
  tkgrid(frameVF_2, padx = 10, pady = 5, sticky = "w")

  tkgrid(frameVF)
  assign("frameVF", frameVF, envir = envValidGUI)
}

# Check loci of reference profiles
checkRefLoci <- function(refValidInput){
  posSn <- intersect(grep("Sample", colnames(refValidInput)), grep("Name", colnames(refValidInput)))
  posRn <- intersect(grep("Reference", colnames(refValidInput)), grep("Name", colnames(refValidInput)))
  snAll <- unique(refValidInput[, posSn])
  sn <- snAll[1]
  refOneS <- refValidInput[refValidInput[, posSn] == sn, , drop = FALSE]
  rnOneS <- unique(refOneS[, posRn])
  rn <- rnOneS[1]
  inputLoci <- refOneS[refOneS[, posRn] == rn, "Marker"]
  outSn <- outRn <- character(0)
  for(i in 1:length(snAll)){
    sn <- snAll[i]
    refOneS <- refValidInput[refValidInput[, posSn] == sn, , drop = FALSE]
    rnOneS <- unique(refOneS[, posRn])
    for(j in 1:length(rnOneS)){
      rn <- rnOneS[j]
      inputLoci2 <- refOneS[refOneS[, posRn] == rn, "Marker"]
      if(!setequal(inputLoci, inputLoci2)){
        outSn <- sn
        outRn <- rn
        break
      }
    }
  }
  return(c(outSn, outRn))
}

checkValidFile <- function(envValidData, envValidGUI){
  infoValidInput <- get("infoValidInput", pos = envValidData)
  cspValidInput <- get("cspValidInput", pos = envValidData)
  refValidInput <- get("refValidInput", pos = envValidData)
  afValidInput <- get("afValidInput", pos = envValidData)
  randValidInput <- get("randValidInput", pos = envValidData)
  if(any(c(length(cspValidInput) == 0, length(refValidInput) == 0, length(afValidInput) == 0))){
    tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
  }else{
    posSnInfo <- intersect(grep("Sample", colnames(infoValidInput)), grep("Name", colnames(infoValidInput)))
    posSnCsp <- intersect(grep("Sample", colnames(cspValidInput)), grep("Name", colnames(cspValidInput)))
    if(setequal(infoValidInput[, posSnInfo], cspValidInput[, posSnCsp])){
      outSn <- checkInputLoci(cspValidInput)
      if(length(outSn) == 0){
        colCsp <- colnames(cspValidInput)
        cspLoci <- unique(cspValidInput[, grep("Marker", colCsp)])
        pathPack <- get("pathPack", pos = envValidGUI)
        cspKit <- searchKit(cspLoci, pathPack)
        if(length(cspKit) > 0){
          kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", cspKit[1], ".csv"), header = TRUE)
          kitInfo <- as.matrix(kitInfo)
          posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
          if(length(posSexMar) > 0){
            sexMar <- unique(kitInfo[posSexMar, "Marker"])
            cspLoci <- setdiff(cspLoci, sexMar)
          }else{
            sexMar <- character(0)
          }
          outSnRn <- checkRefLoci(refValidInput)
          if(length(outSnRn) == 0){
            colRef <- colnames(refValidInput)
            refLoci <- unique(refValidInput[, "Marker"])
            refLoci <- setdiff(refLoci, sexMar)
            
            posAf <- !is.element(colnames(afValidInput), sexMar)
            afValid <- afValidInput[, posAf, drop = FALSE]
            afLoci <- colnames(afValid)[posAf]
            afLoci <- afLoci[- grep("Allele", colnames(afValid))]
            
            posRandMar <- which(colnames(randValidInput) == "Marker")
            randLoci <- randValidInput[, posRandMar]
            posRand <- !is.element(randLoci, sexMar)
            randValid <- randValidInput[posRand, -posRandMar, drop = FALSE]
            randLoci <- setdiff(randLoci, sexMar)
            
            if(all(c(setequal(cspLoci, refLoci), setequal(cspLoci, afLoci), setequal(cspLoci, randLoci)))){
              posCspSn <- intersect(grep("Sample", colCsp), grep("Name", colCsp))
              cspSn <- unique(cspValidInput[, posCspSn])
              posRefSn <- intersect(grep("Sample", colRef), grep("Name", colRef))
              refSn <- unique(refValidInput[, posRefSn])
              if(setequal(cspSn, refSn)){
                nL <- length(cspLoci)
                nS <- length(cspSn)
                cspValid <- matrix("", nL * nS, ncol(cspValidInput))
                colnames(cspValid) <- colCsp
                refValid <- NULL
                posRn <- intersect(grep("Reference", colnames(refValidInput)), grep("Name", colnames(refValidInput)))
                for(i in 1:nS){
                  cspOneS <- cspValidInput[cspValidInput[, posCspSn] == cspSn[i], , drop = FALSE]
                  cspValid[((i - 1) * nL + 1):(i * nL), ] <- cspOneS[match(cspLoci, cspOneS[, "Marker"]), , drop = FALSE]
                  refOneS <- refValidInput[refValidInput[, posRefSn] == cspSn[i], , drop = FALSE]
                  rnOneS <- unique(refOneS[, posRn])
                  for(j in 1:length(rnOneS)){
                    refOneR <- refOneS[refOneS[, posRn] == rnOneS[j], , drop = FALSE]
                    refValid <- rbind(refValid, refOneR[match(cspLoci, refOneR[, "Marker"]), , drop = FALSE])
                  }
                }
                colnames(refValid) <- colRef
                posAf <- match(cspLoci, afLoci)
                afValid <- afValid[, c(1, posAf + 1), drop = FALSE]
                posRand <- match(cspLoci, randLoci)
                randValid <- randValid[posRand, , drop = FALSE]
                
                assign("cspValid", cspValid, envir = envValidData)
                assign("refValid", refValid, envir = envValidData)
                assign("afValid", afValid, envir = envValidData)
                assign("randValid", randValid, envir = envValidData)
                makeTabVS(envValidData, envValidGUI)
                tabsValid <- get("tabsValid", pos = envValidGUI)
                tk2notetab.select(tabsValid, "Setting")
              }else{
                tkmessageBox(message = "Sample names of crime stain profiles are not equal to those of reference profiles!", icon = "error", type = "ok")
              }
            }else{
              tkmessageBox(message = "Autosomal loci of each file was not the same!", icon = "error", type = "ok")
            }
          }else{
            tkmessageBox(message = paste0("Locus set of each reference profile is not the same. Please check the following sample.", "\n",
                                          "Sample Name : ", outSnRn[1], "\n",
                                          "Reference Name : ", outSnRn[2]), icon = "error", type = "ok")
          }
        }else{
          tkmessageBox(message = "Locus set of the crime stain profiles has not been registered. Please register locus set at Tools -> Typing kit.", icon = "error", type = "ok")
        }
      }else{
        tkmessageBox(message = paste0("Locus set of each crime stain profile is not the same. Please check the sample '", outSn, "'."), icon = "error", type = "ok")
      }
    }else{
      tkmessageBox(message = "Sample names of 'Information on Crime Stain Profiles' is not the same as those of 'Crime Stain Profiles'.", icon = "error", type = "ok")
    }
  }
}

makeTabVS <- function(envValidData, envValidGUI){
  pathPack <- get("pathPack", pos = envValidGUI)
  infoValidFn <- get("infoValidFn", pos = envValidData)
  cspValidFn <- get("cspValidFn", pos = envValidData)
  refValidFn <- get("refValidFn", pos = envValidData)
  afValidFn <- get("afValidFn", pos = envValidData)
  randValidFn <- get("randValidFn", pos = envValidData)
  hncFrom_valid <- get("hncFrom_valid", pos = envValidData)
  hncFromVar_valid <- tclVar(hncFrom_valid)
  hncTo_valid <- get("hncTo_valid", pos = envValidData)
  hncToVar_valid <- tclVar(hncTo_valid)
  anaMeth_valid <- get("anaMeth_valid", pos = envValidData)
  anaMethVar_valid <- tclVar(anaMeth_valid)
#  numHdTest <- get("numHdTest", pos = envValidData)
#  numHdTestVar <- tclVar(numHdTest)

  nameMeth <- list.files(paste0(pathPack, "/extdata/analysis_method"))
  nameMeth <- gsub(".csv", "", nameMeth)

  tabVS <- get("tabVS", pos = envValidGUI)
  frameVS <- get("frameVS", pos = envValidGUI)
  tkdestroy(frameVS)
  frameVS <- tkframe(tabVS)
  frameVS_1 <- tkframe(frameVS)
  frameVS_2 <- tkframe(frameVS)

  frameVS_noc <- tkframe(frameVS_1, relief = "groove", borderwidth = 2)
  frameVS_noc_1 <- tkframe(frameVS_noc)
  frameVS_noc_2 <- tkframe(frameVS_noc)
  tkgrid(tklabel(frameVS_noc_1, text = "Number of contributors", font = "Helvetica 10 bold"), sticky = "w")
  hncFromSpin <- tkspinbox(frameVS_noc_2, from = 1, to = 4, increment = 1, textvariable = hncFromVar_valid, width = 6, highlightthickness = 1, justify = "center", background = "white", state = "normal")
  hncToSpin <- tkspinbox(frameVS_noc_2, from = 1, to = 4, increment = 1, textvariable = hncToVar_valid, width = 6, highlightthickness = 1, justify = "center", background = "white", state = "normal")
  tkgrid(tklabel(frameVS_noc_2, text = "    From"), hncFromSpin, padx = 10, sticky = "w")
  tkgrid(tklabel(frameVS_noc_2, text = "    To"),  hncToSpin, padx = 10, sticky = "w")
  tkgrid(frameVS_noc_1, padx = 20)
  tkgrid(frameVS_noc_2, padx = 20)
  tkgrid(frameVS_noc, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameVS_1, sticky = "w")

  frameVS_set <- tkframe(frameVS_2, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(frameVS_set, text = "Setting", font = "Helvetica 10 bold"), sticky = "w")
  tkgrid(tklabel(frameVS_set, text = "Analysis method"),
         ttkcombobox(frameVS_set, values = nameMeth, textvariable = anaMethVar_valid, width = 25, state = "readonly"),
         padx = 10, pady = 5, sticky = "w")
#  tkgrid(tklabel(frameVS_set, text = "Number of non-contributor tests"),
#         tkentry(frameVS_set, textvariable = numHdTestVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
#         padx = 10, pady = 5, sticky = "w")

  frameVS_files <- tkframe(frameVS_2, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(frameVS_files, text = "Files", font = "Helvetica 10 bold"), sticky = "w")
  tkgrid(tklabel(frameVS_files, text = "Information on Crime Stain Profiles"), tklabel(frameVS_files, text = infoValidFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameVS_files, text = "Crime Stain Profile"), tklabel(frameVS_files, text = cspValidFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameVS_files, text = "Reference Profile"), tklabel(frameVS_files, text = refValidFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameVS_files, text = "Allele Frequencies"), tklabel(frameVS_files, text = afValidFn), padx = 10, sticky = "w")
  tkgrid(tklabel(frameVS_files, text = "Non-contributor Profiles"), tklabel(frameVS_files, text = randValidFn), padx = 10, sticky = "w")
  tkgrid(frameVS_set, frameVS_files, padx = 5, pady = 5, sticky = "nw")
  tkgrid(frameVS_2, sticky = "w")

  tkgrid(tkbutton(frameVS, text = "    Analyze    ", cursor = "hand2",
                  command = function() guiValid(envValidData, envValidGUI,
                                                as.numeric(tclvalue(hncFromVar_valid)), as.numeric(tclvalue(hncToVar_valid)),
                                                tclvalue(anaMethVar_valid))), pady = 5)

  tkgrid(frameVS, padx = 20, pady = 10)
  assign("frameVS", frameVS, envir = envValidGUI)
}

guiValid <- function(envValidData, envValidGUI, hncFrom, hncTo, anaMeth){
  infoValid <- get("infoValidInput", pos = envValidData)
  cspValid <- get("cspValid", pos = envValidData)
  refValid <- get("refValid", pos = envValidData)
  af <- get("afValid", pos = envValidData)
  randValid <- get("randValid", pos = envValidData)
  pathPack <- get("pathPack", pos = envValidGUI)

  hncMin <- get("hncMin_valid", pos = envValidData)
  hncMax <- get("hncMax_valid", pos = envValidData)

  assign("hncFrom_valid", hncFrom, pos = envValidData)
  assign("hncTo_valid", hncTo, pos = envValidData)
  assign("anaMeth_valid", anaMeth, pos = envValidData)
#  assign("numHdTest", numHdTest, pos = envValidData)

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

    mcPar <- read.csv(paste(pathPack, "/extdata/parameters/", namePar, sep = ""), header = TRUE)
    mcPar <- as.matrix(mcPar)
    alCor <- read.csv(paste(pathPack, "/extdata/repeat_correction/", nameAlCor, sep = ""), header = TRUE)
    alCor <- as.matrix(alCor)

    kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", kit, ".csv"), header = TRUE)
    kitInfo <- as.matrix(kitInfo)
    kitLoci <- unique(kitInfo[, "Marker"])
    posSexMar <- which(kitInfo[, "Sex_chromosomal_marker"] == "yes")
    if(length(posSexMar) > 0){
      sexMar <- unique(kitInfo[posSexMar, "Marker"])
      kitLoci <- setdiff(kitLoci, sexMar)
    }

    #Check locus set
    cspLoci <- unique(cspValid[, "Marker"])
    if(setequal(cspLoci, kitLoci)){
      assign("validLoci", cspLoci, envir = envValidData)
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
        colCsp <- colnames(cspValid)
        posCspSn <- intersect(grep("Sample", colCsp), grep("Name", colCsp))
        colRef <- colnames(refValid)
        posRefSn <- intersect(grep("Sample", colRef), grep("Name", colRef))
        posRn <- intersect(grep("Reference", colnames(refValid)), grep("Name", colnames(refValid)))
        sampleNames <- unique(cspValid[, posCspSn])
        nS <- length(sampleNames)
        nL <- length(cspLoci)
        colValidData <- c("sample_name", paste0("contrib_", 1:4), paste0("mp_", 1:4), paste0("deg_", 1:4), paste0("like_", c(cspLoci, "Total")))
        validData_1st <- validData_2nd <- matrix("", 30 * nS, length(colValidData))
        colnames(validData_1st) <- colnames(validData_2nd) <- colValidData
        colHdTrueData <- c("sample_name", "hnc", paste0("LR_", c(cspLoci, "Total")))
        hdTrueData_1st <- hdTrueData_2nd <- matrix("", 4 * nS, length(colHdTrueData))
        colnames(hdTrueData_1st) <- colnames(hdTrueData_2nd) <- colHdTrueData

        afList <- list()
        afAl <- af[, grep("Allele", colnames(af))]
        for(i in 1:nL){
          afOneL <- af[, i + 1]
          posAfAl <- !is.na(afOneL)
          afList[[i]] <- makePopFreq(afOneL[posAfAl], afAl[posAfAl], afMeth)
        }

        count <- 1
        countHd <- 1
        for(i in 1:nS){
          sn <- sampleNames[i]
          cat(paste0("Current sample : ", sn), "\n")
          csp <- cspValid[cspValid[, posCspSn] == sn, , drop = FALSE]
          refOneS <- refValid[refValid[, posRefSn] == sn, , drop = FALSE]
          refNames <- unique(refOneS[, posRn])
          ref <- matrix("", nL, 1 + 2 * length(refNames))
          ref[, 1] <- cspLoci
          colnames(ref) <- c("Marker", as.vector(sapply(refNames, rep, 2)))
          for(j in 1:length(refNames)){
            ref[, c(2 * j, 2 * j + 1)] <- refOneS[refOneS[, posRn] == refNames[j], grep("Allele", colRef)]
          }
          cat("1st deconvolution", "\n")
          dataDeconvo_1 <- analyzeCSP(csp, NULL, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                      anaType = "Deconvo", addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("1st LR", "\n")
          dataLR_1 <- analyzeCSP(csp, ref, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                 anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_1, calcAllHyp = 1, addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("2nd deconvolution", "\n")
          dataDeconvo_2 <- analyzeCSP(csp, NULL, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                      anaType = "Deconvo", addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("2nd LR", "\n")
          dataLR_2 <- analyzeCSP(csp, ref, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                 anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_2, calcAllHyp = 1, addRefDropFunc = FALSE, mrDegCut = 0.0001)

          hncAll <- as.numeric(gsub("hnc_", "", names(dataLR_1)))
          for(j in 1:length(dataLR_1)){
            hnc <- hncAll[j]
            dataLR_1_oneHnc <- dataLR_1[[j]]
            dataLR_2_oneHnc <- dataLR_2[[j]]
            for(k in 1:length(dataLR_1_oneHnc)){
              if(count > 30 * nS){
                validData_1st <- rbind(validData_1st, matrix("", 30 * nS, length(colValidData)))
                validData_2nd <- rbind(validData_2nd, matrix("", 30 * nS, length(colValidData)))
              }
              dataLR_1_oneHyp <- dataLR_1_oneHnc[[k]]
              validData_1st[count, "sample_name"] <- sn
              validData_1st[count, grep("contrib_", colValidData)[1:hnc]] <- dataLR_1_oneHyp[[1]]
              likelihoods <- dataLR_1_oneHyp[[2]]
              if(!is.na(likelihoods[length(likelihoods)])){
                validData_1st[count, grep("like_", colValidData)] <- likelihoods
                validData_1st[count, grep("mp_", colValidData)[1:hnc]] <- dataLR_1_oneHyp[[4]]
                validData_1st[count, grep("deg_", colValidData)[1:hnc]] <- dataLR_1_oneHyp[[5]]
              }
              dataLR_2_oneHyp <- dataLR_2_oneHnc[[k]]
              validData_2nd[count, "sample_name"] <- sn
              validData_2nd[count, grep("contrib_", colValidData)[1:hnc]] <- dataLR_2_oneHyp[[1]]
              likelihoods <- dataLR_2_oneHyp[[2]]
              if(!is.na(likelihoods[length(likelihoods)])){
                validData_2nd[count, grep("like_", colValidData)] <- likelihoods
                validData_2nd[count, grep("mp_", colValidData)[1:hnc]] <- dataLR_2_oneHyp[[4]]
                validData_2nd[count, grep("deg_", colValidData)[1:hnc]] <- dataLR_2_oneHyp[[5]]
              }
              count <- count + 1
            }
          }

          cat("Non-contributor tests were started.", "\n")
          for(j in 1:(ncol(randValid) / 2)){
            randOne <- randValid[, c(2 * j - 1, 2 * j)]
            randOne <- t(apply(randOne, 1, sort))
            randOne <- cbind(cspLoci, randOne)
            colnames(randOne) <- c("Marker", paste0(rep("Rand", 2), j))
            dataHdTrue_1 <- analyzeCSP(csp, randOne, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = 1, knownHd = numeric(0),
                                       anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_1, calcAllHyp = 0, addRefDropFunc = FALSE, mrDegCut = 0.0001)
            dataHdTrue_2 <- analyzeCSP(csp, randOne, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = 1, knownHd = numeric(0),
                                       anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_2, calcAllHyp = 0, addRefDropFunc = FALSE, mrDegCut = 0.0001)

            hncAll <- as.numeric(gsub("hnc_", "", names(dataHdTrue_1)))
            for(k in 1:length(dataHdTrue_1)){
              hnc <- hncAll[k]
              dataHdTrue_1_oneHnc <- dataHdTrue_1[[k]]
              dataHdTrue_2_oneHnc <- dataHdTrue_2[[k]]
              if(countHd > 4 * nS){
                hdTrueData_1st <- rbind(hdTrueData_1st, matrix("", 4 * nS, length(colHdTrueData)))
                hdTrueData_2nd <- rbind(hdTrueData_2nd, matrix("", 4 * nS, length(colHdTrueData)))
              }
              hdTrueData_1st[countHd, "sample_name"] <- sn
              hdTrueData_1st[countHd, "hnc"] <- hnc
              hdTrueData_1st[countHd, grep("LR_", colHdTrueData)] <- dataHdTrue_1_oneHnc[[1]][[2]] - dataHdTrue_1_oneHnc[[2]][[2]]
              hdTrueData_2nd[countHd, "sample_name"] <- sn
              hdTrueData_2nd[countHd, "hnc"] <- hnc
              hdTrueData_2nd[countHd, grep("LR_", colHdTrueData)] <- dataHdTrue_2_oneHnc[[1]][[2]] - dataHdTrue_2_oneHnc[[2]][[2]]
              countHd <- countHd + 1
            }
          }
          save(dataDeconvo_1, dataLR_1, dataDeconvo_2, dataLR_2, file = paste0("D:/kongoh_valid/valiData_", sn, ".RData"))
        }
        validData_1st <- validData_1st[1:count, , drop = FALSE]
        validData_2nd <- validData_2nd[1:count, , drop = FALSE]
        hdTrueData_1st <- hdTrueData_1st[1:countHd, , drop = FALSE]
        hdTrueData_2nd <- hdTrueData_2nd[1:countHd, , drop = FALSE]
        write.csv(validData_1st, "D:/kongoh_valid/validData_1st.csv", row.names = FALSE)
        write.csv(validData_2nd, "D:/kongoh_valid/validData_2nd.csv", row.names = FALSE)
        write.csv(hdTrueData_1st, "D:/kongoh_valid/hdTrueData_1st.csv", row.names = FALSE)
        write.csv(hdTrueData_2nd, "D:/kongoh_valid/hdTrueData_2nd.csv", row.names = FALSE)
        assign("validData_1st", validData_1st, envir = envValidData)
        assign("validData_2nd", validData_2nd, envir = envValidData)
        assign("hdTrueData_1st", hdTrueData_1st, envir = envValidData)
        assign("hdTrueData_2nd", hdTrueData_2nd, envir = envValidData)
#        makeTabVR(envValidData, envValidGUI)
      }
    }else{
      tkmessageBox(message = "Locus set of the selected analysis method was not the same as that of input files!", icon = "error", type = "ok")
    }
  }
}
