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
                rownames(randValid) <- cspLoci
                
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

        colInfo <- colnames(infoValid)
        nocInfo <- as.numeric(infoValid[, intersect(grep("Number", colInfo), grep("contributors", colInfo))])
        sensitivity_1st <- sensitivity_2nd <- sensitivity_1st_nocP1 <- sensitivity_2nd_nocP1 <- sensitivity_1st_nocM1 <- sensitivity_2nd_nocM1 <- matrix("", sum(nocInfo), 11)
        nRand <- ncol(randValid) / 2
        specificity_1st <- specificity_2nd <- specificity_1st_nocP1 <- specificity_2nd_nocP1 <- specificity_1st_nocM1 <- specificity_2nd_nocM1 <- matrix("", nS * nRand, 11)
        colnames(sensitivity_1st) <- c("Sample_name", "Number_of_contributors", "Hp", "Hd", "Likelihood_Hp", "Likelihood_Hd", "LR", "MP_Hp", "MP_Hd", "d_Hp", "d_Hd")
        colnames(sensitivity_2nd) <- colnames(sensitivity_1st_nocP1) <- colnames(sensitivity_2nd_nocP1) <- colnames(sensitivity_1st_nocM1) <- colnames(sensitivity_2nd_nocM1) <- colnames(sensitivity_1st)
        colnames(specificity_1st) <- colnames(specificity_2nd) <- colnames(specificity_1st_nocP1) <- colnames(specificity_2nd_nocP1) <- colnames(specificity_1st_nocM1) <- colnames(specificity_2nd_nocM1) <- colnames(sensitivity_1st)
        
        afList <- list()
        afAl <- af[, grep("Allele", colnames(af))]
        for(i in 1:nL){
          afOneL <- af[, i + 1]
          posAfAl <- !is.na(afOneL)
          afList[[i]] <- makePopFreq(afOneL[posAfAl], afAl[posAfAl], afMeth)
        }

        count_sensitivity_1st <- count_sensitivity_2nd <- count_sensitivity_1st_nocP1 <- count_sensitivity_2nd_nocP1 <- count_sensitivity_1st_nocM1 <- count_sensitivity_2nd_nocM1 <- 1
        count_specificity_1st <- count_specificity_2nd <- count_specificity_1st_nocP1 <- count_specificity_2nd_nocP1 <- count_specificity_1st_nocM1 <- count_specificity_2nd_nocM1 <- 1
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
          dataDeconvo_1st <- analyzeCSP(csp, NULL, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                        anaType = "Deconvo", addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("1st LR", "\n")
          dataLR_1st <- analyzeCSP(csp, ref, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                   anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_1, calcAllHyp = 1, addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("2nd deconvolution", "\n")
          dataDeconvo_2nd <- analyzeCSP(csp, NULL, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                        anaType = "Deconvo", addRefDropFunc = FALSE, mrDegCut = 0.0001)

          cat("2nd LR", "\n")
          dataLR_2nd <- analyzeCSP(csp, ref, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = numeric(0), knownHd = numeric(0),
                                   anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_2, calcAllHyp = 1, addRefDropFunc = FALSE, mrDegCut = 0.0001)

          ## Sensitivity
          noc <- nocInfo[i]
          posNoc <- which(hncFrom:hncTo == noc)
          dataLR_1st_Noc <- dataLR_1st[[posNoc]]
          dataLR_2nd_Noc <- dataLR_2nd[[posNoc]]
          
          #### Select Hd 1st
          for(j in 1:length(dataLR_1st_Noc)){
            hypOne <- dataLR_1st_Noc[[j]][[1]]
            if(setequal(hypOne, "U")){
              dataLR_1st_Hd <- dataLR_1st_Noc[[j]]
              break
            }
          }
          
          #### Select Hd 2nd
          for(j in 1:length(dataLR_2nd_Noc)){
            hypOne <- dataLR_2nd_Noc[[j]][[1]]
            if(setequal(hypOne, "U")){
              dataLR_2nd_Hd <- dataLR_2nd_Noc[[j]]
              break
            }
          }
          
          #### Select Hp 1st and make sensitivity_1st
          for(j in 1:length(refNames)){
            for(k in 1:length(dataLR_1st_Noc)){
              hypOne <- dataLR_1st_Noc[[k]][[1]]
              if(setequal(hypOne, c(refNames[j], "U"))){
                dataLR_1st_Hp <- dataLR_1st_Noc[[k]]
                break
              }
            }
            sensitivity_1st[count_sensitivity_1st, "Sample_name"] <- sn
            sensitivity_1st[count_sensitivity_1st, "Number_of_contributors"] <- noc
            sensitivity_1st[count_sensitivity_1st, "Hp"] <- paste(dataLR_1st_Hp[[1]], collapse = "_")
            sensitivity_1st[count_sensitivity_1st, "Hd"] <- paste(dataLR_1st_Hd[[1]], collapse = "_")
            sensitivity_1st[count_sensitivity_1st, "Likelihood_Hp"] <- likeHp <- dataLR_1st_Hp[[2]][nL + 1]
            sensitivity_1st[count_sensitivity_1st, "Likelihood_Hd"] <- likeHd <- dataLR_1st_Hd[[2]][nL + 1]
            sensitivity_1st[count_sensitivity_1st, "LR"] <- 10^(likeHp - likeHd)
            sensitivity_1st[count_sensitivity_1st, "MP_Hp"] <- paste(dataLR_1st_Hp[[4]], collapse = "_")
            sensitivity_1st[count_sensitivity_1st, "MP_Hd"] <- paste(dataLR_1st_Hd[[4]], collapse = "_")
            sensitivity_1st[count_sensitivity_1st, "d_Hp"] <- paste(dataLR_1st_Hp[[5]], collapse = "_")
            sensitivity_1st[count_sensitivity_1st, "d_Hd"] <- paste(dataLR_1st_Hd[[5]], collapse = "_")
            count_sensitivity_1st <- count_sensitivity_1st + 1
          }
          
          #### Select Hp 2nd and make sensitivity_2nd
          for(j in 1:length(refNames)){
            for(k in 1:length(dataLR_2nd_Noc)){
              hypOne <- dataLR_2nd_Noc[[k]][[1]]
              if(setequal(hypOne, c(refNames[j], "U"))){
                dataLR_2nd_Hp <- dataLR_2nd_Noc[[k]]
                break
              }
            }
            sensitivity_2nd[count_sensitivity_2nd, "Sample_name"] <- sn
            sensitivity_2nd[count_sensitivity_2nd, "Number_of_contributors"] <- noc
            sensitivity_2nd[count_sensitivity_2nd, "Hp"] <- paste(dataLR_2nd_Hp[[1]], collapse = "_")
            sensitivity_2nd[count_sensitivity_2nd, "Hd"] <- paste(dataLR_2nd_Hd[[1]], collapse = "_")
            sensitivity_2nd[count_sensitivity_2nd, "Likelihood_Hp"] <- likeHp <- dataLR_2nd_Hp[[2]][nL + 1]
            sensitivity_2nd[count_sensitivity_2nd, "Likelihood_Hd"] <- likeHd <- dataLR_2nd_Hd[[2]][nL + 1]
            sensitivity_2nd[count_sensitivity_2nd, "LR"] <- 10^(likeHp - likeHd)
            sensitivity_2nd[count_sensitivity_2nd, "MP_Hp"] <- paste(dataLR_2nd_Hp[[4]], collapse = "_")
            sensitivity_2nd[count_sensitivity_2nd, "MP_Hd"] <- paste(dataLR_2nd_Hd[[4]], collapse = "_")
            sensitivity_2nd[count_sensitivity_2nd, "d_Hp"] <- paste(dataLR_2nd_Hp[[5]], collapse = "_")
            sensitivity_2nd[count_sensitivity_2nd, "d_Hd"] <- paste(dataLR_2nd_Hd[[5]], collapse = "_")
            count_sensitivity_2nd <- count_sensitivity_2nd + 1
          }
          
          ## Sensitivity N + 1 contributors
          posNocP1 <- which(hncFrom:hncTo == noc + 1)
          if(length(posNocP1) == 1){
            dataLR_1st_NocP1 <- dataLR_1[[posNocP1]]
            dataLR_2nd_NocP1 <- dataLR_2[[posNocP1]]
            
            #### Select Hd 1st
            for(j in 1:length(dataLR_1st_NocP1)){
              hypOne <- dataLR_1st_NocP1[[j]][[1]]
              if(setequal(hypOne, "U")){
                dataLR_1st_NocP1_Hd <- dataLR_1st_NocP1[[j]]
                break
              }
            }
            
            #### Select Hd 2nd
            for(j in 1:length(dataLR_2nd_NocP1)){
              hypOne <- dataLR_2nd_NocP1[[j]][[1]]
              if(setequal(hypOne, "U")){
                dataLR_2nd_NocP1_Hd <- dataLR_2nd_NocP1[[j]]
                break
              }
            }
            
            #### Select Hp 1st and make sensitivity_1st
            for(j in 1:length(refNames)){
              for(k in 1:length(dataLR_1st_NocP1)){
                hypOne <- dataLR_1st_NocP1[[k]][[1]]
                if(setequal(hypOne, c(refNames[j], "U"))){
                  dataLR_1st_NocP1_Hp <- dataLR_1st_NocP1[[k]]
                  break
                }
              }
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Sample_name"] <- sn
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Number_of_contributors"] <- noc + 1
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Hp"] <- paste(dataLR_1st_NocP1_Hp[[1]], collapse = "_")
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Hd"] <- paste(dataLR_1st_NocP1_Hd[[1]], collapse = "_")
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Likelihood_Hp"] <- likeHp <- dataLR_1st_NocP1_Hp[[2]][nL + 1]
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "Likelihood_Hd"] <- likeHd <- dataLR_1st_NocP1_Hd[[2]][nL + 1]
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "LR"] <- 10^(likeHp - likeHd)
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "MP_Hp"] <- paste(dataLR_1st_NocP1_Hp[[4]], collapse = "_")
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "MP_Hd"] <- paste(dataLR_1st_NocP1_Hd[[4]], collapse = "_")
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "d_Hp"] <- paste(dataLR_1st_NocP1_Hp[[5]], collapse = "_")
              sensitivity_1st_nocP1[count_sensitivity_1st_nocP1, "d_Hd"] <- paste(dataLR_1st_NocP1_Hd[[5]], collapse = "_")
              count_sensitivity_1st_nocP1 <- count_sensitivity_1st_nocP1 + 1
            }
            
            #### Select Hp 2nd and make sensitivity_2nd
            for(j in 1:length(refNames)){
              for(k in 1:length(dataLR_2nd_NocP1)){
                hypOne <- dataLR_2nd_NocP1[[k]][[1]]
                if(setequal(hypOne, c(refNames[j], "U"))){
                  dataLR_2nd_NocP1_Hp <- dataLR_2nd_NocP1[[k]]
                  break
                }
              }
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Sample_name"] <- sn
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Number_of_contributors"] <- noc + 1
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Hp"] <- paste(dataLR_2nd_NocP1_Hp[[1]], collapse = "_")
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Hd"] <- paste(dataLR_2nd_NocP1_Hd[[1]], collapse = "_")
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Likelihood_Hp"] <- likeHp <- dataLR_2nd_NocP1_Hp[[2]][nL + 1]
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "Likelihood_Hd"] <- likeHd <- dataLR_2nd_NocP1_Hd[[2]][nL + 1]
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "LR"] <- 10^(likeHp - likeHd)
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "MP_Hp"] <- paste(dataLR_2nd_NocP1_Hp[[4]], collapse = "_")
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "MP_Hd"] <- paste(dataLR_2nd_NocP1_Hd[[4]], collapse = "_")
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "d_Hp"] <- paste(dataLR_2nd_NocP1_Hp[[5]], collapse = "_")
              sensitivity_2nd_nocP1[count_sensitivity_2nd_nocP1, "d_Hd"] <- paste(dataLR_2nd_NocP1_Hd[[5]], collapse = "_")
              count_sensitivity_2nd_nocP1 <- count_sensitivity_2nd_nocP1 + 1
            }
          }else{
            count_sensitivity_1st_nocP1 <- count_sensitivity_1st_nocP1 + length(refNames)
            count_sensitivity_2nd_nocP1 <- count_sensitivity_2nd_nocP1 + length(refNames)
          }
          
          ## Sensitivity N - 1 contributors
          posNocM1 <- which(hncFrom:hncTo == noc - 1)
          if(length(posNocM1) == 1){
            dataLR_1st_NocM1 <- dataLR_1[[posNocM1]]
            dataLR_2nd_NocM1 <- dataLR_2[[posNocM1]]
            
            #### Select Hd 1st
            for(j in 1:length(dataLR_1st_NocM1)){
              hypOne <- dataLR_1st_NocM1[[j]][[1]]
              if(setequal(hypOne, "U")){
                dataLR_1st_NocM1_Hd <- dataLR_1st_NocM1[[j]]
                break
              }
            }
            
            #### Select Hd 2nd
            for(j in 1:length(dataLR_2nd_NocM1)){
              hypOne <- dataLR_2nd_NocM1[[j]][[1]]
              if(setequal(hypOne, "U")){
                dataLR_2nd_NocM1_Hd <- dataLR_2nd_NocM1[[j]]
                break
              }
            }
            
            #### Select Hp 1st and make sensitivity_1st
            for(j in 1:length(refNames)){
              for(k in 1:length(dataLR_1st_NocM1)){
                hypOne <- dataLR_1st_NocM1[[k]][[1]]
                if(setequal(hypOne, c(refNames[j], "U"))){
                  dataLR_1st_NocM1_Hp <- dataLR_1st_NocM1[[k]]
                  break
                }
              }
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Sample_name"] <- sn
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Number_of_contributors"] <- noc - 1
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Hp"] <- paste(dataLR_1st_NocM1_Hp[[1]], collapse = "_")
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Hd"] <- paste(dataLR_1st_NocM1_Hd[[1]], collapse = "_")
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Likelihood_Hp"] <- likeHp <- dataLR_1st_NocM1_Hp[[2]][nL + 1]
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "Likelihood_Hd"] <- likeHd <- dataLR_1st_NocM1_Hd[[2]][nL + 1]
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "LR"] <- 10^(likeHp - likeHd)
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "MP_Hp"] <- paste(dataLR_1st_NocM1_Hp[[4]], collapse = "_")
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "MP_Hd"] <- paste(dataLR_1st_NocM1_Hd[[4]], collapse = "_")
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "d_Hp"] <- paste(dataLR_1st_NocM1_Hp[[5]], collapse = "_")
              sensitivity_1st_nocM1[count_sensitivity_1st_nocM1, "d_Hd"] <- paste(dataLR_1st_NocM1_Hd[[5]], collapse = "_")
              count_sensitivity_1st_nocM1 <- count_sensitivity_1st_nocM1 + 1
            }
            
            #### Select Hp 2nd and make sensitivity_2nd
            for(j in 1:length(refNames)){
              for(k in 1:length(dataLR_2nd_NocM1)){
                hypOne <- dataLR_2nd_NocM1[[k]][[1]]
                if(setequal(hypOne, c(refNames[j], "U"))){
                  dataLR_2nd_NocM1_Hp <- dataLR_2nd_NocM1[[k]]
                  break
                }
              }
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Sample_name"] <- sn
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Number_of_contributors"] <- noc - 1
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Hp"] <- paste(dataLR_2nd_NocM1_Hp[[1]], collapse = "_")
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Hd"] <- paste(dataLR_2nd_NocM1_Hd[[1]], collapse = "_")
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Likelihood_Hp"] <- likeHp <- dataLR_2nd_NocM1_Hp[[2]][nL + 1]
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "Likelihood_Hd"] <- likeHd <- dataLR_2nd_NocM1_Hd[[2]][nL + 1]
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "LR"] <- 10^(likeHp - likeHd)
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "MP_Hp"] <- paste(dataLR_2nd_NocM1_Hp[[4]], collapse = "_")
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "MP_Hd"] <- paste(dataLR_2nd_NocM1_Hd[[4]], collapse = "_")
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "d_Hp"] <- paste(dataLR_2nd_NocM1_Hp[[5]], collapse = "_")
              sensitivity_2nd_nocM1[count_sensitivity_2nd_nocM1, "d_Hd"] <- paste(dataLR_2nd_NocM1_Hd[[5]], collapse = "_")
              count_sensitivity_2nd_nocM1 <- count_sensitivity_2nd_nocM1 + 1
            }
          }else{
            count_sensitivity_1st_nocM1 <- count_sensitivity_1st_nocM1 + length(refNames)
            count_sensitivity_2nd_nocM1 <- count_sensitivity_2nd_nocM1 + length(refNames)
          }

          ## Specificity
          cat("Non-contributor tests were started.", "\n")
          for(j in 1:nRand){
            randOne <- randValid[, c(2 * j - 1, 2 * j)]
            randOne <- t(apply(randOne, 1, sort))
            randOne <- cbind(cspLoci, randOne)
            colnames(randOne) <- c("Marker", paste0(rep("Rand", 2), j))
            dataHdTrue_1st <- analyzeCSP(csp, randOne, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = 1, knownHd = numeric(0),
                                         anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_1, calcAllHyp = 0, addRefDropFunc = FALSE, mrDegCut = 0.0001)
            dataHdTrue_2nd <- analyzeCSP(csp, randOne, af, selectMethData, mcPar, alCor, kitInfo, hncFrom, hncTo, knownHp = 1, knownHd = numeric(0),
                                         anaType = "LR", baseDeconvo = 1, dataDeconvo = dataDeconvo_2, calcAllHyp = 0, addRefDropFunc = FALSE, mrDegCut = 0.0001)
            
            dataHdTrue_1st_Noc <- dataHdTrue_1st[[posNoc]]
            dataHdTrue_2nd_Noc <- dataHdTrue_2nd[[posNoc]]
            
            dataHdTrue_1st_Hp <- dataHdTrue_1st_Noc[[1]]
            dataHdTrue_1st_Hd <- dataHdTrue_1st_Noc[[2]]
            specificity_1st[count_specificity_1st, "Sample_name"] <- sn
            specificity_1st[count_specificity_1st, "Number_of_contributors"] <- noc
            specificity_1st[count_specificity_1st, "Hp"] <- paste(dataHdTrue_1st_Hp[[1]], collapse = "_")
            specificity_1st[count_specificity_1st, "Hd"] <- paste(dataHdTrue_1st_Hd[[1]], collapse = "_")
            specificity_1st[count_specificity_1st, "Likelihood_Hp"] <- likeHp <- dataHdTrue_1st_Hp[[2]][nL + 1]
            specificity_1st[count_specificity_1st, "Likelihood_Hd"] <- likeHd <- dataHdTrue_1st_Hd[[2]][nL + 1]
            specificity_1st[count_specificity_1st, "LR"] <- 10^(likeHp - likeHd)
            specificity_1st[count_specificity_1st, "MP_Hp"] <- paste(dataHdTrue_1st_Hp[[4]], collapse = "_")
            specificity_1st[count_specificity_1st, "MP_Hd"] <- paste(dataHdTrue_1st_Hd[[4]], collapse = "_")
            specificity_1st[count_specificity_1st, "d_Hp"] <- paste(dataHdTrue_1st_Hp[[5]], collapse = "_")
            specificity_1st[count_specificity_1st, "d_Hd"] <- paste(dataHdTrue_1st_Hd[[5]], collapse = "_")
            count_specificity_1st <- count_specificity_1st + 1
            
            dataHdTrue_2nd_Hp <- dataHdTrue_2nd_Noc[[2]]
            dataHdTrue_2nd_Hd <- dataHdTrue_2nd_Noc[[1]]
            specificity_2nd[count_specificity_2nd, "Sample_name"] <- sn
            specificity_2nd[count_specificity_2nd, "Number_of_contributors"] <- noc
            specificity_2nd[count_specificity_2nd, "Hp"] <- paste(dataHdTrue_2nd_Hp[[1]], collapse = "_")
            specificity_2nd[count_specificity_2nd, "Hd"] <- paste(dataHdTrue_2nd_Hd[[1]], collapse = "_")
            specificity_2nd[count_specificity_2nd, "Likelihood_Hp"] <- likeHp <- dataHdTrue_2nd_Hp[[2]][nL + 1]
            specificity_2nd[count_specificity_2nd, "Likelihood_Hd"] <- likeHd <- dataHdTrue_2nd_Hd[[2]][nL + 1]
            specificity_2nd[count_specificity_2nd, "LR"] <- 10^(likeHp - likeHd)
            specificity_2nd[count_specificity_2nd, "MP_Hp"] <- paste(dataHdTrue_2nd_Hp[[4]], collapse = "_")
            specificity_2nd[count_specificity_2nd, "MP_Hd"] <- paste(dataHdTrue_2nd_Hd[[4]], collapse = "_")
            specificity_2nd[count_specificity_2nd, "d_Hp"] <- paste(dataHdTrue_2nd_Hp[[5]], collapse = "_")
            specificity_2nd[count_specificity_2nd, "d_Hd"] <- paste(dataHdTrue_2nd_Hd[[5]], collapse = "_")
            count_specificity_2nd <- count_specificity_2nd + 1
            
            if(length(posNocP1) == 1){
              dataHdTrue_1st_NocP1 <- dataHdTrue_1st[[posNocP1]]
              dataHdTrue_2nd_NocP1 <- dataHdTrue_2nd[[posNocP1]]
              
              dataHdTrue_1st_NocP1_Hp <- dataHdTrue_1st_NocP1[[1]]
              dataHdTrue_1st_NocP1_Hd <- dataHdTrue_1st_NocP1[[2]]
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Sample_name"] <- sn
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Number_of_contributors"] <- noc + 1
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Hp"] <- paste(dataHdTrue_1st_NocP1_Hp[[1]], collapse = "_")
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Hd"] <- paste(dataHdTrue_1st_NocP1_Hd[[1]], collapse = "_")
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Likelihood_Hp"] <- likeHp <- dataHdTrue_1st_NocP1_Hp[[2]][nL + 1]
              specificity_1st_nocP1[count_specificity_1st_nocP1, "Likelihood_Hd"] <- likeHd <- dataHdTrue_1st_NocP1_Hd[[2]][nL + 1]
              specificity_1st_nocP1[count_specificity_1st_nocP1, "LR"] <- 10^(likeHp - likeHd)
              specificity_1st_nocP1[count_specificity_1st_nocP1, "MP_Hp"] <- paste(dataHdTrue_1st_NocP1_Hp[[4]], collapse = "_")
              specificity_1st_nocP1[count_specificity_1st_nocP1, "MP_Hd"] <- paste(dataHdTrue_1st_NocP1_Hd[[4]], collapse = "_")
              specificity_1st_nocP1[count_specificity_1st_nocP1, "d_Hp"] <- paste(dataHdTrue_1st_NocP1_Hp[[5]], collapse = "_")
              specificity_1st_nocP1[count_specificity_1st_nocP1, "d_Hd"] <- paste(dataHdTrue_1st_NocP1_Hd[[5]], collapse = "_")
              count_specificity_1st_nocP1 <- count_specificity_1st_nocP1 + 1
              
              dataHdTrue_2nd_NocP1_Hp <- dataHdTrue_2nd_NocP1[[1]]
              dataHdTrue_2nd_NocP1_Hd <- dataHdTrue_2nd_NocP1[[2]]
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Sample_name"] <- sn
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Number_of_contributors"] <- noc + 1
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Hp"] <- paste(dataHdTrue_2nd_NocP1_Hp[[1]], collapse = "_")
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Hd"] <- paste(dataHdTrue_2nd_NocP1_Hd[[1]], collapse = "_")
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Likelihood_Hp"] <- likeHp <- dataHdTrue_2nd_NocP1_Hp[[2]][nL + 1]
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "Likelihood_Hd"] <- likeHd <- dataHdTrue_2nd_NocP1_Hd[[2]][nL + 1]
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "LR"] <- 10^(likeHp - likeHd)
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "MP_Hp"] <- paste(dataHdTrue_2nd_NocP1_Hp[[4]], collapse = "_")
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "MP_Hd"] <- paste(dataHdTrue_2nd_NocP1_Hd[[4]], collapse = "_")
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "d_Hp"] <- paste(dataHdTrue_2nd_NocP1_Hp[[5]], collapse = "_")
              specificity_2nd_nocP1[count_specificity_2nd_nocP1, "d_Hd"] <- paste(dataHdTrue_2nd_NocP1_Hd[[5]], collapse = "_")
              count_specificity_2nd_nocP1 <- count_specificity_2nd_nocP1 + 1
            }else{
              count_specificity_2nd_nocP1 <- count_specificity_2nd_nocP1 + 1
            }
            
            if(length(posNocM1) == 1){
              dataHdTrue_1st_NocM1 <- dataHdTrue_1st[[posNocM1]]
              dataHdTrue_2nd_NocM1 <- dataHdTrue_2nd[[posNocM1]]
              
              dataHdTrue_1st_NocM1_Hp <- dataHdTrue_1st_NocM1[[1]]
              dataHdTrue_1st_NocM1_Hd <- dataHdTrue_1st_NocM1[[2]]
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Sample_name"] <- sn
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Number_of_contributors"] <- noc - 1
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Hp"] <- paste(dataHdTrue_1st_NocM1_Hp[[1]], collapse = "_")
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Hd"] <- paste(dataHdTrue_1st_NocM1_Hd[[1]], collapse = "_")
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Likelihood_Hp"] <- likeHp <- dataHdTrue_1st_NocM1_Hp[[2]][nL + 1]
              specificity_1st_nocM1[count_specificity_1st_nocM1, "Likelihood_Hd"] <- likeHd <- dataHdTrue_1st_NocM1_Hd[[2]][nL + 1]
              specificity_1st_nocM1[count_specificity_1st_nocM1, "LR"] <- 10^(likeHp - likeHd)
              specificity_1st_nocM1[count_specificity_1st_nocM1, "MP_Hp"] <- paste(dataHdTrue_1st_NocM1_Hp[[4]], collapse = "_")
              specificity_1st_nocM1[count_specificity_1st_nocM1, "MP_Hd"] <- paste(dataHdTrue_1st_NocM1_Hd[[4]], collapse = "_")
              specificity_1st_nocM1[count_specificity_1st_nocM1, "d_Hp"] <- paste(dataHdTrue_1st_NocM1_Hp[[5]], collapse = "_")
              specificity_1st_nocM1[count_specificity_1st_nocM1, "d_Hd"] <- paste(dataHdTrue_1st_NocM1_Hd[[5]], collapse = "_")
              count_specificity_1st_nocM1 <- count_specificity_1st_nocM1 + 1
              
              dataHdTrue_2nd_NocM1_Hp <- dataHdTrue_2nd_NocM1[[1]]
              dataHdTrue_2nd_NocM1_Hd <- dataHdTrue_2nd_NocM1[[2]]
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Sample_name"] <- sn
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Number_of_contributors"] <- noc - 1
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Hp"] <- paste(dataHdTrue_2nd_NocM1_Hp[[1]], collapse = "_")
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Hd"] <- paste(dataHdTrue_2nd_NocM1_Hd[[1]], collapse = "_")
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Likelihood_Hp"] <- likeHp <- dataHdTrue_2nd_NocM1_Hp[[2]][nL + 1]
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "Likelihood_Hd"] <- likeHd <- dataHdTrue_2nd_NocM1_Hd[[2]][nL + 1]
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "LR"] <- 10^(likeHp - likeHd)
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "MP_Hp"] <- paste(dataHdTrue_2nd_NocM1_Hp[[4]], collapse = "_")
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "MP_Hd"] <- paste(dataHdTrue_2nd_NocM1_Hd[[4]], collapse = "_")
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "d_Hp"] <- paste(dataHdTrue_2nd_NocM1_Hp[[5]], collapse = "_")
              specificity_2nd_nocM1[count_specificity_2nd_nocM1, "d_Hd"] <- paste(dataHdTrue_2nd_NocM1_Hd[[5]], collapse = "_")
              count_specificity_2nd_nocM1 <- count_specificity_2nd_nocM1 + 1
            }else{
              count_specificity_2nd_nocM1 <- count_specificity_2nd_nocM1 + 1
            }
          }
          save(dataDeconvo_1st, dataLR_1st, dataDeconvo_2nd, dataLR_2nd, file = paste0("D:/kongoh_valid/valiData_", sn, ".RData"))
        }
        write.csv(sensitivity_1st, "D:/kongoh_valid/sensitivity_1st.csv", row.names = FALSE)
        write.csv(sensitivity_2nd, "D:/kongoh_valid/sensitivity_2nd.csv", row.names = FALSE)
        write.csv(sensitivity_1st_nocP1, "D:/kongoh_valid/sensitivity_1st_nocP1.csv", row.names = FALSE)
        write.csv(sensitivity_2nd_nocP1, "D:/kongoh_valid/sensitivity_2nd_nocP1.csv", row.names = FALSE)
        write.csv(sensitivity_1st_nocM1, "D:/kongoh_valid/sensitivity_1st_nocM1.csv", row.names = FALSE)
        write.csv(sensitivity_2nd_nocM1, "D:/kongoh_valid/sensitivity_2nd_nocM1.csv", row.names = FALSE)
        write.csv(specificity_1st, "D:/kongoh_valid/specificity_1st.csv", row.names = FALSE)
        write.csv(specificity_2nd, "D:/kongoh_valid/specificity_2nd.csv", row.names = FALSE)
        write.csv(specificity_1st_nocP1, "D:/kongoh_valid/specificity_1st_nocP1.csv", row.names = FALSE)
        write.csv(specificity_2nd_nocP1, "D:/kongoh_valid/specificity_2nd_nocP1.csv", row.names = FALSE)
        write.csv(specificity_1st_nocM1, "D:/kongoh_valid/specificity_1st_nocM1.csv", row.names = FALSE)
        write.csv(specificity_2nd_nocM1, "D:/kongoh_valid/specificity_2nd_nocM1.csv", row.names = FALSE)
        assign("sensitivity_1st", sensitivity_1st, envir = envValidData)
        assign("sensitivity_2nd", sensitivity_2nd, envir = envValidData)
        assign("sensitivity_1st_nocP1", sensitivity_1st_nocP1, envir = envValidData)
        assign("sensitivity_2nd_nocP1", sensitivity_2nd_nocP1, envir = envValidData)
        assign("sensitivity_1st_nocM1", sensitivity_1st_nocM1, envir = envValidData)
        assign("sensitivity_2nd_nocM1", sensitivity_2nd_nocM1, envir = envValidData)
        assign("specificity_1st", specificity_1st, envir = envValidData)
        assign("specificity_2nd", specificity_2nd, envir = envValidData)
        assign("specificity_1st_nocP1", specificity_1st_nocP1, envir = envValidData)
        assign("specificity_2nd_nocP1", specificity_2nd_nocP1, envir = envValidData)
        assign("specificity_1st_nocM1", specificity_1st_nocM1, envir = envValidData)
        assign("specificity_2nd_nocM1", specificity_2nd_nocM1, envir = envValidData)
#        makeTabVR(envValidData, envValidGUI)
      }
    }else{
      tkmessageBox(message = "Locus set of the selected analysis method was not the same as that of input files!", icon = "error", type = "ok")
    }
  }
}
