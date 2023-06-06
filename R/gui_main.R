# Make a spinbox
tkspinbox <- function(parent, ...){
  tkwidget(parent, "tk::spinbox", ...)
}

# View the inputted CSP profile
viewCsp <- function(envProj){
  cspInput <- get("cspInput", pos = envProj)
  if(length(cspInput) == 0){
    tkmessageBox(message = "Load a crime stain profile!", icon = "error", type = "ok")
  }else{
    cspDisplay <- cspInput
    colCsp <- colnames(cspDisplay)
    nCol <- length(colCsp)
    cspAlDisplay <- cspDisplay[, grep("Allele", colCsp)]
    pointZero <- cspAlDisplay[grep(".0", cspAlDisplay)]
    pointZero <- round(as.numeric(pointZero), 0)
    cspAlDisplay[grep(".0", cspAlDisplay)] <- pointZero
    cspDisplay[, grep("Allele", colCsp)] <- cspAlDisplay
    cspDisplay[is.na(cspDisplay)] <- ""

    tfCsp <- tktoplevel()
    tkwm.title(tfCsp, "Crime stain profile")
    scrX <- tkscrollbar(tfCsp, orient = "horizontal", repeatinterval = 5, command = function(...) tkxview(mlbCsp, ...))
    scrY <- tkscrollbar(tfCsp, repeatinterval = 5, command = function(...) tkyview(mlbCsp, ...))
    mlbCsp <- tk2mclistbox(tfCsp, width = 100, height = 25, resizablecolumns = TRUE, selectmode = "single", xscrollcommand = function(...) tkset(scrX, ...), yscrollcommand = function(...) tkset(scrY, ...))
    tk2column(mlbCsp, "add", label = colCsp[1], width = 13)
    for(i in 2:nCol){
      tk2column(mlbCsp, "add", label = colCsp[i], width = 10)
    }
    tkgrid(mlbCsp, scrY)
    tkgrid(scrX)
    tk2insert.multi(mlbCsp, "end", cspDisplay)
    tkgrid.configure(scrX, columnspan = 100, sticky = "ews")
    tkgrid.configure(scrY, rowspan = 25, sticky = "nsw")
  }
}

# View the inputted reference profile
viewRef <- function(envProj){
  refInput <- get("refInput", pos = envProj)
  if(length(refInput) == 0){
    tkmessageBox(message = "Load reference profiles!", icon = "error", type = "ok")
  }else{
    refDisplay <- refInput
    colRef <- colnames(refDisplay)
    nCol <- length(colRef)
    for(i in 1:(nCol %/% 2)){
      colRef[2 * i + 1] <- colRef[2 * i]
    }
    refDisplay[is.na(refDisplay)] <- ""

    tfRef <- tktoplevel()
    tkwm.title(tfRef, "Reference profiles")
    scrX <- tkscrollbar(tfRef, orient = "horizontal", repeatinterval = 5, command = function(...) tkxview(mlbRef, ...))
    scrY <- tkscrollbar(tfRef, repeatinterval = 5, command = function(...) tkyview(mlbRef, ...))
    mlbRef <- tk2mclistbox(tfRef, width = 100, height = 25, resizablecolumns = TRUE, selectmode = "single", xscrollcommand = function(...) tkset(scrX, ...), yscrollcommand = function(...) tkset(scrY, ...))
    for(i in 1:nCol){
      tk2column(mlbRef, "add", label = colRef[i], width = 10)
    }
    tkgrid(mlbRef, scrY)
    tkgrid(scrX)
    tk2insert.multi(mlbRef, "end", refDisplay)
    tkgrid.configure(scrX, columnspan = 100, sticky = "ews")
    tkgrid.configure(scrY, rowspan = 25, sticky = "nsw")
  }
}

# View the inputted allele frequencies
viewAf <- function(envProj){
  afInput <- get("afInput", pos = envProj)
  if(length(afInput) == 0){
    tkmessageBox(message = "Load allele frequencies!", icon = "error", type = "ok")
  }else{
    viewInput <- function(){
      mlbAf <- get("mlbAf", pos = envViewAf)
      scrX <- get("scrX", pos = envViewAf)
      scrY <- get("scrY", pos = envViewAf)
      tkdestroy(mlbAf)
      tkdestroy(scrX)
      tkdestroy(scrY)
      scrX <- tkscrollbar(frameAf, orient = "horizontal", repeatinterval = 5, command = function(...) tkxview(mlbAf, ...))
      scrY <- tkscrollbar(frameAf, repeatinterval = 5, command = function(...) tkyview(mlbAf, ...))
      mlbAf <- tk2mclistbox(frameAf, width = 100, height = 25, resizablecolumns = TRUE, selectmode = "single", xscrollcommand = function(...) tkset(scrX, ...), yscrollcommand = function(...) tkset(scrY, ...))
      for(i in 1:nCol){
        tk2column(mlbAf, "add", label = colAf[i], width = 10)
      }
      tkgrid(mlbAf, scrY)
      tkgrid(scrX)
      tk2insert.multi(mlbAf, "end", afDisplay)
      tkgrid.configure(scrX, columnspan = 100, sticky = "ews")
      tkgrid.configure(scrY, rowspan = 25, sticky = "nsw")
      assign("mlbAf", mlbAf, envir = envViewAf)
      assign("scrX", scrX, envir = envViewAf)
      assign("scrY", scrY, envir = envViewAf)
    }

    viewDir <- function(){
      mlbAf <- get("mlbAf", pos = envViewAf)
      scrX <- get("scrX", pos = envViewAf)
      scrY <- get("scrY", pos = envViewAf)
      tkdestroy(mlbAf)
      tkdestroy(scrX)
      tkdestroy(scrY)
      scrX <- tkscrollbar(frameAf, orient = "horizontal", repeatinterval = 5, command = function(...) tkxview(mlbAf, ...))
      scrY <- tkscrollbar(frameAf, repeatinterval = 5, command = function(...) tkyview(mlbAf, ...))
      mlbAf <- tk2mclistbox(frameAf, width = 100, height = 25, resizablecolumns = TRUE, selectmode = "single", xscrollcommand = function(...) tkset(scrX, ...), yscrollcommand = function(...) tkset(scrY, ...))
      for(i in 1:nCol){
        tk2column(mlbAf, "add", label = colAf[i], width = 10)
      }
      tkgrid(mlbAf, scrY)
      tkgrid(scrX)
      afDir <- afDisplay
      for(i in 2:nCol){
        afOneL <- afDir[, i]
        posAf <- which(afOneL != "")
        afOneL <- as.numeric(afOneL[posAf])
        afAlOneL <- afDir[posAf, 1]
        afOneL <- makePopFreq(afOneL, afAlOneL, "Dirichlet")
        afDir[posAf, i] <- signif(afOneL, 3)
      }
      tk2insert.multi(mlbAf, "end", afDir)
      tkgrid.configure(scrX, columnspan = 100, sticky = "ews")
      tkgrid.configure(scrY, rowspan = 25, sticky = "nsw")
      assign("mlbAf", mlbAf, envir = envViewAf)
      assign("scrX", scrX, envir = envViewAf)
      assign("scrY", scrY, envir = envViewAf)
    }

    afDisplay <- afInput
    colAf <- colnames(afDisplay)
    nCol <- length(colAf)
    afDisplay <- signif(afDisplay, 3)
    afDisplay[is.na(afDisplay)] <- ""
    afVal <- as.numeric(afInput[, -1])
    afVal <- afVal[!is.na(afVal)]
    judgeType <- length(which(afVal %% 1 != 0)) == 0
    if(judgeType){
      afType <- "allele count"
    }else{
      afType <- "allele probability"
    }

    tfAf <- tktoplevel()
    tkwm.title(tfAf, "Allele frequencies")
    frameType <- tkframe(tfAf)
    tkgrid(tklabel(frameType, text = paste0("Format of the input file: ", afType), font = "Helvetica 10 bold"), padx = 10, pady = 10, sticky = "w")
    tkpack(frameType, expand = TRUE, fill = "both")

    frameAf <- tkframe(tfAf)
    scrX <- tkscrollbar(frameAf, orient = "horizontal", repeatinterval = 5, command = function(...) tkxview(mlbAf, ...))
    scrY <- tkscrollbar(frameAf, repeatinterval = 5, command = function(...) tkyview(mlbAf, ...))
    envViewAf <- new.env(parent = globalenv())
    mlbAf <- tk2mclistbox(frameAf, width = 100, height = 25, resizablecolumns = TRUE, selectmode = "single", xscrollcommand = function(...) tkset(scrX, ...), yscrollcommand = function(...) tkset(scrY, ...))
    assign("mlbAf", mlbAf, envir = envViewAf)
    assign("scrX", scrX, envir = envViewAf)
    assign("scrY", scrY, envir = envViewAf)
    viewInput()
    tkpack(frameAf, expand = TRUE, fill = "both")

    frameButts <- tkframe(tfAf)
    if(judgeType){
      tkgrid(tkbutton(frameButts, text = "    Input data    ", cursor = "hand2", command = function() viewInput()),
             tkbutton(frameButts, text = "    Dirichlet    ", cursor = "hand2", command = function() viewDir()),
             padx = 10, pady = 10)
    }
    tkpack(frameButts)
  }
}

# Make tabs of Deconvolution and Likelihood ratio
makeTabDL <- function(envProj, envGUI){
  finDeconvo <- get("finDeconvo", pos = envProj)
  if(finDeconvo){
    makeTabDeconvo(envProj, envGUI)
  }else{
    tabDeconvo <- get("tabDeconvo", pos = envGUI)
    frameDeconvo <- get("frameDeconvo", pos = envGUI)
    tkdestroy(frameDeconvo)
    frameDeconvo <- tkframe(tabDeconvo)
    subTabs_Deconvo <- tk2notebook(frameDeconvo, tabs = c("Proposition", "Result"))
    subTabProp_Deconvo <- tk2notetab(subTabs_Deconvo, "Proposition")
    subTabResult_Deconvo <- tk2notetab(subTabs_Deconvo, "Result")
    assign("frameDeconvo", frameDeconvo, envir = envGUI)
    assign("subTabs_Deconvo", subTabs_Deconvo, envir = envGUI)
    assign("subTabProp_Deconvo", subTabProp_Deconvo, envir = envGUI)
    assign("frameProp_Deconvo", tkframe(subTabProp_Deconvo), envir = envGUI)
    assign("subTabResult_Deconvo", subTabResult_Deconvo, envir = envGUI)
    assign("frameResult_Deconvo", tkframe(subTabResult_Deconvo), envir = envGUI)
  }
  finLR <- get("finLR", pos = envProj)
  if(finLR){
    makeTabLR(envProj, envGUI)
  }else{
    tabLR <- get("tabLR", pos = envGUI)
    frameLR <- get("frameLR", pos = envGUI)
    tkdestroy(frameLR)
    frameLR <- tkframe(tabLR)
    subTabs_LR <- tk2notebook(frameLR, tabs = c("Proposition", "Result"))
    subTabProp_LR <- tk2notetab(subTabs_LR, "Proposition")
    subTabResult_LR <- tk2notetab(subTabs_LR, "Result")
    assign("frameLR", frameLR, envir = envGUI)
    assign("subTabs_LR", subTabs_LR, envir = envGUI)
    assign("subTabProp_LR", subTabProp_LR, envir = envGUI)
    assign("frameProp_LR", tkframe(subTabProp_LR), envir = envGUI)
    assign("subTabResult_LR", subTabResult_LR, envir = envGUI)
    assign("frameResult_LR", tkframe(subTabResult_LR), envir = envGUI)
  }
}

# Open a new project
newProj <- function(envProj, envGUI){
  inputOk <- "ok"
  finDeconvo <- get("finDeconvo", pos = envProj)
  finLR <- get("finLR", pos = envProj)
  if(finDeconvo || finLR){
    inputOk <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(inputOk == "ok"){
    setEnvProj(envProj, TRUE)
    makeTabFiles(envProj, envGUI)
    makeTabDL(envProj, envGUI)
    tabs <- get("tabs", pos = envGUI)
    tk2notetab.select(tabs, "Files")
  }
}

# Load a new project
loadProj <- function(tf, envProj_old, envGUI){
  inputOk <- "ok"
  finDeconvo <- get("finDeconvo", pos = envProj_old)
  finLR <- get("finLR", pos = envProj_old)
  if(finDeconvo || finLR){
    inputOk <- tclvalue(tkmessageBox(message = "Unsaved data will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
  }
  if(inputOk == "ok"){
    fpVar <- tclVar("")
    fileName <- tclvalue(tkgetOpenFile(parent = tf, initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{R Data Files} {.Rdata}}"))
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
      for(i in ls(envProj, all.names = TRUE)){
        assign(i, get(i, envProj), envProj_old)
      }
      makeTabFiles(envProj_old, envGUI)
      makeTabDL(envProj_old, envGUI)
    }
  }
}

# Save a project
saveProj <- function(envProj){
  saveAs <- tkgetSaveFile(filetypes = "{{R Data Files} {.RData}}")
  if(tclvalue(saveAs) != ""){
    if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 5, nchar(tclvalue(saveAs))) == ".RData"){
      reportName <- tclvalue(saveAs)
    }else{
      reportName <- paste0(tclvalue(saveAs), ".RData")
    }
    save(envProj, file = reportName)
  }
}

# Input required files
openFile <- function(envProj, envGUI, type, labelFn, viewButt, finConsider, labelFn_new = NULL){
  inputOk <- "ok"
  finDeconvo <- get("finDeconvo", pos = envProj)
  finLR <- get("finLR", pos = envProj)
  if(finConsider){
    if(finDeconvo || finLR){
      inputOk <- tclvalue(tkmessageBox(message = "Calculation results will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      setEnvProj(envProj, FALSE)
      makeTabDL(envProj, envGUI)
    }
  }
  if(inputOk == "ok"){
    if(type == "csp"){
      fp <- get("cspFp", pos = envProj)
      fn <- get("cspFn", pos = envProj)
      buttCursor <- get("viewCspCursor", pos = envProj)
      buttState <- get("viewCspState", pos = envProj)
    }else if(type == "ref"){
      fp <- get("refFp", pos = envProj)
      fn <- get("refFn", pos = envProj)
      buttCursor <- get("viewRefCursor", pos = envProj)
      buttState <- get("viewRefState", pos = envProj)
    }else if(type == "af"){
      fp <- get("afFp", pos = envProj)
      fn <- get("afFn", pos = envProj)
      buttCursor <- get("viewAfCursor", pos = envProj)
      buttState <- get("viewAfState", pos = envProj)
    }
    fpVar <- tclVar(fp)
    fnVar <- tclVar(fn)
    buttCursorVar <- tclVar(buttCursor)
    buttStateVar <- tclVar(buttState)

    fileName <- tclvalue(tkgetOpenFile(initialdir = tclvalue(fpVar), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
    if(!nchar(fileName)){
      tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
    }else{
      tmp <- sub("\\}", fileName, replacement = "")
      tmp2 <- sub("\\{", tmp, replacement = "")
      tclvalue(fpVar) <- tmp2
      foo3 <- strsplit(tmp2, "/")[[1]]
      tclvalue(fnVar) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
      tclvalue(buttCursorVar) <- "hand2"
      tclvalue(buttStateVar) <- "normal"
      if(type == "csp"){
        cspInput <- read.csv(tclvalue(fpVar), header = TRUE)
        cspInput <- as.matrix(cspInput)
        cspInput <- gsub(" ", "", cspInput, fixed = TRUE)
        assign("cspInput", cspInput, envir = envProj)
        assign("cspFp", tclvalue(fpVar), envir = envProj)
        assign("cspFn", tclvalue(fnVar), envir = envProj)
        assign("viewCspCursor", tclvalue(buttCursorVar), envir = envProj)
        assign("viewCspState", tclvalue(buttStateVar), envir = envProj)
      }else if(type == "ref"){
        refInput <- read.csv(tclvalue(fpVar), header = TRUE)
        refInput <- as.matrix(refInput)
        refInput <- gsub(" ", "", refInput, fixed = TRUE)
        assign("refInput", refInput, envir = envProj)
        assign("refFp", tclvalue(fpVar), envir = envProj)
        assign("refFn", tclvalue(fnVar), envir = envProj)
        assign("viewRefCursor", tclvalue(buttCursorVar), envir = envProj)
        assign("viewRefState", tclvalue(buttStateVar), envir = envProj)
      }else if(type == "af"){
        afInput <- read.csv(tclvalue(fpVar), header = TRUE)
        afInput <- as.matrix(afInput)
        colnames(afInput) <- gsub(".", "", colnames(afInput), fixed = TRUE)
        assign("afInput", afInput, envir = envProj)
        assign("afFp", tclvalue(fpVar), envir = envProj)
        assign("afFn", tclvalue(fnVar), envir = envProj)
        assign("viewAfCursor", tclvalue(buttCursorVar), envir = envProj)
        assign("viewAfState", tclvalue(buttStateVar), envir = envProj)
      }
      tkconfigure(labelFn, textvariable = fnVar)
      tkconfigure(viewButt, cursor = tclvalue(buttCursorVar), state = tclvalue(buttStateVar))
      if(length(labelFn_new) > 0){
        tkconfigure(labelFn_new, textvariable = fnVar)
      }
    }
  }
}

# make a Files tab
makeTabFiles <- function(envProj, envGUI){
  cspFn <- get("cspFn", pos = envProj)
  cspFnVar <- tclVar(cspFn)
  viewCspCursor <- get("viewCspCursor", pos = envProj)
  viewCspCursorVar <- tclVar(viewCspCursor)
  viewCspState <- get("viewCspState", pos = envProj)
  viewCspStateVar <- tclVar(viewCspState)
  refFn <- get("refFn", pos = envProj)
  refFnVar <- tclVar(refFn)
  viewRefCursor <- get("viewRefCursor", pos = envProj)
  viewRefCursorVar <- tclVar(viewRefCursor)
  viewRefState <- get("viewRefState", pos = envProj)
  viewRefStateVar <- tclVar(viewRefState)
  afFn <- get("afFn", pos = envProj)
  afFnVar <- tclVar(afFn)
  viewAfCursor <- get("viewAfCursor", pos = envProj)
  viewAfCursorVar <- tclVar(viewAfCursor)
  viewAfState <- get("viewAfState", pos = envProj)
  viewAfStateVar <- tclVar(viewAfState)
  tabFiles <- get("tabFiles", pos = envGUI)
  frameFiles <- get("frameFiles", pos = envGUI)

  tkdestroy(frameFiles)
  frameFiles <- tkframe(tabFiles)

  frameFiles_1 <- tkframe(frameFiles)
  tkgrid(tklabel(frameFiles_1, text = "Input File", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")

  labelCsp <- tklabel(frameFiles_1, text = "Crime Stain Profile")
  labelCspName <- tklabel(frameFiles_1, textvariable = cspFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadCsp <- tkbutton(frameFiles_1, text = "    Load    ", cursor = "hand2", command = function() openFile(envProj, envGUI, "csp", labelCspName, buttViewCsp, TRUE))
  buttViewCsp <- tkbutton(frameFiles_1, text = "    View    ", cursor = tclvalue(viewCspCursorVar), state = tclvalue(viewCspStateVar), command = function() viewCsp(envProj))
  tkgrid(labelCsp, labelCspName, buttLoadCsp, buttViewCsp, padx = 10, pady = 5, sticky = "w")

  labelRef <- tklabel(frameFiles_1, text = "Reference Profile")
  labelRefName <-tklabel(frameFiles_1, textvariable = refFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadRef <- tkbutton(frameFiles_1, text = "    Load    ", cursor = "hand2", command = function() openFile(envProj, envGUI, "ref", labelRefName, buttViewRef, TRUE))
  buttViewRef <- tkbutton(frameFiles_1, text = "    View    ", cursor = tclvalue(viewRefCursorVar), state = tclvalue(viewRefStateVar), command = function() viewRef(envProj))
  tkgrid(labelRef, labelRefName, buttLoadRef, buttViewRef, padx = 10, pady = 5, sticky = "w")

  labelAf <- tklabel(frameFiles_1, text = "Allele Frequencies")
  labelAfName <- tklabel(frameFiles_1, textvariable = afFnVar, width = 35, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
  buttLoadAf <- tkbutton(frameFiles_1, text = "    Load    ", cursor = "hand2", command = function() openFile(envProj, envGUI, "af", labelAfName, buttViewAf, TRUE))
  buttViewAf <- tkbutton(frameFiles_1, text = "    View    ", cursor = tclvalue(viewAfCursorVar), state = tclvalue(viewAfStateVar), command = function() viewAf(envProj))
  tkgrid(labelAf, labelAfName, buttLoadAf, buttViewAf, padx = 10, pady = 5, sticky = "w")

  tkgrid(frameFiles_1, padx = 10, pady = 5, sticky = "w")

  frameFiles_2 <- tkframe(frameFiles)
  tkgrid(tklabel(frameFiles_2, text = "Analysis", font = "Helvetica 10 bold"), padx = 10, pady = 5, sticky = "w")
  buttDeconvo <- tkbutton(frameFiles_2, text = "Deconvolution", cursor = "hand2", command = function() makeTabDeconvo(envProj, envGUI))
  buttLr <- tkbutton(frameFiles_2, text = "Likelihood ratio", cursor = "hand2", command = function() makeTabLR(envProj, envGUI))
  tkgrid(buttDeconvo, buttLr, padx = 10, pady = 5, sticky = "w")
  tkgrid(frameFiles_2, padx = 10, pady = 5, sticky = "w")

  tkgrid(frameFiles)
  assign("frameFiles", frameFiles, envir = envGUI)
  assign("labelRefName", labelRefName, envir = envGUI)
  assign("buttViewRef", buttViewRef, envir = envGUI)
}

# Set environmental objects for envProj
setEnvProj <- function(envProj, setfiles){
  if(setfiles){
    assign("cspInput", NULL, envir = envProj)
    assign("cspFp", character(0), envir = envProj)
    assign("cspFn", character(0), envir = envProj)
    assign("viewCspCursor", "arrow", envir = envProj)
    assign("viewCspState", "disabled", envir = envProj)
    assign("refInput", NULL, envir = envProj)
    assign("refFp", character(0), envir = envProj)
    assign("refFn", character(0), envir = envProj)
    assign("viewRefCursor", "arrow", envir = envProj)
    assign("viewRefState", "disabled", envir = envProj)
    assign("afInput", NULL, envir = envProj)
    assign("afFp", character(0), envir = envProj)
    assign("afFn", character(0), envir = envProj)
    assign("viewAfCursor", "arrow", envir = envProj)
    assign("viewAfState", "disabled", envir = envProj)
  }

  assign("csp", NULL, envir = envProj)
  assign("ref", NULL, envir = envProj)
  assign("af", NULL, envir = envProj)

  assign("hncFrom_deconvo", 1, envir = envProj)
  assign("hncTo_deconvo", 4, envir = envProj)
  assign("hncMin_deconvo", 1, envir = envProj)
  assign("hncMax_deconvo", 4, envir = envProj)
  assign("anaMeth_deconvo", "", envir = envProj)
  assign("dataDeconvo", NULL, envir = envProj)

  assign("hypState", "normal", envir = envProj)
  assign("hncState_lr", "normal", envir = envProj)
  assign("hncFrom_lr", 1, envir = envProj)
  assign("hncTo_lr", 4, envir = envProj)
  assign("hncMin_lr", 1, envir = envProj)
  assign("hncMax_lr", 4, envir = envProj)
  assign("anaMeth_lr", "", envir = envProj)
  assign("baseDeconvo", 0, envir = envProj)
  assign("baseDeconvoState", "disabled", envir = envProj)
  assign("calcAllHyp", 0, envir = envProj)
  assign("calcAllHypState", "normal", envir = envProj)
  assign("nameKnownHp", character(0), envir = envProj)
  assign("nameKnownHd", character(0), envir = envProj)
  assign("dataLR", NULL, envir = envProj)
  assign("cspLoci", character(0), envir = envProj)

  assign("finDeconvo", FALSE, envir = envProj)
  assign("finLR", FALSE, envir = envProj)
}

#' Kongoh
#'
#' @description Main window of Kongoh
#' @usage Kongoh()
#' @export
Kongoh <- function(){
  #Tktable
  addTclPath('/usr/local/lib/Tktable2.9')
  tclRequire("Tktable")

  if(is.element("envProj", ls(envir = parent.env(environment())))){
    envProj <- get("envProj", pos = parent.env(environment()))
  }else{
    envProj <- new.env(parent = globalenv())
    setEnvProj(envProj, TRUE)
  }

  envGUI <- new.env(parent = globalenv())
  softVer <- packageVersion("Kongoh")
  pathPack <- path.package("Kongoh", quiet = FALSE)
#  pathPack <- "D:/RStudio_GitHub/Kongoh/inst"
  assign("softVer", softVer, envir = envGUI)
  assign("pathPack", pathPack, envir = envGUI)

  tf <- tktoplevel()
  tkwm.title(tf, paste0("Kongoh ver. ", softVer))

  topMenu <- tkmenu(tf)
  tkconfigure(tf, menu = topMenu)

  file_menu <- tkmenu(topMenu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(topMenu, "cascade", label = "File", menu = file_menu)
  tkadd(file_menu, "command", label = "New project", command = function() newProj(envProj, envGUI))
  tkadd(file_menu, "command", label = "Load project", command = function() loadProj(tf, envProj, envGUI))
  tkadd(file_menu, "command", label = "Save project", command = function() saveProj(envProj))
  tkadd(file_menu, "separator")
  tkadd(file_menu, "command", label = "Quit", command = function() tkdestroy(tf))

  tools_menu <- tkmenu(topMenu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(topMenu, "cascade", label = "Tools", menu = tools_menu)
  tkadd(tools_menu, "command", label = "Set analysis methods", command = function() windowAnaMeth(envGUI))
  tkadd(tools_menu, "command", label = "Estimate parameters", command = function() windowParEst(envGUI))
  tkadd(tools_menu, "command", label = "Set information on typing kits", command = function() windowKit(envGUI))
  tkadd(tools_menu, "command", label = "Edit repeat correction", command = function() windowRepCor(envGUI))
  tkadd(tools_menu, "command", label = "Validation mode", command = function() windowValid(envGUI))

  help_menu <- tkmenu(topMenu, tearoff = FALSE, activebackground = "lightskyblue1")
  tkadd(topMenu, "cascade", label = "Help", menu = help_menu)
  tkadd(help_menu, "command", label = "User manual", command = function() browseURL(paste0(pathPack, "/extdata/manual/Kongoh_", softVer, "_manual.pdf")))

  tabs <- tk2notebook(tf, tabs = c("Files", "Deconvolution", "Likelihood ratio"))
  tkpack(tabs, fill = "both", expand = 1)
  tabFiles <- tk2notetab(tabs, "Files")
  frameFiles <- tkframe(tabFiles)
  tabDeconvo <- tk2notetab(tabs, "Deconvolution")
  tabLR <- tk2notetab(tabs, "Likelihood ratio")
  assign("tabs", tabs, envir = envGUI)
  assign("tabFiles", tabFiles, envir = envGUI)
  assign("frameFiles", frameFiles, envir = envGUI)
  assign("tabDeconvo", tabDeconvo, envir = envGUI)
  assign("tabLR", tabLR, envir = envGUI)

  frameDeconvo <- tkframe(tabDeconvo)
  subTabs_Deconvo <- tk2notebook(frameDeconvo, tabs = c("Proposition", "Result"))
  subTabProp_Deconvo <- tk2notetab(subTabs_Deconvo, "Proposition")
  subTabResult_Deconvo <- tk2notetab(subTabs_Deconvo, "Result")
  assign("frameDeconvo", frameDeconvo, envir = envGUI)
  assign("subTabs_Deconvo", subTabs_Deconvo, envir = envGUI)
  assign("subTabProp_Deconvo", subTabProp_Deconvo, envir = envGUI)
  assign("frameProp_Deconvo", tkframe(subTabProp_Deconvo), envir = envGUI)
  assign("subTabResult_Deconvo", subTabResult_Deconvo, envir = envGUI)
  assign("frameResult_Deconvo", tkframe(subTabResult_Deconvo), envir = envGUI)

  frameLR <- tkframe(tabLR)
  subTabs_LR <- tk2notebook(frameLR, tabs = c("Proposition", "Result"))
  subTabProp_LR <- tk2notetab(subTabs_LR, "Proposition")
  subTabResult_LR <- tk2notetab(subTabs_LR, "Result")
  assign("frameLR", frameLR, envir = envGUI)
  assign("subTabs_LR", subTabs_LR, envir = envGUI)
  assign("subTabProp_LR", subTabProp_LR, envir = envGUI)
  assign("frameProp_LR", tkframe(subTabProp_LR), envir = envGUI)
  assign("subTabResult_LR", subTabResult_LR, envir = envGUI)
  assign("frameResult_LR", tkframe(subTabResult_LR), envir = envGUI)

  makeTabFiles(envProj, envGUI)
}
