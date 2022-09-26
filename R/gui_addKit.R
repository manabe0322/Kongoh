# Set sizes
setSize <- function(envKit){
  addAllele1 <- function(){
    tfAdd <- tktoplevel()
    tkwm.title(tfAdd, "Add an allele")
    frameAdd_1 <- tkframe(tfAdd)
    frameAdd_2 <- tkframe(tfAdd)
    alVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Allele"),
           tkentry(frameAdd_1, textvariable = alVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")
    sizeVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Size"),
           tkentry(frameAdd_1, textvariable = sizeVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")
    tkgrid(tkbutton(frameAdd_2, text = "    Add    ", cursor = "hand2",
                    command = function() addAllele2(tfAdd, tclvalue(alVar), tclvalue(sizeVar))),
           pady = 5)
    tkgrid(frameAdd_1)
    tkgrid(frameAdd_2)
  }

  addAllele2 <- function(tfAdd, al, size){
    mlbSize <- get("mlbSize", pos = envKit)
    displaySize <- get("displaySize", pos = envKit)
    addData <- c(al, size)
    tk2insert.multi(mlbSize, "end", addData)
    assign("mlbSize", mlbSize, envir = envKit)
    assign("displaySize", rbind(displaySize, addData), envir = envKit)
    tkdestroy(tfAdd)
  }

  editAllele1 <- function(){
    mlbSize <- get("mlbSize", pos = envKit)
    displaySize <- get("displaySize", pos = envKit)
    posEdit <- tclvalue(tkcurselection(mlbSize))
    if(posEdit == ""){
      tkmessageBox(message = "Select one allele!", icon = "error", type = "ok")
    }else{
      posEdit <- as.numeric(posEdit) + 1
      tfEdit <- tktoplevel()
      tkwm.title(tfEdit, "Edit an allele")
      frameEdit_1 <- tkframe(tfEdit)
      frameEdit_2 <- tkframe(tfEdit)
      alVar <- tclVar("")
      tkgrid(tklabel(frameEdit_1, text = "Allele"),
             tkentry(frameEdit_1, textvariable = alVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")
      sizeVar <- tclVar("")
      tkgrid(tklabel(frameEdit_1, text = "Size"),
             tkentry(frameEdit_1, textvariable = sizeVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")
      tkgrid(tkbutton(frameEdit_2, text = "    Edit    ", cursor = "hand2",
                      command = function() editAllele2(tfEdit, posEdit, tclvalue(alVar), tclvalue(sizeVar))),
             pady = 5)
      tkgrid(frameEdit_1)
      tkgrid(frameEdit_2)
    }
  }

  editAllele2 <- function(tfEdit, posEdit, al, size){
    mlbSize <- get("mlbSize", pos = envKit)
    displaySize <- get("displaySize", pos = envKit)
    displaySize[posEdit, ] <- c(al, size)
    tkdestroy(mlbSize)
    mlbSize <- tk2mclistbox(frameSize_1, width = 30, height = 27, resizablecolumns = TRUE, selectmode = "single")
    tk2column(mlbSize, "add", label = "Allele", width = 15)
    tk2column(mlbSize, "add", label = "Size", width = 15)
    tkgrid(mlbSize, padx = 5, pady = 5, sticky = "w")
    tk2insert.multi(mlbSize, "end", displaySize)
    assign("mlbSize", mlbSize, envir = envKit)
    assign("displaySize", displaySize, envir = envKit)
    tkdestroy(tfEdit)
  }

  deleteAllele <- function(){
    mlbSize <- get("mlbSize", pos = envKit)
    displaySize <- get("displaySize", pos = envKit)
    posDelete <- tclvalue(tkcurselection(mlbSize))
    if(posDelete == ""){
      tkmessageBox(message = "Select one allele!", icon = "error", type = "ok")
    }else{
      posDelete <- as.numeric(posDelete) + 1
      displaySize <- displaySize[-posDelete, , drop = FALSE]
      tkdestroy(mlbSize)
      mlbSize <- tk2mclistbox(frameSize_1, width = 30, height = 27, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbSize, "add", label = "Allele", width = 15)
      tk2column(mlbSize, "add", label = "Size", width = 15)
      tkgrid(mlbSize, padx = 5, pady = 5, sticky = "w")
      tk2insert.multi(mlbSize, "end", displaySize)
      assign("mlbSize", mlbSize, envir = envKit)
      assign("displaySize", displaySize, envir = envKit)
    }
  }

  sortSize <- function(){
    mlbSize <- get("mlbSize", pos = envKit)
    displaySize <- get("displaySize", pos = envKit)
    if(length(displaySize) > 0){
      displaySize <- displaySize[order(as.numeric(displaySize[, 2])), ]
      tkdestroy(mlbSize)
      mlbSize <- tk2mclistbox(frameSize_1, width = 30, height = 27, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbSize, "add", label = "Allele", width = 15)
      tk2column(mlbSize, "add", label = "Size", width = 15)
      tkgrid(mlbSize, padx = 5, pady = 5, sticky = "w")
      tk2insert.multi(mlbSize, "end", displaySize)
      assign("mlbSize", mlbSize, envir = envKit)
      assign("displaySize", displaySize, envir = envKit)
    }
  }

  finishSize <- function(){
    displaySize <- get("displaySize", pos = envKit)
    sizeOneL <- as.numeric(displaySize[, 2])
    names(sizeOneL) <- displaySize[, 1]
    sizeList[[posSize]] <- sizeOneL
    displayKit[posSize, 5] <- "yes"
    tkdestroy(mlbKit)
    mlbKit <- tk2mclistbox(frameEditKit_2, width = 90, height = 27, resizablecolumns = TRUE, selectmode = "single")
    tk2column(mlbKit, "add", label = "Marker", width = 15)
    tk2column(mlbKit, "add", label = "Dye", width = 10)
    tk2column(mlbKit, "add", label = "Length of the repeat unit", width = 25)
    tk2column(mlbKit, "add", label = "Sex chromosomal marker", width = 25)
    tk2column(mlbKit, "add", label = "Set sizes", width = 15)
    tkgrid(mlbKit, padx = 5, pady = 5, sticky = "w")
    tk2insert.multi(mlbKit, "end", displayKit)

    assign("sizeList", sizeList, pos = envKit)
    assign("mlbKit", mlbKit, pos = envKit)
    assign("displayKit", displayKit, pos = envKit)
    tkdestroy(tfsize)
  }

  frameEditKit_2 <- get("frameEditKit_2", pos = envKit)
  mlbKit <- get("mlbKit", pos = envKit)
  posSize <- tclvalue(tkcurselection(mlbKit))
  if(posSize == ""){
    tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
  }else{
    posSize <- as.numeric(posSize) + 1
    pathPack <- get("pathPack", pos = envKit)
    sizeList <- get("sizeList", pos = envKit)
    displayKit <- get("displayKit", pos = envKit)
    setSizeState <- displayKit[posSize, 5]

    tfsize <- tktoplevel()
    tkwm.title(tfsize, "Set sizes")
    frameSize_1 <- tkframe(tfsize)
    frameSize_2 <- tkframe(tfsize)

    mlbSize <- tk2mclistbox(frameSize_1, width = 30, height = 27, resizablecolumns = TRUE, selectmode = "single")
    tk2column(mlbSize, "add", label = "Allele", width = 15)
    tk2column(mlbSize, "add", label = "Size", width = 15)
    tkgrid(mlbSize, padx = 5, pady = 5, sticky = "w")
    if(setSizeState == "yes"){
      sizeOneL <- sizeList[[posSize]]
      displaySize <- cbind(names(sizeOneL), sizeOneL)
      tk2insert.multi(mlbSize, "end", displaySize)
    }else{
      displaySize <- NULL
    }
    assign("mlbSize", mlbSize, envir = envKit)
    assign("displaySize", displaySize, envir = envKit)

    tkgrid(tkbutton(frameSize_2, text = "    Add an allele    ", cursor = "hand2", command = function() addAllele1()),
           tkbutton(frameSize_2, text = "    Edit an allele    ", cursor = "hand2", command = function() editAllele1()),
           tkbutton(frameSize_2, text = "    Delete an allele    ", cursor = "hand2", command = function() deleteAllele()),
           tkbutton(frameSize_2, text = "    Sort    ", cursor = "hand2", command = function() sortSize()),
           tkbutton(frameSize_2, text = "    Finish    ", cursor = "hand2", command = function() finishSize()),
           padx = 10, pady = 10, sticky = "w")
    tkgrid(frameSize_1)
    tkgrid(frameSize_2)
  }
}

# Save a kit information
saveKit <- function(envKit, kn, tfEditKit){
  displayKit <- get("displayKit", pos = envKit)
  nameKit <- get("nameKit", pos = envKit)
  if(!all(displayKit[, 5] == "yes")){
    tkmessageBox(message = "Set sizes of all loci!", icon = "error", type = "ok")
  }else if(kn == ""){
    tkmessageBox(message = "Enter the kit name!", icon = "error", type = "ok")
  }else{
    inputOk <- "ok"
    overwrite <- FALSE
    if(is.element(kn, nameKit)){
      overwrite <- TRUE
      inputOk <- tclvalue(tkmessageBox(message = paste0("Information of the '", kn, "' is already registered. Do you want to replace it?"), type = "okcancel", icon = "warning"))
    }
    if(inputOk == "ok"){
      pathPack <- get("pathPack", pos = envKit)
      listKit <- get("listKit", pos = envKit)
      sizeList <- get("sizeList", pos = envKit)
      nAls <- sapply(sizeList, length)
      saveData <- matrix("", sum(nAls), 6)
      colnames(saveData) <- c("Marker", "Dye", "Length_of_the_repeat_unit", "Sex_chromosomal_marker", "Allele", "Size")
      rowCount <- 0
      for(i in 1:length(sizeList)){
        posRow <- (rowCount + 1):(rowCount + nAls[i])
        saveData[posRow, 1] <- displayKit[i, 1]
        saveData[posRow, 2] <- displayKit[i, 2]
        saveData[posRow, 3] <- displayKit[i, 3]
        saveData[posRow, 4] <- displayKit[i, 4]
        saveData[posRow, 5] <- names(sizeList[[i]])
        saveData[posRow, 6] <- sizeList[[i]]
        rowCount <- rowCount + nAls[i]
      }
      options(warn = -1)
      write.csv(saveData, paste0(pathPack, "/extdata/kit/", kn, ".csv"), row.names = FALSE, col.names = TRUE)
      options(warn = 0)
      tkdestroy(tfEditKit)
      if(!overwrite){
        tkinsert(listKit, "end", kn)
        assign("listKit", listKit, envir = envKit)
        nameKit <- c(nameKit, kn)
        assign("nameKit", nameKit, envir = envKit)
      }
      tkmessageBox(message = paste0("Information of the ", kn, " have been saved successfully at '", pathPack, "/extdata/kit'."),
                   icon = "info", type = "ok")
    }
  }
}

# Edit a kit information
editKit <- function(action, envKit){
  addLocus1 <- function(){
    tfAdd <- tktoplevel()
    tkwm.title(tfAdd, "Add a locus")
    frameAdd_1 <- tkframe(tfAdd)
    frameAdd_2 <- tkframe(tfAdd)
    frameAdd_3 <- tkframe(tfAdd)
    lnVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Locus name"),
           tkentry(frameAdd_1, textvariable = lnVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 10, pady = 5, sticky = "w")
    dyeVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Dye"),
           tkentry(frameAdd_1, textvariable = dyeVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 10, pady = 5, sticky = "w")
    lenVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Length of the repeat unit"),
           tkentry(frameAdd_1, textvariable = lenVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 10, pady = 5, sticky = "w")
    sexVar <- tclVar(0)
    tkgrid(tkcheckbutton(frameAdd_2, text = "Sex chromosomal marker", variable = sexVar),
           padx = 10, pady = 5, sticky = "w")
    tkgrid(tkbutton(frameAdd_3, text = "    Add    ", cursor = "hand2",
                    command = function() addLocus2(tfAdd, tclvalue(lnVar), tclvalue(dyeVar), tclvalue(lenVar), tclvalue(sexVar))),
           pady = 5)
    tkgrid(frameAdd_1)
    tkgrid(frameAdd_2)
    tkgrid(frameAdd_3)
  }

  addLocus2 <- function(tfAdd, ln, dye, len, sex){
    mlbKit <- get("mlbKit", pos = envKit)
    displayKit <- get("displayKit", pos = envKit)
    if(sex == 0){
      sex <- "no"
    }else{
      sex <- "yes"
    }
    addData <- c(ln, dye, len, sex, "no")
    tk2insert.multi(mlbKit, "end", addData)
    assign("mlbKit", mlbKit, envir = envKit)
    assign("displayKit", rbind(displayKit, addData), envir = envKit)
    tkdestroy(tfAdd)
  }

  editLocus1 <- function(){
    mlbKit <- get("mlbKit", pos = envKit)
    displayKit <- get("displayKit", pos = envKit)
    posEdit <- tclvalue(tkcurselection(mlbKit))
    if(posEdit == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      posEdit <- as.numeric(posEdit) + 1
      tfEdit <- tktoplevel()
      tkwm.title(tfEdit, "Edit a locus")
      frameEdit_1 <- tkframe(tfEdit)
      frameEdit_2 <- tkframe(tfEdit)
      frameEdit_3 <- tkframe(tfEdit)
      lnVar <- tclVar(displayKit[posEdit, 1])
      tkgrid(tklabel(frameEdit_1, text = "Locus name"),
             tkentry(frameEdit_1, textvariable = lnVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 10, pady = 5, sticky = "w")
      dyeVar <- tclVar(displayKit[posEdit, 2])
      tkgrid(tklabel(frameEdit_1, text = "Dye"),
             tkentry(frameEdit_1, textvariable = dyeVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 10, pady = 5, sticky = "w")
      lenVar <- tclVar(displayKit[posEdit, 3])
      tkgrid(tklabel(frameEdit_1, text = "Length of the repeat unit"),
             tkentry(frameEdit_1, textvariable = lenVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 10, pady = 5, sticky = "w")
      if(displayKit[posEdit, 4] == "yes"){
        sexVar <- tclVar(1)
      }else{
        sexVar <- tclVar(0)
      }
      tkgrid(tkcheckbutton(frameEdit_2, text = "Sex chromosomal marker", variable = sexVar),
             padx = 10, pady = 5, sticky = "w")
      tkgrid(tkbutton(frameEdit_3, text = "    Edit    ", cursor = "hand2",
                      command = function() editLocus2(tfEdit, posEdit, tclvalue(lnVar), tclvalue(dyeVar), tclvalue(lenVar), tclvalue(sexVar))),
             pady = 5)
      tkgrid(frameEdit_1)
      tkgrid(frameEdit_2)
      tkgrid(frameEdit_3)
    }
  }

  editLocus2 <- function(tfEdit, posEdit, ln, dye, len, sex){
    mlbKit <- get("mlbKit", pos = envKit)
    displayKit <- get("displayKit", pos = envKit)
    if(sex == 0){
      sex <- "no"
    }else{
      sex <- "yes"
    }
    displayKit[posEdit, 1:4] <- c(ln, dye, len, sex)
    tkdestroy(mlbKit)
    mlbKit <- tk2mclistbox(frameEditKit_2, width = 90, height = 27, resizablecolumns = TRUE, selectmode = "single")
    tk2column(mlbKit, "add", label = "Marker", width = 15)
    tk2column(mlbKit, "add", label = "Dye", width = 10)
    tk2column(mlbKit, "add", label = "Length of the repeat unit", width = 25)
    tk2column(mlbKit, "add", label = "Sex chromosomal marker", width = 25)
    tk2column(mlbKit, "add", label = "Set sizes", width = 15)
    tkgrid(mlbKit, padx = 5, pady = 5, sticky = "w")
    tk2insert.multi(mlbKit, "end", displayKit)
    assign("mlbKit", mlbKit, envir = envKit)
    assign("displayKit", displayKit, envir = envKit)
    tkdestroy(tfEdit)
  }

  deleteLocus <- function(){
    mlbKit <- get("mlbKit", pos = envKit)
    displayKit <- get("displayKit", pos = envKit)
    sizeList <- get("sizeList", pos = envKit)
    posDelete <- tclvalue(tkcurselection(mlbKit))
    if(posDelete == ""){
      tkmessageBox(message = "Select one locus!", icon = "error", type = "ok")
    }else{
      posDelete <- as.numeric(posDelete) + 1
      displayKit <- displayKit[-posDelete, , drop = FALSE]
      tkdestroy(mlbKit)
      mlbKit <- tk2mclistbox(frameEditKit_2, width = 90, height = 27, resizablecolumns = TRUE, selectmode = "single")
      tk2column(mlbKit, "add", label = "Marker", width = 15)
      tk2column(mlbKit, "add", label = "Dye", width = 10)
      tk2column(mlbKit, "add", label = "Length of the repeat unit", width = 25)
      tk2column(mlbKit, "add", label = "Sex chromosomal marker", width = 25)
      tk2column(mlbKit, "add", label = "Set sizes", width = 15)
      tkgrid(mlbKit, padx = 5, pady = 5, sticky = "w")
      tk2insert.multi(mlbKit, "end", displayKit)
      assign("mlbKit", mlbKit, envir = envKit)
      assign("displayKit", displayKit, envir = envKit)
      assign("sizeList", sizeList[-posDelete], envir = envKit)
    }
  }

  pathPack <- get("pathPack", pos = envKit)
  listKit <- get("listKit", pos = envKit)
  nameKit <- get("nameKit", pos = envKit)

  error <- FALSE
  if(action == "edit"){
    posKit <- tclvalue(tkcurselection(listKit))
    if(posKit == ""){
      error <- TRUE
      tkmessageBox(message = "Select one kit!", icon = "error", type = "ok")
    }else{
      posKit <- as.numeric(posKit) + 1
      selectKit <- nameKit[posKit]
      knVar <- tclVar(selectKit)
      kitInfo <- read.csv(paste0(pathPack, "/extdata/kit/", selectKit, ".csv"), header = TRUE)
      kitInfo <- as.matrix(kitInfo)
      kitLoci <- unique(kitInfo[, "Marker"])
      nL <- length(kitLoci)
      dye_kitLoci <- lenRU <- rep("", nL)
      sexYesNo <- rep("no", nL)
      sizeList <- list()
      for(i in 1:nL){
        posL <- which(kitInfo[, "Marker"] == kitLoci[i])
        posL2 <- posL[1]
        dye_kitLoci[i] <- kitInfo[posL2, "Dye"]
        lenRUOneL <- as.numeric(kitInfo[posL2, "Length_of_the_repeat_unit"])
        if(!is.na(lenRUOneL)){
          lenRU[i] <- lenRUOneL
        }
        sexOneL <- kitInfo[posL2, "Sex_chromosomal_marker"]
        if(sexOneL == "yes"){
          sexYesNo[i] <- sexOneL
        }
        sizeOneL <- as.numeric(kitInfo[posL, "Size"])
        names(sizeOneL) <- kitInfo[posL, "Allele"]
        sizeList[[i]] <- sizeOneL
      }
      displayKit <- matrix("", length(kitLoci), 5)
      displayKit[, 1] <- kitLoci
      displayKit[, 2] <- dye_kitLoci
      displayKit[, 3] <- lenRU
      displayKit[, 4] <- sexYesNo
      displayKit[, 5] <- "yes"
    }
  }else{
    knVar <- tclVar("")
    sizeList <- list()
    displayKit <- NULL
  }
  if(!error){
    tfEditKit <- tktoplevel()
    tkwm.title(tfEditKit, "Edit information of a typing kit")
    frameEditKit_1 <- tkframe(tfEditKit)
    frameEditKit_2 <- tkframe(tfEditKit)
    frameEditKit_3 <- tkframe(tfEditKit)

    tkgrid(tklabel(frameEditKit_1, text = "Kit name"),
           tkentry(frameEditKit_1, textvariable = knVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 5, pady = 5)

    mlbKit <- tk2mclistbox(frameEditKit_2, width = 90, height = 27, resizablecolumns = TRUE, selectmode = "single")
    tk2column(mlbKit, "add", label = "Marker", width = 15)
    tk2column(mlbKit, "add", label = "Dye", width = 10)
    tk2column(mlbKit, "add", label = "Length of the repeat unit", width = 25)
    tk2column(mlbKit, "add", label = "Sex chromosomal marker", width = 25)
    tk2column(mlbKit, "add", label = "Set sizes", width = 15)
    tkgrid(mlbKit, padx = 5, pady = 5)
    if(action == "edit"){
      tk2insert.multi(mlbKit, "end", displayKit)
    }

    tkgrid(tkbutton(frameEditKit_3, text = "    Add a locus    ", cursor = "hand2", command = function() addLocus1()),
           tkbutton(frameEditKit_3, text = "    Edit a locus    ", cursor = "hand2", command = function() editLocus1()),
           tkbutton(frameEditKit_3, text = "    Delete a locus    ", cursor = "hand2", command = function() deleteLocus()),
           tkbutton(frameEditKit_3, text = "    Set sizes    ", cursor = "hand2", command = function() setSize(envKit)),
           tkbutton(frameEditKit_3, text = "    Save    ", cursor = "hand2", command = function() saveKit(envKit, tclvalue(knVar), tfEditKit)),
           padx = 10, pady = 10)

    tkgrid(frameEditKit_1, sticky = "w")
    tkgrid(frameEditKit_2, sticky = "w")
    tkgrid(frameEditKit_3, sticky = "w")

    assign("frameEditKit_2", frameEditKit_2, envir = envKit)
    assign("mlbKit", mlbKit, envir = envKit)
    assign("displayKit", displayKit, envir = envKit)
    assign("sizeList", sizeList, envir = envKit)
  }
}

# Delete a kit information
deleteKit <- function(envKit){
  listKit <- get("listKit", pos = envKit)
  posDelete <- tclvalue(tkcurselection(listKit))
  if(posDelete == ""){
    tkmessageBox(message = "Select one kit!", icon = "error", type = "ok")
  }else{
    deleteOk <- tclvalue(tkmessageBox(message = "Information of the selected kit will be deleted. Do you want to continue?", icon = "warning", type = "ok"))
    if(deleteOk == "ok"){
      pathPack <- get("pathPack", pos = envKit)
      frameKit_1 <- get("frameKit_1", pos = envKit)
      nameKit <- get("nameKit", pos = envKit)
      posDelete <- as.numeric(posDelete) + 1
      deleteKit <- nameKit[posDelete]
      file.remove(paste(pathPack, "/extdata/kit/", deleteKit, ".csv", sep = ""), recursive = FALSE)
      nameKit <- nameKit[-posDelete]
      tkdestroy(listKit)
      listKit <- tk2listbox(frameKit_1, height = 10, width = 60, selectmode = "single")
      tkgrid(listKit, padx = 10, pady = 5)
      for(i in 1:length(nameKit)){
        tkinsert(listKit, "end", nameKit[i])
      }
      assign("listKit", listKit, envir = envKit)
      assign("nameKit", nameKit, envir = envKit)
    }
  }
}

# Make a window for kit information
windowKit <- function(envGUI){
  pathPack <- get("pathPack", pos = envGUI)
  envKit <- new.env(parent = globalenv())
  nameKit <- list.files(paste0(pathPack, "/extdata/kit"))
  nameKit <- gsub(".csv", "", nameKit)
  assign("pathPack", pathPack, envir = envKit)
  assign("nameKit", nameKit, envir = envKit)

  tfKit <- tktoplevel()
  tkwm.title(tfKit, "Typing kits")
  frameKit_1 <- tkframe(tfKit)
  frameKit_2 <- tkframe(tfKit)

  listKit <- tk2listbox(frameKit_1, height = 10, width = 60, selectmode = "single")
  tkgrid(listKit, padx = 10, pady = 5)
  for(i in 1:length(nameKit)){
    tkinsert(listKit, "end", nameKit[i])
  }
  assign("frameKit_1", frameKit_1, envir = envKit)
  assign("listKit", listKit, envir = envKit)
  tkgrid(frameKit_1)

  tkgrid(tkbutton(frameKit_2, text = "    New    ", cursor = "hand2", command = function() editKit("new", envKit)),
         tkbutton(frameKit_2, text = "    Edit    ", cursor = "hand2", command = function() editKit("edit", envKit)),
         tkbutton(frameKit_2, text = "    Delete    ", cursor = "hand2", command = function() deleteKit(envKit)),
         padx = 10, pady = 5)
  tkgrid(frameKit_2)
}
