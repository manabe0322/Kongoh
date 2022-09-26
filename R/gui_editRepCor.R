editRepCor <- function(envRepCor){
  showRepCor <- function(){
    repCor <- get("repCor", pos = envRepCor)
    mlbRepCor <- get("mlbRepCor", pos = envRepCor)
    scrY_RepCor <- get("scrY_RepCor", pos = envRepCor)
    tkdestroy(mlbRepCor)
    tkdestroy(scrY_RepCor)

    displayL <- tclvalue(selectL)
    repCorOneL <- repCor[repCor[, "Marker"] == displayL, , drop = FALSE]
    repCorOneL <- gsub(" ", "", repCorOneL)
    alName <- repCorOneL[, 2]
    al <- as.numeric(alName)
    alName[al %% 1 == 0] <- round(al[al %% 1 == 0], 0)
    repCorOneL[, 2] <- alName

    scrY_RepCor <- tkscrollbar(frameEditRepCor_2, repeatinterval = 5, command = function(...) tkyview(mlbRepCor, ...))
    mlbRepCor <- tk2mclistbox(frameEditRepCor_2, width = 15 * 5, height = 25, resizablecolumns = TRUE, selectmode = "single", yscrollcommand = function(...) tkset(scrY_RepCor, ...))
    for(i in 2:ncol(repCorOneL)){
      tk2column(mlbRepCor, "add", label = colnames(repCorOneL)[i], width = 15)
    }
    tkgrid(mlbRepCor, scrY_RepCor)
    tk2insert.multi(mlbRepCor, "end", repCorOneL[, -1])
    tkgrid.configure(scrY_RepCor, rowspan = 25, sticky = "nsw")

    assign("displayL", displayL, envir = envRepCor)
    assign("scrY_RepCor", scrY_RepCor, envir = envRepCor)
    assign("mlbRepCor", mlbRepCor, envir = envRepCor)
  }

  addRepCor1 <- function(){
    tfAdd <- tktoplevel()
    tkwm.title(tfAdd, "Add an allele")
    frameAdd_1 <- tkframe(tfAdd)
    frameAdd_2 <- tkframe(tfAdd)

    displayL <- get("displayL", pos = envRepCor)
    tkgrid(tklabel(frameAdd_1, text = "Locus"), tklabel(frameAdd_1, text = displayL), padx = 20, pady = 5, sticky = "w")

    alVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "Allele"),
           tkentry(frameAdd_1, textvariable = alVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")

    lusVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "LUS"),
           tkentry(frameAdd_1, textvariable = lusVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")

    cabsrVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "CA_BSR"),
           tkentry(frameAdd_1, textvariable = cabsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")

    cafsrVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "CA_FSR"),
           tkentry(frameAdd_1, textvariable = cafsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")

    cadsrVar <- tclVar("")
    tkgrid(tklabel(frameAdd_1, text = "CA_DSR"),
           tkentry(frameAdd_1, textvariable = cadsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
           padx = 20, pady = 5, sticky = "w")

    tkgrid(tkbutton(frameAdd_2, text = "    Save    ", cursor = "hand2",
                    command = function() addRepCor2(tfAdd, displayL, tclvalue(alVar), tclvalue(lusVar), tclvalue(cabsrVar), tclvalue(cafsrVar), tclvalue(cadsrVar))),
           pady = 5)
    tkgrid(frameAdd_1)
    tkgrid(frameAdd_2)
  }

  addRepCor2 <- function(tfAdd, locus, al, lus, cabsr, cafsr, cadsr){
    selectRepCor <- get("selectRepCor", pos = envRepCor)
    repCor <- get("repCor", pos = envRepCor)
    posL <- which(repCor[, "Marker"] == locus)
    if(posL[length(posL)] == nrow(repCor)){
      repCor <- rbind(repCor, c(locus, al, lus, cabsr, cafsr, cadsr))
    }else{
      repCor1 <- repCor[1:posL[length(posL)], , drop = FALSE]
      repCor2 <- repCor[(posL[length(posL)] + 1):nrow(repCor), , drop = FALSE]
      repCor <- rbind(repCor1, c(locus, al, lus, cabsr, cafsr, cadsr), repCor2)
    }
    repCorOneL <- repCor[repCor[, "Marker"] == locus, , drop = FALSE]
    orderAl <- order(as.numeric(repCorOneL[, "Allele"]))
    repCor[repCor[, "Marker"] == locus, ] <- repCorOneL[orderAl, ]

    write.csv(repCor, paste0(pathPack, "/extdata/repeat_correction/", selectRepCor, ".csv"), row.names = FALSE)
    assign("repCor", repCor, envir = envRepCor)

    showRepCor()
    tkdestroy(tfAdd)
  }

  editRepCor1 <- function(){
    mlbRepCor <- get("mlbRepCor", pos = envRepCor)
    posEdit <- tclvalue(tkcurselection(mlbRepCor))
    if(posEdit == ""){
      tkmessageBox(message = "Select one allele!", icon = "error", type = "ok")
    }else{
      posEdit <- as.numeric(posEdit) + 1
      displayL <- get("displayL", pos = envRepCor)
      repCor <- get("repCor", pos = envRepCor)
      repCorOneL <- repCor[repCor[, "Marker"] == displayL, , drop = FALSE]
      editData <- repCorOneL[posEdit, ]

      tfEdit <- tktoplevel()
      tkwm.title(tfEdit, "Edit an allele")
      frameEdit_1 <- tkframe(tfEdit)
      frameEdit_2 <- tkframe(tfEdit)

      tkgrid(tklabel(frameEdit_1, text = "Locus"), tklabel(frameEdit_1, text = displayL), padx = 20, pady = 5, sticky = "w")

      alVar <- tclVar(editData[2])
      tkgrid(tklabel(frameEdit_1, text = "Allele"),
             tkentry(frameEdit_1, textvariable = alVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")

      lusVar <- tclVar(editData[3])
      tkgrid(tklabel(frameEdit_1, text = "LUS"),
             tkentry(frameEdit_1, textvariable = lusVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")

      cabsrVar <- tclVar(editData[4])
      tkgrid(tklabel(frameEdit_1, text = "CA_BSR"),
             tkentry(frameEdit_1, textvariable = cabsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")

      cafsrVar <- tclVar(editData[5])
      tkgrid(tklabel(frameEdit_1, text = "CA_FSR"),
             tkentry(frameEdit_1, textvariable = cafsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")

      cadsrVar <- tclVar(editData[6])
      tkgrid(tklabel(frameEdit_1, text = "CA_DSR"),
             tkentry(frameEdit_1, textvariable = cadsrVar, width = 20, highlightthickness = 1, relief = "solid", justify = "center", background = "white"),
             padx = 20, pady = 5, sticky = "w")

      tkgrid(tkbutton(frameEdit_2, text = "    Save    ", cursor = "hand2",
                      command = function() editRepCor2(tfEdit, posEdit, displayL, tclvalue(alVar), tclvalue(lusVar), tclvalue(cabsrVar), tclvalue(cafsrVar), tclvalue(cadsrVar))),
             pady = 5)
      tkgrid(frameEdit_1)
      tkgrid(frameEdit_2)
    }
  }

  editRepCor2 <- function(tfEdit, posEdit, locus, al, lus, cabsr, cafsr, cadsr){
    selectRepCor <- get("selectRepCor", pos = envRepCor)
    repCor <- get("repCor", pos = envRepCor)
    posL <- which(repCor[, "Marker"] == locus)
    repCorOneL <- repCor[posL, , drop = FALSE]
    repCorOneL[posEdit, ] <- c(locus, al, lus, cabsr, cafsr, cadsr)
    orderAl <- order(as.numeric(repCorOneL[, "Allele"]))
    repCor[posL, ] <- repCorOneL[orderAl, ]

    write.csv(repCor, paste0(pathPack, "/extdata/repeat_correction/", selectRepCor, ".csv"), row.names = FALSE)
    assign("repCor", repCor, envir = envRepCor)

    showRepCor()
    tkdestroy(tfEdit)
  }

  deleteRepCor <- function(){
    mlbRepCor <- get("mlbRepCor", pos = envRepCor)
    posDelete <- tclvalue(tkcurselection(mlbRepCor))
    if(posDelete == ""){
      tkmessageBox(message = "Select one allele!", icon = "error", type = "ok")
    }else{
      inputOk <- tclvalue(tkmessageBox(message = "Do you want to delete the selected allele?", type = "okcancel", icon = "warning"))
      if(inputOk == "ok"){
        posDelete <- as.numeric(posDelete) + 1
        displayL <- get("displayL", pos = envRepCor)
        repCor <- get("repCor", pos = envRepCor)
        posL <- which(repCor[, "Marker"] == displayL)
        repCor <- repCor[-posL[posDelete], , drop = FALSE]

        write.csv(repCor, paste0(pathPack, "/extdata/repeat_correction/", selectRepCor, ".csv"), row.names = FALSE)
        assign("repCor", repCor, envir = envRepCor)

        showRepCor()
      }
    }
  }

  pathPack <- get("pathPack", pos = envRepCor)
  listRepCor <- get("listRepCor", pos = envRepCor)
  nameRepCor <- get("nameRepCor", pos = envRepCor)

  posRepCor <- tclvalue(tkcurselection(listRepCor))

  if(posRepCor == ""){
    tkmessageBox(message = "Select one dataset of repeat correction!", icon = "error", type = "ok")
  }else{
    posRepCor <- as.numeric(posRepCor) + 1
    selectRepCor <- nameRepCor[posRepCor]
    rcVar <- tclVar(selectRepCor)
    repCor <- read.csv(paste0(pathPack, "/extdata/repeat_correction/", selectRepCor, ".csv"), header = TRUE)
    repCor <- as.matrix(repCor)
    repCor[is.na(repCor)] <- ""

    repCorLoci <- unique(repCor[, "Marker"])
    nL <- length(repCorLoci)

    tfEditRepCor <- tktoplevel()
    tkwm.title(tfEditRepCor, "Edit repeat correction")

    assign("selectRepCor", selectRepCor, envir = envRepCor)
    assign("repCor", repCor, envir = envRepCor)
    assign("mlbRepCor", tk2mclistbox(tfEditRepCor), envir = envRepCor)
    assign("scrY_RepCor", tkscrollbar(tfEditRepCor), envir = envRepCor)
    frameEditRepCor_1 <- tkframe(tfEditRepCor)
    frameEditRepCor_2 <- tkframe(tfEditRepCor)
    frameEditRepCor_3 <- tkframe(tfEditRepCor)

    selectL <- tclVar(repCorLoci[1])
    comboLoci <- ttkcombobox(frameEditRepCor_1, values = repCorLoci, textvariable = selectL, state = "readonly", width = 20)
    tkgrid(tklabel(frameEditRepCor_1, text = "Locus : "), comboLoci, sticky = "w")

    tkbind(comboLoci, "<<ComboboxSelected>>", function(){
      showRepCor()
    })

    showRepCor()

    tkgrid(tkbutton(frameEditRepCor_3, text = "    Add an allele    ", cursor = "hand2", command = function() addRepCor1()),
           tkbutton(frameEditRepCor_3, text = "    Edit an allele    ", cursor = "hand2", command = function() editRepCor1()),
           tkbutton(frameEditRepCor_3, text = "    Delete an allele    ", cursor = "hand2", command = function() deleteRepCor()),
           tkbutton(frameEditRepCor_3, text = "    Finish    ", cursor = "hand2", command = function() tkdestroy(tfEditRepCor)),
           padx = 10, sticky = "w")

    tkgrid(frameEditRepCor_1, padx = 10, pady = 5, sticky = "w")
    tkgrid(frameEditRepCor_2, padx = 10, pady = 5, sticky = "w")
    tkgrid(frameEditRepCor_3, padx = 10, pady = 5, sticky = "w")
  }
}

windowRepCor <- function(envGUI){
  pathPack <- get("pathPack", pos = envGUI)
  envRepCor <- new.env(parent = globalenv())
  nameRepCor <- list.files(paste0(pathPack, "/extdata/repeat_correction"))
  nameRepCor <- gsub(".csv", "", nameRepCor)
  assign("pathPack", pathPack, envir = envRepCor)
  assign("nameRepCor", nameRepCor, envir = envRepCor)

  tfRepCor <- tktoplevel()
  tkwm.title(tfRepCor, "Repeat correction")
  frameRepCor_1 <- tkframe(tfRepCor)
  frameRepCor_2 <- tkframe(tfRepCor)

  listRepCor <- tk2listbox(frameRepCor_1, height = 10, width = 60, selectmode = "single")
  tkgrid(listRepCor, padx = 10, pady = 5)
  for(i in 1:length(nameRepCor)){
    tkinsert(listRepCor, "end", nameRepCor[i])
  }
  assign("frameRepCor_1", frameRepCor_1, envir = envRepCor)
  assign("listRepCor", listRepCor, envir = envRepCor)
  tkgrid(frameRepCor_1)

  tkgrid(tkbutton(frameRepCor_2, text = "    Edit    ", cursor = "hand2", command = function() editRepCor(envRepCor)),
         tkbutton(frameRepCor_2, text = "    Delete    ", cursor = "hand2", command = function() deleteRepCor(envRepCor)),
         padx = 10, pady = 5)
  tkgrid(frameRepCor_2)
}
