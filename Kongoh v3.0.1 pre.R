Kongoh <- function(){
  
  #############################
  # Install required packages #
  #############################
  
  #tcltk
  if(require(tcltk)){
    print("tcltk is loaded correctly")
  }else{
    print("trying to install tcltk...")
    install.packages("tcltk")
    if(require("tcltk")){
      print("tcltk installed and loaded")
      tclRequire("Tktable")
    }else{
      stop("could not install tcltk")
    }
  }
  
  #tcltk2
  if(require(tcltk2)){
    print("tcltk2 is loaded correctly")
  }else{
    print("trying to install tcltk2...")
    install.packages("tcltk2")
    if(require("tcltk2")){
      print("tcltk2 installed and loaded")
    }else{
      stop("could not install tcltk2")
    }
  }
  
  #gtools
  if(require(gtools)){
    print("gtools is loaded correctly")
  }else{
    print("trying to install gtools...")
    install.packages("gtools")
    if(require("gtools")){
      print("gtools installed and loaded")
    }else{
      stop("could not install gtools")
    }
  }
  
  #parallel
  if(require(parallel)){
    print("parallel is loaded correctly")
  }else{
    print("trying to install parallel...")
    install.packages("parallel")
    if(require("parallel")){
      print("parallel installed and loaded")
    }else{
      stop("could not install parallel")
    }
  }
  
  #truncnorm
  if(require(truncnorm)){
    print("truncnorm is loaded correctly")
  }else{
    print("trying to install truncnorm...")
    install.packages("truncnorm")
    if(require("truncnorm")){
      print("truncnorm installed and loaded")
    }else{
      stop("could not install truncnorm")
    }
  }
  
  #Tktable
  addTclPath('/usr/local/lib/Tktable2.9')
  tclRequire("Tktable")
  
  
  ####################
  # hidden parameter #
  ####################
  
  #software version
  softVer <- "Kongoh v3.0.1"
  
  #STR repeat length
  #	repLengthInfo <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5)
  #	names(repLengthInfo) <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX", "D8S1179", "D21S11", "D18S51", "D2S441", "D19S433", "TH01", "FGA", "D22S1045", "D5S818", "D13S317", "D7S820", "SE33", "D10S1248", "D1S1656", "D12S391", "D2S1338", "D6S1043", "Penta D", "Penta E")
  
  #Minumum or maximum of each parameter
  aeMin <- 0.058
  hbMin <- 0.046
  srB1Max <- 0.7
  srF1Max <- 0.35
  srB2Max <- 0.13
  srM2Max <- 0.12
  
  #Default of mixture ratio (MRn) of one contributor (1 to n-1)
  mrDefault <- c(0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)
  mrOne <<- mrDefault
  
  #Threshold of likelihoods for selecting MR and d
  mrDegCutVar <- tclVar("0.01")
  
  #Sex chromosomal marker
  sexChrMar <- c("AMEL", "Amelogenin", "Yindel", "YIndel", "YInDel", "DYS391")
  
  #Size of each allele (used to determine sizes of drop-out alleles in the case of locus drop-out)
  lociID <- c("D8S1179", "D21S11", "D7S820", "CSF1PO", "D3S1358", "TH01", "D13S317", "D16S539", "D2S1338", "D19S433", "vWA", "TPOX", "D18S51", "D5S818", "FGA")
  
  alleleID <- list()
  alleleID[[1]] <- c(7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
  alleleID[[2]] <- c(27, 28, 28.2, 29, 30, 30.2, 30.3, 31, 31.2, 32, 32.2, 33, 33.1, 33.2, 34, 34.2)
  alleleID[[3]] <- c(7, 8, 9, 9.1, 10, 10.1, 10.3, 11, 12, 13, 14, 15)
  alleleID[[4]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  alleleID[[5]] <- c(12, 13, 14, 15, 16, 17, 18, 19, 21)
  alleleID[[6]] <- c(5, 6, 7, 8, 9, 9.3, 10)
  alleleID[[7]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleID[[8]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleID[[9]] <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  alleleID[[10]] <- c(9.2, 10.2, 11, 11.2, 12, 12.2, 13, 13.2, 14, 14.2, 15, 15.2, 16, 16.2, 17.2, 18)
  alleleID[[11]] <- c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  alleleID[[12]] <- c(7, 8, 9, 10, 11, 12, 13, 14)
  alleleID[[13]] <- c(10, 11, 12, 13, 14, 15, 16, 17, 17.1, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
  alleleID[[14]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleID[[15]] <- c(17, 18, 19, 20, 21, 22, 22.2, 23, 23.2, 24, 24.2, 25, 25.2, 26, 27, 28)
  names(alleleID) <- lociID
  
  sizeID <- list()
  sizeID[[1]] <- c(123, 131, 135, 139, 143, 147, 151, 155, 159, 163, 167)
  sizeID[[2]] <- c(198, 202, 204, 206, 210, 212, 213, 214, 216, 218, 220, 222, 223, 224, 226, 228)
  sizeID[[3]] <- c(261, 265, 269, 270, 273, 274, 276, 277, 281, 285, 289, 293)
  sizeID[[4]] <- c(309, 313, 317, 321, 325, 329, 333, 337, 341, 345)
  sizeID[[5]] <- c(113, 117, 121, 125, 129, 133, 137, 141, 149)
  sizeID[[6]] <- c(168, 172, 176, 180, 184, 187, 188)
  sizeID[[7]] <- c(201, 205, 209, 213, 217, 221, 225, 229, 233)
  sizeID[[8]] <- c(260, 264, 268, 272, 276, 280, 284, 288, 292)
  sizeID[[9]] <- c(311, 315, 319, 323, 327, 331, 335, 339, 343, 347, 351, 355, 359)
  sizeID[[10]] <- c(108, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 140, 142)
  sizeID[[11]] <- c(164, 168, 172, 176, 180, 184, 188, 192, 196, 200)
  sizeID[[12]] <- c(221, 225, 229, 233, 237, 241, 245, 249)
  sizeID[[13]] <- c(276, 280, 284, 288, 292, 296, 300, 304, 305, 308, 312, 316, 320, 324, 328, 332, 336, 340, 344)
  sizeID[[14]] <- c(134, 138, 142, 146, 150, 154, 158, 162, 166)
  sizeID[[15]] <- c(214, 218, 222, 226, 230, 234, 236, 238, 240, 242, 244, 246, 248, 250, 254, 258)
  names(sizeID) <- lociID
  
  lociGF <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX", "D8S1179", "D21S11", "D18S51", "D2S441", "D19S433", "TH01", "FGA", "D22S1045", "D5S818", "D13S317", "D7S820", "SE33", "D10S1248", "D1S1656", "D12S391", "D2S1338")
  
  alleleGF <- list()
  alleleGF[[1]] <- c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
  alleleGF[[2]] <- c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  alleleGF[[3]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleGF[[4]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  alleleGF[[5]] <- c(8, 9, 10, 11, 12, 13, 14)
  alleleGF[[6]] <- c(7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
  alleleGF[[7]] <- c(27, 28, 28.2, 29, 30, 30.2, 30.3, 31, 31.2, 32, 32.2, 33, 33.1, 33.2, 34, 34.2)
  alleleGF[[8]] <- c(10, 11, 12, 13, 14, 15, 16, 17, 17.1, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
  alleleGF[[9]] <- c(8, 8.1, 9.1, 10, 10.1, 11, 11.1, 11.3, 12, 13, 14, 15, 16)
  alleleGF[[10]] <- c(7, 9.2, 10.2, 11, 11.2, 12, 12.2, 13, 13.2, 14, 14.2, 15, 15.2, 16, 16.2, 17.2, 18)
  alleleGF[[11]] <- c(5, 6, 7, 8, 9, 9.3, 10)
  alleleGF[[12]] <- c(16, 17, 18, 19, 20, 21, 22, 22.2, 23, 23.2, 24, 24.2, 25, 25.2, 26, 27, 28, 29)
  alleleGF[[13]] <- c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
  alleleGF[[14]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleGF[[15]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  alleleGF[[16]] <- c(7, 8, 9, 9.1, 10, 10.1, 10.3, 11, 12, 13, 14, 15)
  alleleGF[[17]] <- c(13, 14, 14.1, 14.2, 15, 16, 16.2, 17, 17.2, 18, 18.2, 19, 19.2, 20, 20.2, 21, 21.2, 22, 22.2, 23, 23.2, 24, 24.2, 25, 25.2, 26.2, 27, 27.1, 27.2, 27.3, 28, 28.1, 28.2, 28.3, 29.2, 29.3, 30.2, 30.3, 31.1, 31.2, 32.1, 32.2, 33.2, 34.2)
  alleleGF[[18]] <- c(10, 11, 12, 13, 14, 15, 16, 17, 18)
  alleleGF[[19]] <- c(11, 12, 13, 14, 14.2, 15, 15.3, 16, 16.3, 17, 17.3, 18, 18.3, 19.3, 20.3)
  alleleGF[[20]] <- c(12, 14, 15, 16, 17, 18, 19, 19.3, 20, 20.3, 21, 21.3, 22, 23, 24, 25, 26, 27)
  alleleGF[[21]] <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  names(alleleGF) <- lociGF
  
  sizeGF <- list()
  sizeGF[[1]] <- c(109, 113, 117, 121, 125, 129, 133, 137, 141, 145)
  sizeGF[[2]] <- c(165, 169, 173, 177, 181, 185, 189, 193, 197, 201)
  sizeGF[[3]] <- c(236, 240, 244, 248, 252, 256, 260, 264, 268)
  sizeGF[[4]] <- c(286, 290, 294, 298, 302, 306, 310, 314, 318, 322)
  sizeGF[[5]] <- c(351, 355, 359, 363, 367, 371, 375)
  sizeGF[[6]] <- c(122, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166)
  sizeGF[[7]] <- c(195, 199, 201, 203, 207, 209, 210, 211, 213, 215, 217, 219, 220, 221, 223, 225)
  sizeGF[[8]] <- c(273, 277, 281, 285, 289, 293, 297, 301, 302, 305, 309, 313, 317, 321, 325, 329, 333, 337, 341)
  sizeGF[[9]] <- c(77, 78, 82, 85, 86, 89, 90, 92, 93, 97, 101, 105, 109)
  sizeGF[[10]] <- c(122, 131, 135, 137, 139, 141, 143, 145, 147, 149, 151, 153, 155, 157, 159, 163, 165)
  sizeGF[[11]] <- c(183, 187, 191, 195, 199, 202, 203)
  sizeGF[[12]] <- c(236, 240, 244, 248, 252, 256, 260, 262, 264, 266, 268, 270, 272, 274, 276, 280, 284, 288)
  sizeGF[[13]] <- c(97, 100, 103, 106, 109, 112, 115, 118, 121, 124)
  sizeGF[[14]] <- c(139, 143, 147, 151, 155, 159, 163, 167, 171)
  sizeGF[[15]] <- c(207, 211, 215, 219, 223, 227, 231, 235, 239)
  sizeGF[[16]] <- c(266, 270, 274, 275, 278, 279, 281, 282, 286, 290, 294, 298)
  sizeGF[[17]] <- c(342, 346, 347, 348, 350, 354, 356, 358, 360, 362, 364, 366, 368, 370, 372, 374, 376, 378, 380, 382, 384, 386, 388, 390, 392, 396, 398, 399, 400, 401, 402, 403, 404, 405, 408, 409, 412, 413, 415, 416, 419, 420, 424, 428)
  sizeGF[[18]] <- c(94, 98, 102, 106, 110, 114, 118, 122, 126)
  sizeGF[[19]] <- c(168, 172, 176, 180, 182, 184, 187, 188, 191, 192, 195, 196, 199, 203, 207)
  sizeGF[[20]] <- c(208, 216, 220, 224, 228, 232, 236, 239, 240, 243, 244, 247, 248, 252, 256, 260, 264, 268)
  sizeGF[[21]] <- c(302, 306, 310, 314, 318, 322, 326, 330, 334, 338, 342, 346, 350)
  names(sizeGF) <- lociGF
  
  
  #############################
  # Default of GUI parameters #
  #############################
  
  cspName <- tclVar("")
  cspFp <- tclVar("")
  cspButtS <- tclVar("disabled")
  cspButtC <- tclVar("arrow")
  refName <- tclVar("")
  refFp <- tclVar("")
  refButtS <- tclVar("disabled")
  refButtC <- tclVar("arrow")
  afName <- tclVar("")
  afFp <- tclVar("")
  afButtS <- tclVar("disabled")
  afButtC <- tclVar("arrow")
  mcName <- tclVar("")
  mcFp <- tclVar("")
  mcButtS <- tclVar("disabled")
  mcButtC <- tclVar("")
  alCorName <- tclVar("")
  alCorFp <- tclVar("")
  alCorButtS <- tclVar("disabled")
  alCorButtC <- tclVar("")
  
  hpVars <- hdVars <- atVars <- list()
  
  hncFromVar <- tclVar("1")
  hncToVar <- tclVar("4")
  mafVar <- tclVar("0.001")
  thetaVar <- tclVar("0")
  numMcVar <- tclVar("1000")
  
  degWVar <- tclVar("0.0025")
  stVar <- tclVar("500")
  hbFltrVar <- tclVar("0.25")
  stB1FltrVar <- tclVar("0.7")
  stF1FltrVar <- tclVar("0.35")
  stB2FltrVar <- tclVar("0.13")
  stM2FltrVar <- tclVar("0.12")
  afMethVar <- tclVar("0")
  
  fileCkFin <- tclVar("0")
  calcFin <- tclVar("0")
  
  hncFromSpinVar <- tclVar("normal")
  hncToSpinVar <- tclVar("normal")
  atEntryVar <- tclVar("normal")
  thetaEntryVar <- tclVar("normal")
  locusSetVar <- tclVar("readonly")
  paramSetVar <- tclVar("readonly")
  otherArrowVar <- tclVar("hand2")
  otherButtVar <- tclVar("normal")
  resetArrowVar <- tclVar("arrow")
  resetButtVar <- tclVar("disable")
  
  
  ###############
  # 'Files' tab #
  ###############
  
  #Make 'Files' tab
  tab1Make <- function(){
    
    #Imput required files
    openFile <- function(fp, var, top, fileState, cursor){
      imputOk <- "ok"
      if(tclvalue(calcFin) == "1"){
        imputOk <- tclvalue(tkmessageBox(message = "Calculation result for current crime stain profile will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      }
      if(imputOk == "ok"){
        fileName <- tclvalue(tkgetOpenFile(parent = top, initialdir = tclvalue(fp), multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
        if(!nchar(fileName)){
          tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
        }else{
          tmp <- sub("\\}", fileName, replacement = "")
          tmp2 <- sub("\\{", tmp, replacement = "")
          tclvalue(fp) <- tmp2
          foo3 <- strsplit(tmp2, "/")[[1]]
          tclvalue(var) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
          tclvalue(fileState) <- "normal"
          tclvalue(cursor) <- "hand2"
        }
        tclvalue(fileCkFin) <- 0
        tclvalue(calcFin) <- 0
        tab1Make()
        tab2Make()
        tab3Make()
        tab4Make()
      }
    }
    
    #View CSP profile
    cspView <- function(){
      if(paste(tclvalue(cspName)) == ""){
        tkmessageBox(message = "Load Crime Stain Profile!", icon = "error", type = "ok")
      }else{
        csp <- read.csv(tclvalue(cspFp), header = FALSE)
        csp <- as.matrix(csp)
        csp <- gsub(" ", "", csp, fixed = TRUE)
        csp <- csp[, - grep("Sample", csp[1, ])]
        cspLoci <- csp[, grep("Marker", csp[1, ])]
        csp <- csp[!is.element(cspLoci, sexChrMar), , drop = FALSE]
        cspTclArray <- tclArray()
        for(i in 1:nrow(csp)){
          for(j in 1:ncol(csp)){
            if(is.na(csp[i, j])){
              cspTclArray[[i - 1, j - 1]] <- "-"
            }else if(csp[i, j] == ""){
              cspTclArray[[i - 1, j - 1]] <- "-"
            }else{
              cspTclArray[[i - 1, j - 1]] <- csp[i, j]
            }
          }
        }
        cspTf <- tktoplevel()
        tkwm.title(cspTf, "Crime Stain Profile")
        cspFrame <- tkframe(cspTf)
        cspTable <- tkwidget(cspFrame, "table", variable = cspTclArray, rows = nrow(csp), cols = ncol(csp), titlerows = "1", titlecols = "1", selectmode = "extended", colwidth = "15", xscrollcommand = function(...) tkset(xscr, ...), yscrollcommand = function(...) tkset(yscr, ...))
        xscr <- tkscrollbar(cspFrame, orient = "horizontal", command = function(...) tkxview(cspTable, ...))
        yscr <- tkscrollbar(cspFrame, command = function(...) tkyview(cspTable, ...))
        tkgrid(cspTable, yscr)
        tkgrid.configure(yscr, sticky = "nsw")
        tkgrid(xscr, sticky = "new")
        tkconfigure(cspTable, variable = cspTclArray, background = "white", selectmode = "extended", state = "disable")
        tkgrid(cspFrame, padx = 50, pady = 50)
      }
    }
    
    #View reference profile
    refView <- function(){
      if(paste(tclvalue(refName)) == ""){
        tkmessageBox(message = "Load Reference Profiles!", icon = "error", type = "ok")
      }else{
        ref <- read.csv(tclvalue(refFp), header = FALSE)
        ref <- as.matrix(ref)
        ref <- gsub(" ", "", ref, fixed = TRUE)
        refLoci <- ref[, grep("Marker", ref[1, ])]
        ref <- ref[!is.element(refLoci, sexChrMar), , drop = FALSE]
        refTclArray <- tclArray()
        for(i in 1:nrow(ref)){
          for(j in 1:ncol(ref)){
            refTclArray[[i - 1, j - 1]] <- ref[i, j]
          }
        }
        refTf <- tktoplevel()
        tkwm.title(refTf, "Reference Profiles")
        refTable <- tkwidget(refTf, "table", variable = refTclArray, rows = nrow(ref), cols = ncol(ref), titlerows = "1", titlecols = "1", selectmode = "extended", colwidth = "15", background = "white", state = "disable")
        tkpack(refTable)
      }
    }
    
    #View allele frequencies
    afView <- function(){
      if(paste(tclvalue(afName)) == ""){
        tkmessageBox(message = "Load Allele Frequencies!", icon = "error", type = "ok")
      }else{
        af <- read.csv(tclvalue(afFp), header = FALSE, as.is = TRUE, na.strings = "")
        af <- as.matrix(af)
        af <- af[, !is.element(af[1, ], sexChrMar), drop = FALSE]
        afVal <- as.numeric(af[-1, -1])
        afVal <- afVal[!is.na(afVal)]
        if(length(which(afVal %% 1 != 0)) == 0){
          afName <- "Dirichlet distribution"
          for(i in 2:ncol(af)){
            afOneL <- as.numeric(af[-1, i])
            alPos <- which(is.na(afOneL) == FALSE)
            afOneL <- afOneL[alPos]
            afOneL <- (afOneL + 1) / (sum(afOneL) + length(afOneL))
            af[alPos + 1, i] <- afOneL
          }
        }else{
          afName <- "Observed frequencies"
        }
        afTclArray <- tclArray()
        for(j in 1:ncol(af)){
          afTclArray[[0, j - 1]] <- af[1, j]
        }
        for(i in 2:nrow(af)){
          for(j in 1:ncol(af)){
            if(is.na(af[i, j])){
              afTclArray[[i - 1, j - 1]] <- "-"
            }else{
              afTclArray[[i - 1, j - 1]] <- round(as.numeric(af[i, j]), 5)
            }
          }
        }
        afTf <- tktoplevel()
        tkwm.title(afTf, "Allele Frequencies")
        tkgrid(tklabel(afTf, text = afName), padx = 20, pady = 10, sticky = "w")
        afFrame <- tkframe(afTf)
        afTable <- tkwidget(afFrame, "table", rows = nrow(af), cols = ncol(af), titlerows = 1, titlecols = 1, height = 30, colwidth = "10", xscrollcommand = function(...) tkset(xscr, ...), yscrollcommand = function(...) tkset(yscr, ...))
        xscr <- tkscrollbar(afFrame, orient = "horizontal", command = function(...) tkxview(afTable, ...))
        yscr <- tkscrollbar(afFrame, command = function(...) tkyview(afTable, ...))
        tkgrid(afTable, yscr)
        tkgrid.configure(yscr, sticky = "nsw")
        tkgrid(xscr, sticky = "new")
        tkconfigure(afTable, variable = afTclArray, background = "white", selectmode = "extended", state = "disable")
        tkgrid(afFrame, padx = 50, pady = 10)
      }
    }
    
    #View Monte Carlo parameters
    mcView <- function(){
      if(paste(tclvalue(mcName)) == ""){
        tkmessageBox(message = "Load Monte Carlo Parameters!", icon = "error", type = "ok")
      }else{
        mc <- read.csv(tclvalue(mcFp), header = FALSE)
        mc <- as.matrix(mc)
        mcLoci <- mc[, grep("Marker", mc[1, ])]
        mc <- mc[!is.element(mcLoci, sexChrMar), , drop = FALSE]
        mcTclArray <- tclArray()
        for(i in 1:nrow(mc)){
          for(j in 1:ncol(mc)){
            if(mc[i, j] == ""){
              mcTclArray[[i - 1, j - 1]] <- "-"
            }else{
              mcTclArray[[i - 1, j - 1]] <- mc[i, j]
            }
          }
        }
        mcTf <- tktoplevel()
        tkwm.title(mcTf, "Monte Carlo Parameters")
        mcFrame <- tkframe(mcTf)
        mcTable <- tkwidget(mcFrame, "table", variable = mcTclArray, rows = nrow(mc), cols = ncol(mc), titlerows = "1", titlecols = "1", selectmode = "extended", colwidth = "20", xscrollcommand = function(...) tkset(xscr, ...), yscrollcommand = function(...) tkset(yscr, ...))
        xscr <- tkscrollbar(mcFrame, orient = "horizontal", command = function(...) tkxview(mcTable, ...))
        yscr <- tkscrollbar(mcFrame, command = function(...) tkyview(mcTable, ...))
        tkgrid(mcTable, yscr)
        tkgrid.configure(yscr, sticky = "nsw")
        tkgrid(xscr, sticky = "new")
        tkconfigure(mcTable, variable = mcTclArray, background = "white", selectmode = "extended", state = "disable")
        tkgrid(mcFrame, padx = 50, pady = 50)
      }
    }
    
    #View information of allele repeat correction
    alCorView <- function(){
      if(paste(tclvalue(alCorName)) == ""){
        tkmessageBox(message = "Load Crime Stain Profile!", icon = "error", type = "ok")
      }else{
        alCor <- read.csv(tclvalue(alCorFp), header = FALSE)
        alCor <- as.matrix(alCor)
        alCorTclArray <- tclArray()
        for(i in 1:nrow(alCor)){
          for(j in 1:ncol(alCor)){
            if(alCor[i, j] == ""){
              alCorTclArray[[i - 1, j - 1]] <- "-"
            }else{
              alCorTclArray[[i - 1, j - 1]] <- alCor[i, j]
            }
          }
        }
        alCorTf <- tktoplevel()
        tkwm.title(alCorTf, "Allele Repeat Correction")
        alCorFrame <- tkframe(alCorTf)
        alCorTable <- tkwidget(alCorFrame, "table", variable = alCorTclArray, rows = nrow(alCor), cols = ncol(alCor), titlerows = "1", titlecols = "1", selectmode = "extended", colwidth = "20", xscrollcommand = function(...) tkset(xscr, ...), yscrollcommand = function(...) tkset(yscr, ...))
        xscr <- tkscrollbar(alCorFrame, orient = "horizontal", command = function(...) tkxview(alCorTable, ...))
        yscr <- tkscrollbar(alCorFrame, command = function(...) tkyview(alCorTable, ...))
        tkgrid(alCorTable, yscr)
        tkgrid.configure(yscr, sticky = "nsw")
        tkgrid(xscr, sticky = "new")
        tkconfigure(alCorTable, variable = alCorTclArray, background = "white", selectmode = "extended", state = "disable")
        tkgrid(alCorFrame, padx = 50, pady = 50)
      }
    }
    
    tkdestroy(frameTab1)
    frameTab1 <<- tkframe(tab1)
    
    frameTab1_1 <- tkframe(frameTab1)
    cspLabel <- tklabel(frameTab1_1, text = "Crime Stain Profile")
    cspFileLabel <- tklabel(frameTab1_1, textvariable = cspName, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    cspButt <- tkbutton(frameTab1_1, text = "    Load    ", cursor = "hand2", command = function() openFile(cspFp, cspName, tf, cspButtS, cspButtC))
    cspEpgButt <- tkbutton(frameTab1_1, text = "        View Crime Stain Profile        ", cursor = tclvalue(cspButtC), state = tclvalue(cspButtS), command = function() cspView())
    
    refLabel <- tklabel(frameTab1_1, text = "Reference Profiles")
    refFileLabel <- tklabel(frameTab1_1, textvariable = refName, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    refButt <- tkbutton(frameTab1_1, text = "    Load    ", cursor = "hand2", command = function() openFile(refFp, refName, tf, refButtS, refButtC))
    refGtButt <- tkbutton(frameTab1_1, text = "     View Reference Profiles     ", cursor = tclvalue(refButtC), state = tclvalue(refButtS), command = function() refView())
    
    afLabel <- tklabel(frameTab1_1, text = "Allele Frequencies")
    afFileLabel <- tklabel(frameTab1_1, textvariable = afName, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    afButt <- tkbutton(frameTab1_1, text = "    Load    ", cursor = "hand2", command = function() openFile(afFp, afName, tf, afButtS, afButtC))
    afShowButt <- tkbutton(frameTab1_1, text = "  View Frequencies  ", cursor = tclvalue(afButtC), state = tclvalue(afButtS), command = function() afView())
    
    mcLabel <- tklabel(frameTab1_1, text = "Monte Carlo Parameters")
    mcFileLabel <- tklabel(frameTab1_1, textvariable = mcName, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    mcButt <- tkbutton(frameTab1_1, text = "    Load    ", cursor = "hand2", command = function() openFile(mcFp, mcName, tf, mcButtS, mcButtC))
    mcShowButt <- tkbutton(frameTab1_1, text = "  View Monte Carlo Parameters  ", cursor = tclvalue(mcButtC), state = tclvalue(mcButtS), command = function() mcView())
    
    alCorLabel <- tklabel(frameTab1_1, text = "Allele Repeat Correction")
    alCorFileLabel <- tklabel(frameTab1_1, textvariable = alCorName, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    alCorButt <- tkbutton(frameTab1_1, text = "    Load    ", cursor = "hand2", command = function() openFile(alCorFp, alCorName, tf, alCorButtS, alCorButtC))
    alCorShowButt <- tkbutton(frameTab1_1, text = "  View Allele Repeat Correction  ", cursor = tclvalue(alCorButtC), state = tclvalue(alCorButtS), command = function() alCorView())
    
    frameTab1_2 <- tkframe(frameTab1)
    nextButt <- tkbutton(frameTab1_2, text = "    Next    ", cursor = "hand2", command = function() fileCk())
    
    tkgrid(cspLabel, cspFileLabel, cspButt, cspEpgButt, padx = 20, pady = 20, sticky = "w")
    tkgrid(refLabel, refFileLabel, refButt, refGtButt, padx = 20, pady = 20, sticky = "w")
    tkgrid(afLabel, afFileLabel, afButt, afShowButt, padx = 20, pady = 20, sticky = "w")
    tkgrid(mcLabel, mcFileLabel, mcButt, mcShowButt, padx = 20, pady = 20, sticky = "w")
    tkgrid(alCorLabel, alCorFileLabel, alCorButt, alCorShowButt, padx = 20, pady = 20, sticky = "w")
    tkgrid(nextButt, padx = 40, pady = 20, sticky = "w")
    tkgrid(frameTab1_1)
    tkgrid(frameTab1_2)
    tkgrid(frameTab1)
  }
  
  
  #####################
  # Check imput files #
  #####################
  
  #Check imput files
  fileCk <- function(){
    if(any(c(tclvalue(cspFp) == "", tclvalue(refFp) == "", tclvalue(afFp) == "", tclvalue(mcFp) == ""))){
      tkmessageBox(message = "Load required file(s)!", icon = "error", type = "ok")
    }else{
      csp <- read.csv(tclvalue(cspFp), header = TRUE)
      csp <- as.matrix(csp)
      csp <- gsub(" ", "", csp, fixed = TRUE)
      cspLoci <- csp[, grep("Marker", colnames(csp))]
      cspPos <- !is.element(cspLoci, sexChrMar)
      csp <- csp[cspPos, , drop = FALSE]
      cspLoci <- cspLoci[cspPos]
      
      ref <- read.csv(tclvalue(refFp), header = TRUE)
      ref <- as.matrix(ref)
      ref <- gsub(" ", "", ref, fixed = TRUE)
      refLoci <- ref[, grep("Marker", colnames(ref))]
      refPos <- !is.element(refLoci, sexChrMar)
      ref <- ref[refPos, , drop = FALSE]
      refLoci <- refLoci[refPos]
      
      af <- read.csv(tclvalue(afFp), header = TRUE)
      af <- as.matrix(af)
      afPos <- !is.element(colnames(af), sexChrMar)
      af <- af[, afPos, drop = FALSE]
      afLoci <- colnames(af)[afPos]
      afLoci <- afLoci[- grep("Allele", colnames(af))]
      
      mcParam <- read.csv(tclvalue(mcFp), header = TRUE)
      mcParam <- as.matrix(mcParam)
      aeParam <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "AE", , drop = FALSE]
      hbParam <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "Hb", , drop = FALSE]
      srB1Param <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "BSR", , drop = FALSE]
      srF1Param <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "FSR", , drop = FALSE]
      srB2Param <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "DSR", , drop = FALSE]
      srM2Param <- mcParam[mcParam[, grep("Factor", colnames(mcParam))] == "MSR", , drop = FALSE]
      
      aeLoci <- aeParam[, grep("Marker", colnames(aeParam))]
      aePos <- !is.element(aeLoci, sexChrMar)
      aeParam <- aeParam[aePos, , drop = FALSE]
      aeLoci <- aeLoci[aePos]
      
      hbLoci <- hbParam[, grep("Marker", colnames(hbParam))]
      hbPos <- !is.element(hbLoci, sexChrMar)
      hbParam <- hbParam[hbPos, , drop = FALSE]
      hbLoci <- hbLoci[hbPos]
      
      srB1Loci <- srB1Param[, grep("Marker", colnames(srB1Param))]
      srB1Pos <- !is.element(srB1Loci, sexChrMar)
      srB1Param <- srB1Param[srB1Pos, , drop = FALSE]
      srB1Loci <- srB1Loci[srB1Pos]
      
      srF1Loci <- srF1Param[, grep("Marker", colnames(srF1Param))]
      srF1Pos <- !is.element(srF1Loci, sexChrMar)
      srF1Param <- srF1Param[srF1Pos, , drop = FALSE]
      srF1Loci <- srF1Loci[srF1Pos]
      
      srB2Loci <- srB2Param[, grep("Marker", colnames(srB2Param))]
      srB2Pos <- !is.element(srB2Loci, sexChrMar)
      srB2Param <- srB2Param[srB2Pos, , drop = FALSE]
      srB2Loci <- srB2Loci[srB2Pos]
      
      srM2Loci <- srM2Param[, grep("Marker", colnames(srM2Param))]
      srM2Pos <- !is.element(srM2Loci, sexChrMar)
      srM2Param <- srM2Param[srM2Pos, , drop = FALSE]
      srM2Loci <- srM2Loci[srM2Pos]
      
      if(all(c(setequal(cspLoci, refLoci), setequal(cspLoci, afLoci), setequal(cspLoci, aeLoci), setequal(cspLoci, hbLoci), setequal(cspLoci, srB1Loci), setequal(cspLoci, srF1Loci), setequal(cspLoci, srB2Loci), setequal(cspLoci, srM2Loci)))){
        alCorJudge <- TRUE
        if(any(is.element(unique(mcParam[, grep("Model", colnames(mcParam))]), c("LUS", "Multi-seq")))){
          if(tclvalue(alCorFp) == ""){
            alCorJudge <- FALSE
            tkmessageBox(message = "Load a file of 'allele repeat correction'!", icon = "error", type = "ok")
          }
        }
        if(alCorJudge){
          refPos <- match(cspLoci, refLoci)
          ref <- ref[refPos, , drop = FALSE]
          
          afPos <- match(cspLoci, afLoci)
          af <- af[, c(1, afPos + 1), drop = FALSE]
          afVal <- as.numeric(af[, -1])
          afVal <- afVal[!is.na(afVal)]
          if(length(which(afVal %% 1 != 0)) == 0){
            tclvalue(afMethVar) <- 1
          }else{
            tclvalue(afMethVar) <- 0
          }
          
          aePos <- match(cspLoci, aeLoci)
          aeParam <- aeParam[aePos, ]
          aeParamVal <- aeParam[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam))), drop = FALSE]
          aeParamVal <- matrix(as.numeric(aeParamVal), nrow = nrow(aeParamVal))
          
          hbPos <- match(cspLoci, hbLoci)
          hbParam <- hbParam[hbPos, ]
          hbParamVal <- hbParam[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam))), drop = FALSE]
          hbParamVal <- matrix(as.numeric(hbParamVal), nrow = nrow(hbParamVal))
          
          srB1Pos <- match(cspLoci, srB1Loci)
          srB1Param <- srB1Param[srB1Pos, ]
          srB1Consider <- !is.element(srB1Param[, grep("Model", colnames(srB1Param))], "No")
          srB1Model <- srB1Param[, grep("Model", colnames(srB1Param))]
          srB1ParamVal <- srB1Param[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam)), grep("Parameter3", colnames(mcParam))), drop = FALSE]
          srB1ParamVal <- matrix(as.numeric(srB1ParamVal), nrow = nrow(srB1ParamVal))
          srB1ParamVal[which(is.na(srB1ParamVal) == TRUE, arr.ind = TRUE)] <- 0
          
          srF1Pos <- match(cspLoci, srF1Loci)
          srF1Param <- srF1Param[srF1Pos, ]
          srF1Consider <- !is.element(srF1Param[, grep("Model", colnames(srF1Param))], "No")
          srF1Model <- srF1Param[, grep("Model", colnames(srF1Param))]
          srF1ParamVal <- srF1Param[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam)), grep("Parameter3", colnames(mcParam))), drop = FALSE]
          srF1ParamVal <- matrix(as.numeric(srF1ParamVal), nrow = nrow(srF1ParamVal))
          srF1ParamVal[which(is.na(srF1ParamVal) == TRUE, arr.ind = TRUE)] <- 0
          
          srB2Pos <- match(cspLoci, srB2Loci)
          srB2Param <- srB2Param[srB2Pos, ]
          srB2Consider <- !is.element(srB2Param[, grep("Model", colnames(srB2Param))], "No")
          srB2Model <- srB2Param[, grep("Model", colnames(srB2Param))]
          srB2ParamVal <- srB2Param[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam)), grep("Parameter3", colnames(mcParam))), drop = FALSE]
          srB2ParamVal <- matrix(as.numeric(srB2ParamVal), nrow = nrow(srB2ParamVal))
          srB2ParamVal[which(is.na(srB2ParamVal) == TRUE, arr.ind = TRUE)] <- 0
          
          srM2Pos <- match(cspLoci, srM2Loci)
          srM2Param <- srM2Param[srM2Pos, ]
          srM2Consider <- !is.element(srM2Param[, grep("Model", colnames(srM2Param))], "No")
          srM2Model <- srM2Param[, grep("Model", colnames(srM2Param))]
          srM2ParamVal <- srM2Param[, c(grep("Parameter1", colnames(mcParam)), grep("Parameter2", colnames(mcParam))), drop = FALSE]
          srM2ParamVal <- matrix(as.numeric(srM2ParamVal), nrow = nrow(srM2ParamVal))
          srM2ParamVal[which(is.na(srM2ParamVal) == TRUE, arr.ind = TRUE)] <- 0
          
          colnames(aeParamVal) <- colnames(hbParamVal) <- c("meanlog", "variance")
          rownames(aeParamVal) <- rownames(hbParamVal) <- rownames(srB1ParamVal) <- rownames(srF1ParamVal) <- rownames(srB2ParamVal) <- rownames(srM2ParamVal) <- cspLoci
          colnames(srB1ParamVal) <- colnames(srF1ParamVal) <- colnames(srB2ParamVal) <- c("slope", "intercept", "variance")
          colnames(srM2ParamVal) <- c("mean", "variance")
          
          srConsider <- cbind(srB1Consider, srF1Consider, srB2Consider, srM2Consider)
          srModel <- cbind(srB1Model, srF1Model, srB2Model, srM2Model)
          colnames(srConsider) <- c("srB1Consider", "srF1Consider", "srB2Consider", "srM2Consider")
          colnames(srModel) <- c("srB1Model", "srF1Model", "srB2Model", "srM2Model")
          rownames(srConsider) <- rownames(srModel) <- cspLoci
          
          repLengthAll <- as.numeric(aeParam[, grep("Repeat", colnames(aeParam))])
          names(repLengthAll) <- cspLoci
          
          numKnown <- (ncol(ref) - 1) / 2
          for(i in 1:numKnown){
            hpVars[[i]] <- tclVar("0")
            hdVars[[i]] <- tclVar("0")
          }
          
          dyeAllL <- aeParam[, grep("Dye", colnames(aeParam))]
          names(dyeAllL) <- cspLoci
          dyeNames <- unique(dyeAllL)
          for(i in 1:length(dyeNames)){
            atVars[[i]] <- tclVar("100")
          }
          names(atVars) <- dyeNames
          
          tclvalue(fileCkFin) <- 1
          tab2Make(csp, ref, af, aeParamVal, hbParamVal, srB1ParamVal, srF1ParamVal, srB2ParamVal, srM2ParamVal, srConsider, srModel, repLengthAll, dyeAllL, hpVars, hdVars, atVars)
          tk2notetab.select(tabs, "Calculation")
        }
      }else{
        tkmessageBox(message = "Names of loci are not equal in each file!", icon = "error", type = "ok")
      }
    }
  }
  
  #####################
  # 'Calculation' tab #
  #####################
  
  #Correct repeat number of variant alleles
  variantCor <- function(peakOne, repLength){
    corVal <- round(peakOne %% 1 * 10, 0)
    peakCor <- peakOne %/% 1 + round(corVal / repLength, 2)
    return(peakCor)
  }
  
  #Calculate template amount
  tempAmountCalc <- function(heightAllL){
    heightAllL[which(is.na(heightAllL) == TRUE, arr.ind = TRUE)] <- 0
    return(apply(heightAllL, 1, sum))
  }
  
  #Make population frequencies
  popFreqMake <- function(afOneL, afAlOneL, afMeth, maf = NULL, unobsAl = NULL){
    nUnobs <- length(unobsAl)
    if(nUnobs > 0){
      afAlOneL <- c(afAlOneL, unobsAl)
      if(afMeth == 1){
        afOneL <- c(afOneL, rep(0, nUnobs))
        afOneL <- (afOneL + 1) / (sum(afOneL) + length(afOneL))
      }else{
        afOneL <- c(afOneL, rep(maf, nUnobs))
      }
      afAlOneL2 <- afAlOneL[order(afAlOneL)]
      afOneL <- afOneL[order(afAlOneL)]
    }else{
      afAlOneL2 <- afAlOneL
      if(afMeth == 1){
        afOneL <- (afOneL + 1) / (sum(afOneL) + length(afOneL))
      }
    }
    return(list(afOneL, afAlOneL2))
  }
  
  #Extract drop-out alleles from reference profiles
  dropExt <- function(peakOneL, refOneL){
    refAl <- sort(unique(refOneL))
    return(refAl[!is.element(refAl, peakOneL)])
  }
  
  #Estimate size of a drop-out allele
  sizeDropEst <- function(dropOneAl, peakOneL, sizeOneL, repLength){
    peakOneLCor <- sapply(peakOneL, variantCor, repLength = repLength)
    dropOneAlCor <- variantCor(dropOneAl, repLength)
    distance <- abs(peakOneLCor - dropOneAlCor)
    basePeakPos <- which(distance == min(distance))
    basePeak <- peakOneLCor[basePeakPos[1]]
    baseSize <- sizeOneL[basePeakPos[1]]
    if(basePeak < dropOneAlCor){
      sizeDrop <- baseSize + repLength * min(distance) 
    }else{
      sizeDrop <- baseSize - repLength * min(distance)
    }
    return(sizeDrop)
  }
  
  #Estimate size of a drop-out allele in the case of locus drop-out
  sizeDropEst2 <- function(dropOneAl, kitAl, kitSize, repLength){
    dropAlPos <- which(kitAl == dropOneAl)
    if(length(dropAlPos) == 1){
      return(kitSize[dropAlPos])
    }else{
      kitAlCor <- sapply(kitAl, variantCor, repLength = repLength)
      dropOneAlCor <- variantCor(dropOneAl, repLength)
      distance <- abs(kitAlCor - dropOneAlCor)
      basePeakPos <- which(distance == min(distance))
      basePeak <- kitAlCor[basePeakPos[1]]
      baseSize <- kitSize[basePeakPos[1]]
      if(basePeak < dropOneAlCor){
        sizeDrop <- baseSize + repLength * min(distance)
      }else{
        sizeDrop <- baseSize - repLength * min(distance)
      }
      return(sizeDrop)
    }
  }
  
  #Arrange imput data for calculation
  dataArrange <- function(csp, ref, af, atVals, dyeAllL, repLengthAll, afMeth, maf, alCor = NULL, srModel = NULL){
    srXmake <- function(srId){
      modelName <- srModelOneL[srId]
      if(modelName == "Allele"){
        srX <- peakOneL
      }else if(modelName == "LUS"){
        peakOneL2 <- peakOneL
        peakOneL2[peakOneL == 99] <- alQOneL
        srX <- as.numeric(alCorOneL[which(as.numeric(alCorOneL[, 2]) %in% peakOneL2), 3])
      }else if(modelName == "Multi-seq"){
        peakOneL2 <- peakOneL
        peakOneL2[peakOneL == 99] <- alQOneL
        srX <- as.numeric(alCorOneL[which(as.numeric(alCorOneL[, 2]) %in% peakOneL2), 3 + srId])
      }else{
        srX <- numeric(0)
      }
      return(srX)
    }
    
    colCsp <- colnames(csp)
    colRef <- colnames(ref)
    peakAllL <- csp[, grep("Allele", colCsp)]
    peakAllL <- matrix(as.numeric(peakAllL), nrow = nrow(peakAllL))
    sizeAllL <- csp[, grep("Size", colCsp)]
    sizeAllL <- matrix(as.numeric(sizeAllL), nrow = nrow(sizeAllL))
    sizeVec <- as.numeric(sizeAllL)
    sizeVec <- sizeVec[!is.na(sizeVec)]
    heightAllL <- csp[, grep("Height", colCsp)]
    heightAllL <- matrix(as.numeric(heightAllL), nrow = nrow(heightAllL))
    heightVec <- as.numeric(heightAllL)
    heightVec <- heightVec[!is.na(heightVec)]
    tempPerL <- tempAmountCalc(heightAllL)
    lociName <- csp[, grep("Marker", colCsp)]
    refAllL <- ref[, - grep("Marker", colRef)]
    refAllL <- matrix(as.numeric(refAllL), nrow = nrow(refAllL))
    colnames(refAllL) <- colRef[ -grep("Marker", colRef)]
    rownames(refAllL) <- lociName
    afAl <- af[, grep("Allele", colnames(af))]
    
    cspPeak <- cspSize <- cspHeight <- list()
    nL <- length(lociName)
    atAllL <- alQ <- QFreqAll <- rep(0, nL)
    popAlList <- popFreqList <- list()
    srB1Peak <- srF1Peak <- srB2Peak <- list()
    noSeqInfo <- rep("", nL)
    
    for(i in 1:nL){
      lociNameOne <- lociName[i]
      peakOneL <- peakAllL[i, !is.na(peakAllL[i, ])]
      sizeOneL <- sizeAllL[i, !is.na(sizeAllL[i, ])]
      heightOneL <- heightAllL[i, !is.na(heightAllL[i, ])]
      dyePos <- names(atVals) == dyeAllL[i]
      atAllL[i] <- atVals[dyePos]
      peakPos <- heightOneL >= atAllL[i]
      peakOneL <- peakOneL[peakPos]
      sizeOneL <- sizeOneL[peakPos]
      heightOneL <- heightOneL[peakPos]
      refOneL <- refAllL[i, ]
      
      afOneL <- af[, i + 1]
      afAlPos <- !is.na(afOneL)
      afAlOneL <- afAl[afAlPos]
      afOneL <- afOneL[afAlPos]
      unobsAl <- setdiff(unique(c(peakOneL, refOneL)), afAlOneL)
      popFreqData <- popFreqMake(afOneL, afAlOneL, afMeth, maf, unobsAl)
      popFreq <- popFreqData[[1]]
      popAl <- popFreqData[[2]]
      repLength <- repLengthAll[i]
      srModelOneL <- srModel[i, ]
      
      if(length(peakOneL) > 0){
        dropAl <- dropExt(peakOneL, refOneL)
        nD <- length(dropAl)
        if(nD > 0){
          sizeDrop <- sapply(dropAl, sizeDropEst, peakOneL = peakOneL, sizeOneL = sizeOneL, repLength = repLength)
          sizeOneL <- c(sizeOneL, sizeDrop)
          peakOneL <- c(peakOneL, dropAl)
          heightOneL <- c(heightOneL, rep(0, nD))
        }
      }else{
        dropAl <- sort(unique(refOneL))
        if(setequal(lociName, lociID)){
          posLID <- which(lociID == lociNameOne)
          kitAl <- alleleID[[posLID]]
          kitSize <- sizeID[[posLID]]
        }else if(setequal(lociName, lociGF)){
          posLGF <- which(lociGF == lociNameOne)
          kitAl <- alleleGF[[posLGF]]
          kitSize <- sizeGF[[posLGF]]
        }else{
          stop("ERROR: Sizes of drop-out alleles cannot be determined!")
        }
        sizeOneL <- sapply(dropAl, sizeDropEst2, kitAl, kitSize, repLength)
        peakOneL <- dropAl
        heightOneL <- rep(0, length(dropAl))
      }
      
      alQCandPos <- !is.element(popAl, peakOneL)
      alQCand <- popAl[alQCandPos]
      nQ <- length(alQCand)
      if(nQ > 0){
        alQ[i] <- alQOneL <- alQCand[which(popFreq[alQCandPos] == max(popFreq[alQCandPos]))[1]]
        sizeQ <- sizeDropEst(alQOneL, peakOneL, sizeOneL, repLength)
        QFreqAll[i] <- sum(popFreq[alQCandPos])
        peakOneL <- c(peakOneL, alQOneL)
        sizeOneL <- c(sizeOneL, sizeQ)
        heightOneL <- c(heightOneL, 0)
      }
      
      if(length(alCor) > 0){
        alCorOneL <- alCor[alCor[, grep("Marker", colnames(alCor))] == lociNameOne, ]
        seqAlOneL <- as.numeric(alCorOneL[, grep("Allele", colnames(alCorOneL))])
        noSeqAl <- setdiff(unique(c(peakOneL, refOneL)), seqAlOneL)
        noSeqJudge <- TRUE
        nNoSeq <- length(noSeqAl)
        if(nNoSeq > 0){
          srB1SeqJudge <- is.element(srModelOneL[1], c("LUS", "Multi-seq"))
          srF1SeqJudge <- is.element(srModelOneL[2], c("LUS", "Multi-seq"))
          srB2SeqJudge <- is.element(srModelOneL[3], c("LUS", "Multi-seq"))
          if(any(c(srB1SeqJudge, srF1SeqJudge, srB2SeqJudge))){
            noSeqJudge <- FALSE
            noSeqInfoOneL <- paste(lociNameOne, ": ", sep = "")
            for(j in 1:nNoSeq){
              noSeqInfoOneL <- paste(noSeqInfoOneL, noSeqAl[j], " ", sep = "")
            }
            noSeqInfo[i] <- noSeqInfoOneL
          }
        }
      }
      
      if(nQ > 0){
        peakOneL[length(peakOneL)] <- 99
      }
      
      if(noSeqJudge){
        srB1Peak[[i]] <- srXmake(1)
        srF1Peak[[i]] <- srXmake(2)
        srB2Peak[[i]] <- srXmake(3)
      }else{
        srB1Peak[[i]] <- srF1Peak[[i]] <- srB2Peak[[i]] <- numeric(0)
      }
      cspPeak[[i]] <- peakOneL
      cspSize[[i]] <- sizeOneL
      cspHeight[[i]] <- heightOneL
      popAlList[[i]] <- popAl
      popFreqList[[i]] <- popFreq
    }
    names(cspPeak) <- names(cspSize) <- names(cspHeight) <- names(popAlList) <- names(popFreqList) <- names(alQ) <- names(QFreqAll) <- lociName
    return(list(cspPeak, cspSize, cspHeight, refAllL, popAlList, popFreqList, alQ, QFreqAll, atAllL, tempPerL, srB1Peak, srF1Peak, srB2Peak, noSeqInfo))
  }
  
  #Extract calculation results of Hp and Hd from 'resultAllHnc' (including results of all hypotheses)
  resultExt <- function(resultAllHnc, nKnown, HpKnownID, HdKnownID, hncFrom, hncTo, hypIdAllList){
    resultHp <- resultHd <- list()
    countHnc <- 0
    for(i in hncFrom:hncTo){
      countHnc <- countHnc + 1
      hypIdUnique <- sapply(sapply(hypIdAllList[[countHnc]], as.numeric), unique)
      if(i - length(HpKnownID) >= 0){
        resultHp[[countHnc]] <- resultAllHnc[[countHnc]][[which(sapply(hypIdUnique, setequal, c(HpKnownID, rep(0, i - length(HpKnownID)))) == TRUE)]]
      }else{
        resultHp[[countHnc]] <- NULL
      }
      if(i - length(HdKnownID) >= 0){
        resultHd[[countHnc]] <- resultAllHnc[[countHnc]][[which(sapply(hypIdUnique, setequal, c(HdKnownID, rep(0, i - length(HdKnownID)))) == TRUE)]]
      }else{
        resultHd[[countHnc]] <- NULL
      }
    }
    return(list(resultHp, resultHd))
  }
  
  #Make spinbox (GUI)
  tkspinbox <- function(parent, ...){
    tkwidget(parent, "tk::spinbox", ...)
  }
  
  #Make 'Calculation' tab
  tab2Make <- function(csp = NULL, ref = NULL, af = NULL, aeParamVal = NULL, hbParamVal = NULL, srB1ParamVal = NULL, srF1ParamVal = NULL, srB2ParamVal = NULL, srM2ParamVal = NULL, srConsider = NULL, srModel = NULL, repLengthAll = NULL, dyeAllL = NULL, hpVars = NULL, hdVars = NULL, atVars = NULL){
    
    #Customize mixture ratio of one contributor
    mrCustomize <- function(){
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
        nMrOne <- length(mrDefault)
        for(i in 1:nMrOne){
          tkinsert(mrListbox, "end", mrDefault[i])
        }
      }
      
      mrAdd <- function(){
        mrNew <- tclvalue(mrAddVar)
        if(mrNew != ""){
          tkinsert(mrListbox, "end", tclvalue(mrAddVar))
        }
      }
      
      mrDelete <- function(){
        selectPos <- tkcurselection(mrListbox)
        selectPos <- tclvalue(selectPos)
        if(selectPos != ""){
          tkdelete(mrListbox, selectPos)
        }
      }
      
      mrSave <- function(){
        mrList <- tclvalue(mrListVar)
        mrList <- unlist(strsplit(mrList, " "))
        mrOne <<- sort(as.numeric(mrList))
        tkdestroy(mr.tf)
      }
      
      mrListVar <- tclVar("")
      mrAddVar <- tclVar("")
      mr.tf <- tktoplevel()
      tkwm.title(mr.tf, "Customize mixture ratio of one contributor")
      mrListFrame <- tkframe(mr.tf)
      mrListbox <- tk2listbox(mrListFrame, listvariable = mrListVar, height = 10, justify = "center", selectmode = "single")
      tkgrid(tklabel(mrListFrame, text = "    mixture ratio"), padx = 20, sticky = "w")
      tkgrid(mrListbox, padx = 20, sticky = "w")
      nMrOne <- length(mrOne)
      for(i in 1:nMrOne){
        tkinsert(mrListbox, "end", mrOne[i])
      }
      mrButtFrame <- tkframe(mrListFrame)
      mrSortButt <- tkbutton(mrButtFrame, text = " Sort ", command = function() mrSort())
      mrRestoreButt <- tkbutton(mrButtFrame, text = " Restore Defaults ", command = function() mrRestore())
      tkgrid(mrSortButt, mrRestoreButt, padx = 10)
      tkgrid(mrButtFrame, pady = 10)
      
      mrSetFrame <- tkframe(mr.tf)
      mrAddFrame <- tkframe(mrSetFrame)
      mrAddEntry <- tkentry(mrAddFrame, textvariable = mrAddVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      mrAddButt <- tkbutton(mrAddFrame, text = " Add ", command = function() mrAdd())
      mrDeleteFrame <- tkframe(mrSetFrame)
      mrDeleteButt <- tkbutton(mrDeleteFrame, text = " Delete ", command = function() mrDelete())
      mrSaveFrame <- tkframe(mrSetFrame)
      mrSaveButt <- tkbutton(mrSaveFrame, text = "Save", command = function() mrSave())
      tkgrid(tklabel(mrSetFrame, text = "Add an entered value to the list of mixture ratio"), padx = 20, sticky = "w")
      tkgrid(mrAddEntry, mrAddButt, padx = 20)
      tkgrid(mrAddFrame, padx = 20, sticky = "w")
      tkgrid(tklabel(mrSetFrame, text = ""), padx = 20, sticky = "w")
      tkgrid(tklabel(mrSetFrame, text = "Delete a selected mixture ratio"), padx = 20, sticky = "w")
      tkgrid(tklabel(mrDeleteFrame, text = "                    "), mrDeleteButt, padx = 20)
      tkgrid(mrDeleteFrame, padx = 20, sticky = "w")
      tkgrid(tklabel(mrSetFrame, text = ""), padx = 20, sticky = "w")
      tkgrid(tklabel(mrSetFrame, text = "Save the customized mixture ratio"), padx = 20, sticky = "w")
      tkgrid(tklabel(mrSaveFrame, text = "                    "), mrSaveButt, padx = 20)
      tkgrid(mrSaveFrame, padx = 20, sticky = "w")
      tkgrid(mrListFrame, mrSetFrame, pady = 10)
    }
    
    #Make a window for setting other parameters
    paramSetting <- function(){
      tfParamSet <- tktoplevel()
      tkwm.title(tfParamSet, "parameter setting")
      frameParamSet <- tkframe(tfParamSet)
      
      mcLabel <- tklabel(frameParamSet, text = "Number of Monte Carlo simulations")
      mcEntry <- tkentry(frameParamSet, textvariable = numMcVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      
      afLabel <- tklabel(frameParamSet, text = "Allele frequencies")
      mafLabel <- tklabel(frameParamSet, text = "                - Minimum allele frequency")
      if(tclvalue(afMethVar) == 1){
        dirRadioButt <- tkradiobutton(frameParamSet, anchor = "w", width = 40, state = "normal", text = "Dirichlet distribution", variable = afMethVar, value = 1)
        obsRadioButt <- tkradiobutton(frameParamSet, anchor = "w", width = 40, state = "disable", text = "Observed frequencies", variable = afMethVar, value = 0)
        mafEntry <- tkentry(frameParamSet, textvariable = mafVar, width = 10, state = "disable", highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      }else{
        dirRadioButt <- tkradiobutton(frameParamSet, anchor = "w", width = 40, state = "disable", text = "Dirichlet distribution", variable = afMethVar, value = 1)
        obsRadioButt <- tkradiobutton(frameParamSet, anchor = "w", width = 40, state = "normal", text = "Observed frequencies", variable = afMethVar, value = 0)
        mafEntry <- tkentry(frameParamSet, textvariable = mafVar, width = 10, state = "normal", highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      }
      
      mrLabel <- tklabel(frameParamSet, text = "Mixture ratio (MR)")
      mrCusButt <- tkbutton(frameParamSet, text = "    Customize    ", cursor = "hand2", state = "normal", command = function() mrCustomize())
      
      degLabel <- tklabel(frameParamSet, text = "Degradation (d)")
      degWLabel <- tklabel(frameParamSet, text = "        - Width")
      degWEntry <- tkentry(frameParamSet, textvariable = degWVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      
      gtCombExcludeLab <- tklabel(frameParamSet, text = "Thresholds to exclude unrealistic genotype combinations")
      stLabel <- tklabel(frameParamSet, text = "        - Stochastic threshold")
      stEntry <- tkentry(frameParamSet, textvariable = stVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      hbFltrLabel <- tklabel(frameParamSet, text = "        - Heterozygote balance filter")
      hbFltrEntry <- tkentry(frameParamSet, textvariable = hbFltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      stB1FltrLabel <- tklabel(frameParamSet, text = "        - Back stutter filter")
      stB1FltrEntry <- tkentry(frameParamSet, textvariable = stB1FltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      stF1FltrLabel <- tklabel(frameParamSet, text = "        - Forward stutter filter")
      stF1FltrEntry <- tkentry(frameParamSet, textvariable = stF1FltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      stB2FltrLabel <- tklabel(frameParamSet, text = "        - Double-back stutter filter")
      stB2FltrEntry <- tkentry(frameParamSet, textvariable = stB2FltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      stM2FltrLabel <- tklabel(frameParamSet, text = "        - Minus 2 base stutter filter")
      stM2FltrEntry <- tkentry(frameParamSet, textvariable = stM2FltrVar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
      
      tkgrid(mcLabel, mcEntry, padx = 20, sticky = "w")
      tkgrid(afLabel, padx = 20, sticky = "w")
      tkgrid(dirRadioButt, padx = 40, sticky = "w")
      tkgrid(obsRadioButt, padx = 40, sticky = "w")
      tkgrid(mafLabel, mafEntry, padx = 20, sticky = "w")
      tkgrid(mrLabel, mrCusButt, padx = 20, sticky = "w")
      tkgrid(degLabel, padx = 20, sticky = "w")
      tkgrid(degWLabel, degWEntry, padx = 20, sticky = "w")
      tkgrid(gtCombExcludeLab, padx = 20, sticky = "w")
      tkgrid(stLabel, stEntry, padx = 20, sticky = "w")
      tkgrid(hbFltrLabel, hbFltrEntry, padx = 20, sticky = "w")
      tkgrid(stB1FltrLabel, stB1FltrEntry, padx = 20, sticky = "w")
      tkgrid(stF1FltrLabel, stF1FltrEntry, padx = 20, sticky = "w")
      tkgrid(stB2FltrLabel, stB2FltrEntry, padx = 20, sticky = "w")
      tkgrid(stM2FltrLabel, stM2FltrEntry, padx = 20, sticky = "w")
      tkgrid(frameParamSet, pady = 10)
    }
    
    #Reset parameter
    resetParam <- function(){
      imputOk <- tclvalue(tkmessageBox(message = "Calculation result for current crime stain profile will be deleted. Do you want to continue?", type = "okcancel", icon = "warning"))
      if(imputOk == "ok"){
        tclvalue(calcFin) <- 0
        tclvalue(hncFromSpinVar) <- "normal"
        tclvalue(hncToSpinVar) <- "normal"
        tclvalue(atEntryVar) <- "normal"
        tclvalue(thetaEntryVar) <- "normal"
        tclvalue(otherArrowVar) <- "hand2"
        tclvalue(otherButtVar) <- "normal"
        tclvalue(resetArrowVar) <- "arrow"
        tclvalue(resetButtVar) <- "disable"
        fileCk()
        tab3Make()
        tab4Make()
      }
    }
    
    #Prepare and finish calculation
    calcFunc <- function(){
      hncFrom <- as.numeric(tclvalue(hncFromVar))
      hncTo <- as.numeric(tclvalue(hncToVar))
      atVals <- as.numeric(sapply(atVars, tclvalue))
      names(atVals) <- names(atVars)
      degW <- abs(as.numeric(tclvalue(degWVar)))
      afMeth <- as.numeric(tclvalue(afMethVar))
      if(afMeth == 1){
        maf <- 1
      }else{
        maf <- as.numeric(tclvalue(mafVar))
      }
      theta <- as.numeric(tclvalue(thetaVar))
      numMc <- as.numeric(tclvalue(numMcVar))
      mrDegCut <- as.numeric(tclvalue(mrDegCutVar))
      st <- as.numeric(tclvalue(stVar))
      hbFltr <- as.numeric(tclvalue(hbFltrVar))
      stFltr <- c(as.numeric(tclvalue(stB1FltrVar)), as.numeric(tclvalue(stF1FltrVar)), as.numeric(tclvalue(stB2FltrVar)), as.numeric(tclvalue(stM2FltrVar)))
      names(stFltr) <- c("stB1Fltr", "stF1Fltr", "stB2Fltr", "stM2Fltr")
      calcCond <- c(hncFrom, hncTo, atVals, degW, afMeth, maf, theta, numMc, mrDegCut, st, hbFltr, stFltr)
      names(calcCond) <- c("hncFrom", "hncTo", names(atVals), "degW", "afMeth", "maf", "theta", "numMc", "mrDegCut", "st", "hbFltr", names(stFltr))
      alCorJudge <- TRUE
      
      if(tclvalue(alCorFp) != ""){
        alCor <- read.csv(tclvalue(alCorFp), header = TRUE)
        alCor <- as.matrix(alCor)
      }else{
        alCor <- NULL
      }
      dataAll <- dataArrange(csp, ref, af, atVals, dyeAllL, repLengthAll, afMeth, maf, alCor, srModel)
      cspPeak <- dataAll[[1]]
      cspSize <- dataAll[[2]]
      cspHeight <- dataAll[[3]]
      refAllL <- dataAll[[4]]
      popAlList <- dataAll[[5]]
      popFreqList <- dataAll[[6]]
      alQ <- dataAll[[7]]
      QFreqAll <- dataAll[[8]]
      atAllL <- dataAll[[9]]
      tempPerL <- dataAll[[10]]
      srB1Peak <- dataAll[[11]]
      srF1Peak <- dataAll[[12]]
      srB2Peak <- dataAll[[13]]
      noSeqInfo <- dataAll[[14]]
      
      if(tclvalue(calcFin) == "0"){
        if(!all(is.element(noSeqInfo, ""))){
          alCorJudge <- FALSE
          noSeqInfo <- noSeqInfo[noSeqInfo != ""]
          noSeqMessage <- paste(noSeqInfo[1])
          if(length(noSeqInfo) >= 2){
            for(i in 2:length(noSeqInfo)){
              noSeqMessage <- paste(noSeqMessage, "\n", noSeqInfo[i], sep = "")
            }
          }
          tkmessageBox(message = paste("There is a lack of information about allele repeat correction of the following allele(s): ", "\n", noSeqMessage, sep = ""), icon = "error", type = "ok")
        }
        
        if(alCorJudge){
          resultData <- cspInterpret(cspPeak, cspSize, cspHeight, refAllL, popAlList, popFreqList, QFreqAll, atAllL, tempPerL, srB1Peak, srF1Peak, srB2Peak, aeParamVal, hbParamVal, srB1ParamVal, srF1ParamVal, srB2ParamVal, srM2ParamVal, srConsider, repLengthAll, hncFrom, hncTo, mrOne, degW, theta, numMc, mrDegCut, st, hbFltr, stFltr)
          resultAllHnc <<- resultData[[1]]
          gammaAllList <<- resultData[[2]]
          hypIdAllList <<- resultData[[3]]
          calcTime <<- resultData[[4]]
          mrOneCList <<- resultData[[5]]
          degOneC <<- resultData[[6]]
        }
      }
      if(alCorJudge){
        if(length(resultAllHnc) == 0){
          tkmessageBox(message = "Your crime stain profile cannot be explained by your setting hypotheses. Please recalculate after setting appropriate hypotheses.", icon = "error", type = "ok")
        }else{
          HpKnownID <- setdiff(which(as.numeric(sapply(hpVars, tclvalue)) == 1), 0)
          HdKnownID <- setdiff(which(as.numeric(sapply(hdVars, tclvalue)) == 1), 0)
          resultHpHd <- resultExt(resultAllHnc, numKnown, HpKnownID, HdKnownID, hncFrom, hncTo, hypIdAllList)
          resultHp <- resultHpHd[[1]]
          resultHd <- resultHpHd[[2]]
          tclvalue(calcFin) <- 1
          tclvalue(hncFromSpinVar) <- "disable"
          tclvalue(hncToSpinVar) <- "disable"
          tclvalue(atEntryVar) <- "disable"
          tclvalue(thetaEntryVar) <- "disable"
          tclvalue(locusSetVar) <- "disable"
          tclvalue(paramSetVar) <- "disable"
          tclvalue(otherArrowVar) <- "arrow"
          tclvalue(otherButtVar) <- "disable"
          tclvalue(resetArrowVar) <- "hand2"
          tclvalue(resetButtVar) <- "normal"
          tab2Make(csp, ref, af, aeParamVal, hbParamVal, srB1ParamVal, srF1ParamVal, srB2ParamVal, srM2ParamVal, srConsider, srModel, repLengthAll, dyeAllL, hpVars, hdVars, atVars)
          tab3Make(resultHp, resultHd, cspPeak, cspHeight, gammaAllList, dyeAllL, calcCond, mrOneCList, degOneC, repLengthAll)
          tab4Make(resultHp, resultHd, calcCond, mrOne, unique(dyeAllL), calcTime)
          tk2notetab.select(tabs, "Likelihood ratio")
        }
      }
    }
    
    tkdestroy(frameTab2)
    frameTab2 <<- tkframe(tab2)
    if(tclvalue(cspName) == ""){
      tkgrid(tklabel(frameTab2, text = "        Load crime stain profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(refName) == ""){
      tkgrid(tklabel(frameTab2, text = "        Load reference profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(afName) == ""){
      tkgrid(tklabel(frameTab2, text = "        Load allele frequencies!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(mcName) == ""){
      tkgrid(tklabel(frameTab2, text = "        Load Monte Carlo Parameters!        "), pady = 10, sticky = "w")
    }
    if(all(c(tclvalue(cspName), tclvalue(refName), tclvalue(afName), tclvalue(mcName)) != "")){
      if(tclvalue(fileCkFin) == "0"){
        tkgrid(tklabel(frameTab2, text = "        Push 'Next' button in the Files tab.        "), pady = 10, sticky = "w")
      }else{
        frameTab2_hypotheses <- tkframe(frameTab2)
        frameTab2_Hp <- tkframe(frameTab2_hypotheses, relief = "groove", borderwidth = 2)
        frameTab2_Hd <- tkframe(frameTab2_hypotheses, relief = "groove", borderwidth = 2)
        labelHp <- tklabel(frameTab2_Hp, text = "Prosecutor hypothesis", font = "Helvetica 10 bold")
        labelHd <- tklabel(frameTab2_Hd, text = "Defense hypothesis", font = "Helvetica 10 bold")
        frameHpKnown <- tkframe(frameTab2_Hp)
        frameHdKnown <- tkframe(frameTab2_Hd)
        
        numKnown <- (ncol(ref) - 1) / 2
        nameKnown <- colnames(ref[, 2:(numKnown * 2 + 1)])[2:(numKnown * 2 + 1) %% 2 == 0]
        for(i in 1:length(hpVars)){
          tkgrid(tkcheckbutton(frameHpKnown, text = paste(nameKnown[i]), variable = hpVars[[i]]), sticky = "w")
          tkgrid(tkcheckbutton(frameHdKnown, text = paste(nameKnown[i]), variable = hdVars[[i]]), sticky = "w")
        }
        tkgrid(labelHp, padx = 20)
        tkgrid(labelHd, padx = 20)
        tkgrid(frameHpKnown, padx = 20)
        tkgrid(frameHdKnown, padx = 20)
        tkgrid(tklabel(frameTab2_Hp, text = "(+ unknown contributor(s))"))
        tkgrid(tklabel(frameTab2_Hd, text = "(+ unknown contributor(s))"))
        tkgrid(frameTab2_Hp, frameTab2_Hd, padx = 20, pady = 10)
        tkgrid(frameTab2_hypotheses, padx = 20, pady = 10)
        
        frameTab2_param <- tkframe(frameTab2, relief = "groove", borderwidth = 2)
        tkgrid(tklabel(frameTab2_param, text = "Parameters", font = "Helvetica 10 bold"), sticky = "w")
        frameTab2_param1 <- tkframe(frameTab2_param)
        hncFromSpin <- tkspinbox(frameTab2_param1, from = 1, to = 4, increment = 1, textvariable = hncFromVar, width = 6, highlightthickness = 1, justify = "center", background = "white", state = tclvalue(hncFromSpinVar))
        hncToSpin <- tkspinbox(frameTab2_param1, from = 1, to = 4, increment = 1, textvariable = hncToVar, width = 6, highlightthickness = 1, justify = "center", background = "white", state = tclvalue(hncToSpinVar))
        tkgrid(tklabel(frameTab2_param1, text = "Number of contributors"), sticky = "w")
        tkgrid(tklabel(frameTab2_param1, text = "    From"), hncFromSpin, sticky = "w")
        tkgrid(tklabel(frameTab2_param1, text = "    To"),  hncToSpin, sticky = "w")
        thetaEntry <- tkentry(frameTab2_param1, textvariable = thetaVar, width = 6, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = tclvalue(thetaEntryVar))
        tkgrid(tklabel(frameTab2_param1, text = "Theta"), thetaEntry, sticky = "w")
        
        frameTab2_param2 <- tkframe(frameTab2_param)
        tkgrid(tklabel(frameTab2_param2, text = "Analytical threshold"), sticky = "w")
        dyeNames <- unique(dyeAllL)
        for(i in 1:length(atVars)){
          tkgrid(tklabel(frameTab2_param2, text = dyeNames[i]), tkentry(frameTab2_param2, textvariable = atVars[[i]], width = 6, highlightthickness = 1, relief = "solid", justify = "center", background = "white", state = tclvalue(atEntryVar)), padx = 20, sticky = "w")
        }
        tkgrid(frameTab2_param1, frameTab2_param2, padx = 20, pady = 10, sticky = "nw")
        
        otherButt <- tkbutton(frameTab2_param, text = "    Others    ", cursor = tclvalue(otherArrowVar), state = tclvalue(otherButtVar), command = function() paramSetting())
        resetButt <- tkbutton(frameTab2_param, text = "    Reset    ", cursor = tclvalue(resetArrowVar), state = tclvalue(resetButtVar), command = function() resetParam())
        tkgrid(otherButt, resetButt, padx = 20, pady = 10, sticky = "w")
        tkgrid(frameTab2_param, padx = 20, pady = 10)
        tkgrid(tkbutton(frameTab2, text = "    Calculate    ", cursor = "hand2", command = function() calcFunc()), padx = 20, pady = 10)
      }
    }
    tkgrid(frameTab2)
  }
  
  
  ####################
  # Main calculation #
  ####################
  
  #Set mixture ratio
  mrSetting <- function(hnc, mrOne){
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
  
  #Set degradation parameter of one contributor
  degOneCMake <- function(cspSize, cspHeight, tempPerL, degVal){
    sizeMeanCalc <- function(sizeVec, heightVec){
      sum(sizeVec * heightVec) / sum(heightVec)
    }
    
    lsDeg <- function(d, sizeVec, sizeMean, heightVec, tempMean){
      sum((exp(d * (sizeVec - sizeMean)) * tempMean / 2 - heightVec)^2)
    }
    
    sizeVec <- unlist(cspSize)
    heightVec <- unlist(cspHeight)
    sizeMean <- sizeMeanCalc(sizeVec, heightVec)
    tempMean <- mean(tempPerL)
    degError <- sapply(degVal, lsDeg, sizeVec = sizeVec, sizeMean = sizeMean, heightVec = heightVec, tempMean = tempMean)
    degOneContrib <- degVal[is.element(rank(degError), 1:3)]
    return(list(degOneContrib, sizeMean))
  }
  
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
  
  #Estimate expected peak heights
  ephEstimate <- function(peakOneL, sizeOneL, numMc, tempMean, mrOneC, degOneC, sizeMean, srB1PeakOneL, srF1PeakOneL, srB2PeakOneL, aeParamOneL, hbParamOneL, srB1ParamOneL, srF1ParamOneL, srB2ParamOneL, srM2ParamOneL){
    
    #Make conditions of mr, peak name, and d
    paramCondMake <- function(mrOneC, peakOneL, degOneC){
      numMr <- length(mrOneC)
      numPeak <- length(peakOneL)
      numDeg <- length(degOneC)
      numCond <- numMr * numPeak * numDeg
      paramCond <- matrix(0, 3, numCond)
      rownames(paramCond) <- c("mixture ratio", "allele", "degradation")
      paramCond[1, ] <- rep(mrOneC, numCond / numMr)
      paramCond[2, ] <- rep(as.vector(sapply(peakOneL, rep, numMr)), numDeg)
      paramCond[3, ] <- as.vector(sapply(degOneC, rep, numMr * numPeak))
      return(paramCond)
    }
    
    #Cauculate Dal values
    degValCalc <- function(d, sizeOneL, sizeMean){
      exp(d * (sizeOneL - sizeMean))
    }
    
    paramCond <- paramCondMake(mrOneC, peakOneL, degOneC)
    
    step2Height <- tempMean * mrOneC
    dalVal <- sapply(degOneC, degValCalc, sizeOneL = sizeOneL, sizeMean = sizeMean)
    step4Height <- as.vector(matrix(step2Height / 2, ncol = 1) %*% matrix(dalVal, nrow = 1))
    
    nStep4 <- length(step4Height)
    step5Height <- matrix(0, numMc, nStep4)
    aeMean <- as.numeric(aeParamOneL[1])
    aeVar <- as.numeric(aeParamOneL[2])
    for(i in 1:nStep4){
      aeVal <- exp(rtruncnorm(numMc, a = log(aeMin), b = log(1 / aeMin), mean = aeMean, sd = sqrt(aeVar / step4Height[i])))
      step5Height[, i] <- aeVal * step4Height[i]
    }
    
    nStep5 <- ncol(step5Height)
    step6Height <- matrix(0, numMc, nStep5)
    hbMean <- as.numeric(hbParamOneL[1])
    hbVar <- as.numeric(hbParamOneL[2])
    for(i in 1:nStep5){
      hbVal <- exp(rtruncnorm(numMc, a = log(hbMin), b = log(1 / hbMin), mean = hbMean, sd = sqrt(hbVar / step5Height[, i])))
      step6Height[, i] <- 2 / (1 + hbVal) * step5Height[, i]
    }
    
    nMrOne <- length(mrOneC)
    nDegOne <- length(degOneC)
    
    nStep6 <- ncol(step6Height)
    srB1Val <- matrix(0, numMc, nStep6)
    if(length(srB1PeakOneL) != 0){
      srB1Mean <- srB1ParamOneL[1] * srB1PeakOneL + srB1ParamOneL[2]      
      srB1Mean <- rep(as.vector(matrix(rep(srB1Mean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
      srB1Var <- srB1ParamOneL[3]
      for(i in 1:nStep6){
        srB1Val[, i] <- exp(rtruncnorm(numMc, b = log(srB1Max), mean = log(srB1Mean[i]), sd = sqrt(srB1Var / step6Height[, i])))
      }
    }
    
    srF1Val <- srB1Val * 0
    if(length(srF1PeakOneL) != 0){
      srF1Mean <- srF1ParamOneL[1] * srF1PeakOneL + srF1ParamOneL[2]
      srF1Mean <- rep(as.vector(matrix(rep(srF1Mean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
      srF1Var <- srF1ParamOneL[3]
      for(i in 1:nStep6){
        srF1Val[, i] <- exp(rtruncnorm(numMc, b = log(srF1Max), mean = log(srF1Mean[i]), sd = sqrt(srF1Var / step6Height[, i])))
      }
    }
    
    srB2Val <- srB1Val * 0
    if(length(srB2PeakOneL) != 0){
      srB2Mean <- srB2ParamOneL[1] * srB2PeakOneL + srB2ParamOneL[2]
      srB2Mean <- rep(as.vector(matrix(rep(srB2Mean, nMrOne), nrow = nMrOne, byrow = TRUE)), nDegOne)
      srB2Var <- srB2ParamOneL[3]
      for(i in 1:nStep6){
        srB2Val[, i] <- exp(rtruncnorm(numMc, b = log(srB2Max), mean = log(srB2Mean[i]), sd = sqrt(srB2Var / step6Height[, i])))
      }
    }
    
    srM2Val <- srB1Val * 0
    srM2Mean <- as.numeric(srM2ParamOneL[1])
    srM2Var <- as.numeric(srM2ParamOneL[2])
    if((srM2Mean != 0) && (srM2Var != 0)){
      for(i in 1:nStep6){
        srM2Val[, i] <- exp(rtruncnorm(numMc, b = log(srM2Max), mean = log(srM2Mean), sd = sqrt(srM2Var / step6Height[, i])))
      }
    }
    
    srSum <- 1 + srB1Val + srF1Val + srB2Val + srM2Val
    ephAl <- 1 / srSum * step6Height
    ephStB1 <- srB1Val / srSum * step6Height
    ephStF1 <- srF1Val / srSum * step6Height
    ephStB2 <- srB2Val / srSum * step6Height
    ephStM2 <- srM2Val / srSum * step6Height
    
    return(list(ephAl, ephStB1, ephStF1, ephStB2, ephStM2, paramCond))
  }
  
  #Estimate parameters of gamma distribution
  gammaEstimate <- function(eph){
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
  
  #Obtain density of gamma distribution
  gammaDens <- function(gtCombOne, mrDegAll, peakOneL, heightOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC, at){
    
    #Sum gamma distributions of same allele repeat number
    gammaSumCalc <- function(gtCombOne, mr, deg, peakOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC){
      nAl <- length(gtCombOne)
      nPeak <- length(peakOneL)
      nMrOneC <- length(mrOneC)
      nDegOneC <- length(degOneC)
      
      gammaSumPre1Al <- matrix(0, nAl, nPeak)
      gammaSumPre1StB1 <- gammaSumPre1StF1 <- gammaSumPre1StB2 <- gammaSumPre1StM2 <- gammaSumPre2Al <- gammaSumPre2StB1 <- gammaSumPre2StF1 <- gammaSumPre2StB2 <- gammaSumPre2StM2 <- gammaSumPre1Al
      
      peakAlPos <- match(gtCombOne, peakOneL)
      mrPos <- rep(match(mr, mrOneC), times = 1, each = 2)
      degPos <- rep(match(deg, degOneC), times = 1, each = 2)
      condPos <- nMrOneC * nPeak * (degPos - 1) + nMrOneC * (peakAlPos - 1) + mrPos
      
      gammaAlPos <- cbind(1:nAl, peakAlPos)
      p1 <- gammaAl[1, condPos]
      p2 <- gammaAl[2, condPos]
      gammaSumPre1Al[gammaAlPos] <- p1 * p2
      gammaSumPre2Al[gammaAlPos] <- p1 * (p2^2)
      
      gammaStPos <- matrix(0, nAl, 2)
      gammaStPos[, 1] <- 1:nAl
      
      peakStB1Pos <- match(round(gtCombOne - 1, 1), peakOneL)
      gammaStPos[, 2] <- peakStB1Pos
      peakStB1ObsPos <- !is.na(peakStB1Pos)
      gammaStB1Pos <- gammaStPos[peakStB1ObsPos, , drop = FALSE]
      p1 <- gammaStB1[1, condPos]
      p2 <- gammaStB1[2, condPos]
      gammaSumPre1StB1[gammaStB1Pos] <- (p1 * p2)[peakStB1ObsPos]
      gammaSumPre2StB1[gammaStB1Pos] <- (p1 * (p2^2))[peakStB1ObsPos]
      
      peakStF1Pos <- match(round(gtCombOne + 1, 1), peakOneL)
      gammaStPos[, 2] <- peakStF1Pos
      peakStF1ObsPos <- !is.na(peakStF1Pos)
      gammaStF1Pos <- gammaStPos[peakStF1ObsPos, , drop = FALSE]
      p1 <- gammaStF1[1, condPos]
      p2 <- gammaStF1[2, condPos]
      gammaSumPre1StF1[gammaStF1Pos] <- (p1 * p2)[peakStF1ObsPos]
      gammaSumPre2StF1[gammaStF1Pos] <- (p1 * (p2^2))[peakStF1ObsPos]
      
      peakStB2Pos <- match(round(gtCombOne - 2, 1), peakOneL)
      gammaStPos[, 2] <- peakStB2Pos
      peakStB2ObsPos <- !is.na(peakStB2Pos)
      gammaStB2Pos <- gammaStPos[peakStB2ObsPos, , drop = FALSE]
      p1 <- gammaStB2[1, condPos]
      p2 <- gammaStB2[2, condPos]
      gammaSumPre1StB2[gammaStB2Pos] <- (p1 * p2)[peakStB2ObsPos]
      gammaSumPre2StB2[gammaStB2Pos] <- (p1 * (p2^2))[peakStB2ObsPos]
      
      peakStM2Pos <- rep(0, nAl)
      m2AlPos1 <- gtCombOne %% 1 < 0.15
      m2AlPos2 <- gtCombOne %% 1 >= 0.15
      peakStM2Pos[m2AlPos1] <- match(round(gtCombOne[m2AlPos1] - 0.8, 1), peakOneL)
      peakStM2Pos[m2AlPos2] <- match(round(gtCombOne[m2AlPos2] - 0.2, 1), peakOneL)
      gammaStPos[, 2] <- peakStM2Pos
      peakStM2ObsPos <- !is.na(peakStM2Pos)
      gammaStM2Pos <- gammaStPos[peakStM2ObsPos, , drop = FALSE]
      p1 <- gammaStM2[1, condPos]
      p2 <- gammaStM2[2, condPos]
      gammaSumPre1StM2[gammaStM2Pos] <- (p1 * p2)[peakStM2ObsPos]
      gammaSumPre2StM2[gammaStM2Pos] <- (p1 * (p2^2))[peakStM2ObsPos]
      
      gammaSumPre1 <- apply(gammaSumPre1Al + gammaSumPre1StB1 + gammaSumPre1StF1 + gammaSumPre1StB2 + gammaSumPre1StM2, 2, sum)
      gammaSumPre2 <- apply(gammaSumPre2Al + gammaSumPre2StB1 + gammaSumPre2StF1 + gammaSumPre2StB2 + gammaSumPre2StM2, 2, sum)
      
      gammaSum <- matrix(0, 2, nPeak)
      gammaSum[1, ] <- gammaSumPre1^2 / gammaSumPre2
      gammaSum[2, ] <- gammaSumPre2 / gammaSumPre1
      return(gammaSum)
    }
    
    #Compare observed peak heights with gamma distribution (expected peak heights)
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
    
    hnc <- length(gtCombOne) / 2
    prodGtCombOne <- rep(0, nrow(mrDegAll))
    for(i in 1:nrow(mrDegAll)){
      mrDeg <- mrDegAll[i, ]
      mr <- mrDeg[1:hnc]
      deg <- mrDeg[(hnc + 1):(2 * hnc)]
      gammaSum <- gammaSumCalc(gtCombOne, mr, deg, peakOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC)
      densPos <- !is.nan(gammaSum[1, ])
      dens <- cfOphEph(heightOneL[densPos], gammaSum[, densPos, drop = FALSE], at)
      prodGtCombOne[i] <- dens
    }
    return(prodGtCombOne)
  }
  
  #Calculate genotype probability
  gtProbCalc <- function(gtCombOne, hypIdOne, popAl, popFreq, theta, knownGt = NULL, QFreq = NULL){
    hnc <- length(gtCombOne) / 2
    gtUnique <- unique(gtCombOne)
    alCount <- rep(0, length(gtUnique))
    names(alCount) <- sort(gtUnique)
    nKnownAl <- length(knownGt)
    if(nKnownAl > 0){
      for(i in 1:nKnownAl){
        alCountPos <- which(as.numeric(names(alCount)) == knownGt[i])
        if(length(alCountPos) != 0){
          alCount[alCountPos] <- alCount[alCountPos] + 1
        }
      }
    }
    gtOneProb <- rep(0, hnc)
    for(i in 1:hnc){
      Id <- hypIdOne[i]
      gtOne <- gtCombOne[c(2 * i - 1, 2 * i)]
      if(Id != 0){
        if(setequal(gtOne, knownGt[c(2 * Id - 1, 2 * Id)])){
          gtOneProb[i] <- 1
        }
      }else{
        alProb <- rep(0, 2)
        for(j in 1:2){
          alOne <- gtOne[j]
          if(alOne == 99){
            alOneFreq <- QFreq
          }else{
            alOneFreq <- popFreq[popAl == alOne]
          }
          alOnePos <- as.numeric(names(alCount)) == alOne
          alProb[j] <- (alCount[alOnePos] * theta + (1 - theta) * alOneFreq) / ((nKnownAl + 2 * (i - 1) + j - 1) * theta + 1 - theta)
          alCount[alOnePos] <- alCount[alOnePos] + 1
        }
        if(gtOne[1] == gtOne[2]){
          gtOneProb[i] <- prod(alProb)
        }else{
          gtOneProb[i] <- 2 * prod(alProb)
        }
      }
    }
    return(prod(gtOneProb))
  }
  
  #Make all hypotheses
  hypIdMake <- function(nRef, hnc){
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
  
  #Make the best hypothesis
  hypBestMake <- function(hypBestId, nameKnown){
    n1 <- length(hypBestId)
    hypBest <- rep(0, n1)
    for(i in 1:n1){
      if(hypBestId[i] == 0){
        hypBest[i] <- "Unknown"
      }else{
        hypBest[i] <- nameKnown[hypBestId[i]]
      }
    }
    return(hypBest)
  }
  
  #Make a list of results of Hp and Hd
  resultMake <- function(hypIdAll, nameKnown, mrDegAll, gtCombList, productsList, gtProbList, log10LikeList, overallLike, lociName){
    resultList <- list()
    nL <- length(lociName)
    countHyp <- 0
    for(i in 1:length(hypIdAll)){
      hypIdOneAll <- hypIdAll[[i]]
      maxLikeCand <- apply(overallLike[countHyp + 1:nrow(hypIdOneAll), , drop = FALSE], 1, max)
      maxLike <- max(maxLikeCand)
      hypBestIdPos <- which(maxLikeCand == maxLike)[1]
      hypBestId <- hypIdOneAll[hypBestIdPos, ]
      hypBest <- hypBestMake(hypBestId, nameKnown)
      mrDegBestId <- which(overallLike[countHyp + hypBestIdPos, ] == maxLike)[1]
      mrDegBest <- mrDegAll[mrDegBestId, ]
      mrBest <- mrDegBest[1:(length(mrDegBest) / 2)]
      degBest <- mrDegBest[(length(mrDegBest) / 2 + 1):(length(mrDegBest))]
      names(mrBest) <- names(degBest) <- hypBest
      pgResultList <- list()
      likeBestPerLocus <- rep(0, nL)
      for(j in 1:nL){
        gtComb <- gtCombList[[j]]
        productsBest <- productsList[[j]][, mrDegBestId]
        gtProbBest <- gtProbList[[j]][countHyp + hypBestIdPos, ]
        likeEachComb <- productsBest * gtProbBest
        possibleCombPos <- likeEachComb != 0
        pgResult <- cbind(gtComb[possibleCombPos, , drop = FALSE], productsBest[possibleCombPos] / sum(productsBest[possibleCombPos]), productsBest[possibleCombPos], gtProbBest[possibleCombPos])
        colnames(pgResult) <- c(as.vector(sapply(hypBest, rep, 2)), "weight", "product", "genotype freq")
        pgResultList[[j]] <- pgResult[order(pgResult[, ncol(pgResult) - 2], decreasing = TRUE), , drop = FALSE]
        likeBestPerLocus[j] <- log10LikeList[[j]][countHyp + hypBestIdPos, mrDegBestId]
      }
      names(likeBestPerLocus) <- lociName
      names(pgResultList) <- lociName
      resultOne <- list()
      resultOne[[1]] <- likeBestPerLocus
      resultOne[[2]] <- maxLike
      resultOne[[3]] <- pgResultList
      resultOne[[4]] <- mrBest
      resultOne[[5]] <- degBest
      resultList[[i]] <- resultOne
      names(resultList[[i]]) <- c("likelihood per locus", "overall likelihood", "probabilistic genotyping", "mixture ratio", "degradation")
      countHyp <- countHyp + nrow(hypIdOneAll)
    }
    return(resultList)
  }
  
  #Interpret CSP
  cspInterpret <- function(cspPeak, cspSize, cspHeight, refAllL, popAlList, popFreqList, QFreqAll, atAllL, tempPerL, srB1Peak, srF1Peak, srB2Peak, aeParamVal, hbParamVal, srB1ParamVal, srF1ParamVal, srB2ParamVal, srM2ParamVal, srConsider, repLengthAll, hncFrom, hncTo, mrOne, degW, theta, numMc, mrDegCut, st, hbFltr, stFltr){
    lociName <- names(cspPeak)
    nL <- length(lociName)
    nK <- ncol(refAllL) / 2
    nameKnown <- colnames(refAllL)[1:(nK * 2) %% 2 == 1]
    tempMean <- mean(tempPerL)
    degVal <- seq(-0.05, 0, by = degW)
    degData <- degOneCMake(cspSize, cspHeight, tempPerL, degVal)
    degOneC <- degData[[1]]
    sizeMean <- degData[[2]]
    
    plot(c(0, 100), c(0, 2 * (hncTo - hncFrom + 1) + 2), type = "n", xlab = "", ylab = "", axes = FALSE, mar = c(3, 3, 3, 3))
    for(i in hncFrom:hncTo){
      nBerPos <- 2 * (hncTo - i) + 1
      rect(0, nBerPos, 100, nBerPos + 1, col = "white")
      text(25, nBerPos + 1.5, paste("Hypothesis : ", i, "-person contribution", sep = ""))
    }
    par(xpd = TRUE)
    text(50, 2 * (hncTo - hncFrom + 1) + 2, "Progress Bar", cex = 1.25)
    
    calcTime <- matrix(0, nrow = 1, ncol = hncTo - hncFrom + 1)
    calcTimeName <- rep(0, hncTo - hncFrom + 1)
    
    ##########
    resultAllHnc <- hypIdAllList <- gammaAllList <- mrOneCList <- list()
    countHnc <- 0
    cl <- makeCluster(4, type = "PSOCK")
    for(i in hncFrom:hncTo){
      t <- proc.time()
      countHnc <- countHnc + 1
      hnc <- i
      cat(paste("CSP Interpretation (", hnc, "-person contribution) was started.", sep = ""), "\n")
      nBerPos <- 2 * (hncTo - hnc) + 1
      mrAll <- mrSetting(hnc, mrOne)
      degAll <- matrix(rep(degOneC, hnc), nrow = length(degOneC))
      mrDegAll <- cbind(matrix(as.numeric(apply(mrAll, 1, rep, nrow(degAll))), ncol = hnc, byrow = TRUE), matrix(rep(as.numeric(t(degAll)), nrow(mrAll)), ncol = hnc, byrow = TRUE))
      nMD <- nrow(mrDegAll)
      mrDegID <- 1:nMD
      mrOneC <- sort(unique(as.vector(mrAll)))
      ##########
      mrOneCList[[countHnc]] <- mrOneC
      ##########
      hypIdAllList[[countHnc]] <- hypIdAll <- hypIdMake(ncol(refAllL) / 2, hnc)
      nH <- sum(sapply(hypIdAll, nrow))
      gammaList <- gtCombList <- productsList <- log10LikeList <- gtProbList <- list()
      overallLike <- matrix(0, nH, nMD)
      impossible <- FALSE
      for(j in 1:nL){
        peakOneL <- cspPeak[[j]]
        sizeOneL <- cspSize[[j]]
        heightOneL <- cspHeight[[j]]
        srConsiderOneL <- srConsider[j, ]
        refOneL <- refAllL[j, ]
        srB1PeakOneL <- srB1Peak[[j]]
        srF1PeakOneL <- srF1Peak[[j]]
        srB2PeakOneL <- srB2Peak[[j]]
        aeParamOneL <- aeParamVal[j, ]
        hbParamOneL <- hbParamVal[j, ]
        srB1ParamOneL <- srB1ParamVal[j, ]
        srF1ParamOneL <- srF1ParamVal[j, ]
        srB2ParamOneL <- srB2ParamVal[j, ]
        srM2ParamOneL <- srM2ParamVal[j, ]
        repLength <- repLengthAll[names(repLengthAll) == lociName[j]]
        popAl <- popAlList[[j]]
        popFreq <- popFreqList[[j]]
        QFreq <- QFreqAll[j]
        at <- atAllL[j]
        
        gtComb <- gtCombMake(peakOneL, heightOneL, hnc, srConsiderOneL, hbFltr, stFltr, st)
        gtCombList[[j]] <- gtComb
        if(length(gtComb) == 0){
          impossible <- TRUE
          break
        }else{
          ephData <- ephEstimate(peakOneL, sizeOneL, numMc, tempMean, mrOneC, degOneC, sizeMean, srB1PeakOneL, srF1PeakOneL, srB2PeakOneL, aeParamOneL, hbParamOneL, srB1ParamOneL, srF1ParamOneL, srB2ParamOneL, srM2ParamOneL)
          gammaAl <- gammaEstimate(ephData[[1]])
          gammaStB1 <- gammaEstimate(ephData[[2]])
          gammaStF1 <- gammaEstimate(ephData[[3]])
          gammaStB2 <- gammaEstimate(ephData[[4]])
          gammaStM2 <- gammaEstimate(ephData[[5]])
          gammaList[[j]] <- list(gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2)
          
          nGC <- nrow(gtComb)
          products <- matrix(0, nGC, nMD)
          if(hnc >= 3){
            productsOneL <- parRapply(cl, gtComb, gammaDens, mrDegAll[mrDegID, , drop = FALSE], peakOneL, heightOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC, at) 
            products[, mrDegID] <- matrix(productsOneL, nrow = nGC, byrow = TRUE)
          }else{
            for(k in 1:nGC){
              products[k, mrDegID] <- gammaDens(gtComb[k, ], mrDegAll[mrDegID, , drop = FALSE], peakOneL, heightOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC, at) 
            }
          }
          productsList[[j]] <- products
          log10Like <- mrDegAdopt <- matrix(0, nH, nMD)
          gtProb <- matrix(0, nH, nGC)
          countHyp <- 0
          nHI1 <- length(hypIdAll)
          for(k in 1:nHI1){
            hypIdOneAll <- hypIdAll[[k]]
            nHI2 <- nrow(hypIdOneAll)
            for(l in 1:nHI2){
              countHyp <- countHyp + 1
              hypIdOne <- hypIdOneAll[l, ]
              gtProb[countHyp, ] <- apply(gtComb, 1, gtProbCalc, hypIdOne, popAl, popFreq, theta, refOneL, QFreq)
              log10LikeOne <- log10(matrix(gtProb[countHyp, ], nrow = 1) %*% products)
              log10Like[countHyp, ] <- log10LikeOne
            }
            likePos <- (countHyp - nHI2 + 1):countHyp
            log10LikeOne2 <- as.numeric(log10Like[likePos, ])
            mrDegAdoptPos <- intersect(which(log10LikeOne2 != -Inf), which(log10LikeOne2 >= max(log10LikeOne2) + log10(mrDegCut)))
            if(length(mrDegAdoptPos) > 0){
              mrDegAdopt2 <- rep(0, length(log10LikeOne2))
              mrDegAdopt2[mrDegAdoptPos] <- 1
              mrDegAdopt[likePos, ] <- matrix(mrDegAdopt2, ncol = nMD)
            }
          }
          mrDegID <- which(apply(mrDegAdopt, 2, sum) != 0)
          log10LikeList[[j]] <- log10Like
          overallLike <- overallLike + log10Like
          gtProbList[[j]] <- gtProb
          rect(0, nBerPos, 100 * j / nL, nBerPos + 1, col = "greenyellow")
        }
      }
      if(impossible){
        resultAllHnc[[countHnc]] <- NULL
        rect(0, nBerPos, 100, nBerPos + 1, col = "greenyellow")
      }else{
        resultAllHnc[[countHnc]] <- resultMake(hypIdAll, nameKnown, mrDegAll, gtCombList, productsList, gtProbList, log10LikeList, overallLike, lociName)
      }
      ##########
      gammaAllList[[countHnc]] <- gammaList
      timeOneHnc <- proc.time() - t
      calcTime[1, countHnc] <- timeOneHnc[3]
      calcTimeName[countHnc] <- paste(hncFrom + countHnc - 1, "-person", sep = "")
      cat(paste("Time of CSP interpretation (", hnc, "-person contribution) : ", round(timeOneHnc[3], 3), " sec.", sep = ""), "\n")
    }
    stopCluster(cl)
    colnames(calcTime) <- calcTimeName
    return(list(resultAllHnc, gammaAllList, hypIdAllList, calcTime, mrOneCList, degOneC))
  }
  
  
  ##################################
  # 'Probabilistic genotyping' tab #
  ##################################
  
  #Make 'Probabilistic genotyping' tab
  tab3Make <- function(resultHp = NULL, resultHd = NULL, cspPeak = NULL, cspHeight = NULL, gammaList = NULL, dyeAllL = NULL, calcCond = NULL, mrOneCList = NULL, degOneC = NULL, repLengthAll = NULL){
    tkdestroy(frameTab3)
    frameTab3 <<- tkframe(tab3)
    if(tclvalue(cspName) == ""){
      tkgrid(tklabel(frameTab3, text = "        Load crime stain profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(refName) == ""){
      tkgrid(tklabel(frameTab3, text = "        Load reference profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(afName) == ""){
      tkgrid(tklabel(frameTab3, text = "        Load allele frequencies!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(mcName) == ""){
      tkgrid(tklabel(frameTab3, text = "        Load Monte Carlo Parameters!        "), pady = 10, sticky = "w")
    }
    if(all(c(tclvalue(cspName), tclvalue(refName), tclvalue(afName)) != "")){
      if(tclvalue(calcFin) == "0"){
        tkgrid(tklabel(frameTab3, text = "        Perform calculation!        "), pady = 10, sticky = "w")
      }else{
        pgRefine <- function(pg){
          n1 <- ncol(pg)
          gt <- pg[, 1:(n1 - 3), drop = FALSE]
          nC <- ncol(gt) / 2
          gtRefine <- matrix(0, nrow(gt), nC)
          nameC <- rep("", nC)
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
              gtRefine[j, i] <- paste("(", al_1, ", ", al_2, ")", sep = "")
            }
            nameC[i] <- colnames(gt)[2 * i - 1]
          }
          pgNew <- cbind(gtRefine, sprintf("%.2e", pg[, n1 - 2]))
          colnames(pgNew) <- c(nameC, colnames(pg)[n1 - 2])
          return(pgNew)
        }
        
        #Sum gamma distributions of same allele repeat number
        gammaSumCalc <- function(gtCombOne, mr, deg, peakOneL, gammaAl, gammaStB1, gammaStF1, gammaStB2, gammaStM2, mrOneC, degOneC){
          nAl <- length(gtCombOne)
          nPeak <- length(peakOneL)
          nMrOneC <- length(mrOneC)
          nDegOneC <- length(degOneC)
          
          gammaSumPre1Al <- matrix(0, nAl, nPeak)
          gammaSumPre1StB1 <- gammaSumPre1StF1 <- gammaSumPre1StB2 <- gammaSumPre1StM2 <- gammaSumPre2Al <- gammaSumPre2StB1 <- gammaSumPre2StF1 <- gammaSumPre2StB2 <- gammaSumPre2StM2 <- gammaSumPre1Al
          
          peakAlPos <- match(gtCombOne, peakOneL)
          mrPos <- rep(match(mr, mrOneC), times = 1, each = 2)
          degPos <- rep(match(deg, degOneC), times = 1, each = 2)
          condPos <- nMrOneC * nPeak * (degPos - 1) + nMrOneC * (peakAlPos - 1) + mrPos
          
          gammaAlPos <- cbind(1:nAl, peakAlPos)
          p1 <- gammaAl[1, condPos]
          p2 <- gammaAl[2, condPos]
          gammaSumPre1Al[gammaAlPos] <- p1 * p2
          gammaSumPre2Al[gammaAlPos] <- p1 * (p2^2)
          
          gammaStPos <- matrix(0, nAl, 2)
          gammaStPos[, 1] <- 1:nAl
          
          peakStB1Pos <- match(round(gtCombOne - 1, 1), peakOneL)
          gammaStPos[, 2] <- peakStB1Pos
          peakStB1ObsPos <- !is.na(peakStB1Pos)
          gammaStB1Pos <- gammaStPos[peakStB1ObsPos, , drop = FALSE]
          p1 <- gammaStB1[1, condPos]
          p2 <- gammaStB1[2, condPos]
          gammaSumPre1StB1[gammaStB1Pos] <- (p1 * p2)[peakStB1ObsPos]
          gammaSumPre2StB1[gammaStB1Pos] <- (p1 * (p2^2))[peakStB1ObsPos]
          
          peakStF1Pos <- match(round(gtCombOne + 1, 1), peakOneL)
          gammaStPos[, 2] <- peakStF1Pos
          peakStF1ObsPos <- !is.na(peakStF1Pos)
          gammaStF1Pos <- gammaStPos[peakStF1ObsPos, , drop = FALSE]
          p1 <- gammaStF1[1, condPos]
          p2 <- gammaStF1[2, condPos]
          gammaSumPre1StF1[gammaStF1Pos] <- (p1 * p2)[peakStF1ObsPos]
          gammaSumPre2StF1[gammaStF1Pos] <- (p1 * (p2^2))[peakStF1ObsPos]
          
          peakStB2Pos <- match(round(gtCombOne - 2, 1), peakOneL)
          gammaStPos[, 2] <- peakStB2Pos
          peakStB2ObsPos <- !is.na(peakStB2Pos)
          gammaStB2Pos <- gammaStPos[peakStB2ObsPos, , drop = FALSE]
          p1 <- gammaStB2[1, condPos]
          p2 <- gammaStB2[2, condPos]
          gammaSumPre1StB2[gammaStB2Pos] <- (p1 * p2)[peakStB2ObsPos]
          gammaSumPre2StB2[gammaStB2Pos] <- (p1 * (p2^2))[peakStB2ObsPos]
          
          peakStM2Pos <- rep(0, nAl)
          m2AlPos1 <- gtCombOne %% 1 < 0.15
          m2AlPos2 <- gtCombOne %% 1 >= 0.15
          peakStM2Pos[m2AlPos1] <- match(round(gtCombOne[m2AlPos1] - 0.8, 1), peakOneL)
          peakStM2Pos[m2AlPos2] <- match(round(gtCombOne[m2AlPos2] - 0.2, 1), peakOneL)
          gammaStPos[, 2] <- peakStM2Pos
          peakStM2ObsPos <- !is.na(peakStM2Pos)
          gammaStM2Pos <- gammaStPos[peakStM2ObsPos, , drop = FALSE]
          p1 <- gammaStM2[1, condPos]
          p2 <- gammaStM2[2, condPos]
          gammaSumPre1StM2[gammaStM2Pos] <- (p1 * p2)[peakStM2ObsPos]
          gammaSumPre2StM2[gammaStM2Pos] <- (p1 * (p2^2))[peakStM2ObsPos]
          
          gammaSumPre1 <- apply(gammaSumPre1Al + gammaSumPre1StB1 + gammaSumPre1StF1 + gammaSumPre1StB2 + gammaSumPre1StM2, 2, sum)
          gammaSumPre2 <- apply(gammaSumPre2Al + gammaSumPre2StB1 + gammaSumPre2StF1 + gammaSumPre2StB2 + gammaSumPre2StM2, 2, sum)
          
          gammaSum <- matrix(0, 2, nPeak)
          gammaSum[1, ] <- gammaSumPre1^2 / gammaSumPre2
          gammaSum[2, ] <- gammaSumPre2 / gammaSumPre1
          return(gammaSum)
        }
        
        #Make a graph of observed peaks vs estimated gamma distributions
        pgGraphMake <- function(gtComb, at, mrEstimate, degEstimate, gammaParamOneL, mrOneC, degOneC, repLength){
          if(tclvalue(tkcurselection(pgMlb)) == ""){
            tkmessageBox(message = "Select one genotype combination!", icon = "error", type = "ok")
          }else{
            lociName <- names(cspPeak)
            lPos <- which(lociName == tclvalue(selectL))
            peakOneL2 <- cspPeak[[lPos]]
            heightOneL2 <- cspHeight[[lPos]]
            
            peakPos <- heightOneL2 >= at
            peakOneL <- peakOneL2[peakPos]
            heightOneL <- heightOneL2[peakPos]
            
            gtCombOne <- gtComb[as.numeric(tclvalue(tkcurselection(pgMlb))) + 1, ]
            
            dropAl <- setdiff(gtCombOne, peakOneL)
            nD <- length(dropAl)
            if(nD > 0){
              peakOneL <- c(peakOneL, dropAl)
              heightOneL <- c(heightOneL, rep(0, nD))
            }
            
            if(max(heightOneL) >= at){
              yMax <- 2 * max(heightOneL)
            }else{
              yMax <- 2 * at
            }
            
            gammaSum <- gammaSumCalc(gtCombOne, mrEstimate, degEstimate, peakOneL2, gammaParamOneL[[1]], gammaParamOneL[[2]], gammaParamOneL[[3]], gammaParamOneL[[4]], gammaParamOneL[[5]], mrOneC, degOneC)
            gamHeightCand <- 1:yMax
            
            peakOneLGraph <- peakOneL
            if(any(is.element(peakOneL, 99))){
              peakOneLGraph[which(peakOneL == 99)] <- max(peakOneL[which(peakOneL != 99)]) + 2
            }
            
            plot(-1, -1, xlim = c(min(peakOneLGraph) - 1, max(peakOneLGraph) + 1), ylim = c(0, yMax), xaxt = "n", xlab = "Allele repeat number", ylab = "Peak height (RFU)", las = 1)
            segments(min(peakOneLGraph) - 1, 0, max(peakOneLGraph) + 1, 0)
            for(i in 1:length(peakOneLGraph)){
              peakOne <- variantCor(peakOneLGraph[i], repLength)
              segments(peakOne - 0.1, 0, peakOne, heightOneL[i], col = "blue")
              segments(peakOne + 0.1, 0, peakOne, heightOneL[i], col = "blue")
              dens <- dgamma(gamHeightCand, shape = gammaSum[1, i], scale = gammaSum[2, i])
              densPos <- dens > 0.005 * max(dens)
              dens <- dens[densPos]
              par(new = TRUE)
              plot(peakOne - 0.1 - 0.3 / max(dens) * dens, gamHeightCand[densPos], type = "l", col = "red", xlim = c(min(peakOneLGraph) - 1, max(peakOneLGraph) + 1), ylim = c(0, yMax), xlab = "", ylab = "", axes = FALSE)
              if(peakOneL[i] == 99){
                xLabelOne <- "Q"
              }else{
                xLabelOne <- peakOneL[i]
              }
              par(xpd = TRUE)
              text(peakOne, - yMax / 12, xLabelOne)
              par(xpd = FALSE)
            }
          }
        }
        
        #Show results of probabilistic genotyping
        pgShow <- function(hyp, numContributor, locusId, hncFrom){
          tkdestroy(pgMlb)
          tkdestroy(pgGraphButt)
          if(hyp == "Hp"){
            resultList <- resultHp
          }else{
            resultList <- resultHd
          }
          resultOneHnc <- resultList[[numContributor - hncFrom + 1]]
          if(length(resultOneHnc) == 0){
            pgMlb <<- tklabel(frameTab3_2, text = "Inappropriate hypothesis")
            tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
            pgGraphButt <<- tklabel(frameTab3_2, text = "")
            tkgrid(pgGraphButt, padx = 20, pady = 20, sticky = "w")
          }else if(length(resultOneHnc[[3]][[locusId]]) == 0){
            pgMlb <<- tklabel(frameTab3_2, text = "Inappropriate hypothesis")
            tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
            pgGraphButt <<- tklabel(frameTab3_2, text = "")
            tkgrid(pgGraphButt, padx = 20, pady = 20, sticky = "w")
          }else{
            pgResultOneL <- pgRefine(resultOneHnc[[3]][[locusId]])
            pgMlb <<- tk2mclistbox(frameTab3_2, width = 15 * ncol(pgResultOneL), height = 20, resizablecolumns = TRUE, selectmode = "single")
            for(i in 1:ncol(pgResultOneL)){
              tk2column(pgMlb, "add", label = colnames(pgResultOneL)[i], width = 15)
            }
            tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
            tk2insert.multi(pgMlb, "end", pgResultOneL)
            pgGraphButt <<- tkbutton(frameTab3_2, text = "    Graph    ", cursor = "hand2", command = function() pgGraphMake(resultOneHnc[[3]][[locusId]][, 1:(2 * numContributor), drop = FALSE], calcCond[names(calcCond) == dyeAllL[locusId]], resultOneHnc[[4]], resultOneHnc[[5]], gammaAllList[[numContributor - hncFrom + 1]][[locusId]], mrOneCList[[numContributor - hncFrom + 1]], degOneC, repLengthAll[[locusId]]))
            tkgrid(pgGraphButt, padx = 20)
          }
        }
        
        #Export all results of probabilistic genotyping
        exportPg <- function(hncFrom, hncTo){
          savePg <- function(){
            if(tclvalue(pgFolderName) == ""){
              tkmessageBox(message = "Enter the folder name for saving files!", icon = "error", type = "ok")
            }else{
              folderPath <- choose.dir()
              if(!is.na(folderPath)){
                folderPath <- paste(folderPath, "/", tclvalue(pgFolderName), sep = "")
                dir.create(path = folderPath)
                countHnc <- 0
                for(i in hncFrom:hncTo){
                  countHnc <- countHnc + 1
                  resultOneHnc <- resultHp[[countHnc]]
                  if(length(resultOneHnc) != 0){
                    lociName <- names(resultOneHnc[[3]])
                    for(j in 1:length(resultOneHnc[[3]])){
                      if(length(resultOneHnc[[3]][[j]]) != 0){
                        pgResultOneL <- pgRefine(resultOneHnc[[3]][[j]])
                        write.csv(pgResultOneL, paste(folderPath, "/pgResult_Hp_", i, "-contributors_", lociName[j], ".csv", sep = ""), row.names = FALSE)
                      }
                    }
                  }
                  resultOneHnc <- resultHd[[countHnc]]
                  if(length(resultOneHnc) != 0){
                    lociName <- names(resultOneHnc[[3]])
                    for(j in 1:length(resultOneHnc[[3]])){
                      if(length(resultOneHnc[[3]][[j]]) != 0){
                        pgResultOneL <- pgRefine(resultOneHnc[[3]][[j]])
                        write.csv(pgResultOneL, paste(folderPath, "/pgResult_Hd_", i, "-contributors_", lociName[j], ".csv", sep = ""), row.names = FALSE)
                      }
                    }
                  }
                }
                tkmessageBox(message = paste("Results of probabilistic genotyping have been saved successfully in the folder of ", tclvalue(pgFolderName), ".", sep = ""), icon = "info", type = "ok")
              }
            }
          }
          exportPg.tf <- tktoplevel()
          tkwm.title(exportPg.tf, "Export results of probabilistic genotyping")
          tkgrid(tklabel(exportPg.tf, text = "Enter the folder name for saving files."), padx = 10, pady = 10, sticky = "w")
          pgFolderName <- tclVar("")
          tkgrid(tkentry(exportPg.tf, textvariable = pgFolderName, width = 30, highlightthickness = 1, relief = "solid", justify = "center", background = "white"), padx = 20, pady = 10, sticky = "w")
          tkgrid(tkbutton(exportPg.tf, text = "    Choose location of the folder    ", cursor = "hand2", command = function() savePg()), padx = 20, pady = 20, sticky = "w")
        }
        
        frameTab3_1 <- frameTab3_2 <- tkframe(frameTab3)
        frameTab3_1_1 <- tkframe(frameTab3_1)
        hypotheses <- c("Hp", "Hd")
        selectHyp <- tclVar("Hp")
        selectHypComboBox <- ttkcombobox(frameTab3_1_1, values = hypotheses, textvariable = selectHyp, state = "readonly")
        tkgrid(tklabel(frameTab3_1_1, text = "Hypothesis : "), selectHypComboBox, sticky = "w")
        hncFrom <- calcCond[names(calcCond) == "hncFrom"]
        hncTo <- calcCond[names(calcCond) == "hncTo"]
        nC <- hncFrom:hncTo
        selectNc <- tclVar(nC[1])
        selectNcComboBox <- ttkcombobox(frameTab3_1, values = nC, textvariable = selectNc, state = "readonly")
        tkgrid(tklabel(frameTab3_1_1, text = "Number of contributors : "), selectNcComboBox, sticky = "w")
        lociName <- names(resultHp[[which(sapply(resultHp, length) != 0)[1]]][[1]])
        selectL <- tclVar(lociName[1])
        selectLComboBox <- ttkcombobox(frameTab3_1_1, values = lociName, textvariable = selectL, state = "readonly")
        tkgrid(tklabel(frameTab3_1_1, text = "Locus : "), selectLComboBox, sticky = "w")
        pgShowButt <- tkbutton(frameTab3_1_1, text = "    Show    ", cursor = "hand2", command = function() pgShow(tclvalue(selectHyp), as.numeric(tclvalue(selectNc)), which(lociName == tclvalue(selectL)), hncFrom))
        pgExportButt <- tkbutton(frameTab3_1_1, text = "    Export    ", cursor = "hand2", command = function() exportPg(hncFrom, hncTo))
        tkgrid(pgShowButt, pgExportButt, sticky = "w")
        tkgrid(frameTab3_1_1, sticky = "w")
        tkgrid(frameTab3_1, padx = 20, pady = 20, sticky = "w")
        resultOneHnc <- resultHp[[as.numeric(tclvalue(selectNc)) - hncFrom + 1]]
        if(length(resultOneHnc) == 0){
          pgMlb <- tklabel(frameTab3_2, text = "Inappropriate hypothesis")
          tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
          pgGraphButt <- tklabel(frameTab3_2, text = "")
          tkgrid(pgGraphButt, padx = 20, pady = 20, sticky = "w")
        }else if(length(resultOneHnc[[3]][[1]]) == 0){
          pgMlb <- tklabel(frameTab3_2, text = "Inappropriate hypothesis")
          tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
          pgGraphButt <- tklabel(frameTab3_2, text = "")
          tkgrid(pgGraphButt, padx = 20, pady = 20, sticky = "w")
        }else{
          pgResultOneL <- pgRefine(resultOneHnc[[3]][[1]])
          pgMlb <- tk2mclistbox(frameTab3_2, width = 15 * ncol(pgResultOneL), height = 20, resizablecolumns = TRUE, selectmode = "single")
          for(i in 1:ncol(pgResultOneL)){
            tk2column(pgMlb, "add", label = colnames(pgResultOneL)[i], width = 15)
          }
          tkgrid(pgMlb, padx = 20, pady = 20, sticky = "w")
          tk2insert.multi(pgMlb, "end", pgResultOneL)
          pgGraphButt <- tkbutton(frameTab3_2, text = "    Graph    ", cursor = "hand2", command = function() pgGraphMake(resultOneHnc[[3]][[1]][, 1:(2 * as.numeric(tclvalue(selectNc))), drop = FALSE], calcCond[names(calcCond) == dyeAllL[1]], resultOneHnc[[4]], resultOneHnc[[5]], gammaAllList[[1]][[1]], mrOneC[[1]], degOneC, repLengthAll[[1]]))
          tkgrid(pgGraphButt, padx = 20)
        }
        tkgrid(frameTab3_2, padx = 20, pady = 20, sticky = "w")
      }
    }
    tkgrid(frameTab3)
  }
  
  
  ##########################
  # 'Likelihood Ratio' tab #
  ##########################
  
  #Make a report
  reportMake <- function(HpKnownName, HdKnownName, log10LikeHp, log10LikeHd, lrAll, likeHpMax, likeHdMax, lrMax, mrEstimateHpAll, mrEstimateHdAll, dEstimateHpAll, dEstimateHdAll, calcCond, mrOne, dyeNames, calcTime){
    saveAs <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
    if(tclvalue(saveAs) != ""){
      if(substr(tclvalue(saveAs), nchar(tclvalue(saveAs)) - 3, nchar(tclvalue(saveAs))) == ".csv"){
        reportName <- tclvalue(saveAs)
      }else{
        reportName <- paste(tclvalue(saveAs), ".csv", sep = "")
      }
      write.table("======== Software version ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE)
      write.table(softVer, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Imput files ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Crime stain profile : ", tclvalue(cspName), sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Reference profile : ", tclvalue(refName), sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Allele frequencies : ", tclvalue(afName), sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Monte Carlo parameters : ", tclvalue(mcName), sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Allele repeat correction : ", tclvalue(alCorName), sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Parameters ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Number of contributors : from ", calcCond[names(calcCond) == "hncFrom"], " to ", calcCond[names(calcCond) == "hncTo"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("Analytical Threshold", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      for(i in 1:length(dyeNames)){
        write.table(paste("  - ", dyeNames[i], ": ", calcCond[names(calcCond) == dyeNames[i]], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      }
      write.table(paste("Theta value : ", calcCond[names(calcCond) == "theta"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Number of Monte Carlo simulation : ", calcCond[names(calcCond) == "numMc"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("Mixture ratio", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(matrix(mrOne, nrow = 1), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("Degradation", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Width : ", calcCond[names(calcCond) == "degW"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("Allele frequencies", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      afMeth <- calcCond[names(calcCond) == "afMeth"]
      if(afMeth == 1){
        afMethName <- "Dirichlet distribution"
        write.table(paste("  - Method : ", afMethName, sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      }else{
        afMethName <- "Observed frequencies"
        write.table(paste("  - Method : ", afMethName, sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        write.table(paste("  - Minimum allele frequency : ", calcCond[names(calcCond) == "maf"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      }
      write.table("Thresholds to exclude unrealistic genotype combinations", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Stochastic threshold: ", calcCond[names(calcCond) == "st"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Heterozygote balance filter: ", calcCond[names(calcCond) == "hbFltr"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Back stutter filter : ", calcCond[names(calcCond) == "stB1Fltr"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Forward stutter filter : ", calcCond[names(calcCond) == "stF1Fltr"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Double-back stutter filter : ", calcCond[names(calcCond) == "stB2Fltr"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("  - Minus 2 base stutter filter : ", calcCond[names(calcCond) == "stM2Fltr"], sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Hypothesis ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Prosecutor hypothesis (Hp) : ", paste(HpKnownName, collapse = " + "), " (+ unknown contributors)", sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("Defense hypothesis (Hd) : ", paste(HdKnownName, collapse = " + "), " (+ unknown contributors)", sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Log10 (likelihood) in Hp ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(log10LikeHp, file = reportName, sep = ",", row.names = TRUE, col.names = NA, append = TRUE)
      
      write.table("---- Estimated Mixture Ratio ----", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(mrEstimateHpAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
      
      write.table("---- Estimated Degradation Parameters ----", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(dEstimateHpAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Log10 (likelihood) in Hd ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(log10LikeHd, file = reportName, sep = ",", row.names = TRUE, col.names = NA, append = TRUE)
      
      write.table("---- Estimated Mixture Ratio ----", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(mrEstimateHdAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
      
      write.table("---- Estimated Degradation Parameters ----", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(dEstimateHdAll, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
      write.table("", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Likelihood ratio (Hp/Hd) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(lrAll, file = reportName, sep = ",", row.names = TRUE, col.names = NA, append = TRUE)
      write.table(paste("---- Maximum Log10 (likelihood) in Hp (", colnames(log10LikeHp)[log10LikeHp[nrow(log10LikeHp), ] == likeHpMax], " contribution) ----", sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(likeHpMax, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(paste("---- Maximum Log10 (likelihood) in Hd (", colnames(log10LikeHd)[log10LikeHd[nrow(log10LikeHd), ] == likeHdMax], " contribution) ----", sep = ""), file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(likeHdMax, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("---- Ratio of Maximum Likelihoods ----", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(lrMax, file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      
      write.table("======== Calculation time (sec) ========", file = reportName, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table(calcTime, file = reportName, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    }
  }
  
  #Make 'Likelihood Ratio' tab
  tab4Make <- function(resultHp = NULL, resultHd = NULL, calcCond = NULL, mrOne = NULL, dyeNames = NULL, calcTime = NULL){
    tkdestroy(frameTab4)
    frameTab4 <<- tkframe(tab4)
    if(tclvalue(cspName) == ""){
      tkgrid(tklabel(frameTab4, text = "        Load crime stain profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(refName) == ""){
      tkgrid(tklabel(frameTab4, text = "        Load reference profile!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(afName) == ""){
      tkgrid(tklabel(frameTab4, text = "        Load allele frequencies!        "), pady = 10, sticky = "w")
    }
    if(tclvalue(mcName) == ""){
      tkgrid(tklabel(frameTab4, text = "        Load Monte Carlo Parameters!        "), pady = 10, sticky = "w")
    }
    if(all(c(tclvalue(cspName), tclvalue(refName), tclvalue(afName)) != "")){
      if(tclvalue(calcFin) == "0"){
        tkgrid(tklabel(frameTab4, text = "        Perform calculation!        "), pady = 10, sticky = "w")
      }else{
        frameTab4_1 <- tkframe(frameTab4, relief = "groove", borderwidth = 2)
        frameTab4_1_1 <- tkframe(frameTab4_1)
        frameTab4_1_2 <- tkframe(frameTab4_1)
        frameTab4_1_2_1 <- tkframe(frameTab4_1_2)
        frameTab4_1_2_2 <- tkframe(frameTab4_1_2)
        frameTab4_1_2_3 <- tkframe(frameTab4_1_2)
        frameTab4_1_2_4 <- tkframe(frameTab4_1_2)
        tkgrid(tklabel(frameTab4_1_1, text = "Prosecutor hypothesis", font = "Helvetica 10 bold"))
        tkgrid(tklabel(frameTab4_1_2_1, text = ""))
        tkgrid(tklabel(frameTab4_1_2_2, text = "Log10 (likelihood)"))
        tkgrid(tklabel(frameTab4_1_2_3, text = "Estimated mixture ratio"))
        tkgrid(tklabel(frameTab4_1_2_4, text = "Estimated degradation"))
        
        frameTab4_2 <- tkframe(frameTab4, relief = "groove", borderwidth = 2)
        frameTab4_2_1 <- tkframe(frameTab4_2)
        frameTab4_2_2 <- tkframe(frameTab4_2)
        frameTab4_2_2_1 <- tkframe(frameTab4_2_2)
        frameTab4_2_2_2 <- tkframe(frameTab4_2_2)
        frameTab4_2_2_3 <- tkframe(frameTab4_2_2)
        frameTab4_2_2_4 <- tkframe(frameTab4_2_2)
        tkgrid(tklabel(frameTab4_2_1, text = "Defense hypothesis", font = "Helvetica 10 bold"))
        tkgrid(tklabel(frameTab4_2_2_1, text = ""))
        tkgrid(tklabel(frameTab4_2_2_2, text = "Log10 (likelihood)"))
        tkgrid(tklabel(frameTab4_2_2_3, text = "Estimated mixture ratio"))
        tkgrid(tklabel(frameTab4_2_2_4, text = "Estimated degradation"))
        
        lociName <- names(resultHp[[which(sapply(resultHp, length) != 0)[1]]][[1]])
        nL <- length(lociName)
        HpKnownName <- setdiff(names(resultHp[[which(sapply(resultHp, length) != 0)[1]]][[4]]), "Unknown")
        HdKnownName <- setdiff(names(resultHd[[which(sapply(resultHd, length) != 0)[1]]][[4]]), "Unknown")
        log10LikeHp <- matrix(-Inf, nL + 1, length(resultHp))
        rownames(log10LikeHp) <- c(lociName, "Total")
        colnames(log10LikeHp) <- rep("", length(resultHp))
        log10LikeHd <- matrix(-Inf, nL + 1, length(resultHd))
        rownames(log10LikeHd) <- c(lociName, "Total")
        colnames(log10LikeHd) <- rep("", length(resultHd))
        hncFrom <- calcCond[1]
        hncTo <- calcCond[2]
        mrEstimateHpAll <- mrEstimateHdAll <- dEstimateHpAll <- dEstimateHdAll <- matrix("", hncTo, 3 * hncTo - 1)
        colnames(mrEstimateHpAll) <- colnames(mrEstimateHdAll) <- colnames(dEstimateHpAll) <- colnames(dEstimateHdAll) <- rep("", 3 * hncTo - 1)
        
        countHnc <- 0
        for(i in hncFrom:hncTo){
          countHnc <- countHnc + 1
          tkgrid(tklabel(frameTab4_1_2_1, text = paste("Hp", countHnc, ": ", i, "-person", sep = "")))
          colnames(log10LikeHp)[countHnc] <- colnames(log10LikeHd)[countHnc] <- paste(hncFrom + countHnc - 1, "-person", sep = "")
          colnames(mrEstimateHpAll)[3 * countHnc - 1] <- colnames(dEstimateHpAll)[3 * countHnc - 1] <- colnames(mrEstimateHdAll)[3 * countHnc - 1] <- colnames(dEstimateHdAll)[3 * countHnc - 1] <- paste(hncFrom + countHnc - 1, "-person", sep = "")
          if(length(resultHp[[countHnc]]) != 0){
            if(!is.element(0, sapply(resultHp[[countHnc]][[3]], length))){
              log10LikeHp[1:nL, countHnc] <- resultHp[[countHnc]][[1]]
              log10LikeHp[nL + 1, countHnc] <- resultHp[[countHnc]][[2]]
              tkgrid(tklabel(frameTab4_1_2_2, text = paste(signif(log10LikeHp[nL + 1, countHnc], 3))))
              mrEstimateHpAll[1:i, 3 * countHnc - 2] <- names(resultHp[[countHnc]][[4]])
              mrEstimateHpAll[1:i, 3 * countHnc - 1] <- mrEstimateHp <- resultHp[[countHnc]][[4]]
              dEstimateHpAll[1:i, 3 * countHnc - 2] <- names(resultHp[[countHnc]][[5]])
              dEstimateHpAll[1:i, 3 * countHnc - 1] <- dEstimateHp <- resultHp[[countHnc]][[5]]
              tkgrid(tklabel(frameTab4_1_2_3, text = round(mrEstimateHp, 4)))
              tkgrid(tklabel(frameTab4_1_2_4, text = round(dEstimateHp, 4)))
            }else{
              mrEstimateHpAll[1:i, 3 * countHnc - 1] <- dEstimateHpAll[1:i, 3 * countHnc - 1] <- "-"
              tkgrid(tklabel(frameTab4_1_2_2, text = "-Inf"))
              tkgrid(tklabel(frameTab4_1_2_3, text = " - "))
              tkgrid(tklabel(frameTab4_1_2_4, text = " - "))
            }
          }else{
            mrEstimateHpAll[1:i, 3 * countHnc - 1] <- dEstimateHpAll[1:i, 3 * countHnc - 1] <- "-"
            tkgrid(tklabel(frameTab4_1_2_2, text = "-Inf"))
            tkgrid(tklabel(frameTab4_1_2_3, text = " - "))
            tkgrid(tklabel(frameTab4_1_2_4, text = " - "))
          }
          tkgrid(tklabel(frameTab4_2_2_1, text = paste("Hd", i - hncFrom + 1, ": ", i, "-person", sep = "")))
          if(length(resultHd[[countHnc]]) != 0){ 
            if(!is.element(0, sapply(resultHd[[countHnc]][[3]], length))){
              log10LikeHd[1:nL, countHnc] <- resultHd[[countHnc]][[1]]
              log10LikeHd[nL + 1, countHnc] <- resultHd[[countHnc]][[2]]
              tkgrid(tklabel(frameTab4_2_2_2, text = paste(signif(log10LikeHd[nL + 1, countHnc], 3))))
              mrEstimateHdAll[1:i, 3 * countHnc - 2] <- names(resultHd[[countHnc]][[4]])
              mrEstimateHdAll[1:i, 3 * countHnc - 1] <- mrEstimateHd <- resultHd[[countHnc]][[4]]
              dEstimateHdAll[1:i, 3 * countHnc - 2] <- names(resultHd[[countHnc]][[5]])
              dEstimateHdAll[1:i, 3 * countHnc - 1] <- dEstimateHd <- resultHd[[countHnc]][[5]]
              tkgrid(tklabel(frameTab4_2_2_3, text = round(mrEstimateHd, 4)))
              tkgrid(tklabel(frameTab4_2_2_4, text = round(dEstimateHd, 4)))
            }else{
              mrEstimateHdAll[1:i, 3 * countHnc - 1] <- dEstimateHdAll[1:i, 3 * countHnc - 1] <- "-"
              tkgrid(tklabel(frameTab4_2_2_2, text = "-Inf"))
              tkgrid(tklabel(frameTab4_2_2_3, text = " - "))
              tkgrid(tklabel(frameTab4_2_2_4, text = " - "))
            }
          }else{
            mrEstimateHdAll[1:i, 3 * countHnc - 1] <- dEstimateHdAll[1:i, 3 * countHnc - 1] <- "-"
            tkgrid(tklabel(frameTab4_2_2_2, text = "-Inf"))
            tkgrid(tklabel(frameTab4_2_2_3, text = " - "))
            tkgrid(tklabel(frameTab4_2_2_4, text = " - "))
          }
        }
        likeHpMaxPos <- which(log10LikeHp[nL + 1, ] == max(log10LikeHp[nL + 1, ]))[1]
        likeHdMaxPos <- which(log10LikeHd[nL + 1, ] == max(log10LikeHd[nL + 1, ]))[1]
        likeHpMax <- log10LikeHp[nL + 1, likeHpMaxPos]
        likeHdMax <- log10LikeHd[nL + 1, likeHdMaxPos]
        lrMax <- 10^(likeHpMax - likeHdMax)
        tkgrid(frameTab4_1_2_1, frameTab4_1_2_2, frameTab4_1_2_3, frameTab4_1_2_4, padx = 10)
        tkgrid(frameTab4_1_1, pady = 5)
        tkgrid(frameTab4_1_2, pady = 5)
        tkgrid(frameTab4_1, padx = 20, pady = 5)
        tkgrid(frameTab4_2_2_1, frameTab4_2_2_2, frameTab4_2_2_3, frameTab4_2_2_4, padx = 10)
        tkgrid(frameTab4_2_1, pady = 5)
        tkgrid(frameTab4_2_2, pady = 5)
        tkgrid(frameTab4_2, padx = 20, pady = 5)
        
        frameTab4_3 <- tkframe(frameTab4)
        frameTab4_3_1 <- tkframe(frameTab4_3, relief = "groove", borderwidth = 2)
        frameTab4_3_1_1 <- tkframe(frameTab4_3_1)
        frameTab4_3_1_2 <- tkframe(frameTab4_3_1)
        frameTab4_3_1_2_1 <- tkframe(frameTab4_3_1_2)
        frameTab4_3_1_2_2 <- tkframe(frameTab4_3_1_2)
        tkgrid(tklabel(frameTab4_3_1_1, text = "Likelihood ratio (LR)", font = "Helvetica 10 bold"))
        lrAll <- matrix(0, nL + 1, ncol(log10LikeHp) * ncol(log10LikeHd))
        rownames(lrAll) <- c(lociName, "Total")
        colnames(lrAll) <- rep("", ncol(log10LikeHp) * ncol(log10LikeHd))
        for(i in 1:ncol(log10LikeHp)){
          for(j in 1:ncol(log10LikeHd)){
            lrAll[, ncol(log10LikeHd) * (i - 1) + j] <- 10^(log10LikeHp[, i] - log10LikeHd[, j])
            colnames(lrAll)[ncol(log10LikeHd) * (i - 1) + j] <- paste(colnames(log10LikeHp)[i], " vs ", colnames(log10LikeHd)[j], sep = "")
            tkgrid(tklabel(frameTab4_3_1_2_1, text = paste("Hp", i, "/Hd", j, sep = "")))
            tkgrid(tklabel(frameTab4_3_1_2_2, text = paste(signif(lrAll[nL + 1, ncol(log10LikeHd) * (i - 1) + j], 3))))
          }
        }
        tkgrid(frameTab4_3_1_2_1, frameTab4_3_1_2_2, padx = 10)
        tkgrid(frameTab4_3_1_1, pady = 5)
        tkgrid(frameTab4_3_1_2, pady = 5)
        frameTab4_3_2 <- tkframe(frameTab4_3, relief = "groove", borderwidth = 2)
        frameTab4_3_2_1 <- tkframe(frameTab4_3_2)
        frameTab4_3_2_2 <- tkframe(frameTab4_3_2)
        frameTab4_3_2_2_1 <- tkframe(frameTab4_3_2_2)
        frameTab4_3_2_2_2 <- tkframe(frameTab4_3_2_2)
        tkgrid(tklabel(frameTab4_3_2_1, text = "Ratio of maximum likelihoods", font = "Helvetica 10 bold"))
        tkgrid(tklabel(frameTab4_3_2_2_1, text = paste("Hp", likeHpMaxPos, " / Hd", likeHdMaxPos, sep = "")))
        tkgrid(tklabel(frameTab4_3_2_2_2, text = paste(signif(lrMax, 3))))
        tkgrid(frameTab4_3_2_2_1, frameTab4_3_2_2_2, padx = 10)
        tkgrid(frameTab4_3_2_1, pady = 5)
        tkgrid(frameTab4_3_2_2, pady = 5)
        tkgrid(frameTab4_3_1, frameTab4_3_2, padx = 10, pady = 5, sticky = "n")
        tkgrid(frameTab4_3, padx = 20, pady = 5)
        log10LikeHp[which(log10LikeHp == -Inf, arr.ind = TRUE)] <- "NA"
        log10LikeHd[which(log10LikeHd == -Inf, arr.ind = TRUE)] <- "NA"
        
        frameTab4_4 <- tkframe(frameTab4)
        reportButt <- tkbutton(frameTab4_4, text = "    Report   ", cursor = "hand2", command = function() reportMake(HpKnownName, HdKnownName, log10LikeHp, log10LikeHd, lrAll, likeHpMax, likeHdMax, lrMax, mrEstimateHpAll, mrEstimateHdAll, dEstimateHpAll, dEstimateHdAll, calcCond, mrOne, dyeNames, calcTime))
        tkgrid(reportButt, padx = 10, pady = 5)
        tkgrid(frameTab4_4, pady = 5)
      }
    }
    tkgrid(frameTab4)
  }
  
  
  ########################
  # Make toplevel window #
  ########################
  
  tf <- tktoplevel()
  tkwm.title(tf, softVer)
  topMenu <- tkmenu(tf)
  tkconfigure(tf, menu = topMenu)
  tabs <- tk2notebook(tf, tabs = c("Files", "Calculation", "Probabilistic genotyping", "Likelihood ratio"))
  tkpack(tabs, fill = "both", expand = 1)
  tab1 <- tk2notetab(tabs, "Files")
  tab2 <- tk2notetab(tabs, "Calculation")
  tab3 <- tk2notetab(tabs, "Probabilistic genotyping")
  tab4 <- tk2notetab(tabs, "Likelihood ratio")
  frameTab1 <- tkframe(tab1)
  tab1Make()
  frameTab2 <- tkframe(tab2)
  tab2Make()
  frameTab3 <- tkframe(tab3)
  tab3Make()
  frameTab4 <- tkframe(tab4)
  tab4Make()
}
