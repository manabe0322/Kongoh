Kongoh <- function(){
  if(!require(tcltk)){
    stop("Package tcltk is required!")
  }
  if(!require(tcltk2)){
    stop("Package tcltk2 is required!")
  }
  if(!require(gtools)){
    stop("Package gtools is required!")
  }
  if(!require(MASS)){
    stop("Package truncnorm is required!")
  }
  if(!require(truncnorm)){
    stop("Package truncnorm is required!")
  }
  if(!require(snow)){
    stop("Package snow is required!")
  }
  tclRequire("Tktable")

  IDloci <- c("D8S1179", "D21S11", "D7S820", "CSF1PO", "D3S1358", "TH01", "D13S317", "D16S539", "D2S1338", "D19S433", "vWA", "TPOX", "D18S51", "D5S818", "FGA")

  Amp.mat <- matrix(c("Marker", IDloci, 
                      "Variance", 33.3, 9.71, 9.66, 18.3, 24.4, 30.8, 16.3, 20.3, 21.0, 24.3, 9.83, 8.06, 8.67, 9.51, 5.54, 
                      "Mean", 1.38, 1.05, 0.794, 0.948, 1.25, 1.31, 1.17, 1.10, 0.877, 0.911, 0.950, 0.846, 0.754, 0.860, 0.792
                      ), nrow = 16, ncol = 3
                    )
  Hb.mat <- matrix(c("Marker", IDloci, 
                     "Sigma", 69.2, 54.6, 41.2, 56.6, 67.3, 62.9, 67.9, 78.2, 74.0, 45.3, 66.0, 42.3, 53.3, 38.7, 51.5, 
                     "Myu", 0.00428, -0.00686, -0.00171, -0.0358, 0.0439, -0.0377, -0.0322, 0.0127, -0.022, -0.0179, 0.00314, -0.00239, -0.0363, -0.0327, -0.0635
                     ), nrow = 16, ncol = 3
                   )
  SR.mat <- matrix(c("Marker", IDloci, 
                     "Param1", 14.7, 11.0, 19.2, 13.2, 11.0, 62.5, 42.9, 14.3, 9.21, 9.02, 20.9, 51.7, 8.03, 16.6, 13.2, 
                     "Param2", -0.0506, -0.229, -0.0427, -0.0516, -0.0714, -0.0196, -0.0711, -0.0591, -0.104, -0.0739, -0.172, -0.0259, -0.0370, -0.0467, -0.0832, 
                     "Param3", -0.0275, -0.251, 0.00882, 0.00974, 0.00950, 0.00582, 0.0114, 0.0106, -0.0784, 0.0107, 0.0145, 0.00584, 0.00750, 0.00952, 0.00716, 
                     "Param4", 0.00820, 0.0102, NA, NA, NA, NA, NA, NA, 0.00804, NA, NA, NA, NA, NA, NA, 
                     "Param5", 19.1, NA, NA, NA, NA, NA, NA, NA, 48.0, NA, NA, NA, NA, NA, NA, 
                     "Param6", -1.43, NA, NA, NA, NA, NA, NA, NA, -2.34, NA, NA, NA, NA, NA, NA
                     ), nrow = 16, ncol = 7
                   )

  JP.ID.aList <- list()
  JP.ID.aList[[1]] <- c(7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
  JP.ID.aList[[2]] <- c(27, 28, 28.2, 29, 30, 30.2, 30.3, 31, 31.2, 32, 32.2, 33, 33.1, 33.2, 34, 34.2)
  JP.ID.aList[[3]] <- c(7, 8, 9, 9.1, 10, 10.1, 10.3, 11, 12, 13, 14, 15)
  JP.ID.aList[[4]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  JP.ID.aList[[5]] <- c(12, 13, 14, 15, 16, 17, 18, 19, 21)
  JP.ID.aList[[6]] <- c(5, 6, 7, 8, 9, 9.3, 10)
  JP.ID.aList[[7]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  JP.ID.aList[[8]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  JP.ID.aList[[9]] <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  JP.ID.aList[[10]] <- c(9.2, 10.2, 11, 11.2, 12, 12.2, 13, 13.2, 14, 14.2, 15, 15.2, 16, 16.2, 17.2, 18)
  JP.ID.aList[[11]] <- c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  JP.ID.aList[[12]] <- c(7, 8, 9, 10, 11, 12, 13, 14)
  JP.ID.aList[[13]] <- c(10, 11, 12, 13, 14, 15, 16, 17, 17.1, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
  JP.ID.aList[[14]] <- c(7, 8, 9, 10, 11, 12, 13, 14, 15)
  JP.ID.aList[[15]] <- c(17, 18, 19, 20, 21, 22, 22.2, 23, 23.2, 24, 24.2, 25, 25.2, 26, 27, 28)
  names(JP.ID.aList) <- IDloci

  JP.ID.bList <- list()
  JP.ID.bList[[1]] <- c(123, 131, 135, 139, 143, 147, 151, 155, 159, 163, 167)
  JP.ID.bList[[2]] <- c(198, 202, 204, 206, 210, 212, 213, 214, 216, 218, 220, 222, 223, 224, 226, 228)
  JP.ID.bList[[3]] <- c(261, 265, 269, 270, 273, 274, 276, 277, 281, 285, 289, 293)
  JP.ID.bList[[4]] <- c(309, 313, 317, 321, 325, 329, 333, 337, 341, 345)
  JP.ID.bList[[5]] <- c(113, 117, 121, 125, 129, 133, 137, 141, 149)
  JP.ID.bList[[6]] <- c(168, 172, 176, 180, 184, 187, 188)
  JP.ID.bList[[7]] <- c(201, 205, 209, 213, 217, 221, 225, 229, 233)
  JP.ID.bList[[8]] <- c(260, 264, 268, 272, 276, 280, 284, 288, 292)
  JP.ID.bList[[9]] <- c(311, 315, 319, 323, 327, 331, 335, 339, 343, 347, 351, 355, 359)
  JP.ID.bList[[10]] <- c(108, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132, 134, 136, 140, 142)
  JP.ID.bList[[11]] <- c(164, 168, 172, 176, 180, 184, 188, 192, 196, 200)
  JP.ID.bList[[12]] <- c(221, 225, 229, 233, 237, 241, 245, 249)
  JP.ID.bList[[13]] <- c(276, 280, 284, 288, 292, 296, 300, 304, 305, 308, 312, 316, 320, 324, 328, 332, 336, 340, 344)
  JP.ID.bList[[14]] <- c(134, 138, 142, 146, 150, 154, 158, 162, 166)
  JP.ID.bList[[15]] <- c(214, 218, 222, 226, 230, 234, 236, 238, 240, 242, 244, 246, 248, 250, 254, 258)
  names(JP.ID.bList) <- IDloci

  MRinterval1 <- tclVar("0.02")
  MRinterval2 <- tclVar("0.1")
  ATvar <- tclVar("30")
  MCvar <- tclVar("1000")
  MAF.var <- tclVar("0.00185")
  Theta.var <- tclVar("0")
  SFvar <- tclVar("1")
  Max.sph.var <- tclVar("1000")
  Hbf.var <- tclVar("0")
  Weight.finish <- tclVar("0")
  LR.finish <- tclVar("0")
  NC.Var1 <- tclVar("1")
  NC.Var2 <- tclVar("4")

  LRcalcbutt.state <- tclVar("disabled")
  LRcalcbutt.cursor <- tclVar("arrow")

  CSP.filepath <- tclVar("")
  Ref.filepath <- tclVar("")
  AF.filepath <- tclVar("")
  CSP.var <- tclVar("")
  Ref.var <- tclVar("")
  AF.var <- tclVar("")
  CSPbutt.state <- tclVar("disabled")
  CSPbutt.cursor <- tclVar("arrow")
  Refbutt.state <- tclVar("disabled")
  Refbutt.cursor <- tclVar("arrow")
  Afbutt.state <- tclVar("disabled")
  Afbutt.cursor <- tclVar("arrow")

  tkspinbox <- function(parent, ...){
    tkwidget(parent, "tk::spinbox", ...)
  }

  Sapplywhich <- function(Rightvalue, Leftvec){
    return(which(Leftvec == Rightvalue))
  }

  Apply.prod <- function(Vec1, Vec2){
    return(Vec1 * Vec2)
  }

  AFlist.make <- function(){
    if(paste(tclvalue(AF.var)) == ""){
      tkmessageBox(message = "Load Allele Frequencies!", icon = "error", type = "ok")
    }else{
      freqFile <- read.csv(tclvalue(AF.filepath), header = TRUE, as.is = TRUE, na.strings = "")
      freqFile <- as.matrix(freqFile)
      aList <- cpList <- list()
      for(i in 1:(ncol(freqFile) - 1)){
        freqoneL <- freqFile[, i + 1]
        cpList[[i]] <- freqoneL[!is.element(freqoneL, NA)]
        aList[[i]] <- freqFile[!is.element(freqoneL, NA), 1]
      }
    }
    names(aList) <- names(cpList) <- colnames(freqFile)[2:ncol(freqFile)]
    return(list(aList, cpList))
  }

  TAB6 <- function(LRHplist = NULL, LRHdlist = NULL, Kgtmat = NULL, HpKCID = NULL, HdKCID = NULL, Hp.var.list = NULL, Hd.var.list = NULL){
    tkdestroy(frame6)
    frame6 <<- tkframe(tab6)
    if(paste(tclvalue(CSP.var)) == ""){
      tkgrid(tklabel(frame6, text = "        Load Crime Stain Profile!        "), pady = 10, sticky = "w")
    }
    if(paste(tclvalue(Ref.var)) == ""){
      tkgrid(tklabel(frame6, text = "        Load Reference Profile!        "), pady = 10, sticky = "w")
    }
    if(paste(tclvalue(AF.var)) == ""){
      tkgrid(tklabel(frame6, text = "        Load Allele Frequencies!        "), pady = 10, sticky = "w")
    }
    if(all(c(paste(tclvalue(CSP.var)), paste(tclvalue(Ref.var)), paste(tclvalue(AF.var))) != "")){
      if(paste(tclvalue(Weight.finish)) == "0"){
        tkgrid(tklabel(frame6, text = "        Calculate weight values!        "), pady = 10, sticky = "w")
      }
      if(paste(tclvalue(LR.finish)) == "0"){
        tkgrid(tklabel(frame6, text = "        Calculate likelihood values!        "), pady = 10, sticky = "w")
      }else{
        frame6_1 <- tkframe(frame6, relief = "groove", borderwidth = 2)
        frame6_1_1 <- tkframe(frame6_1)
        frame6_1_2 <- tkframe(frame6_1)
        frame6_1_2_1 <- tkframe(frame6_1_2)
        frame6_1_2_2 <- tkframe(frame6_1_2)
        frame6_1_2_3 <- tkframe(frame6_1_2)
        frame6_1_2_4 <- tkframe(frame6_1_2)

        tkgrid(tklabel(frame6_1_1, text = "Prosecutor hypothesis", font = "Helvetica 10 bold"))
        tkgrid(tklabel(frame6_1_2_1, text = ""))
        tkgrid(tklabel(frame6_1_2_2, text = "Likelihood"))
        tkgrid(tklabel(frame6_1_2_3, text = "Estimated mixture ratio"))
        tkgrid(tklabel(frame6_1_2_4, text = "Estimated degradation"))

        NL <- nrow(Kgtmat)
        HpLs.perL <- matrix(0, NL, length(LRHplist))
        HpLs <- rep(0, length(LRHplist))
        for(i in 1:length(LRHplist)){
          HpLs[i] <- LRHplist[[i]][[2]]
          NCHp <- LRHplist[[i]][[5]]
          tkgrid(tklabel(frame6_1_2_1, text = paste("Hp", i, " : ", NCHp, "-person", sep = "")))
          tkgrid(tklabel(frame6_1_2_2, text = paste(signif(HpLs[i], 3))))
          if(is.matrix(LRHplist[[i]][[4]])){
            tkgrid(tklabel(frame6_1_2_3, text = paste(LRHplist[[i]][[4]][1, 1:(ncol(LRHplist[[i]][[4]]) - 1)])))
            tkgrid(tklabel(frame6_1_2_4, text = paste(LRHplist[[i]][[4]][1, ncol(LRHplist[[i]][[4]])])))
          }else{
            tkgrid(tklabel(frame6_1_2_3, text = " - "))
            tkgrid(tklabel(frame6_1_2_4, text = " - "))
          }
        }
        tkgrid(frame6_1_2_1, frame6_1_2_2, frame6_1_2_3, frame6_1_2_4, padx = 10)
        tkgrid(frame6_1_1, pady = 10)
        tkgrid(frame6_1_2, pady = 10)
        tkgrid(frame6_1, padx = 20, pady = 10)

        frame6_2 <- tkframe(frame6, relief = "groove", borderwidth = 2)
        frame6_2_1 <- tkframe(frame6_2)
        frame6_2_2 <- tkframe(frame6_2)
        frame6_2_2_1 <- tkframe(frame6_2_2)
        frame6_2_2_2 <- tkframe(frame6_2_2)
        frame6_2_2_3 <- tkframe(frame6_2_2)
        frame6_2_2_4 <- tkframe(frame6_2_2)

        tkgrid(tklabel(frame6_2_1, text = "Defense hypothesis", font = "Helvetica 10 bold"))
        tkgrid(tklabel(frame6_2_2_1, text = ""))
        tkgrid(tklabel(frame6_2_2_2, text = "Likelihood"))
        tkgrid(tklabel(frame6_2_2_3, text = "Estimated mixture ratio"))
        tkgrid(tklabel(frame6_2_2_4, text = "Estimated degradation"))

        HdLs <- rep(0, length(LRHdlist))
        for(i in 1:length(LRHdlist)){
          HdLs[i] <- LRHdlist[[i]][[2]]
          NCHd <- LRHdlist[[i]][[5]]
          tkgrid(tklabel(frame6_2_2_1, text = paste("Hd", i, " : ", NCHd, "-person", sep = "")))
          tkgrid(tklabel(frame6_2_2_2, text = paste(signif(HdLs[i],3))))
          if(is.matrix(LRHdlist[[i]][[4]])){
            tkgrid(tklabel(frame6_2_2_3, text = paste(LRHdlist[[i]][[4]][1, 1:(ncol(LRHdlist[[i]][[4]]) - 1)])))
            tkgrid(tklabel(frame6_2_2_4, text = paste(LRHdlist[[i]][[4]][1, ncol(LRHdlist[[i]][[4]])])))
          }else{
            tkgrid(tklabel(frame6_2_2_3, text = " - "))
            tkgrid(tklabel(frame6_2_2_4, text = " - "))
          }
        }
        tkgrid(frame6_2_2_1, frame6_2_2_2, frame6_2_2_3, frame6_2_2_4, padx = 10)
        tkgrid(frame6_2_1, pady = 10)
        tkgrid(frame6_2_2, pady = 10)
        tkgrid(frame6_2, padx = 20, pady = 10)

        frame6_3 <- tkframe(frame6)
        frame6_3_1 <- tkframe(frame6_3, relief = "groove", borderwidth = 2)
        frame6_3_1_1 <- tkframe(frame6_3_1)
        frame6_3_1_2 <- tkframe(frame6_3_1)
        frame6_3_1_2_1 <- tkframe(frame6_3_1_2)
        frame6_3_1_2_2 <- tkframe(frame6_3_1_2)

        tkgrid(tklabel(frame6_3_1_1, text = "Likelihood ratio (LR)", font = "Helvetica 10 bold"))
        NHNC_Hp <- length(LRHplist)
        NHNC_Hd <- length(LRHdlist)
        LRmat <- matrix(0, NL + 1, NHNC_Hp * NHNC_Hd)
        rownames(LRmat) <- c(IDloci, "Total")
        LRnamemat <- matrix("", 1, NHNC_Hp * NHNC_Hd + 1)
        for(i in 1:NHNC_Hp){
          for(j in 1:NHNC_Hd){
            LRmat[1:NL, NHNC_Hd * (i - 1) + j] <- LRHplist[[i]][[1]] / LRHdlist[[j]][[1]]
            LRmat[NL + 1, NHNC_Hd * (i - 1) + j] <- LRHplist[[i]][[2]] / LRHdlist[[j]][[2]]
            LRnamemat[1, NHNC_Hd * (i - 1) + j + 1] <- paste(LRHplist[[i]][[5]], "-person vs ", LRHdlist[[j]][[5]], "-person", sep = "")
            tkgrid(tklabel(frame6_3_1_2_1, text = paste("Hp", i, "/Hd", j, sep = "")))
            tkgrid(tklabel(frame6_3_1_2_2, text = paste(signif(LRHplist[[i]][[2]] / LRHdlist[[j]][[2]], 3))))
          }
        }
        tkgrid(frame6_3_1_2_1, frame6_3_1_2_2, padx = 10)
        tkgrid(frame6_3_1_1, pady = 10)
        tkgrid(frame6_3_1_2, pady = 10)

        frame6_3_2 <- tkframe(frame6_3, relief = "groove", borderwidth = 2)
        frame6_3_2_1 <- tkframe(frame6_3_2)
        frame6_3_2_2 <- tkframe(frame6_3_2)
        frame6_3_2_2_1 <- tkframe(frame6_3_2_2)
        frame6_3_2_2_2 <- tkframe(frame6_3_2_2)

        tkgrid(tklabel(frame6_3_2_1, text = "Ratio of maximum likelihoods", font = "Helvetica 10 bold"))
        HpLmaxpos <- which(HpLs == max(HpLs))[1]
        HdLmaxpos <- which(HdLs == max(HdLs))[1]
        tkgrid(tklabel(frame6_3_2_2_1, text = paste("Hp", HpLmaxpos, " / Hd", HdLmaxpos, sep = "")))
        tkgrid(tklabel(frame6_3_2_2_2, text = paste(signif(HpLs[HpLmaxpos] / HdLs[HdLmaxpos], 3))))
        tkgrid(frame6_3_2_2_1, frame6_3_2_2_2, padx = 10)
        tkgrid(frame6_3_2_1, pady = 10)
        tkgrid(frame6_3_2_2, pady = 10)
        tkgrid(frame6_3_1, frame6_3_2, padx = 10, pady = 10, sticky = "n")
        tkgrid(frame6_3, padx = 20, pady = 10)

        Report.make <- function(){
          Save.as <- tkgetSaveFile(filetypes = "{{CSV Files} {.csv}}")
          if(length(as.character(Save.as)) > 0){
            NL <- length(LRHplist[[1]][[1]])
            NHNC_Hp <- length(LRHplist)
            NHNC_Hd <- length(LRHdlist)
            Hpmat <- matrix(0, NL + 1, NHNC_Hp)
            Hdmat <- matrix(0, NL + 1, NHNC_Hd)
            rownames(Hpmat) <- rownames(Hdmat) <- c(IDloci, "Total")
            Colname.mat_Hp <- matrix("", 1, NHNC_Hp + 1)
            Colname.mat_Hd <- matrix("", 1, NHNC_Hd + 1)
            HNCvec_Hp <- rep(0, NHNC_Hp)
            HNCvec_Hd <- rep(0, NHNC_Hd)

            for(i in 1:NHNC_Hp){
              Hpmat[1:NL, i] <- LRHplist[[i]][[1]]
              Hpmat[NL + 1, i] <- LRHplist[[i]][[2]]
              HNCvec_Hp[i] <- LRHplist[[i]][[5]]
              Colname.mat_Hp[1, i + 1] <- paste(LRHplist[[i]][[5]], "-person", sep = "")
            }
            for(i in 1:NHNC_Hd){
              Hdmat[1:NL, i] <- LRHdlist[[i]][[1]]
              Hdmat[NL + 1, i] <- LRHdlist[[i]][[2]]
              HNCvec_Hd[i] <- LRHdlist[[i]][[5]]
              Colname.mat_Hd[1, i + 1] <- paste(LRHdlist[[i]][[5]], "-person", sep = "")
            }

            Max.HNC_Hp <- max(HNCvec_Hp)
            Max.HNC_Hd <- max(HNCvec_Hd)
            MRmat.Hp <- matrix("", Max.HNC_Hp, NHNC_Hp * 3)
            MRmat.Hd <- matrix("", Max.HNC_Hd, NHNC_Hd * 3)
            colnames(MRmat.Hp) <- rep("", NHNC_Hp * 3)
            colnames(MRmat.Hd) <- rep("", NHNC_Hd * 3)
            Degmat.Hp <- matrix("", 1, NHNC_Hp)
            Degmat.Hd <- matrix("", 1, NHNC_Hd)
            rownames(Degmat.Hp) <- rownames(Degmat.Hd) <- ""
            for(i in 1:NHNC_Hp){
              if(is.matrix(LRHplist[[i]][[4]])){
                HNC.Hp <- ncol(LRHplist[[i]][[4]]) - 1
                MRmat.Hp[1:HNC.Hp, 3 * i - 1] <- "Unknown"
                colnames(MRmat.Hp)[3 * i] <- paste(LRHplist[[i]][[5]], "-person", sep = "")
                MRmat.Hp[1:HNC.Hp, 3 * i] <- LRHplist[[i]][[4]][1, 1:HNC.Hp]
                for(j in 1:length(HpKCID)){
                  MRmat.Hp[LRHplist[[i]][[3]][j], 3 * i - 1] <- colnames(Kgtmat)[2 * HpKCID[j] - 1]
                }
                Degmat.Hp[1, i] <- LRHplist[[i]][[4]][HNC.Hp + 1]
              }
            }
            for(i in 1:NHNC_Hd){
              if(is.matrix(LRHdlist[[i]][[4]])){
                HNC.Hd <- ncol(LRHdlist[[i]][[4]]) - 1
                MRmat.Hd[1:HNC.Hd, 3 * i - 1] <- "Unknown"
                colnames(MRmat.Hd)[3 * i] <- paste(LRHdlist[[i]][[5]], "-person", sep = "")
                MRmat.Hd[1:HNC.Hd, 3 * i] <- LRHdlist[[i]][[4]][1, 1:HNC.Hd]
                for(j in 1:length(HdKCID)){
                  MRmat.Hd[LRHdlist[[i]][[3]][j], 3 * i - 1] <- colnames(Kgtmat)[2 * HdKCID[j] - 1]
                }
                Degmat.Hd[1, i] <- LRHdlist[[i]][[4]][HNC.Hd + 1]
              }
            }

            write.table("======== Imput files ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = FALSE)
            write.table(paste("Crime stain profile : ", tclvalue(CSP.var), sep=""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Reference profile : ", tclvalue(Ref.var), sep=""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Allele frequencies : ", tclvalue(AF.var), sep=""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table("======== Parameters ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Analytical Threshold : ", tclvalue(ATvar), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Number of Monte Carlo simulation : ", tclvalue(MCvar), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Minimum allele frequency : ", tclvalue(MAF.var), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("Theta value : ", tclvalue(Theta.var), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("Thresholds to exclude genotype combination", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("  - Stutter filter : ", tclvalue(SFvar), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("  - Maximum stutter peak height : ", tclvalue(Max.sph.var), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(paste("  - Heterozygote balance : ", tclvalue(Hbf.var), sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table("======== Hypothesis ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            KCname <- colnames(Kgtmat)[which(1:ncol(Kgtmat) %% 2 == 1)]
            KCname.Hp <- paste(KCname[which(as.numeric(sapply(Hp.var.list, tclvalue)) == 1)], collapse = " + ")
            write.table(paste("Prosecutor hypothesis (Hp) : ", KCname.Hp, " (+ unknown contributors)", sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            KCname.Hd <- paste(KCname[which(as.numeric(sapply(Hd.var.list, tclvalue)) == 1)], collapse = " + ")
            write.table(paste("Defense hypothesis (Hd) : ", KCname.Hd, " (+ unknown contributors)", sep = ""), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table("======== Likelihoods in Hp ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Colname.mat_Hp, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Hpmat, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = TRUE, col.names = FALSE, append = TRUE)

            write.table("---- Estimated Mixture Ratio ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(MRmat.Hp, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

            write.table("---- Estimated Degradation Parameters ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Colname.mat_Hp, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Degmat.Hp, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = TRUE, col.names = FALSE, append = TRUE)
            write.table("", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table("======== Likelihoods in Hd ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Colname.mat_Hd, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Hdmat, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = TRUE, col.names = FALSE, append = TRUE)

            write.table("---- Estimated Mixture Ratio ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(MRmat.Hd, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

            write.table("---- Estimated Degradation Parameters ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Colname.mat_Hd, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(Degmat.Hd, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = TRUE, col.names = FALSE, append = TRUE)
            write.table("", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

            write.table("======== Likelihood ratio (Hp/Hd) ========", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(LRnamemat, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(LRmat, file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = TRUE, col.names = FALSE, append = TRUE)

            write.table("---- Maximum Likelihood in Hp ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(matrix(c(HpLs[HpLmaxpos], paste("(", Colname.mat_Hp[1, HpLmaxpos+1], " contribution)"), sep = ""), 1, 2), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("---- Maximum Likelihood in Hd ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(matrix(c(HdLs[HdLmaxpos], paste("(", Colname.mat_Hd[1, HdLmaxpos+1], " contribution)"), sep = ""), 1, 2), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table("---- Ratio of Maximum Likelihoods ----", file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
            write.table(matrix(HpLs[HpLmaxpos] / HdLs[HdLmaxpos], 1, 1), file = paste(tclvalue(Save.as), ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }
        frame6_4 <- tkframe(frame6)
        tkgrid(tkbutton(frame6_4, text = "    Report   ", cursor = "hand2", command = function() Report.make()))
        tkgrid(frame6_4, pady = 10)
      }
    }
    tkgrid(frame6)
  }

  TAB4 <- function(){
    options(show.error.messages = TRUE)
    tkdestroy(frame4)
    tkdestroy(frame5)
    frame4 <<- tkframe(tab4)
    frame5 <<- tkframe(tab4)
    if(paste(tclvalue(CSP.var)) == ""){
      tkgrid(tklabel(frame4, text = "        Load Crime Stain Profile!        "), pady = 10, sticky = "w")
    }
    if(paste(tclvalue(Ref.var)) == ""){
      tkgrid(tklabel(frame4, text = "        Load Reference Profile!        "), pady = 10, sticky = "w")
    }
    if(paste(tclvalue(AF.var)) == ""){
      tkgrid(tklabel(frame4, text = "        Load Allele Frequencies!        "), pady = 10, sticky = "w")
    }
    if(all(c(paste(tclvalue(CSP.var)), paste(tclvalue(Ref.var)), paste(tclvalue(AF.var))) != "")){
      CSPFile <- read.csv(tclvalue(CSP.filepath), header = TRUE)
      Llabel.CSP <- as.vector(CSPFile$Marker)
      Llabel.CSP <- gsub(" ", "", Llabel.CSP, fixed = TRUE)
      if(any("AMEL" %in% Llabel.CSP)){
        CSPFile <- CSPFile[-which(Llabel.CSP == "AMEL"), ]
        Llabel.CSP <- Llabel.CSP[-which(Llabel.CSP == "AMEL")]
      }

      RefFile <- read.csv(tclvalue(Ref.filepath), header = TRUE)
      Llabel.Ref <- as.vector(RefFile$Marker)
      Llabel.Ref <- gsub(" ", "", Llabel.Ref, fixed = TRUE)
      if(any("AMEL" %in% Llabel.Ref)){
        RefFile <- RefFile[-which(Llabel.Ref == "AMEL"), ]
        Llabel.Ref <- Llabel.Ref[-which(Llabel.Ref == "AMEL")]
      }

      Kgtmat <- RefFile[, -1]
      Kgtmat <- matrix(as.numeric(as.matrix(Kgtmat)), nrow(Kgtmat), ncol(Kgtmat))
      colnames(Kgtmat) <- names(RefFile[, -1])

      FRAME45 <- function(PGresults = NULL, Kgtmat = NULL, aList = NULL, cpList = NULL, Qfreqvec = NULL){
        tkdestroy(frame4)
        tkdestroy(frame5)
        frame4 <<- tkframe(tab4, relief = "groove", borderwidth = 2)
        frame5 <<- tkframe(tab4, relief = "groove", borderwidth = 2)
        if(all(c(paste(tclvalue(CSP.var)), paste(tclvalue(Ref.var)), paste(tclvalue(AF.var))) != "")){
          LRcalc <- function(Hp.var.list, Hd.var.list){
            Hpvarvec <- as.numeric(sapply(Hp.var.list, tclvalue))
            HpKCID <- which(Hpvarvec == 1)
            Hdvarvec <- as.numeric(sapply(Hd.var.list, tclvalue))
            HdKCID <- which(Hdvarvec == 1)
            Kgt2pos <- sort(unique(c(2 * HpKCID - 1, 2 * HpKCID, 2 * HdKCID - 1, 2 * HdKCID)))
            LRHplist <- list()
            if(length(HpKCID) != 0){
              Kgtpos <- sort(c(2 * HpKCID - 1, 2 * HpKCID))
              for(i in 1:length(PGresults)){
                LRHplist[[i]] <- Likelihood.calc(PGresults[[i]][[1]], PGresults[[i]][[2]], PGresults[[i]][[3]], PGresults[[i]][[4]], PGresults[[i]][[5]], aList, cpList, PGresults[[i]][[6]], as.numeric(tclvalue(MAF.var)), as.numeric(tclvalue(Theta.var)), Kgtmat[, Kgtpos], Kgtmat[, Kgt2pos])
              }
              LRHplist <- LRHplist[which(sapply(LRHplist, length) != 0)]
            }else{
              for(i in 1:length(PGresults)){
                LRHplist[[i]] <- Likelihood.calc(PGresults[[i]][[1]], PGresults[[i]][[2]], PGresults[[i]][[3]], PGresults[[i]][[4]], PGresults[[i]][[5]], aList, cpList, PGresults[[i]][[6]], as.numeric(tclvalue(MAF.var)), as.numeric(tclvalue(Theta.var)), NULL, Kgtmat[, Kgt2pos])
              }
            }
            LRHdlist <- list()
            if(length(HdKCID) != 0){
              Kgtpos <- sort(c(2 * HdKCID - 1, 2 * HdKCID))
              for(i in 1:length(PGresults)){
                LRHdlist[[i]] <- Likelihood.calc(PGresults[[i]][[1]], PGresults[[i]][[2]], PGresults[[i]][[3]], PGresults[[i]][[4]], PGresults[[i]][[5]], aList, cpList, PGresults[[i]][[6]], as.numeric(tclvalue(MAF.var)), as.numeric(tclvalue(Theta.var)), Kgtmat[, Kgtpos], Kgtmat[, Kgt2pos])
              }
              LRHdlist <- LRHdlist[which(sapply(LRHdlist, length) != 0)]
            }else{
              for(i in 1:length(PGresults)){
                LRHdlist[[i]] <- Likelihood.calc(PGresults[[i]][[1]], PGresults[[i]][[2]], PGresults[[i]][[3]], PGresults[[i]][[4]], PGresults[[i]][[5]], aList, cpList, PGresults[[i]][[6]], as.numeric(tclvalue(MAF.var)), as.numeric(tclvalue(Theta.var)), NULL, Kgtmat[, Kgt2pos])
              }
            }
            tclvalue(LR.finish) <- "1"
            TAB6(LRHplist, LRHdlist, Kgtmat, HpKCID, HdKCID, Hp.var.list, Hd.var.list)
            cat("Calculation of likelihood values was finished!", "\n")
            tk2notetab.select(Tabs, "Result")
          }

          GFsub.onecomb <- function(Hgtone, alone, cplone, Qfreq, MAF, theta, unkpos = NULL, knowngt = NULL){
            if(length(unkpos) == 0){
              return(1)
            }else{
              if(length(knowngt) == 0){
                Alnames <- sort(unique(Hgtone))
                Alcount <- rep(0, length(Alnames))
              }else{
                Alnames <- sort(unique(c(Hgtone, knowngt)))
                Alcount <- table(factor(knowngt, levels = Alnames))
              }
              Nal <- length(Hgtone[unkpos])
              GF <- rep(0, Nal)
              NKC <- length(knowngt) / 2
              Hgtone2 <- Hgtone[unkpos]
              for(i in 1:Nal){
                A1 <- Hgtone2[i]
                if(A1 == 99){
                  Afreq <- Qfreq
                }else if(is.element(A1, alone)){
                  Afreq <- cplone[which(alone == A1)]
                }else{
                  Afreq <- MAF
                }
                Alpos <- which(Alnames == A1)
                GF[i] <- ((Alcount[Alpos]) * theta + (1 - theta) * Afreq) / (1 + (2 * NKC - 2 + i) * theta)
                Alcount[Alpos] <- Alcount[Alpos] + 1
              }
              Hetero.judge <- round(Hgtone2[which(1:Nal %% 2 == 1)] - Hgtone2[which(1:Nal %% 2 == 0)], 1)
              return(prod(GF) * 2^length(which(Hetero.judge != 0)))
            }
          }

          Likelihood.oneL <- function(OPoneL, HgtoneL, ProdoneL, FreqoneL, alone, cplone, Qfreq, MAF, theta, Kgtvec = NULL, KQID = NULL, Kgtvec2 = NULL){
            NKC <- length(Kgtvec) / 2
            NMR <- ncol(ProdoneL)
            HNC <- ncol(HgtoneL) / 2
            Known.posmat <- matrix(0, NKC, nrow(HgtoneL))
            for(i in 1:NKC){
              KQIDone <- KQID[i]
              Known.posmat[i, ] <- apply(HgtoneL[, c(2 * KQIDone - 1, 2 * KQIDone), drop = FALSE], 1, setequal, y = Kgtvec[c(2 * i - 1, 2 * i)])
            }
            options(warn = -1)
            Known.pos <- which(apply(Known.posmat, 2, all))
            options(warn = 0)
            if(length(Known.pos) == 0){
              return(rep(0, NMR))
            }else{
              Unkpos <- setdiff(1:HNC, KQID)
              if(length(Unkpos) != 0){
                if(theta > 0){
                  Hgtknown <- HgtoneL[Known.pos, , drop = FALSE]
                  FreqoneL <- rep(0, nrow(Hgtknown))
                  for(k in 1:nrow(Hgtknown)){
                    FreqoneL[k] <- GFsub.onecomb(Hgtknown[k, ], alone, cplone, Qfreq, MAF, theta, c(2 * Unkpos - 1, 2 * Unkpos), knowngt = Kgtvec2)
                  }
                }else{
                  FreqoneL <- apply(FreqoneL[Known.pos, Unkpos, drop = FALSE], 1, prod)
                }
                Lone <- rep(0, NMR)
                for(j in 1:NMR){
                  Lone[j] <- sum(ProdoneL[Known.pos, j] * FreqoneL)
                }
              }else{
                Lone <- apply(ProdoneL[Known.pos, , drop = FALSE], 2, sum)
              }
              return(Lone)
            }
          }

          Likelihood.calc <- function(OPlist, Hgtlist, Prodlist, Freqlist, MRrefine, alist, cplist, Qfreqvec, MAF, theta, Kgtmat = NULL, Kgtmat2 = NULL){
            NL <- length(Hgtlist)
            NMR <- ncol(Prodlist[[1]])
            HNC <- ncol(Hgtlist[[1]]) / 2
            if(length(Kgtmat) != 0){
              NKC <- (ncol(Kgtmat) / 2)
              if(HNC < NKC){
                return(list(rep(0, NL), 0, 0, 0, HNC))
              }else{
                KQIDs <- permutations(n = HNC, r = NKC, repeats.allowed = FALSE)
                NKQID <- nrow(KQIDs)
                Likelihoods <- list()
                Likelihood.perMR <- matrix(0, NKQID, NMR)
                for(i in 1:NKQID){
                  Likelihood.KQID <- matrix(0, NL, NMR)
                  for(j in 1:NL){
                    Likelihood.KQID[j, ] <- Likelihood.oneL(OPlist[[j]], Hgtlist[[j]], Prodlist[[j]], Freqlist[[j]], alist[[j]], cplist[[j]], Qfreqvec[j], MAF, theta, Kgtmat[j, ], KQIDs[i, ], Kgtmat2[j, ])
                  }
                  Likelihoods[[i]] <- Likelihood.KQID
                  Likelihood.perMR[i, ] <- apply(Likelihood.KQID, 2, prod)
                }
                Maxpos <- which(Likelihood.perMR == max(Likelihood.perMR), arr.ind = TRUE)[1, ]
                Likelihood_perL <- Likelihoods[[Maxpos[1]]][, Maxpos[2]]
                return(list(Likelihood_perL, prod(Likelihood_perL), KQIDs[Maxpos[1], ], MRrefine[Maxpos[2], , drop = FALSE], HNC))
              }
            }else{
              Likelihood.perMR <- matrix(0, NL, NMR)
              for(i in 1:NL){
                if(theta > 0){
                  HgtoneL <- Hgtlist[[i]]
                  FreqoneL <- rep(0, nrow(HgtoneL))
                  for(k in 1:nrow(HgtoneL)){
                    FreqoneL[k] <- GFsub.onecomb(HgtoneL[k, ], alist[[i]], cplist[[i]], Qfreqvec[i], MAF, theta, 1:ncol(HgtoneL), knowngt = Kgtmat2[i, ])
                  }
                }else{
                  FreqoneL <- apply(Freqlist[[i]], 1, prod)
                }
                Likelihoods <- apply(Prodlist[[i]], 2, Apply.prod, Vec2 = FreqoneL)
                if(!is.vector(Likelihoods)){
                  Likelihood.perMR[i, ] <- apply(Likelihoods, 2, sum)
                }else{
                  Likelihood.perMR[i, ] <- Likelihoods
                }
              }
              Likelihood.perMR2 <- apply(Likelihood.perMR, 2, prod)
              Maxpos <- which(Likelihood.perMR2 == max(Likelihood.perMR2))
              Likelihood_perL <- Likelihood.perMR[, Maxpos]
              return(list(Likelihood_perL, prod(Likelihood_perL), 0, MRrefine[Maxpos, , drop = FALSE], HNC))
            }
          }

          if(paste(tclvalue(Weight.finish)) == "1"){
            tclvalue(LRcalcbutt.state) <- "normal"
            tclvalue(LRcalcbutt.cursor) <- "hand2"
          }
          NKCinput <- ncol(Kgtmat)/2
          KCname <- colnames(Kgtmat)[which(1:(NKCinput * 2) %% 2 == 1)]
          frame5_1 <- tkframe(frame5)
          frame5_Hp <- tkframe(frame5_1, relief = "groove", borderwidth = 2)
          frame5_Hd <- tkframe(frame5_1, relief = "groove", borderwidth = 2)
          frame5_Hp_1 <- tkframe(frame5_Hp)
          frame5_Hp_2 <- tkframe(frame5_Hp)
          frame5_Hd_1 <- tkframe(frame5_Hd)
          frame5_Hd_2 <- tkframe(frame5_Hd)
          frame5_2 <- tkframe(frame5)

          Hp.label <- tklabel(frame5_Hp_1, text = "Prosecutor hypothesis", font = "Helvetica 10 bold")
          Hd.label <- tklabel(frame5_Hd_1, text = "Defense hypothesis", font = "Helvetica 10 bold")
          Hp.var.list <- Hd.var.list <- list()
          for(i in 1:NKCinput){
            Hp.var.list[[i]] <- tclVar("0")
            Hd.var.list[[i]] <- tclVar("0")
            tkgrid(tkcheckbutton(frame5_Hp_2, text = paste(KCname[i]), variable = Hp.var.list[[i]]), sticky = "w")
            tkgrid(tkcheckbutton(frame5_Hd_2, text = paste(KCname[i]), variable = Hd.var.list[[i]]), sticky = "w")
          }
          tkgrid(tklabel(frame5_Hp_2, text = "(+ unknown contributor(s))"))
          tkgrid(tklabel(frame5_Hd_2, text = "(+ unknown contributor(s))"))

          LRcalc.butt <- tkbutton(frame5_2, text = "    Calculate   ", state = tclvalue(LRcalcbutt.state), 
                                  cursor = tclvalue(LRcalcbutt.cursor), command = function() LRcalc(Hp.var.list, Hd.var.list))

          tkgrid(tklabel(frame5, text = "2. Likelihood Calculation", fg = "darkblue", font = "Helvetica 10 bold"), padx = 20, pady = 20)
          tkgrid(Hp.label)
          tkgrid(Hd.label)
          tkgrid(frame5_Hp_1, padx = 20)
          tkgrid(frame5_Hp_2, padx = 20)
          tkgrid(frame5_Hd_1, padx = 20)
          tkgrid(frame5_Hd_2, padx = 20)
          tkgrid(frame5_Hp, pady = 10)
          tkgrid(frame5_Hd, pady = 10)
          tkgrid(LRcalc.butt)
          tkgrid(frame5_1, padx = 20)
          tkgrid(frame5_2, pady = 20)

          frame4_1 <- tkframe(frame4)
          frame4_2 <- tkframe(frame4)
          frame4_3 <- tkframe(frame4)
          frame4_4 <- tkframe(frame4)

          tkgrid(tklabel(frame4, text = "1. Weight Calculation", fg = "darkblue", font = "Helvetica 10 bold"), padx = 20, pady = 20)
          tkgrid(tklabel(frame4_1, text = "Set the range of assumed contributor numbers"))
          tkgrid(tklabel(frame4_1, text = "(1 - 4 contributors)"))
          NC1.label <- tklabel(frame4_2, text = "From")
          NC1.spinbox <- tkspinbox(frame4_2, from = 1, to = 4, increment = 1, textvariable = NC.Var1, width = 6, justify = "center", background = "white")
          NC2.label <- tklabel(frame4_3, text = "  To  ")
          NC2.spinbox <- tkspinbox(frame4_3, from = 1, to = 4, increment = 1, textvariable = NC.Var2, width = 6, justify = "center", background = "white")

          Analysis.butt <- tkbutton(frame4_4, text = "    Calculate   ", cursor = "hand2", command = function() Weight.calc())

          tkgrid(NC1.label, NC1.spinbox, padx = 10, pady = 10)
          tkgrid(NC2.label, NC2.spinbox, padx = 10, pady = 10)
          tkgrid(Analysis.butt, padx = 20, pady = 20)
          tkgrid(frame4_1, padx = 20, pady = 10)
          tkgrid(frame4_2)
          tkgrid(frame4_3)
          tkgrid(frame4_4)

          tkgrid(frame4, frame5, padx = 20, pady = 20, sticky = "n")
        }
      }

      Weight.calc <- function(){
        options(show.error.messages = TRUE)
        AFlist <- AFlist.make()
        aList <- AFlist[[1]]
        cpList <- AFlist[[2]]
        Llabel.AF <- names(aList)
        NCfrom <- round(as.numeric(tclvalue(NC.Var1)), 0)
        NCto <- round(as.numeric(tclvalue(NC.Var2)), 0)

        if(!setequal(Llabel.CSP, Llabel.Ref)){
          tkmessageBox(message = "Confirm locus names of your input files!", icon = "error", type = "ok")
          tk2notetab.select(Tabs, "Files")
        }else if((!all(is.element(Llabel.CSP, Llabel.AF))) || (!all(is.element(Llabel.Ref, Llabel.AF)))){
          tkmessageBox(message = "Confirm locus names of your input files!", icon = "error", type = "ok")
          tk2notetab.select(Tabs, "Files")
        }else if((tclvalue(NC.Var1) == "") || (tclvalue(NC.Var2) == "")){
          tkmessageBox(message = "Set the number of contributors!", icon = "error", type = "ok")
        }else if((NCfrom <= 0) || (NCto <= 0)){
          tkmessageBox(message = "You must set the number of contributors more than 1!", icon = "error", type = "ok")
        }else{
          if((NCfrom >= 5) || (NCto >= 5)){
            NCyesno <- tkmessageBox(message = "Calculation of 5 or more persons' contribution is extremely time consuming. Do you continue the weight calculation?", icon = "question", type = "yesno")
            if(as.character(NCyesno) == "no"){
              options(show.error.messages = FALSE)
              stop()
            }
          }else if((NCfrom == 4) || (NCto == 4)){
            NCyesno2 <- tkmessageBox(message = "It will take approximate 10 hours for calculating weight values in 4 contributors. Do you continue the weight calculation?", icon = "question", type = "yesno")
            if(as.character(NCyesno2) == "no"){
              options(show.error.messages = FALSE)
              stop()
            }
          }
          NL <- nrow(CSPFile)

          Param.A <- matrix(as.numeric(Amp.mat[2:nrow(Amp.mat), 2:ncol(Amp.mat)]), 15, 2)
          Param.Hb <- matrix(as.numeric(Hb.mat[2:nrow(Hb.mat), 2:ncol(Hb.mat)]), 15, 2)
          Param.SR <- matrix(as.numeric(SR.mat[2:nrow(SR.mat), 2:ncol(SR.mat)]), 15, 6)

          OPHdata.refine <- function(LID, Peak, Size, Height, Kgtmat, LOD){
            Peakone <- Peak[LID, ]
            Peakone <- as.numeric(Peakone[!is.na(Peakone)])
            Sizeone <- Size[LID, ]
            Sizeone <- as.numeric(Sizeone[!is.na(Sizeone)])
            Heightone <- Height[LID, ]
            Heightone <- as.numeric(Heightone[!is.na(Heightone)])
            Up.LOD.pos <- which(Heightone >= LOD)
            Peakone <- Peakone[Up.LOD.pos]
            Sizeone <- Sizeone[Up.LOD.pos]
            Heightone <- Heightone[Up.LOD.pos]
            Dropal <- setdiff(Kgtmat[LID, ], Peakone)
            if(length(Dropal) != 0){
              Sizeone <- c(Sizeone, Size.make(LID, Peakone, Sizeone, Dropal))
              Peakone <- c(Peakone, Dropal)
              Heightone <- c(Heightone, rep(0, length(Dropal)))
            }
            OPHone <- rbind(Peakone, Sizeone, Heightone)
            return(OPHone[, order(OPHone[1, ]), drop = FALSE])
          }

          Size.make <- function(LID, Peakone, Sizeone, Dropal){
            NDal <- length(Dropal)
            Dalsize <- rep(0, NDal)
            for(i in 1:NDal){
              if(length(Peakone) > 0){
                Distance.DOP <- Dropal[i] - Peakone
                DOPpos <- which(round(abs(Distance.DOP), 1) == min(round(abs(Distance.DOP), 1)))[1]
                DOPmin <- Distance.DOP[DOPpos]
                if(round(DOPmin %% 1, 1) == 0){
                  Dalsize[i] <- Sizeone[DOPpos] + 4 * DOPmin
                }else if(round(DOPmin %% 1, 1) == 0.1){
                  Dalsize[i] <- Sizeone[DOPpos] + 4 * (DOPmin %/% 1 + 0.25)
                }else if(round(DOPmin %% 1, 1) == 0.2){
                  Dalsize[i] <- Sizeone[DOPpos] + 4 * (DOPmin %/% 1 + 0.5)
                }else if(round(DOPmin %% 1, 1) == 0.3){
                  Dalsize[i] <- Sizeone[DOPpos] + 4 * (DOPmin %/% 1 + 0.75)
                }
              }else{
                alone <- JP.ID.aList[[LID]]
                blone <- JP.ID.bList[[LID]]
                Dropal.pos <- which(alone == Dropal[i])
                if(length(Dropal.pos) != 0){
                  Dalsize[i] <- blone[Dropal.pos]
                }else{
                  Distance.DOP <- Dropal[i] - alone
                  DOPpos <- which(round(abs(Distance.DOP), 1) == min(round(abs(Distance.DOP), 1)))[1]
                  DOPmin <- Distance.DOP[DOPpos]
                  if(round(DOPmin %% 1, 1) == 0){
                    Dalsize[i] <- blone[DOPpos] + 4 * DOPmin
                  }else if(round(DOPmin %% 1, 1) == 0.1){
                    Dalsize[i] <- blone[DOPpos] + 4 * (DOPmin %/% 1 + 0.25)
                  }else if(round(DOPmin %% 1, 1) == 0.2){
                    Dalsize[i] <- blone[DOPpos] + 4 * (DOPmin %/% 1 + 0.5)
                  }else if(round(DOPmin %% 1, 1) == 0.3){
                    Dalsize[i] <- blone[DOPpos] + 4 * (DOPmin %/% 1 + 0.75)
                  }
                }
              }
            }
            return(Dalsize)
          }

          GF.onecomb <- function(Hgtone, alone, cplone, Qfreq, MAF){
            HNC <- length(Hgtone) / 2
            freqs <- rep(0, HNC)
            for(i in 1:HNC){
              Hgtone1C <- Hgtone[c(2 * i - 1, 2 * i)]
              NumQ <- length(which(Hgtone1C == 99))
              freq <- Qfreq^NumQ
              if(NumQ != length(Hgtone)){
                Hgtone1C_2 <- Hgtone1C[which(Hgtone1C != 99)]
                Onladder.judge <- is.element(Hgtone1C_2, alone)
                if(length(which(Onladder.judge == TRUE)) != 0){
                  freq <- freq * prod(cplone[sapply(Hgtone1C_2[which(Onladder.judge == TRUE)], Sapplywhich, Leftvec = alone)])
                }
                freq <- freq * MAF^length(which(Onladder.judge == FALSE))
              }
              if(Hgtone1C[1] != Hgtone1C[2]){
                freq <- freq * 2
              }
              freqs[i] <- freq
            }
            return(freqs)
          }

          Lists.make <- function(OPHdata, Kgtmat, aList, cpList, MAF, HNC, St.filter, StPH.T, Hb.T, LOD){
            OPlist <- OHlist <- OHlist.correct <- OBlist <- Hgtlist <- Freqlist <- list()

            Peak <- OPHdata[, grep("Allele", names(OPHdata))]
            Nrow.Peak <- nrow(Peak)
            Ncol.Peak <- ncol(Peak)
            Peak <- gsub(" ", "", as.matrix(Peak), fixed = TRUE)
            if(is.vector(Peak)){
              Peak <- matrix(Peak, nrow = Nrow.Peak, ncol = Ncol.Peak)
            }
            Peak[which(Peak == "", arr.ind = TRUE)] <- NA

            Size <- OPHdata[, grep("Size", names(OPHdata))]
            Nrow.Size <- nrow(Size)
            Ncol.Size <- ncol(Size)
            Size <- gsub(" ", "", as.matrix(Size), fixed = TRUE)
            if(is.vector(Size)){
              Size <- matrix(Size, nrow = Nrow.Size, ncol = Ncol.Size)
            }
            Size[which(Size == "", arr.ind = TRUE)] <- NA

            Height <- OPHdata[, grep("Height", names(OPHdata))]
            Nrow.Height <- nrow(Height)
            Ncol.Height <- ncol(Height)
            Height <- gsub(" ", "", as.matrix(Height), fixed = TRUE)
            if(is.vector(Height)){
              Height <- matrix(Height, nrow = Nrow.Height, ncol = Ncol.Height)
            }
            Height[which(Height == "", arr.ind = TRUE)] <- NA

            NL <- nrow(Height)
            TBperL <- Qfreqvec <- rep(0, NL)

            for(i in 1:NL){
              OPHone <- OPHdata.refine(i, Peak, Size, Height, Kgtmat, LOD)
              OPlist[[i]] <- OP <- OPHone[1, ]
              OBlist[[i]] <- OB <- OPHone[2, ]
              OHlist[[i]] <- OH <- OPHone[3, ]
              OHlist.correct[[i]] <- OH[which(OH != 0)] / Param.A[i, 2]
              TBperL[i] <- sum(OH * OB)
              Gt.and.freq <- Genotype.comb(OP, OH, HNC, St.filter, StPH.T, Hb.T, LOD, aList[[i]], cpList[[i]], MAF)
              Hgtlist[[i]] <- Gt.and.freq[[1]]
              Freqlist[[i]] <- Gt.and.freq[[2]]
              Qfreqvec[i] <- Gt.and.freq[[3]]
            }
            OHs <- unlist(OHlist)
            return(list(OPlist, OBlist, OHlist, OHlist.correct, Hgtlist, sum(TBperL) / sum(OHs), sum(OHs), Freqlist, Qfreqvec))
          }

          Genotype.comb <- function(OP, OH, HNC, St.filter, StPH.T, Hb.T, LOD, alone, cplone, MAF){
            Qpos <- !is.element(alone, OP)
            alone.Q <- alone[Qpos]
            if(length(alone.Q) > 0){
              OP <- c(OP, 99)
              OH <- c(OH, 0)
              Qfreq <- sum(cplone[Qpos])
            }else{
              Qfreq <- 0
            }
            NOP <- length(OP)
            if(NOP == 1){
              Gcomb <- matrix(1, 1, 2)
            }else{
              Gcomb <- rbind(cbind(1:NOP, 1:NOP), combinations(n = NOP, r = 2))
            }
            Ncomb <- nrow(Gcomb)
            CGcomb <- permutations(n = Ncomb, r = HNC, repeats.allowed = TRUE)
            Possible.combID <- numeric(0)
            No.DOpos <- which(OH != 0)
            OP.NoDO <- OP[No.DOpos]
            OH.NoDO <- OH[No.DOpos]
            for(i in 1:nrow(CGcomb)){
              GenoID <- as.vector(t(Gcomb[CGcomb[i, ], ]))
              H.Allele <- unique(OP[GenoID])
              H.Peak <- round(unique(c(H.Allele, H.Allele - 1)), 1)
              if(all(is.element(OP[No.DOpos], H.Peak))){
                Pos <- which(is.element(OP[No.DOpos], H.Allele) == FALSE)
                Judge <- rep(0, length(Pos))
                for(j in 1:length(Pos)){
                  Judge[j] <- OH.NoDO[Pos[j]] / OH[which(OP == round(OP.NoDO[Pos[j]] + 1, 1))]
                }
                if((length(which(Judge > St.filter)) == 0) && (length(which(OH.NoDO[Pos] > StPH.T)) == 0)){
                  OH2 <- OH
                  OH2[which(OH2 == 0)] <- LOD
                  Judge <- 1
                  for(j in 1:HNC){
                    if(!any(is.element(GenoID[c(2 * j - 1, 2 * j)], GenoID[-c(2 * j - 1, 2 * j)]))){
                      if((OH2[GenoID[2 * j]] / OH2[GenoID[2 * j - 1]] < Hb.T) || (OH2[GenoID[2 * j]] / OH2[GenoID[2 * j - 1]] > 1 / Hb.T)){
                        Judge <- 0
                        break
                      }
                    }
                  }
                  if(Judge == 1){
                    Possible.combID <- c(Possible.combID, i)
                  }
                }
              }
            }
            NPC <- length(Possible.combID)

            if(NPC == 0){
              return(list(numeric(0), numeric(0), Qfreq))
            }else{
              PCgt.mat <- matrix(0, NPC, 2 * HNC)
              GF.mat <- matrix(0, NPC, HNC)
              for(i in 1:NPC){
                PCgt.mat[i, ] <- Gtcomb.one <- OP[t(Gcomb[CGcomb[Possible.combID[i], ], ])]
                GF.mat[i, ] <- GF.onecomb(Gtcomb.one, alone, cplone, Qfreq, MAF)
              }
              return(list(PCgt.mat, GF.mat, Qfreq))
            }
          }

          SR.LorH.make <- function(NRV, NMRlab, Ndeg, Nal, Pi){
            LorHvec <- rep(0, NRV * NMRlab * Ndeg * Nal)
            for(i in 1:NRV){
              for(j in 1:Ndeg){
                for(k in 1:Nal){
                  LorHvec[NMRlab * Nal * Ndeg * (i - 1) + NMRlab * Nal * (j - 1) + (NMRlab * (k - 1) + 1):(NMRlab * k)] <- sample(c(0, 1), NMRlab, prob = c(1 - Pi[k], Pi[k]), replace = TRUE)
                }
              }
            }
            return(LorHvec)
          }

          SRmix.RV <- function(NRV, NMRlab, Ndeg, Nal, SRmean.low, SRmean.high, SRvar, ATMRdHb, Pi){
            SR.Lrep <- rep(as.vector(sapply(log(SRmean.low), rep, times = NMRlab)), Ndeg)
            SR.Hrep <- rep(as.vector(sapply(log(SRmean.high), rep, times = NMRlab)), Ndeg)
            LorH <- SR.LorH.make(NRV, NMRlab, Ndeg, Nal, Pi)
            PosLow <- which(LorH == 0)
            PosHigh <- which(LorH == 1)
            SR <- rep(0, NRV * NMRlab * Ndeg * Nal)
            SR[PosLow] <- rlnorm(length(PosLow), meanlog = rep(SR.Lrep, NRV)[PosLow], sdlog = sqrt(SRvar / ATMRdHb[PosLow]))
            SR[PosHigh] <- rlnorm(length(PosHigh), meanlog = rep(SR.Hrep, NRV)[PosHigh], sdlog = sqrt(SRvar / ATMRdHb[PosHigh]))
            return(SR)
          }

          MRsetting <- function(HNC){
            if(HNC == 1){
              return(matrix(1, 1, 1))
            }else{
              MRlab <- round(c(seq(0, 0.1, by = as.numeric(tclvalue(MRinterval1))), seq(0.1, 0.5, by = as.numeric(tclvalue(MRinterval2)))), 3)
              MRlab <- MRlab[2:(length(MRlab) - 1)]
              MR <- combinations(n = length(MRlab), r = HNC - 1, repeats.allowed = TRUE)
              MR <- matrix(MRlab[MR], ncol = HNC - 1)
              MR <- MR[which(apply(MR, 1, sum) < 1), , drop = FALSE]
              MR <- cbind(MR, round(1 - apply(MR, 1, sum), 2))
              MRplus <- rep(round(1 / HNC, 2), HNC - 1)
              MR <- rbind(MR, c(MRplus, 1 - sum(MRplus)))
              MR <- t(apply(MR, 1, sort))
              Count <- 1
              repeat{
                Cutpos <- setdiff(which(apply(MR, 1, setequal, MR[Count, ]) == TRUE), Count)
                if(length(Cutpos) != 0){
                  MR <- MR[-Cutpos, ]
                }
                Count <- Count + 1
                if(nrow(MR) < Count){
                  break
                }
              }
              if(HNC >= 3){
                for(i in 2:(HNC - 1)){
                  MR <- MR[is.element(MR[, i], c(MRlab, MRplus)), ]
                }
              }
              return(MR)
            }
          }

          Degradation <- function(d, m, mmean){
            exp(d * m) / exp(d * mmean)
          }

          Gamma.param.estimate <- function(EPH){
            GPmat <- matrix(0, 2, ncol(EPH))
            for(i in 1:ncol(EPH)){
              EPHrm0 <- EPH[which(EPH[, i] >= 0.0001), i]
              Gamma.param <- fitdistr(EPHrm0[which(EPHrm0 <= 10000)], "gamma")
              GPmat[1, i] <- Gamma.param[[1]][1]
              GPmat[2, i] <- 1 / Gamma.param[[1]][2]
            }
            return(GPmat)
          }

          Products.calc <- function(OPHdata, Kgtmat, NRV, HNC, St.filter, StPH.T, Hb.T, LOD, aList, cpList, MAF, Cutpos.T){
            Lists <- Lists.make(OPHdata, Kgtmat, aList, cpList, MAF, HNC, St.filter, StPH.T, Hb.T, LOD)
            Hgtlist <- Lists[[5]]
            Freqlist <- Lists[[8]]
            Qfreqvec <- Lists[[9]]
            if(any(is.element(sapply(Hgtlist, length), 0))){
              cat(paste(HNC, "-person contribution is impossible!", sep = ""), "\n")
              return(0)
            }else{
              Ncombs <- sum(sapply(Hgtlist, nrow))

              Peak.type <- c(rep(1, 2 * HNC), rep(2, 2 * HNC))

              OPlist <- Lists[[1]]
              OBlist <- Lists[[2]]
              OHlist <- Lists[[3]]

              MR <- MRsetting(HNC)
              MRlab <- MRlab2 <- sort(unique(as.vector(MR)))
              NMRlab <- length(MRlab)
              MRlab.pos <- 1:NMRlab
              NMR <- nrow(MR)

              Mmean <- Lists[[6]]
              NL <- nrow(OPHdata)
              Tap <- Lists[[7]] / NL * MRlab

              OB_for_d.pos <- which(unlist(OHlist) != 0)
              MLE.degparam <- function(d){
                sum((exp(d * (unlist(OBlist)[OB_for_d.pos])) / exp(d * Mmean) * Lists[[7]] / NL - unlist(Lists[[4]]))^2)
              }
              MLEdeg <- optimize(MLE.degparam, lower = -0.05, upper = 0)[[1]]
              Deg.param <- seq(0, -0.05, length = 21)
              Deg.extract.pos <- which(abs(MLEdeg - Deg.param) <= 0.0025)
              if(length(Deg.extract.pos) == 0){
                MLEdeg <- round(MLEdeg, 3)
                Deg.param <- Deg.param2 <- Deg.param[c(40, 41)]
              }else{
                Deg.param <- Deg.param2 <- Deg.param[Deg.extract.pos]
              }
              Ndeg <- Ndeg2 <- length(Deg.param)
              DegID <- rep(1:Ndeg, NMR)
              DegID2 <- unique(DegID)
              MRID <- as.vector(matrix(rep(1:NMR, Ndeg), ncol = NMR, byrow = TRUE))

              Products.list <- list()
              LIDvec <- rbind(1:NL, sapply(Hgtlist, nrow))
              LIDvec <- LIDvec[1, order(LIDvec[2, ])]
              Ncut <- NMR * Ndeg
              Cutpos <- 1:Ncut

              for(i in 1:NL){
                LID <- LIDvec[i]
                OP <- OPlist[[LID]]
                OB <- OBlist[[LID]]
                OH <- OHlist[[LID]]

                Qpos <- !is.element(aList[[LID]], OP)
                alone.Q <- aList[[LID]][Qpos]
                if(length(alone.Q) > 0){
                  QOP <- alone.Q[which(cpList[[LID]][Qpos] == max(cpList[[LID]][Qpos]))[1]]
                  OP2 <- c(OP, QOP)
                  OB2 <- c(OB, Size.make(LID, OP, OB, QOP))
                  OH2 <- c(OH, 0)
                  Nal <- length(OP) + 1
                }else{
                  OP2 <- OP
                  OB2 <- OB
                  OH2 <- OH
                  Nal <- length(OP)
                }

                Hgtmat <- Hgtlist[[LID]]
                Freqvec <- apply(Freqlist[[LID]], 1, prod)
                Ncomb <- nrow(Hgtmat)
                Posbase1 <- NRV * NMRlab * Nal * Ndeg2
                Posbase2 <- NMRlab * Nal * Ndeg2
                Posbase3 <- NMRlab * Nal

                Deg <- sapply(Deg.param2, Degradation, m = OB2, mmean = Mmean)

                TMRd <- as.vector(matrix(Tap[MRlab.pos], ncol = 1) %*% matrix(Deg, nrow = 1))

                MCsimu <- function(){
                  Amp <- rtruncnorm(Posbase1, a = 0, b = Inf, mean = rep(Param.A[LID, 2], Posbase1), sd = sqrt(Param.A[LID, 1] / rep(TMRd, NRV)))
                  ATMRd <- Amp * rep(TMRd / 2, NRV)

                  Hb <- rlnorm(Posbase1, meanlog = rep(Param.Hb[LID, 2], Posbase1), sdlog = sqrt(Param.Hb[LID, 1] / ATMRd))
                  ATMRdHb <- ATMRd * 2 / (1 + Hb)

                  if((LID == 1) || (LID == 9)){
                    SRmean.low <- Param.SR[LID, 2] + Param.SR[LID, 4] * OP2
                    SRmean.high <- Param.SR[LID, 3] + Param.SR[LID, 4] * OP2
                    Exp <- exp(Param.SR[LID, 5] + Param.SR[LID, 6] * OP2)
                    Pi <- Exp / (1 + Exp)
                    SR <- SRmix.RV(NRV, NMRlab, Ndeg2, Nal, SRmean.low, SRmean.high, Param.SR[LID, 1], ATMRdHb, Pi)
                  }else{
                    if(LID == 2){
                      SRmean <- rep(0, Nal)
                      SRmean[round(OP2 %% 1, 1) == 0] <- Param.SR[LID, 2] + Param.SR[LID, 4] * OP2[round(OP2 %% 1, 1) == 0]
                      SRmean[round(OP2 %% 1, 1) != 0] <- Param.SR[LID, 3] + Param.SR[LID, 4] * OP2[round(OP2 %% 1, 1) != 0]
                    }else if(LID == 6){
                      OPcorrect <- OP2
                      OPcorrect[round(OP2 %% 1, 1) == 0.3] <- OP2[round(OP2 %% 1, 1) == 0.3] - 3.3
                      SRmean <- Param.SR[LID, 2] + Param.SR[LID, 3] * OPcorrect
                    }else{
                      SRmean <- Param.SR[LID, 2] + Param.SR[LID, 3] * OP2
                    }
                    SRrep <- rep(as.vector(sapply(log(SRmean), rep, times = NMRlab)), Ndeg2)
                    SR <- rlnorm(Posbase1, meanlog = rep(SRrep, NRV), sdlog = sqrt(Param.SR[LID, 1] / ATMRdHb))
                  }
                  EPH.st <- ATMRdHb * SR / (1 + SR)
                  EPH.al <- ATMRdHb * 1 / (1 + SR)

                  EPH <- matrix(0, NRV, Posbase2 * 2)
                  EPH[, 1:(Posbase2)] <- matrix(EPH.st, nrow = NRV, byrow = TRUE)
                  EPH[, (Posbase2 + 1):ncol(EPH)] <- matrix(EPH.al, nrow = NRV, byrow = TRUE)
                  EPH[which(is.na(EPH) == TRUE, arr.ind = TRUE)] <- 0
                  return(EPH)
                }

                repeat{
                  EPH <- MCsimu()
                  Gamma.param.mat <- try(Gamma.param.estimate(EPH), silent = FALSE)
                  if(class(Gamma.param.mat) != "try-error"){
                    break
                  }
                }

                OEcomparison <- function(comb.ID){
                  Products.onecomb <- rep(0, NMR * Ndeg)
                  if(length(alone.Q) > 0){
                    Allele <- sapply(Hgtmat[comb.ID, ], Sapplywhich, Leftvec = c(OP, 99))
                    EPname2 <- round(c(Hgtmat[comb.ID, ] - 1, Hgtmat[comb.ID, ]), 1)
                    EPHpos <- is.element(EPname2, c(OP, 99))
                    EPname1 <- rep(Allele, 2)[EPHpos]
                    EPname2 <- sapply(EPname2[EPHpos], Sapplywhich, Leftvec = c(OP, 99))
                  }else{
                    Allele <- sapply(Hgtmat[comb.ID, ], Sapplywhich, Leftvec = OP)
                    EPname2 <- round(c(Hgtmat[comb.ID, ] - 1, Hgtmat[comb.ID, ]), 1)
                    EPHpos <- is.element(EPname2, OP)
                    EPname1 <- rep(Allele, 2)[EPHpos]
                    EPname2 <- sapply(EPname2[EPHpos], Sapplywhich, Leftvec = OP)
                  }
                  Peak.type.new <- Peak.type[EPHpos]
                  NEP1 <- length(EPname1)
                  AdoptEPHpos <- is.element(1:Nal, EPname2)
                  OH3 <- OH2[AdoptEPHpos]

                  for(k in 1:Ncut){
                    MRpos <- MRID[Cutpos[k]]
                    MRone <- rep(as.vector(sapply(MR[MRpos, ], rep, times = 2)), 2)[EPHpos]
                    MRpos2 <- sapply(MRone, Sapplywhich, Leftvec = MRlab2)
                    Degpos <- which(Deg.param2 == Deg.param[DegID[Cutpos[k]]])
                    Gamma.sum.param.pre <- matrix(0, 2, Nal)
                    for(l in 1:NEP1){
                      Gampone <- Gamma.param.mat[, Posbase2 * (Peak.type.new[l] - 1) + Posbase3 * (Degpos - 1) + NMRlab * (EPname1[l] - 1)  + MRpos2[l]]
                      Gamma.sum.param.pre[1, EPname2[l]] <- Gamma.sum.param.pre[1, EPname2[l]] + prod(Gampone)
                      Gamma.sum.param.pre[2, EPname2[l]] <- Gamma.sum.param.pre[2, EPname2[l]] + Gampone[1] * (Gampone[2]^2)
                    }
                    Gamma.sum.param.pre <- Gamma.sum.param.pre[, AdoptEPHpos, drop = FALSE]
                    NEP <- ncol(Gamma.sum.param.pre)
                    Gamma.sum.param <- matrix(0, 2, NEP)
                    Gamma.sum.param[1, ] <- (Gamma.sum.param.pre[1, ])^2 / Gamma.sum.param.pre[2, ]
                    Gamma.sum.param[2, ] <- Gamma.sum.param.pre[2, ] / Gamma.sum.param.pre[1, ]
                    Prob <- rep(0, NEP)
                    for(l in 1:NEP){
                      Shape <- Gamma.sum.param[1, l]
                      Scale <- Gamma.sum.param[2, l]
                      if(OH3[l] == 0){
                        Prob[l] <- mean(dgamma(1:(LOD - 1), shape = Shape, scale = Scale))
                      }else{
                        Prob[l] <- dgamma(OH3[l], shape = Shape, scale = Scale)
                      }
                    }
                    Products.onecomb[Ndeg * (MRpos - 1) + DegID[Cutpos[k]]] <- prod(Prob)
                  }
                  return(Products.onecomb)
                }

                Products.list[[LID]] <- Products <- t(parSapply(Cluster, 1:Ncomb, OEcomparison))

                likelihoods <- apply(Products, 2, Apply.prod, Vec2 = Freqvec)
                if(!is.vector(likelihoods)){
                  Sumlikelihood <- apply(likelihoods, 2, sum)
                }else{
                  Sumlikelihood <- likelihoods
                }

                Cutpos <- which(Sumlikelihood / max(Sumlikelihood[!is.nan(Sumlikelihood)]) >= Cutpos.T)
                Ncut <- length(Cutpos)

                MRlab2 <- sort(unique(as.vector(MR[MRID[Cutpos], ])))
                MRlab.pos <- which(is.element(MRlab, MRlab2) == TRUE)
                NMRlab <- length(MRlab2)

                Deg.param2 <- sort(unique(Deg.param[DegID[Cutpos]]))
                Ndeg2 <- length(Deg.param2)

                info <- sprintf(paste("%d%% done", " (Hypothesis of ", HNC, "-person contribution)", sep = ""), round(i * 100 / NL))
                setTkProgressBar(pb, i * 100 / NL, sprintf("Calculation"), info)
              }
              Products.list.new <- list()
              if(Ncut != 0){
                for(j in 1:NL){
                  Products.list.new[[j]] <- Products.list[[j]][, Cutpos, drop = FALSE]
                }
              }
              return(list(OPlist, Hgtlist, Products.list.new, Freqlist, cbind(MR[MRID[Cutpos], , drop = FALSE], Deg.param[DegID[Cutpos]]), Qfreqvec))
            }
          }

          PGresults <- list()
          Count <- 1
          Cluster <- makeCluster(4, type = "SOCK")
          for(i in NCfrom:NCto){
            t <- proc.time()
            cat(paste("Calculation of weight values in ", i, "-person contribution was started!", sep = ""), "\n")
            pb <- tkProgressBar("Calculation", paste("0% done (Hypothesis of ", i, "-person contribution)", sep = ""), 0, 100, 0)
            PGone <- Products.calc(CSPFile, Kgtmat, as.numeric(tclvalue(MCvar)), i, as.numeric(tclvalue(SFvar)), as.numeric(tclvalue(Max.sph.var)), as.numeric(tclvalue(Hbf.var)), as.numeric(tclvalue(ATvar)), aList, cpList, as.numeric(tclvalue(MAF.var)), 1 / 10000)
            if(is.list(PGone) == TRUE){
              PGresults[[Count]] <- PGone
              Count <- Count + 1
            }
            close(pb)
            Time.oneNC <- proc.time() - t
            cat(paste("Calculation of weight values in ", i, "-person contribution was finished!", sep = ""), "\n")
            cat(paste("Calculation time of ", i, "-person contribution : ", round(Time.oneNC[3], 3), " sec.", sep = ""), "\n")
          }
          stopCluster(Cluster)
          cat("Calculation of all weight values was finished!", "\n")
          tclvalue(Weight.finish) <- "1"
          FRAME45(PGresults, Kgtmat, aList, cpList)
          tclvalue(LR.finish) <- "0"
          TAB6()
        }
      }
      FRAME45(NULL, Kgtmat, NULL, NULL)
    }else{
      tkgrid(frame4, frame5)
    }
  }

  TAB3 <- function(){
    frame3 <- tkframe(tab3)

    ATlab <- tklabel(frame3, text = "        Analytical threshold        ")
    ATentry <- tkentry(frame3, textvariable = ATvar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    MClab <- tklabel(frame3, text = "        Number of Monte Carlo simulation        ")
    MCentry <- tkentry(frame3, textvariable = MCvar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    MAF.lab <- tklabel(frame3, text = "        Minimum allele frequency        ")
    MAF.entry <- tkentry(frame3, textvariable = MAF.var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    Theta.lab <- tklabel(frame3, text = "        Theta value        ")
    Theta.entry <- tkentry(frame3, textvariable = Theta.var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    Space.lab1 <- tklabel(frame3, text = "")
    Space.lab2 <- tklabel(frame3, text = "")
    Exclude.gc.lab <- tklabel(frame3, text = "        Thresholds to exclude unrealistic genotype combinations        ")
    Stutter.filter.lab <- tklabel(frame3, text = "            - Stutter filter        ")
    Stutter.filter.entry <- tkentry(frame3, textvariable = SFvar, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    Max.sph.lab <- tklabel(frame3, text = "            - Maximum stutter peak height        ")
    Max.sph.entry <- tkentry(frame3, textvariable = Max.sph.var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")
    Hb.filter.lab <- tklabel(frame3, text = "            - Heterozygote balance        ")
    Hb.filter.entry <- tkentry(frame3, textvariable = Hbf.var, width = 10, highlightthickness = 1, relief = "solid", justify = "center", background = "white")

    tkgrid(ATlab, ATentry, pady = 10, sticky = "w")
    tkgrid(MClab, MCentry, pady = 10, sticky = "w")
    tkgrid(MAF.lab, MAF.entry, pady = 10, sticky = "w")
    tkgrid(Theta.lab, Theta.entry, pady = 10, sticky = "w")
    tkgrid(Space.lab1, Space.lab2, pady = 10, sticky = "w")
    tkgrid(Exclude.gc.lab, pady = 10, sticky = "w")
    tkgrid(Stutter.filter.lab, Stutter.filter.entry, pady = 10, sticky = "w")
    tkgrid(Max.sph.lab, Max.sph.entry, pady = 10, sticky = "w")
    tkgrid(Hb.filter.lab, Hb.filter.entry, pady = 10, sticky = "w")
    tkgrid(frame3)
  }

  OpenFile <- function(fp, var, top, filestate, Cursor){
    FileName <- tclvalue(tkgetOpenFile(parent = top, initialdir = tclvalue(fp), 
                         multiple = "true", filetypes = "{{CSV Files} {.csv}}"))
    if(!nchar(FileName)){
      tkmessageBox(message = "No file was selected!", icon = "error", type = "ok")
    }else{
      tmp <- sub("\\}", FileName, replacement = "")
      tmp2 <- sub("\\{", tmp, replacement = "")
      tclvalue(fp) <- tmp2
      foo3 <- strsplit(tmp2, "/")[[1]]
      tclvalue(var) <- strsplit(foo3[length(foo3)], "\\.csv")[[1]][1]
      tclvalue(filestate) <- "normal"
      tclvalue(Cursor) <- "hand2"
    }
    TAB1()
    tclvalue(Weight.finish) <- "0"
    tclvalue(LR.finish) <- "0"
    tclvalue(LRcalcbutt.state) <- "disabled"
    tclvalue(LRcalcbutt.cursor) <- "arrow"
    TAB4()
    TAB6()
  }

  EPG.view <- function(){
    if(paste(tclvalue(CSP.var)) == ""){
      tkmessageBox(message = "Load Crime Stain Profile!", icon = "error", type = "ok")
    }else{
      CSPFile <- read.csv(tclvalue(CSP.filepath), header = TRUE)
      Marker <- as.vector(CSPFile[, match("Marker", names(CSPFile))])
      Marker <- gsub(" ", "", Marker, fixed = TRUE)
      if(any("AMEL" %in% Marker)){
        CSPFile <- CSPFile[-which(Marker == "AMEL"),]
        Marker <- Marker[-which(Marker == "AMEL")]
      }
      Locus.info <- c(4, 4, 4, 4, 3, 3, 3, 3, 3, 1, 1, 1, 1, 2, 2)
      if(!setequal(Marker, IDloci)){
        tkmessageBox(message = "Imput locus set is not the Identifiler Plus locus set!", icon = "error", type = "ok")
      }else{
        Save.as <- tkgetSaveFile(filetypes = "{{PDF Files} {.pdf}}")
        if(length(as.character(Save.as)) > 0){
          Peak <- CSPFile[, grep("Allele", names(CSPFile))]
          Nrow.Peak <- nrow(Peak)
          Ncol.Peak <- ncol(Peak)
          Peak <- gsub(" ", "", as.matrix(Peak), fixed = TRUE)
          if(is.vector(Peak)){
            Peak <- matrix(Peak, nrow = Nrow.Peak, ncol = Ncol.Peak)
          }
          Peak[which(Peak == "", arr.ind = TRUE)] <- NA

          Size <- CSPFile[, grep("Size", names(CSPFile))]
          Nrow.Size <- nrow(Size)
          Ncol.Size <- ncol(Size)
          Size <- gsub(" ", "", as.matrix(Size), fixed = TRUE)
          if(is.vector(Size)){
            Size <- matrix(Size, nrow = Nrow.Size, ncol = Ncol.Size)
          }
          Size[which(Size == "", arr.ind = TRUE)] <- NA
          Sizevec <- as.numeric(Size)
          MinSize <- min(Sizevec[!is.na(Sizevec)])
          MaxSize <- max(Sizevec[!is.na(Sizevec)])

          Height <- CSPFile[, grep("Height", names(CSPFile))]
          Nrow.Height <- nrow(Height)
          Ncol.Height <- ncol(Height)
          Height <- gsub(" ", "", as.matrix(Height), fixed = TRUE)
          if(is.vector(Height)){
            Height <- matrix(Height, nrow = Nrow.Height, ncol = Ncol.Height)
          }
          Height[which(Height == "", arr.ind = TRUE)] <- NA
          Heightvec <- as.numeric(Height)
          MaxHeight <- max(Heightvec[!is.na(Heightvec)])

          pdf(file = paste(tclvalue(Save.as), ".pdf", sep = ""))

          par(mfrow = c(4, 1))
          Peak1 <- as.numeric(Peak[1, !is.na(Peak[1, ])])
          Size1 <- as.numeric(Size[1, !is.na(Size[1, ])])
          Height1 <- as.numeric(Height[1, !is.na(Height[1, ])])

          par(plt = c(0.1, 0.95, 0.25, 0.8))
          plot(0, -1, xlab = "", ylab = "RFU", xaxt = "n", cex.axis = 0.5, xlim = c(MinSize - 20, MaxSize + 20), ylim = c(0, MaxHeight + MaxHeight / 20))
          axis(3, xlim = c(90, 350), cex.axis = 0.5)
          segments(0, 0, 400, 0, col = "gray60", lwd = 0.75)
          text(x = mean(JP.ID.bList[[1]]), y = MaxHeight, Marker[1], cex = 0.75)
          par(xpd = TRUE)
          Deno <- 8
          if(length(Peak1) != 0){
            for(i in 1:length(Peak1)){
              segments(Size1[i] - 0.5, 0, Size1[i], Height1[i], col = Locus.info[1])
              segments(Size1[i] + 0.5, 0, Size1[i], Height1[i], col = Locus.info[1])
              if(i >= 2){
                if(Peak1[i] - Peak1[i - 1] < 2){
                  if(Deno == 8){
                    Deno <- 3
                  }else{
                    Deno <- 8
                  }
                }else{
                  if(Deno == 3){
                    Deno <- 8
                  }
                }
              }
              segments(Size1[i], -MaxHeight / 24, Size1[i], -MaxHeight / Deno + MaxHeight / 24, lwd = 0.5)
              polygon(c(Size1[i] - 3, Size1[i] + 3, Size1[i] + 3, Size1[i] - 3), 
                      c(-MaxHeight / Deno + MaxHeight / 24, -MaxHeight / Deno + MaxHeight / 24, -MaxHeight / Deno - MaxHeight / 8, -MaxHeight / Deno - MaxHeight / 8), 
                      lwd = 0.5, col = "white")
              text(x = Size1[i], y = -MaxHeight / Deno, labels = Peak1[i], cex = 0.5)
              text(x = Size1[i], y = -MaxHeight / Deno - MaxHeight / 12, labels = Height1[i], cex = 0.5)
            }
          }
          par(xpd = FALSE)
          for(i in 2:15){
            Peak1 <- as.numeric(Peak[i, !is.na(Peak[i, ])])
            Size1 <- as.numeric(Size[i, !is.na(Size[i, ])])
            Height1 <- as.numeric(Height[i, !is.na(Height[i, ])])
            if(Locus.info[i - 1] != Locus.info[i]){
              par(plt=c(0.1, 0.95, 0.25, 0.8))
              plot(0, -1, xlab = "", ylab = "RFU", xaxt = "n", cex.axis = 0.5, xlim = c(MinSize - 20, MaxSize + 20), ylim = c(0, MaxHeight + MaxHeight / 20))
              axis(3, xlim = c(90, 350), cex.axis = 0.5)
              segments(0, 0, 400, 0, col = "gray60", lwd = 0.75)
            }
            text(x = mean(JP.ID.bList[[i]]), y = MaxHeight, Marker[i], cex = 0.75)
            par(xpd = TRUE)
            Deno <- 8
            if(length(Peak1) != 0){
              for(j in 1:length(Peak1)){
                segments(Size1[j] - 0.5, 0, Size1[j], Height1[j], col = Locus.info[i])
                segments(Size1[j] + 0.5, 0, Size1[j], Height1[j], col = Locus.info[i])
                if(j >= 2){
                  if(Peak1[j] - Peak1[j - 1] < 2){
                    if(Deno == 8){
                      Deno <- 3
                    }else{
                      Deno <- 8
                    }
                  }else{
                    if(Deno == 3){
                      Deno <- 8
                    }
                  }
                }
                segments(Size1[j], -MaxHeight / 24, Size1[j], -MaxHeight / Deno + MaxHeight / 24, lwd = 0.5)
                polygon(c(Size1[j] - 3, Size1[j] + 3, Size1[j] + 3, Size1[j] - 3), 
                        c(-MaxHeight / Deno + MaxHeight / 24, -MaxHeight / Deno + MaxHeight / 24, -MaxHeight / Deno - MaxHeight / 8, -MaxHeight / Deno - MaxHeight / 8), 
                        lwd = 0.5, col = "white")
                text(x = Size1[j], y = -MaxHeight / Deno, labels = Peak1[j], cex = 0.5)
                text(x = Size1[j], y = -MaxHeight / Deno - MaxHeight / 12, labels = Height1[j], cex = 0.5)
              }
            }
            par(xpd = FALSE)
          }
          dev.off()
        }
      }
    }
  }

  Profile.view <- function(){
    if(paste(tclvalue(Ref.var)) == ""){
      tkmessageBox(message = "Load Reference Profiles!", icon = "error", type = "ok")
    }else{
      RefFile <- read.csv(tclvalue(Ref.filepath), header = FALSE)
      RefFile <- as.matrix(RefFile)
      Ref.tclArray <- tclArray()
      for(i in 1:(nrow(RefFile))){
        for(j in 1:(ncol(RefFile))){
          Ref.tclArray[[i - 1, j - 1]] <- RefFile[i, j]
        }
      }
      Ref.tf <- tktoplevel()
      tkwm.title(Ref.tf, "Reference Profile")
      Ref.table <- tkwidget(Ref.tf, "table", variable = Ref.tclArray, rows = nrow(RefFile), cols = ncol(RefFile), titlerows = "1", titlecols = "1", selectmode = "extended", colwidth = "15", background = "white")
      tkpack(Ref.table)
    }
  }

  Freq.view <- function(){
    if(paste(tclvalue(AF.var)) == ""){
      tkmessageBox(message = "Load Allele Frequencies!", icon = "error", type = "ok")
    }else{
      freqFile <- read.csv(tclvalue(AF.filepath), header = FALSE, as.is = TRUE, na.strings = "")
      freqFile <- as.matrix(freqFile)
      freq.tclArray <- tclArray()
      for(j in 1:ncol(freqFile)){
        freq.tclArray[[0, j - 1]] <- freqFile[1, j]
      }
      for(i in 2:nrow(freqFile)){
        for(j in 1:ncol(freqFile)){
          if(is.na(freqFile[i, j])){
            freq.tclArray[[i - 1, j - 1]] <- "-"
          }else{
            freq.tclArray[[i - 1, j - 1]] <- round(as.numeric(freqFile[i, j]), 5)
          }
        }
      }
      freq.tf <- tktoplevel()
      tkwm.title(freq.tf, "Allele Frequencies")
      frame.freq <- tkframe(freq.tf)
      freq.table <- tkwidget(frame.freq, "table", rows = nrow(freqFile), cols = ncol(freqFile), titlerows = 1, titlecols = 1, height = 30, colwidth = "10",  
                             xscrollcommand = function(...) tkset(xscr, ...), yscrollcommand = function(...) tkset(yscr, ...))
      xscr <- tkscrollbar(frame.freq, orient = "horizontal", command = function(...) tkxview(freq.table, ...))
      yscr <- tkscrollbar(frame.freq, command = function(...) tkyview(freq.table, ...))
      tkgrid(freq.table, yscr)
      tkgrid.configure(yscr, sticky = "nsw")
      tkgrid(xscr, sticky = "new")
      tkconfigure(freq.table, variable = freq.tclArray, background = "white", selectmode = "extended")
      tkgrid(frame.freq, padx = 50, pady = 50)
    }
  }

  TAB1 <- function(){
    tkdestroy(frame1)
    frame1 <<- tkframe(tab1)

    csp.label <- tklabel(frame1, text = "Crime Stain Profile")
    cspfile.label <- tklabel(frame1, textvariable = CSP.var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    csp.butt <- tkbutton(frame1, text = "    Load    ", cursor = "hand2", command = function() OpenFile(CSP.filepath, CSP.var, tf, CSPbutt.state, CSPbutt.cursor))
    csp.epg.butt <- tkbutton(frame1, text = "        View EPG        ", cursor = tclvalue(CSPbutt.cursor), state = tclvalue(CSPbutt.state), command = function() EPG.view())

    ref.label <- tklabel(frame1, text = "Reference Profiles")
    reffile.label <- tklabel(frame1, textvariable = Ref.var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    ref.butt <- tkbutton(frame1, text = "    Load    ", cursor = "hand2", command = function() OpenFile(Ref.filepath, Ref.var, tf, Refbutt.state, Refbutt.cursor))
    ref.gt.butt <- tkbutton(frame1, text = "     View Profiles     ", cursor = tclvalue(Refbutt.cursor), state = tclvalue(Refbutt.state), command = function() Profile.view())

    af.label <- tklabel(frame1, text = "Allele Frequencies")
    affile.label <- tklabel(frame1, textvariable = AF.var, width = 30, highlightthickness = 1, relief = "groove", justify = "center", background = "white")
    af.butt <- tkbutton(frame1, text = "    Load    ", cursor = "hand2", command = function() OpenFile(AF.filepath, AF.var, tf, Afbutt.state, Afbutt.cursor))
    af.show.butt <- tkbutton(frame1, text = "  View Frequencies  ", cursor = tclvalue(Afbutt.cursor), state = tclvalue(Afbutt.state), command = function() Freq.view())

    tkgrid(csp.label, cspfile.label, csp.butt, csp.epg.butt, padx = 20, pady = 20, sticky = "w")
    tkgrid(ref.label, reffile.label, ref.butt, ref.gt.butt, padx = 20, pady = 20, sticky = "w")
    tkgrid(af.label, affile.label, af.butt, af.show.butt, padx = 20, pady = 20, sticky = "w")
    tkgrid(frame1)
  }

  rm(.Random.seed, envir = globalenv())
  tf <- tktoplevel()
  tkwm.title(tf, "Kongoh")

  Tabs <- tk2notebook(tf, tabs = c("Files", "Calculation", "Result", "Setting"))
  tkpack(Tabs, fill = "both", expand = 1)
  tab1 <- tk2notetab(Tabs, "Files")
  tab3 <- tk2notetab(Tabs, "Setting")
  tab4 <- tk2notetab(Tabs, "Calculation")
  tab6 <- tk2notetab(Tabs, "Result")

  frame1 <- tkframe(tab1)
  TAB1()
  TAB3()
  frame4 <- tkframe(tab4)
  frame5 <- tkframe(tab4)
  TAB4()
  frame6 <- tkframe(tab6)
  TAB6()
}

