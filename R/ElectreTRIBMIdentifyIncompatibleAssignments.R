#############################################################################
#
# Copyright Patrick Meyer and Sébastien Bigaret, 2013
#
# Contributors:
#   Patrick Meyer <patrick.meyer@telecom-bretagne.eu>
#   Sébastien Bigaret <sebastien.bigaret@telecom-bretagne.eu>
#    
# This software, MCDA, is a package for the R statistical software which 
# allows to use MCDA algorithms and methods. 
# 
# This software is governed by the CeCILL license (v2) under French law
# and abiding by the rules of distribution of free software. You can
# use, modify and/ or redistribute the software under the terms of the
# CeCILL license as circulated by CEA, CNRS and INRIA at the following
# URL "http://www.cecill.info".
# 
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#		
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#		
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
##############################################################################

MRSortIdentifyIncompatibleAssignments <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    assignments <- assignments[alternativesIDs]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  tempPath <- tempdir()
  
  modelFile <- system.file("extdata","ElectreTRIBMIdentifyIncompatibleAssignmentsModel.gmpl", package="MCDA")
  
  dataFile <- tempfile()
  
  file.copy(modelFile, dataFile)
  
  sink(dataFile, append=TRUE)
  
  cat("data;\n")
  
  cat("param X := ")
  cat(numAlt)
  cat(";\n\n")
  
  cat("param F := ")
  cat(numCrit)
  cat(";\n\n")
  
  cat("param Fdir := \n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    if (criteriaMinMax[i]=="min")
      cat("-1")
    else
      cat("1")
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n")
  }
  
  cat("param Fmin :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, min)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param Fmax :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, max)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param K := ")
  cat(numCat)
  cat(";\n\n")
  
  cat("param A:=\n")
  for (i in 1:numAlt){
    cat(i)
    cat("\t")
    cat(categoriesRanks[assignments[i]])
    if (i!=numAlt)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param PTx : ")
  cat(1:numCrit)
  cat(" := \n")
  for (i in 1:numAlt){
    cat(i)
    cat("\t")
    cat(performanceTable[i,])
    if (i!=numAlt)
      cat("\n")
    else
      cat(";\n\n")
  }
  
  cat("param M :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, max)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n")
    
  }
  
  cat("param gamma:=0.01;\n")
  
  cat("param B:=1000;\n")
  
  cat("end;")
  sink()
  
  lp<-initProbGLPK()
  
  tran<-mplAllocWkspGLPK()
  
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  
  out<-mplReadModelGLPK(tran, dataFile, skip=0)
  
  if (is.null(out))
    out <- mplGenerateGLPK(tran)
  else 
    stop(return_codeGLPK(out))
  
  if (is.null(out))
    mplBuildProbGLPK(tran,lp)
  else 
    stop(return_codeGLPK(out))
  
  solveMIPGLPK(lp)
  
  if(mipStatusGLPK(lp)==5){
    
    mplPostsolveGLPK(tran, lp, sol = GLP_MIP)
    
    solution <- mipColsValGLPK(lp)
    
    varnames <- c()
    
    for (i in 1:length(solution))
      varnames <- c(varnames,getColNameGLPK(lp,i))
    
    
    
    #     weightsnames <- c()
    #     
    #     for (i in 1:numCrit)
    #     {
    #       weightsnames <- c(weightsnames,paste("w[",i,"]",sep=""))
    #     }
    #     
    #     weights <- c()
    #     
    #     for (i in 1:numCrit)
    #       weights <- c(weights,solution[varnames==weightsnames[i]])
    #     
    #     names(weights) <- colnames(performanceTable)
    #     
    #     ptknames <- matrix(nrow=numCat,ncol=numCrit)
    #     
    #     for (i in 2:(numCat+1)){
    #       for (j in 1:numCrit)
    #       {
    #         ptknames[i-1,j] <- paste("PTk[",i,",",j,"]",sep="")
    #       }
    #     }
    #     
    #     profilesPerformances <- matrix(nrow=numCat,ncol=numCrit)
    #     
    #     for (i in 1:numCat){
    #       for (j in 1:numCrit)
    #         profilesPerformances[i,j] <- solution[varnames==ptknames[i,j]]
    #     }
    #     
    #     rownames(profilesPerformances) <- names(categoriesRanks)
    #     colnames(profilesPerformances) <- colnames(performanceTable)
    #     
    #     epsilon <- solution[varnames=="ksep"]
    
    onoffnames <- c()
    
    for (i in 1:numAlt)
    {
      onoffnames <- c(onoffnames,paste("OnOff[",i,"]",sep=""))
    }
    
    onOff <- c()
    
    for (i in 1:numAlt)
      if(solution[varnames==onoffnames[i]] == 1)
        onOff <- c(onOff,TRUE)
    else
      onOff <- c(onOff,FALSE)
    
    names(onOff) <- rownames(performanceTable)
    
    numIncompatibleAssignments <- mipObjValGLPK(lp)
    #     
    #     
    #     
    #   
    #   file.copy(dataFile,"/home/pat/temp/")
    #   
    #   lp<-initProbGLPK(ptrtype = "glpk_prob")
    #   
    #   tran<-mplAllocWkspGLPK(ptrtype = "tr_wksp")
    #   
    #   setMIPParmGLPK(PRESOLVE, GLP_ON)
    #   
    #   out<-mplReadModelGLPK(tran, dataFile, skip=0)
    #   
    #   if (is.null(out))
    #     out <- mplGenerateGLPK(tran)
    #   else 
    #     stop(return_codeGLPK(out))
    #   
    #   if (is.null(out))
    #     mplBuildProbGLPK(tran,lp)
    #   else 
    #     stop(return_codeGLPK(out))
    #   
    #   out<-solveMIPGLPK(lp)
    #   
    #   if(mipStatusGLPK(lp)==5){
    #     
    #     solution <- mipColsValGLPK(lp)
    #     epsilon <- mipObjValGLPK(lp)
    #     
    #     lambda <- solution[1]
    #     
    #     weights <- solution[2:(2+numCrit-1)]
    #     
    #     names(weights) <- colnames(performanceTable)
    #     
    #     b<-2+numCrit + numCrit
    #     e<-2+numCrit + numCrit + numCat * numCrit -1
    #     
    #     profilesPerformances <- matrix(solution[b:e],nrow=numCat,ncol=numCrit,byrow=TRUE)
    #     
    #     colnames(profilesPerformances) <- colnames(performanceTable)
    #     
    #     rownames(profilesPerformances) <- names(categoriesRanks)
    #     
    #     b<-e+1
    #     e<-b+numAlt-1
    
    #     onOff <- c(solution[b:e])
    #     
    #     names(onOff) <- rownames(performanceTable)
    
    return(list(numIncompatibleAssignments = numIncompatibleAssignments, incompatibleAssignments = onOff))
  }
  else
    return(NULL)
}