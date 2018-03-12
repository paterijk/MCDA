LPDMRSortInferenceExact <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, majorityRule = "M", readableWeights = FALSE, readableProfiles = FALSE, minmaxLPD = FALSE, alternativesIDs = NULL, criteriaIDs = NULL, solver="glpk", cplexTimeLimit = NULL, cplexIntegralityTolerance = NULL, cplexThreads = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a character or a string of characters")
  else if (!(majorityRule %in% c("M","V","D","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'M','V','D','v','d','dV','Dv','dv'}")
  
  if (!is.logical(readableWeights))
    stop("readableWeights should be a boolean")
  
  if (!is.logical(readableProfiles))
    stop("readableProfiles should be a boolean")
  
  if (!is.logical(minmaxLPD))
    stop("minmaxLPD should be a boolean")
  
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
  
  # get model file depending on function options

  modelfilename <- paste("MRSort", c("","V","D","DV1","DV2","DV3","DV4","DV5")[match(majorityRule,c("M","V","D","v","d","dV","Dv","dv"))], "InferenceModel", sep = "")
  
  if(readableWeights || readableProfiles)
  {
    modelfilename <- paste(modelfilename, "Spread", sep = "")
    if(readableWeights)
      modelfilename <- paste(modelfilename, "Weights", sep = "")
    if(readableProfiles)
      modelfilename <- paste(modelfilename, "Profiles", sep = "")
  }
  if(minmaxLPD & majorityRule != "")
    modelfilename <- paste(modelfilename, "LPD", sep = "")
  
  modelfilename <- paste(modelfilename, ".gmpl", sep = "")
  
  modelFile <- system.file("extdata", modelfilename, package="MCDA")
  
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
  
  cat("param gamma:=0.001;\n")
  
  cat("end;\n")
  sink()
  
  lp<-initProbGLPK()
  
  tran<-mplAllocWkspGLPK()
  
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  
  termOutGLPK(GLP_OFF)
  
  out<-mplReadModelGLPK(tran, dataFile, skip=0)
  
  if (is.null(out))
    out <- mplGenerateGLPK(tran)
  else 
    stop(return_codeGLPK(out))
  
  if (is.null(out))
    mplBuildProbGLPK(tran,lp)
  else 
    stop(return_codeGLPK(out))
  
  if (solver == "cplex")
  {
    
    if (!requireNamespace("cplexAPI", quietly = TRUE)) stop("cplexAPI package could not be loaded")
    
    cplexOutFile <- tempfile()
    
    writeLPGLPK(lp, cplexOutFile)
    
    # Open a CPLEX environment
    env <- cplexAPI::openEnvCPLEX()
    
    # Create a problem object
    prob <- cplexAPI::initProbCPLEX(env)
    
    if (!is.null(cplexTimeLimit))
      cplexAPI::setDblParmCPLEX(env,cplexAPI::CPX_PARAM_TILIM,cplexTimeLimit)
    
    if (!is.null(cplexIntegralityTolerance))
      cplexAPI::setDblParmCPLEX(env,cplexAPI::CPX_PARAM_EPINT,cplexIntegralityTolerance)
    
    if (!is.null(cplexThreads))
      cplexAPI::setDblParmCPLEX(env,cplexAPI::CPX_PARAM_THREADS,cplexThreads)
    
    # Read MIP problem from cplexOutFile
    out <- cplexAPI::readCopyProbCPLEX(env, prob, cplexOutFile, ftype = "LP")
    
    # solve the problem
    if (out == 0)
      cplexAPI::mipoptCPLEX(env,prob)
    else
      stop(out)
    
    solverStatus <- paste('Failed (',cplexAPI::status_codeCPLEX(env, cplexAPI::getStatCPLEX(env,prob)),')')
    
    error <- TRUE
    
    if (cplexAPI::getStatCPLEX(env,prob) %in% c(1,5,15,17,19,20,101,102,115,121,123,125,129,130)){
      solverStatus <- 'Solution found'
      
      solution <- cplexAPI::solutionCPLEX(env,prob)$x
      
      varnames <- cplexAPI::getColNameCPLEX(env,prob, 0,length(solution)-1)
      
      paro <- "("
      parc <- ")"
      
      error <- FALSE
    }
    
  } else if (solver == "glpk"){
    
    solveMIPGLPK(lp)
    
    solverStatus <- paste("Failed (",return_codeGLPK(mipStatusGLPK(lp)),")")
    
    error <- TRUE
    
    if(mipStatusGLPK(lp)==5){
      solverStatus <- 'Solution found'
      
      mplPostsolveGLPK(tran, lp, sol = GLP_MIP)
      
      solution <- mipColsValGLPK(lp)
      
      varnames <- c()
      
      for (i in 1:length(solution))
        varnames <- c(varnames,getColNameGLPK(lp,i))
      
      paro <- "["
      parc <- "]"
      
      error <- FALSE
    }
  }
  
  if (!error){
    
    lambda <- solution[varnames=="lambda"]
    
    weightsnames <- c()
    
    for (i in 1:numCrit)
    {
      weightsnames <- c(weightsnames,paste("w",paro,i,parc,sep=""))
    }
    
    weights <- c()
    
    for (i in 1:numCrit)
      weights <- c(weights,solution[varnames==weightsnames[i]])
    
    names(weights) <- colnames(performanceTable)
    
    ptknames <- matrix(nrow=numCat,ncol=numCrit)
    
    for (i in 2:(numCat+1)){
      for (j in 1:numCrit)
      {
        ptknames[i-1,j] <- paste("PTk",paro,i,",",j,parc,sep="")
      }
    }
    
    profilesPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
    
    # the last profile (bottom one) doesn't do anything so we keep it NA
    for (i in 1:(numCat-1)){
      for (j in 1:numCrit)
        profilesPerformances[i,j] <- solution[varnames==ptknames[i,j]]
    }
    
    rownames(profilesPerformances) <- names(categoriesRanks)
    colnames(profilesPerformances) <- colnames(performanceTable)
    
    vetoPerformances <- NULL
    
    if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
    {
      ptvnames <- matrix(nrow=numCat,ncol=numCrit)
      
      for (i in 2:(numCat+1)){
        for (j in 1:numCrit)
        {
          ptvnames[i-1,j] <- paste("PTv",paro,i,",",j,parc,sep="")
        }
      }
      
      vetoPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
      
      # bottom profile doesn't do anything, keep it as NA
      for (i in 1:(numCat-1)){
        for (j in 1:numCrit)
          vetoPerformances[i,j] <- solution[varnames==ptvnames[i,j]]
      }
      
      rownames(vetoPerformances) <- names(categoriesRanks)
      colnames(vetoPerformances) <- colnames(performanceTable)
    }
    
    dictatorPerformances <- NULL
    
    if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
    {
      ptdnames <- matrix(nrow=numCat,ncol=numCrit)
      
      for (i in 2:(numCat+1)){
        for (j in 1:numCrit)
        {
          ptdnames[i-1,j] <- paste("PTd",paro,i,",",j,parc,sep="")
        }
      }
      
      dictatorPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
      
      # bottom profile doesn't do anything, keep it as NA
      for (i in 1:(numCat-1)){
        for (j in 1:numCrit)
          dictatorPerformances[i,j] <- solution[varnames==ptdnames[i,j]]
      }
      
      rownames(dictatorPerformances) <- names(categoriesRanks)
      colnames(dictatorPerformances) <- colnames(performanceTable)
    }
    
    if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
    {
      # determine which vetoes are actually used and remove those that are simply an artefact of the linear program
      
      used <- LPDMRSortIdentifyUsedVetoProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, lambda, weights, profilesPerformances, vetoPerformances, dictatorPerformances, majorityRule, alternativesIDs, criteriaIDs)
      
      for (k in (numCat-1):1)
      {
        cat <- names(categoriesRanks)[categoriesRanks == k]
        for (j in 1:numCrit)
        {
          if (!used[cat,j])
            vetoPerformances[cat,j] <- NA
        }
      }
    }
    
    if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
    {
      # determine which dictators are actually used and remove those that are simply an artefact of the linear program
      
      used <- LPDMRSortIdentifyUsedDictatorProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, lambda, weights, profilesPerformances, dictatorPerformances, vetoPerformances, majorityRule, alternativesIDs, criteriaIDs)
      
      for (k in (numCat-1):1)
      {
        cat <- names(categoriesRanks)[categoriesRanks == k]
        for (j in 1:numCrit)
        {
          if (!used[cat,j])
            dictatorPerformances[cat,j] <- NA
        }
      }
    }
    
    return(list(lambda = lambda, weights = weights, profilesPerformances = profilesPerformances, vetoPerformances = vetoPerformances, dictatorPerformances = dictatorPerformances, solverStatus = solverStatus))
    
  }
  else
  {
    return(list(solverStatus = solverStatus))
  }
}
