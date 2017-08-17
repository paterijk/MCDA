SRMPInferenceNoInconsistFixedLexicographicOrder <- function(performanceTable, criteriaMinMax, lexicographicOrder, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, solver="glpk", timeLimit = NULL, cplexIntegralityTolerance = NULL, cplexThreads = NULL){
  
  ## check the input data
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(lexicographicOrder)))
    stop("lexicographicOrder should be a vector")
  
  if (!(is.null(timeLimit)))
  {
    if(!is.numeric(timeLimit))
      stop("timeLimit should be numeric")
    if(timeLimit <= 1)
      stop("timeLimit should be strictly positive (and ideally above one second)")
  }

  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if(dim(preferencePairs)[2] != 2)
    stop("preferencePairs should have two columns")
  
  if(!is.null(indifferencePairs))
    if(dim(indifferencePairs)[2] != 2)
      stop("indifferencePairs should have two columns")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    preferencePairs <- preferencePairs[(preferencePairs[,1] %in% alternativesIDs) & (preferencePairs[,2] %in% alternativesIDs),]
    if(dim(preferencePairs)[1] == 0)
      preferencePairs <- NULL
    if(!is.null(indifferencePairs))
    {
      indifferencePairs <- indifferencePairs[(indifferencePairs[,1] %in% alternativesIDs) & (indifferencePairs[,2] %in% alternativesIDs),]
      if(dim(indifferencePairs)[1] == 0)
        indifferencePairs <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (is.null(dim(preferencePairs))) 
    stop("preferencePairs is empty or the provided alternativesIDs have filtered out everything from within")
  
  if (!all(sort(lexicographicOrder) == 1:length(lexicographicOrder)))
    stop("lexicographicOrder should be a permutation of profiles indices")
  
  # -------------------------------------------------------
  
  numAlt <- dim(performanceTable)[1]
  numCrit <- dim(performanceTable)[2]
  tempPath <- tempdir()
  
  # get model file depending on function options

  modelfilename <- "SRMPNoInconsist.gmpl"
  if(length(indifferencePairs) > 0)
    modelfilename <- "SRMPNoInconsistIndif.gmpl"

  modelFile <- system.file("extdata", modelfilename, package="MCDA")
  
  dataFile <- tempfile()
  
  file.copy(modelFile, dataFile)
  
  sink(dataFile, append=TRUE)
  
  cat("data;\n")
  
  cat("param n := ")
  cat(numAlt)
  cat(";\n\n")
  
  cat("param m := ")
  cat(numCrit)
  cat(";\n\n")
  
  cat("param dir := \n")
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
  
  cat("param min :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, min)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param max :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, max)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param p := ")
  cat(length(lexicographicOrder))
  cat(";\n\n")
  
  if(length(indifferencePairs) == 0)
  {
    cat("param q := ")
    cat(dim(preferencePairs)[1])
    cat(";\n\n")
  }
  else
  {
    cat("param qp := ")
    cat(dim(preferencePairs)[1])
    cat(";\n\n")
    
    cat("param qi := ")
    cat(dim(indifferencePairs)[1])
    cat(";\n\n")
  }
  
  cat("param LO :=\n")
  for (i in 1:length(lexicographicOrder)){
    cat(i)
    cat("\t")
    cat(lexicographicOrder[i])
    if (i!=length(lexicographicOrder))
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param A : ")
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
  
  cat("param BC : ")
  cat(1:2)
  cat(" := \n")
  for (i in 1:dim(preferencePairs)[1]){
    cat(i)
    cat("\t")
    cat(which(rownames(performanceTable) == preferencePairs[i,1]))
    cat("\t")
    cat(which(rownames(performanceTable) == preferencePairs[i,2]))
    if (i!=dim(preferencePairs)[1])
      cat("\n")
    else
    {
      if(length(indifferencePairs) == 0)
        cat(";\n\n")
      else
        cat("\n")
    }
  }
  if(!is.null(indifferencePairs))
    for (i in 1:dim(indifferencePairs)[1]){
      cat(dim(preferencePairs)[1] + i)
      cat("\t")
      cat(which(rownames(performanceTable) == indifferencePairs[i,1]))
      cat("\t")
      cat(which(rownames(performanceTable) == indifferencePairs[i,2]))
      if (i!=dim(indifferencePairs)[1])
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
    
    if (!is.null(timeLimit))
      cplexAPI::setDblParmCPLEX(env,cplexAPI::CPX_PARAM_TILIM,timeLimit*1000)
    
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
    
    solverStatus <- cplexAPI::getStatCPLEX(env,prob)
    
    error <- TRUE
    
    if ((cplexAPI::getStatCPLEX(env,prob) == 101) | (cplexAPI::getStatCPLEX(env,prob) == 102)){
      solution <- cplexAPI::solutionCPLEX(env,prob)$x
      
      varnames <- cplexAPI::getColNameCPLEX(env,prob, 0,length(solution)-1)
      
      paro <- "("
      parc <- ")"
      
      error <- FALSE
    }
    
  } else if (solver == "glpk"){
    
    if(!is.null(timeLimit))
      setMIPParmGLPK(TM_LIM, timeLimit * 1000)
    
    solveMIPGLPK(lp)
    
    solverStatus <- mipStatusGLPK(lp)
    
    error <- TRUE
    
    if(mipStatusGLPK(lp)==5){
      
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
    
    weightsnames <- c()
    
    for (i in 1:numCrit)
    {
      weightsnames <- c(weightsnames,paste("w",paro,i,parc,sep=""))
    }
    
    weights <- c()
    
    for (i in 1:numCrit)
      weights <- c(weights,solution[varnames==weightsnames[i]])
    
    names(weights) <- colnames(performanceTable)
    
    profilenames <- matrix(nrow=length(lexicographicOrder),ncol=numCrit)
    
    for (i in 1:length(lexicographicOrder)){
      for (j in 1:numCrit)
      {
        profilenames[i,j] <- paste("P",paro,i,",",j,parc,sep="")
      }
    }
    
    referenceProfiles <- matrix(nrow=length(lexicographicOrder),ncol=numCrit)
    
    for (i in 1:length(lexicographicOrder)){
      for (j in 1:numCrit)
        referenceProfiles[i,j] <- solution[varnames==profilenames[i,j]]
    }
    
    colnames(referenceProfiles) <- colnames(performanceTable)
    
    
    return(list(weights = weights, referenceProfiles = referenceProfiles, solverStatus = solverStatus, humanReadableStatus = "Solution is optimal."))
    
  }
  else
  {
    humanReadableStatus <- "Unknown"
    if(solverStatus %in% c(5,101,102))
      humanReadableStatus <- "Solution is optimal"
    else if(solverStatus %in% c(3,4,103,102))
      humanReadableStatus <- "Solution is infeasible"
    else if(solverStatus %in% c(111,112))
      humanReadableStatus <- "Memory limit"
    else if(solverStatus %in% c(107,108))
      humanReadableStatus <- "Time limit"
    else if(solverStatus %in% c(6,118))
      humanReadableStatus <- "No unbounded solution"
    return(list(solverStatus = solverStatus, humanReadableStatus = humanReadableStatus))
  }
}
