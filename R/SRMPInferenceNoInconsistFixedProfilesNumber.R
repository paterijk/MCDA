SRMPInferenceNoInconsistFixedProfilesNumber <- function(performanceTable, criteriaMinMax, profilesNumber, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, solver="glpk", timeLimit = NULL, cplexIntegralityTolerance = NULL, cplexThreads = NULL){
  
  ## check the input data
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.numeric(profilesNumber)))
    stop("profilesNumber should be numberic")
  
  profilesNumber <- as.integer(profilesNumber)
  
  if (!(is.null(timeLimit)))
      {
        if(!is.numeric(timeLimit))
          stop("timeLimit should be numeric")
        if(timeLimit <= 0)
          stop("timeLimit should be strictly pozitive")
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
  
  if (!(profilesNumber > 0))
    stop("profilesNumber should be strictly pozitive")
  
  lexicographicOrders <- permn(1:profilesNumber)
  
  startTime <- Sys.time()
  
  result <- (list(humanReadableStatus = "No solution found in the given time limit"))
  
  for(lexicographicOrder in lexicographicOrders)
  {
    currentTime <- Sys.time()
    
    timeLeft <- NULL
    
    if(!is.null(timeLimit))
    {
      timeLeft <- as.double(timeLimit - as.double(currentTime - startTime))
      if(timeLeft < 1)
        return(result)
    }
    
    result <- SRMPInferenceNoInconsistFixedLexicographicOrder(performanceTable, criteriaMinMax, lexicographicOrder, preferencePairs, indifferencePairs, alternativesIDs, criteriaIDs, solver, timeLeft, cplexIntegralityTolerance, cplexThreads)
    
    if(result$solverStatus == 5)
      return(list(criteriaWeights = result$criteriaWeights, referenceProfiles = result$referenceProfiles, lexicographicOrder = lexicographicOrder, solverStatus = result$solverStatus, humanReadableStatus = result$humanReadableStatus))
  }
  
  return(result)
}
