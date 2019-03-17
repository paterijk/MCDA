SRMP <- function(performanceTable, referenceProfiles, lexicographicOrder, criteriaWeights, criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL){
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(referenceProfiles)))
    stop("referenceProfiles should be a matrix")
  
  if (!(is.vector(lexicographicOrder)))
    stop("lexicographicOrder should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaWeights <- criteriaWeights[criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    referenceProfiles <- referenceProfiles[,criteriaIDs]
  }
  
  numCrit <- dim(performanceTable)[2]
  numAlt <- dim(performanceTable)[1]
  numRefProf <- dim(referenceProfiles)[1]
  
  # data is filtered, check for some data consistency
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if(length(lexicographicOrder) != numRefProf)
    stop("lexicographicOrder does not contain the same number of profiles as referenceProfiles")
  
  if(!all(sort(lexicographicOrder) == 1:numRefProf))
    stop("lexicographicOrder should contain the indices of referenceProfiles")
  
  if(numRefProf > 1)
    for(k1 in 1:(numRefProf-1))
      for(k2 in (k1+1):numRefProf)
        for (i in 1:numCrit)
        {
          if (criteriaMinMax[i] == "max")
          {
            if(referenceProfiles[k1,i] > referenceProfiles[k2,i])
              stop("referenceProfiles should be ordered from the one with the worst performances to the one with the best; they should also be in a dominance relationship")
          }
          else
          {
            if(referenceProfiles[k1,i] < referenceProfiles[k2,i])
              stop("referenceProfiles should be ordered from the one with the worst performances to the one with the best; they should also be in a dominance relationship")
          }
        }
  
  # -------------------------------------------------------
  
  outranking <- function(alternativePerformances1, alternativePerformances2, profilePerformances, criteriaWeights, criteriaMinMax){
    for (k in lexicographicOrder)
    {
      weightedSum1 <- 0
      weightedSum2 <- 0
      for (i in 1:numCrit)
      {
        if (criteriaMinMax[i] == "min")
        {
          if (alternativePerformances1[[i]] %<=% profilePerformances[[k,i]])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[[i]] %<=% profilePerformances[[k,i]])
            weightedSum2 = weightedSum2 + criteriaWeights[i]
        }
        else
        {
          if (alternativePerformances1[[i]] %>=% profilePerformances[[k,i]])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[[i]] %>=% profilePerformances[[k,i]])
            weightedSum2 = weightedSum2 + criteriaWeights[i]
        }
      }
      
      # can we differentiate between the two alternatives?
      if(weightedSum1 > weightedSum2)
        return(1)
      else if(weightedSum1 < weightedSum2)
        return(-1)
    }
    # could not differentiate between the alternatives
    return(0)
  }
  
  alternativesValues <- rep(1, numAlt)
  
  names(alternativesValues) <- rownames(performanceTable)
  
  for (i in 2:numAlt)
  {
    minVal <- 0
    
    maxVal <- i
    
    for(j in 1:(i-1))
    {
      #print(c('j',j))
      #print(alternativesValues)
      if(alternativesValues[j] >= minVal && alternativesValues[j] <= maxVal)
      {
        comparison <- outranking(performanceTable[i,],performanceTable[j,],referenceProfiles, criteriaWeights, criteriaMinMax)
        
        if(comparison == 1)
        {
          # i is better than j
          minVal <- alternativesValues[j] + 1
        }
        else if(comparison == 0)
        {
          # i is the same as j
          alternativesValues[i] <- alternativesValues[j]
          
          minVal <- -1
          
          break
        }
        else
        {
          # i is worse than j
          maxVal <- alternativesValues[j] - 1
        }
      }
    }
    
    if(minVal == i)
      alternativesValues[i] <- i
    else if(minVal == 0)
      alternativesValues <- sapply(1:numAlt, function(index){if(index<i) (alternativesValues[index] + 1) else alternativesValues[index]})
    else if(minVal >= 0)
    {
      alternativesValues <- sapply(1:numAlt, function(index){if(index<i && alternativesValues[index] >= minVal) (alternativesValues[index] + 1) else alternativesValues[index]})
      
      alternativesValues[i] <- minVal
    }
  }
  
  return(alternativesValues)
}
