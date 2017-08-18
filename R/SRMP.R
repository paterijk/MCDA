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
          if (alternativePerformances1[i] %<=% profilePerformances[k,i])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %<=% profilePerformances[k,i])
            weightedSum2 = weightedSum2 + criteriaWeights[i]
        }
        else
        {
          if (alternativePerformances1[i] %>=% profilePerformances[k,i])
            weightedSum1 = weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %>=% profilePerformances[k,i])
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
  
  preorder <- list(rownames(performanceTable)[1])
  
  for (i in 2:numAlt)
  {
    k <- 1
    while(k <= length(preorder))
    {
      j <- preorder[[k]][1]
      comparison <- outranking(performanceTable[i,],performanceTable[j,],referenceProfiles, criteriaWeights, criteriaMinMax)
      if(comparison == 1)
      {
        # alternative i is better than alternative j -> we put it before j and go to the next alternative
        if(k == 1)
          preorder <- c(rownames(performanceTable)[i],preorder)
        else
          preorder <- c(preorder[1:(k-1)],rownames(performanceTable)[i],preorder[k:length(preorder)])
        break
      }
      else if(comparison == 0)
      {
        # alternative i is indifferent to j -> we put it on the same spot and go to the next alternative
        preorder[[k]] <- c(preorder[[k]],rownames(performanceTable)[i])
        break
      }
      
      k <- k + 1
      
      if(k > length(preorder))
      {
        # we've reached the end and could not find an alternative that is worse or indifferent to i
        preorder <- c(preorder,rownames(performanceTable)[i])
        break
      }
    }
  }
  
  return(preorder)
}
