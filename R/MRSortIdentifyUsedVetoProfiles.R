MRSortIdentifyUsedVetoProfiles <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, majorityThreshold, criteriaWeights, profilesPerformances, vetoPerformances, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.numeric(majorityThreshold))
    stop("majorityThreshold should be numeric")
  
  if (length(majorityThreshold) > 1)
    stop("majorityThreshold should be a single number")
  
  if (majorityThreshold > 1 || majorityThreshold < 0.5)
    stop("majorityThreshold should be a value between 0.5 and 1")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.matrix(profilesPerformances)))
    stop("profilesPerformances should be a matrix")
  
  if (!(is.matrix(vetoPerformances)))
    stop("vetoPerformances should be a matrix")
  
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
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  used_vetoes <- matrix(rep(FALSE,numCat * numCrit),nrow=numCat,ncol=numCrit)
      
  rownames(used_vetoes) <- names(sort(categoriesRanks))
  colnames(used_vetoes) <- colnames(performanceTable)
  
  for (i in 1:numAlt)
  {
    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks[categoriesRanks == k])
      
      weightedSum <- 0
      
      for (j in 1:numCrit)
      {
        if (criteriaMinMax[j] == "min")
        {
          if (performanceTable[i,j] %<=% profilesPerformances[cat,j])
            weightedSum <- weightedSum + criteriaWeights[j]
        }
        else
        {
          if (performanceTable[i,j] %>=% profilesPerformances[cat,j])
            weightedSum <- weightedSum + criteriaWeights[j]
        }
      }
      
      vetoActive <- rep(FALSE, numCrit)
      
      for (j in 1:numCrit)
      {
        if (criteriaMinMax[j] == "min")
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %>=% vetoPerformances[cat,j])
            {
              vetoActive[j] <- TRUE
            }
        }
        else
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %<=% vetoPerformances[cat,j])
            {
              vetoActive[j] <- TRUE
            }
        }
      }
      
      # stopping condition
      if(weightedSum < majorityThreshold || any(vetoActive))
      {
        # was the veto necessary ?
        if(any(vetoActive) && !(weightedSum < majorityThreshold))
          used_vetoes[cat,] <- sapply(1:numCrit, function(j) (used_vetoes[cat,j] || vetoActive[j]))
        
        break
      }
    }
  }

  return(used_vetoes)
}
