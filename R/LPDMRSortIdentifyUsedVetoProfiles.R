LPDMRSortIdentifyUsedVetoProfiles <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, majorityThreshold, criteriaWeights, profilesPerformances, vetoPerformances, dictatorPerformances = NULL, majorityRule = 'V', alternativesIDs = NULL, criteriaIDs = NULL){
  
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
  
  if (!(majorityThreshold %<=% 1 && majorityThreshold %>=% 0.5))
    stop("majorityThreshold should be a value between 0.5 and 1")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.matrix(profilesPerformances)))
    stop("profilesPerformances should be a matrix")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if (!is.matrix(vetoPerformances))
    stop("vetoPerformances should be a matrix")
  
  if (!(is.null(dictatorPerformances) || is.matrix(dictatorPerformances)))
    stop("dictatorPerformances should be a matrix")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a string")
  else if (!(majorityRule %in% c("V","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'V','v','d','dV','Dv','dv'}")
  
  if (majorityRule %in% c("D","d","dV","Dv","dv") && is.null(dictatorPerformances))
    stop("majorityRule requires non-NULL dictatorPerformances")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,,drop = FALSE]
    assignments <- assignments[alternativesIDs,drop = FALSE]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs,drop = FALSE]
    criteriaMinMax <- criteriaMinMax[criteriaIDs,drop = FALSE]
  }
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  used <- matrix(rep(FALSE,numCat * numCrit),nrow=numCat,ncol=numCrit)
  
  rownames(used) <- names(sort(categoriesRanks))
  colnames(used) <- colnames(performanceTable)
  
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
      dictatorActive <- rep(FALSE, numCrit)
      
      for (j in 1:numCrit)
      {
        if (criteriaMinMax[j] == "min")
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %>=% vetoPerformances[cat,j])
              vetoActive[j] <- TRUE
          if(!is.null(dictatorPerformances))
          {
            if (!is.null(dictatorPerformances[cat,j]) && !is.na(dictatorPerformances[cat,j]))
              if (performanceTable[i,j] %<=% dictatorPerformances[cat,j])
                dictatorActive[j] <- TRUE
          }
        }
        else
        {
          if (!is.null(vetoPerformances[cat,j]) && !is.na(vetoPerformances[cat,j]))
            if (performanceTable[i,j] %<=% vetoPerformances[cat,j])
              vetoActive[j] <- TRUE
          if(!is.null(dictatorPerformances))
          {
            if (!is.null(dictatorPerformances[cat,j]) && !is.na(dictatorPerformances[cat,j]))
              if (performanceTable[i,j] %>=% dictatorPerformances[cat,j])
                dictatorActive[j] <- TRUE
          }
        }
      }
      # stopping condition
      if(majorityRule == 'V')
      {
        if(weightedSum < majorityThreshold || any(vetoActive))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'v')
      {
        if(weightedSum < majorityThreshold || (any(vetoActive) && !any(dictatorActive)))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'd')
      {
        if(weightedSum < majorityThreshold && (!any(dictatorActive) || any(vetoActive)))
        {
          # was the veto necessary ?
          if(any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'dV')
      {
        if((weightedSum < majorityThreshold && !any(dictatorActive)) || any(vetoActive))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'Dv')
      {
        if(!any(dictatorActive) && (any(vetoActive) || weightedSum < majorityThreshold))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
      else if(majorityRule == 'dv')
      {
        if((any(vetoActive) && !any(dictatorActive)) || (weightedSum < majorityThreshold && ((any(vetoActive) && any(dictatorActive)) || (!any(vetoActive) && !any(dictatorActive)))))
        {
          # was the veto necessary ?
          if(!(weightedSum < majorityThreshold) || any(dictatorActive))
            used[cat,] <- sapply(1:numCrit, function(j) (used[cat,j] || vetoActive[j]))
          
          break
        }
      }
    }
  }
  
  return(used)
}
