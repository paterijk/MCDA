MRSort <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, criteriaWeights, criteriaMinMax, majorityThreshold, criteriaVetos = NULL, alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(categoriesLowerProfiles)))
    stop("categoriesLowerProfiles should be a matrix")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if (!(is.null(categoriesIDs) || is.vector(categoriesIDs)))
    stop("categoriesIDs should be a vector")
  
  if (!(is.null(criteriaVetos) || is.matrix(criteriaVetos)))
    stop("criteriaVetos should be a matrix")
  
  # check if we have a lower profile for the worst category
  
  worstCat <- names(categoriesRanks)[categoriesRanks == length(categoriesRanks)]
  
  if(!(worstCat %in% rownames(categoriesLowerProfiles)))
  {
    categoriesLowerProfiles <- rbind(categoriesLowerProfiles, rep(NA,length(criteriaMinMax)))
    
    rownames(categoriesLowerProfiles)[length(categoriesRanks)] <- worstCat
  }
  
  if (!is.null(criteriaVetos))
  {
    if(!(worstCat %in% rownames(criteriaVetos)))
    {
      criteriaVetos <- rbind(criteriaVetos, rep(NA,length(criteriaMinMax)))
      
      rownames(criteriaVetos)[length(categoriesRanks)] <- worstCat
    }
  }
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,,drop=FALSE]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs,drop=FALSE]
    criteriaWeights <- criteriaWeights[criteriaIDs,drop=FALSE]
    criteriaMinMax <- criteriaMinMax[criteriaIDs,drop=FALSE]
    categoriesLowerProfiles <- categoriesLowerProfiles[,criteriaIDs,drop=FALSE]
  }
  
  if ((!is.null(criteriaIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[,criteriaIDs,drop=FALSE]  
  }
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[categoriesIDs,,drop=FALSE]
  }
    
  if (!is.null(categoriesIDs)){
    categoriesLowerProfiles <- categoriesLowerProfiles[categoriesIDs,,drop=FALSE]
  }
  
  if (!is.null(categoriesIDs)){
    # filter out categories
    categoriesRanks <- categoriesRanks[names(categoriesRanks) %in% categoriesIDs]
    # check if we took out all categories
    if(length(categoriesRanks) == 0)
      stop('categoriesIDs have filtered out all categories')
    # order the remaining ones
    categoriesRanks <- sort(categoriesRanks)
    # store their order
    catOrder <- names(categoriesRanks)
    # adjust their indices to a range from 1 to the number of remaining categories
    categoriesRanks <- 1:length(categoriesRanks)
    # rename them
    names(categoriesRanks) <- catOrder
  }
  
  # -------------------------------------------------------

  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  # -------------------------------------------------------
  
  getCategory <- function(i)
  {
    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks)[categoriesRanks == k]
      
      weightedSum <- 0
      
      for (crit in names(criteriaMinMax))
      {
        if (criteriaMinMax[crit] == "min")
        {
          if (performanceTable[i,crit] %<=% categoriesLowerProfiles[cat,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
        else
        {
          if (performanceTable[i,crit] %>=% categoriesLowerProfiles[cat,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
      }
      
      vetoActive <- FALSE
      
      if(!is.null(criteriaVetos))
      {
        for (crit in names(criteriaMinMax))
        {
          if(!is.na(criteriaVetos[cat,crit]) & !is.null(criteriaVetos[cat,crit]))
          {
            if (criteriaMinMax[crit] == "min")
            {
              if (performanceTable[i,crit] %>=% criteriaVetos[cat,crit])
              {
                vetoActive <- TRUE
                break
              }
            }
            else
            {
              if (performanceTable[i,crit] %<=% criteriaVetos[cat,crit])
              {
                vetoActive <- TRUE
                break
              }
            }
          }
        }
      }
      
      # stopping condition
      if(weightedSum < majorityThreshold || vetoActive)
        return(names(categoriesRanks)[categoriesRanks == (k + 1)])
    }
    # better than all profiles -> top categ
    return(names(categoriesRanks)[categoriesRanks == 1])
  }
  
  assignments <- sapply(1:numAlt, getCategory)
  
  names(assignments) <- rownames(performanceTable)
  
  return(assignments)
}
