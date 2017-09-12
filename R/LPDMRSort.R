LPDMRSort <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, criteriaWeights, criteriaMinMax, majorityThreshold, criteriaVetos = NULL, criteriaDictators = NULL, majorityRule = "M", alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL){
  
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
  
  if (!(is.null(criteriaDictators) || is.matrix(criteriaDictators)))
    stop("criteriaDictators should be a matrix")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a string")
  else if (!(majorityRule %in% c("M","V","D","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'M','V','D','v','d','dV','Dv','dv'}")
  
  if (majorityRule %in% c("V","v","dV","Dv","dv") && is.null(criteriaVetos))
    stop("majorityRule requires non-NULL criteriaVetos")
  
  if (majorityRule %in% c("D","d","dV","Dv","dv") && is.null(criteriaDictators))
    stop("majorityRule requires non-NULL criteriaDictators")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaWeights <- criteriaWeights[criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    categoriesLowerProfiles <- categoriesLowerProfiles[,criteriaIDs]
  }
  
  if ((!is.null(criteriaIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[,criteriaIDs]  
  }
  
  if ((!is.null(criteriaIDs)) && (!is.null(criteriaDictators))){
    criteriaDictators <- criteriaDictators[,criteriaIDs]  
  }
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[categoriesIDs,]
  }
    
  if (!is.null(categoriesIDs)){
    categoriesLowerProfiles <- categoriesLowerProfiles[categoriesIDs,]
  }
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaDictators))){
    criteriaDictators <- criteriaDictators[categoriesIDs,]
  }  
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
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
      
      if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
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
      
      dictatorActive <- FALSE
      
      
      if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
      {
        for (crit in names(criteriaMinMax))
        {
          if(!is.na(criteriaDictators[cat,crit]) & !is.null(criteriaDictators[cat,crit]))
          {
            if (criteriaMinMax[crit] == "min")
            {
              if (performanceTable[i,crit] %<=% criteriaDictators[cat,crit])
              {
                dictatorActive <- TRUE
                break
              }
            }
            else
            {
              if (performanceTable[i,crit] %>=% criteriaDictators[cat,crit])
              {
                dictatorActive <- TRUE
                break
              }
            }
          }
        }
      }
      # stopping condition
      if(majorityRule == 'M')
      {
        if(weightedSum < majorityThreshold)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'V')
      {
        if(weightedSum < majorityThreshold || vetoActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'D')
      {
        if(weightedSum < majorityThreshold && !dictatorActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'v')
      {
        if(weightedSum < majorityThreshold || (vetoActive && !dictatorActive))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      else if(majorityRule == 'd')
      {
        if(weightedSum < majorityThreshold && (!dictatorActive || vetoActive))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'dV')
      {
        if((weightedSum < majorityThreshold && !dictatorActive) || vetoActive)
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'Dv')
      {
        if(!dictatorActive && (vetoActive || weightedSum < majorityThreshold))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
      if(majorityRule == 'dv')
      {
        if((vetoActive && !dictatorActive) || (weightedSum < majorityThreshold && ((vetoActive && dictatorActive) || (!vetoActive && !dictatorActive))))
          return(names(categoriesRanks)[categoriesRanks == (k + 1)])
      }
    }
    # better than all profiles -> top categ
    return(names(categoriesRanks)[categoriesRanks == 1])
  }
  
  assignments <- sapply(1:numAlt, getCategory)
  
  names(assignments) <- rownames(performanceTable)
  
  return(assignments)
}
