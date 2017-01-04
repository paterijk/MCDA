MRSort <- function(performanceTable, categoriesLowerProfiles, criteriaWeights, criteriaMinMax, majorityThreshold, criteriaVetos = NULL, alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(categoriesLowerProfiles)))
    stop("categoriesLowerProfiles should be a matrix")
  
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
  
  if ((!is.null(categoriesIDs)) && (!is.null(criteriaVetos))){
    criteriaVetos <- criteriaVetos[categoriesIDs,]
  }
    
  if (!is.null(categoriesIDs)){
    categoriesLowerProfiles <- categoriesLowerProfiles[categoriesIDs,]
  }
  
  
  
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- dim(categoriesLowerProfiles)[1]
  
  
  
  # -------------------------------------------------------
  
  outranking <- function(alternativePerformances, profilePerformances, criteriaWeights, criteriaMinMax, majorityThreshold, profileCriteriaVetos=NULL){
    localConcordance <- rep(0,numCrit)
    veto <- 0
    for (i in 1:numCrit){
      if (criteriaMinMax[i] == "min"){
        if (alternativePerformances[i] %<=% profilePerformances[i])
          localConcordance[i] = 1
        if (!is.null(profileCriteriaVetos)){
          if (!is.na(profileCriteriaVetos[i])){
            if (alternativePerformances[i] %>=% profileCriteriaVetos[i])
              veto = 1 
          }
        }
      }
      else{
        if (alternativePerformances[i] %>=% profilePerformances[i])
          localConcordance[i] = 1
        if (!is.null(profileCriteriaVetos)){
          if (!is.na(profileCriteriaVetos[i])){
            if (alternativePerformances[i] %<=% profileCriteriaVetos[i])
              veto = 1 
          }
        }
      }
    }
    
    concordance = sum(localConcordance*criteriaWeights)
    
    if ((veto == 1) || (concordance < majorityThreshold))
      return(FALSE)
    else 
      return(TRUE)
  }
  
  
  assignments <- c()
  
  for (i in 1:numAlt){
    categoryNotFound <- TRUE
    k <- 1
    while ((categoryNotFound) && k<=numCat-1){
      if (is.null(criteriaVetos)){
        if(outranking(performanceTable[i,],categoriesLowerProfiles[k,], criteriaWeights, criteriaMinMax, majorityThreshold, profileCriteriaVetos = NULL)){
          category <- k
          categoryNotFound <- FALSE
        }
        else
          k<-k+1
      } else {
        if(outranking(performanceTable[i,],categoriesLowerProfiles[k,], criteriaWeights, criteriaMinMax, majorityThreshold, profileCriteriaVetos = criteriaVetos[k,])){
          category <- k
          categoryNotFound <- FALSE
        }
        else
          k<-k+1
      }
    }
    assignments <- c(assignments, rownames(categoriesLowerProfiles)[k]) 
  }
  
  names(assignments) <- rownames(performanceTable)
  
  return(assignments)
}
