weightedSum <- function(performanceTable, criteriaWeights, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performance table, should be a matrix or a data frame")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteria weights should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the performance table and the criteria according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
  
  if (!is.null(criteriaIDs)) performanceTable <- performanceTable[,criteriaIDs]
  
  if (!is.null(criteriaIDs)) criteriaWeights <- criteriaWeights[criteriaIDs]
  
  ## transform performanceTable to matrix
  
  performanceTable<-as.matrix(performanceTable)  
  
  ## now calculate the weighted sum
  
  out<-c()
  
  for (i in 1:dim(performanceTable)[1]){
    out<-rbind(out, crossprod(performanceTable[i,],criteriaWeights))
  }
  
  out<-as.vector(out)
  
  names(out) <- rownames(performanceTable)
  
  return(out)
}

