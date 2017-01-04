normalizePerformanceTable <- function(performanceTable, normalizationTypes=NULL, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## http://en.wikipedia.org/wiki/Feature_scaling
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performance table, should be a matrix or a data frame")
  
  if (!(is.null(normalizationTypes) || is.vector(normalizationTypes)))
    stop("normalizationTypes should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the performance table and the criteria according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
  
  if (!is.null(criteriaIDs)) performanceTable <- performanceTable[,criteriaIDs]
  
  if (!is.null(criteriaIDs) && !is.null(normalizationTypes)) normalizationTypes <- normalizationTypes[criteriaIDs]
  
  for (i in 1:dim(performanceTable)[2]){
    if (normalizationTypes[i] == "percentageOfMax"){
      performanceTable[,i] <- percentageOfMax(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "rescaling"){
      performanceTable[,i] <- rescaling(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "standardization"){
      performanceTable[,i] <- standardization(performanceTable[,i])
    }
    else if (normalizationTypes[i] == "scaleToUnitLength"){
      performanceTable[,i] <- scaleToUnitLength(performanceTable[,i])
    }
  }

  return(performanceTable)
  
}

percentageOfMax <- function(data){
  max <- max(data)
  data <- data/max
  return(data)
}

rescaling <- function(data){
  max <- max(data)
  min <- min(data)
  data <- (data-min)/(max-min)
  return(data)
}

standardization <- function(data){
  mean <- mean(data)
  sd <- sd(data)
  data <- (data - mean)/sd
  return(data)
}

scaleToUnitLength <- function(data){
  norm <- sqrt(sum(data^2))
  data <- data/norm
  return(data)
}
