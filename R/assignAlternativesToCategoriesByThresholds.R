assignAlternativesToCategoriesByThresholds <- function(alternativesScores, categoriesLowerBounds, alternativesIDs = NULL, categoriesIDs=NULL){
  
  ## check the input data
  
  if (!(is.vector(alternativesScores)))
    stop("alternatives scores should be in a vector")
    
  if (!(is.vector(categoriesLowerBounds)))
    stop("categories lower bounds should be in a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(categoriesIDs) || is.vector(categoriesIDs)))
    stop("categories IDs should be in a vector")
  
  ## filter the data according to the given criteria and alternatives
  
  if (!is.null(alternativesIDs)){
    alternativesScores <- alternativesScores[alternativesIDs]
  } 
  
  if (!is.null(categoriesIDs)){
    categoriesLowerBounds <- categoriesLowerBounds[categoriesIDs]
  } 
  
  # -------------------------------------------------------
    
  numAlt <- length(alternativesScores)
  
  categoriesIDs <- names(categoriesLowerBounds)
  
  numCat <- length(categoriesIDs)
  
  if (numCat<1)
    stop("no categories left after filtering, should be at least one")
  
  # -------------------------------------------------------
  
  assignments <- rep(NA,length(alternativesScores))
  names(assignments) <- names(alternativesScores)
  
  sortedCategoriesLowerBounds <- sort(categoriesLowerBounds, decreasing=T)
  
  for (i in 1:length(alternativesScores)){
    
    for (j in 1:length(categoriesLowerBounds)){
      if (j==1){
        if (alternativesScores[i]>=categoriesLowerBounds[j])
          assignments[names(alternativesScores[i])] <- names(categoriesLowerBounds)[j]
      }
      else
      {
        if ((alternativesScores[i]>=categoriesLowerBounds[j]) & alternativesScores[i]<categoriesLowerBounds[j-1])
          assignments[names(alternativesScores[i])] <- names(categoriesLowerBounds)[j]
      }   
    }
  }
  
  return(assignments)
}
