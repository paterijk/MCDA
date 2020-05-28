PROMETHEEII<-function(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  # This function consists of the (P, I) complete ranking which  is obtained from the net outranking
  # flow which is the balance between the positive and the negative outranking flows.This function returns two matrices P (for Preference relations) and I(for indifference relations).
  #Each matrix contains only 0 and 1. 1 (at the position (i,j) ) means that a_i P a_j (in the matrix P), or a_i I a_j (in the matrix I)
  # and 0 else.  
{ 
  numAlt<-dim(performanceTable)[1] # number of alternatives
  # Call of the function PROMETHEEOutrankingFlows
  result<-PROMETHEEOutrankingFlows(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  posflows<-result[[1]]
  negflows<-result[[2]]
  netflows<-posflows - negflows
  ord <- order(netflows,decreasing = TRUE)
  O <- list()
  prev <- Inf
  j <- 0
  for (i in ord)
  {
    if (netflows[[i]] == prev)
    {
      O[[j]] <- c(O[[j]],names(netflows)[i])
    }
    else
    {
      j <- j + 1
      O[[j]] <- names(netflows)[i]
      prev <- netflows[[i]]
    }
  }
  O
}