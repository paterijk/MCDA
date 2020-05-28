PROMETHEEOutrankingFlows<- function(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  
{ 
  numAlt<-dim(performanceTable)[1] # number of alternatives
  outrankingFlowsPos<-rep(0,numAlt)  #the positive outranking flow
  outrankingFlowsNeg<-rep(0,numAlt)  #the negative outranking flow
  preferenceTable<-PROMETHEEPreferenceIndices(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      outrankingFlowsPos[i] <- outrankingFlowsPos[i]+preferenceTable[i,j]
      outrankingFlowsNeg[i] <- outrankingFlowsNeg[i]+preferenceTable[j,i]
    }
    outrankingFlowsPos[i] <- outrankingFlowsPos[i]/numAlt
    outrankingFlowsNeg[i] <- outrankingFlowsNeg[i]/numAlt
  }
  names(outrankingFlowsPos) = rownames(preferenceTable)
  names(outrankingFlowsNeg) = rownames(preferenceTable)
  list(outrankingFlowsPos=outrankingFlowsPos,outrankingFlowsNeg=outrankingFlowsNeg)
}