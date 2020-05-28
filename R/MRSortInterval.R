MRSortInterval<-function(performanceTable,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdPes,majorityThresholdOpt){
  
  numcrit<-dim(performanceTable)[2]
  numalt<-dim(performanceTable)[1]
  #Definition of pessimistic and optimistic  versions of an alternative
  performanceTablepes=performanceTableopt<-as.list(rep(0,numalt*numcrit),c(numalt,numcrit))
  dim(performanceTablepes)=dim(performanceTableopt)<- c(numalt,numcrit)
  for (i in (1:numalt)){
    for (j in (1:numcrit)){
      if(is.numeric(performanceTable[[i,j]])) 
      {
        if (criteriaMinMax[j] == "max")
        {
          performanceTablepes[[i,j]]<-range(performanceTable[[i,j]])[1]
          performanceTableopt[[i,j]]<-range(performanceTable[[i,j]])[2]
        }
        else
        {
          performanceTablepes[[i,j]]<-range(performanceTable[[i,j]])[2]
          performanceTableopt[[i,j]]<-range(performanceTable[[i,j]])[1]
        }
      }
      
      else 
      {
        if (criteriaMinMax[j] == "max")
        {
          performanceTablepes[[i,j]]<- -Inf
          performanceTableopt[i,j]<- Inf
        }
        else
        {
          performanceTablepes[[i,j]]<- Inf
          performanceTableopt[i,j]<- -Inf
        }
      }
      
    }
  }
  performanceTableopt<-matrix(as.numeric(unlist(performanceTableopt)),numalt,numcrit)
  performanceTablepes<-matrix(as.numeric(unlist(performanceTablepes)),numalt,numcrit)
  rownames(performanceTablepes)=rownames(performanceTableopt)<-rownames(performanceTable)  
  colnames(performanceTablepes)=colnames(performanceTableopt)<-colnames(performanceTable)
  #The lower bound and the upper bound for an assignment using its pessimistic and its optimistic versions respectively  
  assignmentspes<-MRSort(performanceTablepes,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdPes)
  assignmentsopt<-MRSort(performanceTableopt,categoriesLowerProfiles,categoriesRanks,criteriaWeights,criteriaMinMax,majorityThresholdOpt)
  hpes<-NULL
  hopt<-NULL
  for (i in 1:length(assignmentsopt))
  {
    hpes[i]<-categoriesRanks[assignmentspes[i]]
    hopt[i]<-categoriesRanks[assignmentsopt[i]]
  }
  assignments<-mapply(seq,hpes,hopt)
  names(assignments)<-rownames(performanceTable)
  
  assignmentsfinal<-vector(mode = "list", length = length(assignments))
  
  for (i in 1:length(assignments))
  {
    for (j in 1:length(assignments[[i]]))
    {
      assignmentsfinal[[i]][j]<-names(categoriesRanks[assignments[[i]][j]])
    }
  }
  names(assignmentsfinal)<-rownames(performanceTable)
  return(assignmentsfinal)
}
