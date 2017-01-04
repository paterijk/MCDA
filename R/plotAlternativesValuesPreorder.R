plotAlternativesValuesPreorder <- function(alternativesValues, decreasing = TRUE, alternativesIDs = NULL){
  
  # if (!require(Rgraphviz)) stop("Rgraphviz package could not be loaded")
  
  if (!requireNamespace("Rgraphviz", quietly = TRUE)) stop("Rgraphviz package could not be loaded")
  
  ## check the input data

  if (!(is.vector(alternativesValues)))
    stop("alternativesValues should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    alternativesValues <- alternativesValues[alternativesIDs]
  } 
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (length(alternativesValues)<2) 
    stop("less than 2 criteria or 2 alternatives")
  
  # -------------------------------------------------------
  
  numAlt <- length(alternativesValues)
  
  
  
  graph<-NULL
  
  # we first order the ranks from best to worst
  
  orderedRanks<-alternativesValues[order(alternativesValues,decreasing=decreasing)]
  
  # we now construct the nodes of the graph (alternatives with equal rank are put together into one node)
  
  prevRank<-orderedRanks[1]
  curLab<-names(orderedRanks)[1]
  labels<-curLab
  
  for (i in 2:length(orderedRanks))
  {
    
    if (orderedRanks[i] != orderedRanks[i-1]){
      
      labels<-c(labels, names(orderedRanks)[i])
    }
    else
    {
      labels[length(labels)] <- paste(labels[length(labels)],names(orderedRanks)[i],sep=",")
    }
  }
  
  # we now construct the edges of the graph
  
  edg<-vector("list",length=length(labels))
  names(edg)<-labels
  if (length(labels)==1){
    edg[[1]]<-list(edges=character(0))
  }
  else{
    for (i in 1:(length(labels)-1))
    {
      edg[[i]]<-list(edges=labels[i+1])
    }
    edg[[length(labels)]]<-list(edges=character(0))
  }
  
  # finally we construct the graph
  
  graph <- new("graphNEL", nodes=labels, edgeL=edg, edgemode="directed")
  
  Rgraphviz::plot(graph, attrs = list(node = list(shape = "box", fixedsize = FALSE)))  
}
