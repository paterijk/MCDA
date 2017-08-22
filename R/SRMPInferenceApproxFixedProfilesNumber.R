SRMPInferenceApproxFixedProfilesNumber <- function(performanceTable, criteriaMinMax, profilesNumber, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, timeLimit = 60, populationSize = 100, mutationProb = 0.5){
  
  ## check the input data
  
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if(is.null(colnames(performanceTable)))
    stop("performanceTable columns should be named")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if(!all(sort(colnames(performanceTable)) == sort(names(criteriaMinMax))))
    stop("criteriaMinMax should be named as the columns of performanceTable")
  
  if (!(is.numeric(profilesNumber)))
    stop("profilesNumber should be numberic")
  
  profilesNumber <- as.integer(profilesNumber)
  
  if (!(is.null(timeLimit)))
  {
    if(!is.numeric(timeLimit))
      stop("timeLimit should be numeric")
    if(timeLimit <= 0)
      stop("timeLimit should be strictly positive")
  }
  
  if (!(is.null(populationSize)))
  {
    if(!is.numeric(populationSize))
      stop("populationSize should be numeric")
    if(populationSize < 10)
      stop("populationSize should be at least 10")
  }
  
  if (!(is.null(mutationProb)))
  {
    if(!is.numeric(mutationProb))
      stop("mutationProb should be numeric")
    if(mutationProb < 0 || mutationProb > 1)
      stop("mutationProb should be between 0 and 1")
  }

  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if(dim(preferencePairs)[2] != 2)
    stop("preferencePairs should have two columns")
  
  if(!is.null(indifferencePairs))
    if(dim(indifferencePairs)[2] != 2)
      stop("indifferencePairs should have two columns")
  
  if (!(profilesNumber > 0))
    stop("profilesNumber should be strictly pozitive")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    preferencePairs <- preferencePairs[(preferencePairs[,1] %in% alternativesIDs) & (preferencePairs[,2] %in% alternativesIDs),]
    if(dim(preferencePairs)[1] == 0)
      preferencePairs <- NULL
    if(!is.null(indifferencePairs))
    {
      indifferencePairs <- indifferencePairs[(indifferencePairs[,1] %in% alternativesIDs) & (indifferencePairs[,2] %in% alternativesIDs),]
      if(dim(indifferencePairs)[1] == 0)
        indifferencePairs <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (is.null(dim(preferencePairs))) 
    stop("preferencePairs is empty or the provided alternativesIDs have filtered out everything from within")
  
  # -------------------------------------------------------
  
  numAlt <- dim(performanceTable)[1]
  numCrit <- dim(performanceTable)[2]
  minEvaluations <- apply(performanceTable, 2, min)
  maxEvaluations <- apply(performanceTable, 2, max)
  
  # -------------------------------------------------------
  
  outranking <- function(alternativePerformances1, alternativePerformances2, profilePerformances, criteriaWeights, lexicographicOrder, criteriaMinMax){
    for (k in lexicographicOrder)
    {
      weightedSum1 <- 0
      weightedSum2 <- 0
      for (i in 1:numCrit)
      {
        if (criteriaMinMax[i] == "min")
        {
          if (alternativePerformances1[i] %<=% profilePerformances[k,i])
            weightedSum1 <- weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %<=% profilePerformances[k,i])
            weightedSum2 <- weightedSum2 + criteriaWeights[i]
        }
        else
        {
          if (alternativePerformances1[i] %>=% profilePerformances[k,i])
            weightedSum1 <- weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %>=% profilePerformances[k,i])
            weightedSum2 <- weightedSum2 + criteriaWeights[i]
        }
      }
      
      # can we differentiate between the two alternatives?
      if(weightedSum1 > weightedSum2)
        return(1)
      else if(weightedSum1 < weightedSum2)
        return(-1)
    }
    # could not differentiate between the alternatives
    return(0)
  }
  
  InitializePopulation <- function(){
    population <- list()
    for(i in 1:populationSize)
    {
      values <- c(0,sort(runif(numCrit-1,0,1)),1)
      weights <- sapply(1:numCrit, function(i) return(values[i+1]-values[i]))
      names(weights) <- colnames(performanceTable)
      
      profiles <- NULL
      for(j in 1:numCrit)
        profiles <- cbind(profiles,sort(runif(profilesNumber,minEvaluations[j],maxEvaluations[j])))
      colnames(profiles) <- colnames(performanceTable)
      
      lexicographicOrder <- sample(1:profilesNumber, profilesNumber)
      
      population[[length(population)+1]] <- list(criteriaWeights = weights, referenceProfiles = profiles, lexicographicOrder = lexicographicOrder)
    }
    return(population)
  }
  
  Fitness <- function(individual){
    total <- 0
    ok <- 0
    for (i in 1:dim(preferencePairs)[1]){
      comparison <- outranking(performanceTable[preferencePairs[i,1],],performanceTable[preferencePairs[i,2],],individual$referenceProfiles, individual$criteriaWeights, individual$lexicographicOrder, criteriaMinMax)
      if(comparison == 1)
        ok <- ok + 1
      total <- total + 1
    }
    if(!is.null(indifferencePairs))
      for (i in 1:dim(indifferencePairs)[1]){
        comparison <- outranking(performanceTable[indifferencePairs[i,1],],performanceTable[indifferencePairs[i,2],],individual$referenceProfiles, individual$criteriaWeights, individual$lexicographicOrder, criteriaMinMax)
        if(comparison == 0)
          ok <- ok + 1
        total <- total + 1
      }
    return(ok/total)
  }
  
  Reproduce <- function(parents){
    children <- list()
    
    numPairs <- as.integer(length(parents)/2)
    
    pairings <- matrix(sample(1:length(parents),numPairs*2),numPairs,2)
    
    for(i in 1:numPairs)
    {
      parent1 <- parents[[pairings[i,1]]]
      
      parent2 <- parents[[pairings[i,2]]]
      
      # crossover bewtween profiles
      
      criteria <- sample(colnames(performanceTable), numCrit)
      
      pivot <- runif(1,1,numCrit - 1)
      
      profiles1 <- matrix(rep(0,numCrit*profilesNumber),profilesNumber,numCrit)
      profiles2 <- matrix(rep(0,numCrit*profilesNumber),profilesNumber,numCrit)
      
      colnames(profiles1) <- colnames(performanceTable)
      colnames(profiles2) <- colnames(performanceTable)
      
      for(k in 1:profilesNumber)
        for(j in 1:numCrit)
        {
          if(j <= pivot)
          {
            profiles1[k,criteria[j]] <- parent1$referenceProfiles[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent2$referenceProfiles[k,criteria[j]]
          }
          else
          {
            profiles1[k,criteria[j]] <- parent2$referenceProfiles[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent1$referenceProfiles[k,criteria[j]]
          }
        }
      
      # child identical to first parent - will get mutated in the second step
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent1$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child identical to second parent
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
      
      # child takes weights from first parent and profiles from second
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from first
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from first parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from first parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent1$lexicographicOrder)
      
      # if the lexicographic orders are different then we add more children
      
      if(!all(parent1$lexicographicOrder == parent2$lexicographicOrder))
      {
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent2$lexicographicOrder)
      }
    }
    
    # mutate children
    
    numChildren <- length(children)
    
    for(i in 1:numChildren)
    {
      if(runif(1,0,1) < mutationProb)
      {
        # mutate one profile evaluation
        
        criterion <- sample(colnames(performanceTable),1)
        
        k <- sample(1:profilesNumber,1)
        
        maxVal <- maxEvaluations[criterion]

        minVal <- minEvaluations[criterion]
        
        if(k < profilesNumber)
        {
          if(criteriaMinMax[criterion] == 'max')
            maxVal <- children[[i]]$referenceProfiles[k+1,criterion]
          else
            minVal <- children[[i]]$referenceProfiles[k+1,criterion]
        }
        
        if(k > 1)
        {
          if(criteriaMinMax[criterion] == 'max')
            minVal <- children[[i]]$referenceProfiles[k-1,criterion]
          else
            maxVal <- children[[i]]$referenceProfiles[k-1,criterion]
        }
        
        children[[i]]$referenceProfiles[k,criterion] <- runif(1,minVal,maxVal)
      }
      
      if(runif(1,0,1) < mutationProb)
      {
        # mutate two criteria weights
        
        criteria <- sample(colnames(performanceTable),2)
        
        minVal <- 0 - children[[i]]$criteriaWeights[criteria[1]]
        
        maxVal <- children[[i]]$criteriaWeights[criteria[2]]
        
        tradeoff <- runif(1,minVal,maxVal)
        
        children[[i]]$criteriaWeights[criteria[1]] <- children[[i]]$criteriaWeights[criteria[1]] + tradeoff
        
        children[[i]]$criteriaWeights[criteria[2]] <- children[[i]]$criteriaWeights[criteria[2]] - tradeoff
      }
      
      if(runif(1,0,1) < mutationProb)
      {
        # mutate the lexicographic order
        
        i1 <- sample(1:profilesNumber, 1)
        
        adjacent <- NULL
        
        if(i1 > 1)
          adjacent <- c(adjacent, i1 - 1)
        
        if(i1 < profilesNumber)
          adjacent <- c(adjacent, i1 + 1)
        
        i2 <- sample(adjacent, 1)
                     
        temp <- children[[i]]$lexicographicOrder[i1]
        
        children[[i]]$lexicographicOrder[i1] <- children[[i]]$lexicographicOrder[i2]
        
        children[[i]]$lexicographicOrder[i2] <- temp
      }
    }
    
    return(children)
  }
  
  # -------------------------------------------------------
  
  startTime <- Sys.time()
  
  # Initialize population
  
  population <- InitializePopulation()
  
  bestIndividual <- list(fitness = 0)
  
  # Main loop
  
  ct <- 0
  
  while(as.double(Sys.time() - startTime) < timeLimit)
  {
    # Evaluate population
    
    evaluations <- unlist(lapply(population, Fitness))
    
    # Store best individual if better than the overall best
    
    maxFitness <- max(evaluations)
    
    if(maxFitness > bestIndividual$fitness)
    {
      bestIndividual <- population[[match(maxFitness,evaluations)]]
      
      bestIndividual$fitness <- maxFitness
    }
    
    # report
    
    if(as.double(Sys.time() - startTime) / 5 > ct)
    {
      ct <- ct + 1
      
      print(sprintf("Best fitness so far: %6.2f%%", bestIndividual$fitness * 100))
    }
    
    # check if we are done
    
    if(bestIndividual$fitness == 1)
      break
    
    # Selection - not the first iteration
    
    if(length(population) > populationSize)
    {
      evaluations <- evaluations^2
      
      newPopulation <- list()
      
      i <- 1
      
      while(length(newPopulation) < populationSize)
      {
        if(runif(1,0,1) <= evaluations[i])
        {
          evaluations[i] <- -1
          
          newPopulation[[length(newPopulation)+1]] <- population[[i]]
        }
        
        i <- i + 1
        
        if(i > length(population))
          i <- 1
      }
      
      population <- newPopulation
    }
    
    # Reproduction
    
    population <- Reproduce(population)
  }
  
  print(sprintf("Final model fitness: %6.2f%%", bestIndividual$fitness * 100))
  
  return(bestIndividual)
}
