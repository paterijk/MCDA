LPDMRSortInferenceApprox <- function(performanceTable, criteriaMinMax, categoriesRanks, assignments, majorityRules = c("M","V","D","v","d","dV","Dv","dv"), alternativesIDs = NULL, criteriaIDs = NULL, timeLimit = 60, populationSize = 20, mutationProb = 0.1){
  
  ## check the input data
  
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if(is.null(colnames(performanceTable)))
    stop("performanceTable columns should be named")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if(is.null(names(assignments)))
    stop("assignments should be named")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if(!all(sort(colnames(performanceTable)) == sort(names(criteriaMinMax))))
    stop("criteriaMinMax should be named as the columns of performanceTable")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(assignments %in% names(categoriesRanks)))
    stop("some of the assignments reference a category which does not exist in categoriesRanks")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!is.character(majorityRules))
    stop("majorityRules should be a character, a string of characters, or a vector of strings of characters")
  else if (!(all(majorityRules %in% c("M","V","D","v","d","dV","Dv","dv"))))
    stop("majorityRules needs to take values in {'M','V','D','v','d','dV','Dv','dv'}")
  
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
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    assignments <- assignments[names(assignments) %in% alternativesIDs]
  }
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (length(assignments) == 0) 
    stop("assignments is empty or the provided alternativesIDs have filtered out everything from within")
  
  # -------------------------------------------------------
  
  numAlt <- dim(performanceTable)[1]
  numCrit <- dim(performanceTable)[2]
  numCat <- length(categoriesRanks)
  minEvaluations <- apply(performanceTable, 2, min)
  maxEvaluations <- apply(performanceTable, 2, max)
  
  # -------------------------------------------------------
  
  getCategory <- function(alternativePerformances, criteriaWeights, majorityThreshold, profilesPerformances, vetoPerformances, dictatorPerformances, majorityRule, criteriaMinMax){
    for (k in (numCat-1):1)
    {
      weightedSum <- 0
      
      for (crit in names(criteriaMinMax))
      {
        if (criteriaMinMax[crit] == "min")
        {
          if (alternativePerformances[crit] %<=% profilesPerformances[k,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
        else
        {
          if (alternativePerformances[crit] %>=% profilesPerformances[k,crit])
            weightedSum <- weightedSum + criteriaWeights[crit]
        }
      }
      
      vetoActive <- FALSE
      
      if(majorityRule %in% c("V","v","dV","Dv","dv"))
      {
        for (crit in names(criteriaMinMax))
        {
          if (criteriaMinMax[crit] == "min")
          {
            if (alternativePerformances[crit] %>=% vetoPerformances[k,crit])
            {
              vetoActive <- TRUE
              break
            }
          }
          else
          {
            if (alternativePerformances[crit] %<=% vetoPerformances[k,crit])
            {
              vetoActive <- TRUE
              break
            }
          }
        }
      }
      
      dictatorActive <- FALSE
      
      if(majorityRule %in% c("D","d","dV","Dv","dv"))
      {
        for (crit in names(criteriaMinMax))
        {
          if (criteriaMinMax[crit] == "min")
          {
            if (alternativePerformances[crit] %<=% dictatorPerformances[k,crit])
            {
              dictatorActive <- TRUE
              break
            }
          }
          else
          {
            if (alternativePerformances[crit] %>=% dictatorPerformances[k,crit])
            {
              dictatorActive <- TRUE
              break
            }
          }
        }
      }
      
      # stopping condition
      if(majorityRule == 'M')
      {
        if(weightedSum < majorityThreshold)
          return(k + 1)
      }
      else if(majorityRule == 'V')
      {
        if(weightedSum < majorityThreshold || vetoActive)
          return(k + 1)
      }
      else if(majorityRule == 'D')
      {
        if(weightedSum < majorityThreshold && !dictatorActive)
          return(k + 1)
      }
      else if(majorityRule == 'v')
      {
        if(weightedSum < majorityThreshold || (vetoActive && !dictatorActive))
          return(k + 1)
      }
      else if(majorityRule == 'd')
      {
        if(weightedSum < majorityThreshold && (!dictatorActive || vetoActive))
          return(k + 1)
      }
      if(majorityRule == 'dV')
      {
        if((weightedSum < majorityThreshold && !dictatorActive) || vetoActive)
          return(k + 1)
      }
      if(majorityRule == 'Dv')
      {
        if(!dictatorActive && (vetoActive || weightedSum < majorityThreshold))
          return(k + 1)
      }
      if(majorityRule == 'dv')
      {
        if((vetoActive && !dictatorActive) || (weightedSum < majorityThreshold && ((vetoActive && dictatorActive) || (!vetoActive && !dictatorActive))))
          return(k + 1)
      }
    }
    # better than all profiles -> top categ
    return(1)
  }
  
  InitializePopulation <- function()
  {
    population <- list()
    for(i in 1:populationSize)
    {
      values <- c(0,sort(runif(numCrit-1,0,1)),1)
      weights <- sapply(1:numCrit, function(i) return(values[i+1]-values[i]))
      names(weights) <- colnames(performanceTable)
      
      majority <- runif(1,0.5,1)
      
      majorityRule <- sample(majorityRules, 1)
      
      profiles <- NULL
      
      for(j in 1:numCrit)
      {
        if(criteriaMinMax[j] == 'max')
          profiles <- cbind(profiles,sort(runif(numCat - 1,minEvaluations[j],maxEvaluations[j]), decreasing = TRUE))
        else
          profiles <- cbind(profiles,sort(runif(numCat - 1,minEvaluations[j],maxEvaluations[j])))
      }
      colnames(profiles) <- colnames(performanceTable)
      
      vetoes <- NULL
      
      for(j in 1:numCrit)
      {
        if(criteriaMinMax[j] == 'max')
          vetoes <- cbind(vetoes,rep(minEvaluations[j] - 1, numCat - 1))
        else
          vetoes <- cbind(vetoes,rep(maxEvaluations[j] + 1, numCat - 1))
      }
      rownames(vetoes) <- c()
      colnames(vetoes) <- colnames(performanceTable)
      
      dictators <- NULL
      
      for(j in 1:numCrit)
      {
        if(criteriaMinMax[j] == 'max')
          dictators <- cbind(dictators,rep(maxEvaluations[j] + 1, numCat - 1))
        else
          dictators <- cbind(dictators,rep(minEvaluations[j] - 1, numCat - 1))
      }
      rownames(dictators) <- c()
      colnames(dictators) <- colnames(performanceTable)
      
      population[[length(population)+1]] <- list(majorityThreshold = majority, criteriaWeights = weights, majorityRule = majorityRule, profilesPerformances = profiles, vetoPerformances = vetoes, dictatorPerformances = dictators)
    }
    return(population)
  }
  
  Fitness <- function(individual)
  {
    ok <- 0
    for (alternative in names(assignments))
    {
      category <- getCategory(performanceTable[alternative,],individual$criteriaWeights, individual$majorityThreshold, individual$profilesPerformances, individual$vetoPerformances, individual$dictatorPerformances, individual$majorityRule, criteriaMinMax)
      if(category == categoriesRanks[assignments[alternative]])
        ok <- ok + 1
    }
    return(ok/length(assignments))
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
      
      profiles1 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      profiles2 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      vetoes1 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      vetoes2 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      dictators1 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      dictators2 <- matrix(rep(0,numCrit*(numCat - 1)),numCat - 1,numCrit)
      
      colnames(profiles1) <- colnames(performanceTable)
      colnames(profiles2) <- colnames(performanceTable)
      colnames(vetoes1) <- colnames(performanceTable)
      colnames(vetoes2) <- colnames(performanceTable)
      colnames(dictators1) <- colnames(performanceTable)
      colnames(dictators2) <- colnames(performanceTable)
      
      for(k in 1:(numCat - 1))
        for(j in 1:numCrit)
        {
          if(j <= pivot)
          {
            profiles1[k,criteria[j]] <- parent1$profilesPerformances[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent2$profilesPerformances[k,criteria[j]]
            vetoes1[k,criteria[j]] <- parent1$vetoPerformances[k,criteria[j]]
            vetoes2[k,criteria[j]] <- parent2$vetoPerformances[k,criteria[j]]
            dictators1[k,criteria[j]] <- parent1$dictatorPerformances[k,criteria[j]]
            dictators2[k,criteria[j]] <- parent2$dictatorPerformances[k,criteria[j]]
          }
          else
          {
            profiles1[k,criteria[j]] <- parent2$profilesPerformances[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent1$profilesPerformances[k,criteria[j]]
            vetoes1[k,criteria[j]] <- parent2$vetoPerformances[k,criteria[j]]
            vetoes2[k,criteria[j]] <- parent1$vetoPerformances[k,criteria[j]]
            dictators1[k,criteria[j]] <- parent2$dictatorPerformances[k,criteria[j]]
            dictators2[k,criteria[j]] <- parent1$dictatorPerformances[k,criteria[j]]
          }
        }
      
      # child identical to first parent - will get mutated in the second step
      
      children[[length(children)+1]] <- list(majorityThreshold = parent1$majorityThreshold, criteriaWeights = parent1$criteriaWeights, majorityRule = parent1$majorityRule, profilesPerformances = parent1$profilesPerformances, vetoPerformances = parent1$vetoPerformances, dictatorPerformances = parent1$dictatorPerformances)
      
      # child identical to second parent
      
      children[[length(children)+1]] <- list(majorityThreshold = parent2$majorityThreshold, criteriaWeights = parent2$criteriaWeights, majorityRule = parent2$majorityRule, profilesPerformances = parent2$profilesPerformances, vetoPerformances = parent2$vetoPerformances, dictatorPerformances = parent2$dictatorPerformances)
      
      # child takes weights and threshold from first parent and profiles from second
      
      children[[length(children)+1]] <- list(majorityThreshold = parent1$majorityThreshold, criteriaWeights = parent1$criteriaWeights, majorityRule = parent1$majorityRule, profilesPerformances = parent2$profilesPerformances, vetoPerformances = parent2$vetoPerformances, dictatorPerformances = parent2$dictatorPerformances)
      
      # child takes weights and threshold from second parent and profiles from first
      
      children[[length(children)+1]] <- list(majorityThreshold = parent2$majorityThreshold, criteriaWeights = parent2$criteriaWeights, majorityRule = parent2$majorityRule, profilesPerformances = parent1$profilesPerformances, vetoPerformances = parent1$vetoPerformances, dictatorPerformances = parent1$dictatorPerformances)
      
      # child takes weights and threshold from first parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(majorityThreshold = parent1$majorityThreshold, criteriaWeights = parent1$criteriaWeights, majorityRule = parent1$majorityRule, profilesPerformances = profiles1, vetoPerformances = vetoes1, dictatorPerformances = dictators1)
      
      # child takes weights and threshold from first parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(majorityThreshold = parent1$majorityThreshold, criteriaWeights = parent1$criteriaWeights, majorityRule = parent1$majorityRule, profilesPerformances = profiles2, vetoPerformances = vetoes2, dictatorPerformances = dictators2)
      
      # child takes weights and threshold from second parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(majorityThreshold = parent2$majorityThreshold, criteriaWeights = parent2$criteriaWeights, majorityRule = parent2$majorityRule, profilesPerformances = profiles1, vetoPerformances = vetoes1, dictatorPerformances = dictators1)
      
      # child takes weights from second parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(majorityThreshold = parent2$majorityThreshold, criteriaWeights = parent2$criteriaWeights, majorityRule = parent2$majorityRule, profilesPerformances = profiles2, vetoPerformances = vetoes2, dictatorPerformances = dictators2)
    }
    
    # mutate children
    
    numChildren <- length(children)
    
    for(i in 1:numChildren)
    {
      if(runif(1,0,1) < mutationProb)
      {
        # mutate majority rule
        
        choicesMatrix <- list(c("V","D"),c("M","v","dV"),c("M","d","Dv"),c("V","Dv","dv"),c("D","dV","dv"),c("V","d","dv"),c("D","v","dv"),c("d","v","Dv","dV"))
        
        names(choicesMatrix) <- c("M","V","D","v","d","dV","Dv","dv")
        
        oldMajorityRule <- children[[i]]$majorityRule
        
        choices <- choicesMatrix[[oldMajorityRule]][choicesMatrix[[oldMajorityRule]] %in% majorityRules]
        
        if(length(choices) != 0)
          children[[i]]$majorityRule <- sample(choices, 1)
      }
      
      if(runif(1,0,1) < mutationProb)
      {
        # mutate majority threshold
        
        children[[i]]$majorityThreshold <- runif(1,0.5,1)
      }
      
      for(j1 in 1:(numCrit-1))
      {
        for(j2 in (j1+1):numCrit)
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate two criteria weights
            
            criteria <- c(colnames(performanceTable)[j1],colnames(performanceTable)[j2])
            
            minVal <- 0 - children[[i]]$criteriaWeights[criteria[1]]
            
            maxVal <- children[[i]]$criteriaWeights[criteria[2]]
            
            tradeoff <- runif(1,minVal,maxVal)
            
            children[[i]]$criteriaWeights[criteria[1]] <- children[[i]]$criteriaWeights[criteria[1]] + tradeoff
            
            children[[i]]$criteriaWeights[criteria[2]] <- children[[i]]$criteriaWeights[criteria[2]] - tradeoff
          }
        }
      }
      
      for(k in 1:(numCat - 1))
      {
        for(criterion in colnames(performanceTable))
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate profile evaluation
            
            maxVal <- maxEvaluations[criterion]
            
            minVal <- minEvaluations[criterion]
            
            if(k < (numCat - 1))
            {
              if(criteriaMinMax[criterion] == 'max')
                minVal <- children[[i]]$profilesPerformances[k+1,criterion]
              else
                maxVal <- children[[i]]$profilesPerformances[k+1,criterion]
            }
            
            if(k > 1)
            {
              if(criteriaMinMax[criterion] == 'max')
                maxVal <- children[[i]]$profilesPerformances[k-1,criterion]
              else
                minVal <- children[[i]]$profilesPerformances[k-1,criterion]
            }
            
            if(criteriaMinMax[criterion] == 'max')
            {
              if(children[[i]]$vetoPerformances[k,criterion] %>=% minVal)
                minVal <- children[[i]]$vetoPerformances[k,criterion] + 0.0000000001
              if(children[[i]]$dictatorPerformances[k,criterion] %<=% maxVal)
                maxVal <- children[[i]]$dictatorPerformances[k,criterion] - 0.0000000001
            }
            else
            {
              if(children[[i]]$vetoPerformances[k,criterion] %<=% maxVal)
                maxVal <- children[[i]]$vetoPerformances[k,criterion] - 0.0000000001
              if(children[[i]]$dictatorPerformances[k,criterion] %>=% minVal)
                minVal <- children[[i]]$dictatorPerformances[k,criterion] + 0.0000000001
            }
            
            children[[i]]$profilesPerformances[k,criterion] <- runif(1,minVal,maxVal)
          }
        }
      }
      
      for(k in 1:(numCat - 1))
      {
        for(criterion in colnames(performanceTable))
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate one veto evaluation
            
            maxVal <- maxEvaluations[criterion]
            
            if(criteriaMinMax[criterion] == 'min')
              maxVal <- maxEvaluations[criterion] + 1
            
            minVal <- minEvaluations[criterion]
            
            if(criteriaMinMax[criterion] == 'max')
              minVal <- minEvaluations[criterion] - 1
            
            if(k < (numCat - 1))
            {
              if(criteriaMinMax[criterion] == 'max')
                minVal <- children[[i]]$vetoPerformances[k+1,criterion]
              else
                maxVal <- children[[i]]$vetoPerformances[k+1,criterion]
            }
            
            if(k > 1)
            {
              if(criteriaMinMax[criterion] == 'max')
                maxVal <- children[[i]]$vetoPerformances[k-1,criterion]
              else
                minVal <- children[[i]]$vetoPerformances[k-1,criterion]
            }
            
            if(criteriaMinMax[criterion] == 'max')
            {
              if(children[[i]]$profilesPerformances[k,criterion] %<=% maxVal)
                maxVal <- children[[i]]$profilesPerformances[k,criterion] - 0.0000000001
            }
            else
            {
              if(children[[i]]$profilesPerformances[k,criterion] %>=% minVal)
                minVal <- children[[i]]$profilesPerformances[k,criterion] + 0.0000000001
            }
            
            children[[i]]$vetoPerformances[k,criterion] <- runif(1,minVal,maxVal)
          }
        }
      }
      
      for(k in 1:(numCat - 1))
      {
        for(criterion in colnames(performanceTable))
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate one dictator evaluation
            
            maxVal <- maxEvaluations[criterion]
            
            if(criteriaMinMax[criterion] == 'max')
              maxVal <- maxEvaluations[criterion] + 1
            
            minVal <- minEvaluations[criterion]
            
            if(criteriaMinMax[criterion] == 'min')
              minVal <- minEvaluations[criterion] - 1
            
            if(k < (numCat - 1))
            {
              if(criteriaMinMax[criterion] == 'max')
                minVal <- children[[i]]$dictatorPerformances[k+1,criterion]
              else
                maxVal <- children[[i]]$dictatorPerformances[k+1,criterion]
            }
            
            if(k > 1)
            {
              if(criteriaMinMax[criterion] == 'max')
                maxVal <- children[[i]]$dictatorPerformances[k-1,criterion]
              else
                minVal <- children[[i]]$dictatorPerformances[k-1,criterion]
            }
            
            if(criteriaMinMax[criterion] == 'max')
            {
              if(children[[i]]$profilesPerformances[k,criterion] %>=% minVal)
                minVal <- children[[i]]$profilesPerformances[k,criterion] + 0.0000000001
            }
            else
            {
              if(children[[i]]$profilesPerformances[k,criterion] %<=% maxVal)
                maxVal <- children[[i]]$profilesPerformances[k,criterion] - 0.0000000001
            }
            
            children[[i]]$dictatorPerformances[k,criterion] <- runif(1,minVal,maxVal)
          }
        }
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
  
  while(as.double(difftime(Sys.time(), startTime, units = 'secs')) < timeLimit)
  {
    # Evaluate population
    
    evaluations <- unlist(lapply(population, Fitness))
    
    # Store best individual if better than the overall best
    
    maxFitness <- max(evaluations)
    
    if(maxFitness >= bestIndividual$fitness)
    {
      bestIndividual <- population[[match(maxFitness,evaluations)]]
      
      bestIndividual$fitness <- maxFitness
    }
    
    # report
    
    if(as.double(difftime(Sys.time(), startTime, units = 'secs')) / 5 > ct)
    {
      ct <- ct + 1
      
      # print(sprintf("Best fitness so far: %6.2f%%", bestIndividual$fitness * 100))
    }
    
    # check if we are done
    
    if(bestIndividual$fitness == 1)
      break
    
    # Selection - not the first iteration
    
    if(length(population) > populationSize)
    {
      evaluations <- evaluations^2
      
      newPopulation <- list()
      
      newPopulation[[length(newPopulation)+1]] <- bestIndividual
      
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
  
  # print(sprintf("Final model fitness: %6.2f%%", bestIndividual$fitness * 100))
  
  # add dummy profiles
  
  bestIndividual$profilesPerformances <- rbind(bestIndividual$profilesPerformances,rep(NA,numCrit))
  
  bestIndividual$vetoPerformances <- rbind(bestIndividual$vetoPerformances,rep(NA,numCrit))
  
  bestIndividual$dictatorPerformances <- rbind(bestIndividual$dictatorPerformances,rep(NA,numCrit))
  
  rownames(bestIndividual$profilesPerformances) <- names(sort(categoriesRanks))
  
  rownames(bestIndividual$vetoPerformances) <- rownames(bestIndividual$profilesPerformances)
  
  rownames(bestIndividual$dictatorPerformances) <- rownames(bestIndividual$profilesPerformances)
  
  if(bestIndividual$majorityRule %in% c("V","v","d","dV","Dv","dv"))
  {
    # determine which vetoes are actually used and remove those that are simply an artefact of the linear program
    
    used <- LPDMRSortIdentifyUsedVetoProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, bestIndividual$majorityThreshold, bestIndividual$criteriaWeights, bestIndividual$profilesPerformances, bestIndividual$vetoPerformances, bestIndividual$dictatorPerformances, bestIndividual$majorityRule, alternativesIDs, criteriaIDs)

    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks)[categoriesRanks == k]
      for (j in 1:numCrit)
      {
        if (!used[cat,j])
          bestIndividual$vetoPerformances[cat,j] <- NA
      }
    }
  }
  
  if(bestIndividual$majorityRule %in% c("D","v","d","dV","Dv","dv"))
  {
    # determine which dictators are actually used and remove those that are simply an artefact of the linear program
    
    used <- LPDMRSortIdentifyUsedDictatorProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, bestIndividual$majorityThreshold, bestIndividual$criteriaWeights, bestIndividual$profilesPerformances, bestIndividual$dictatorPerformances, bestIndividual$vetoPerformances, bestIndividual$majorityRule, alternativesIDs, criteriaIDs)
    
    for (k in (numCat-1):1)
    {
      cat <- names(categoriesRanks)[categoriesRanks == k]
      for (j in 1:numCrit)
      {
        if (!used[cat,j])
          bestIndividual$dictatorPerformances[cat,j] <- NA
      }
    }
  }
  
  return(bestIndividual)
}
