plotMRSortSortingProblem <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, assignments, criteriaMinMax, criteriaUBs, criteriaLBs, categoriesDictators = NULL, categoriesVetoes = NULL, majorityRule = NULL, criteriaWeights = NULL, majorityThreshold = NULL, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(categoriesLowerProfiles)))
    stop("categoriesLowerProfiles should be a matrix")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaLBs)))
    stop("criteriaLBs should be a vector")
  
  if (!(is.vector(criteriaUBs)))
    stop("criteriaUBs should be a vector")

  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")

  if (!(is.null(categoriesDictators) || is.matrix(categoriesDictators)))
    stop("categoriesDictators should be a matrix")
  
  if (!(is.null(categoriesVetoes) || is.matrix(categoriesVetoes)))
    stop("categoriesVetoes should be a matrix")
  
  if (!(is.null(criteriaWeights) || is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(majorityThreshold) || is.numeric(majorityThreshold)))
    stop("majorityThreshold should be a number")
  
  if (!(is.null(majorityRule) || (majorityRule %in% c("V","D","v","d","dV","Dv","dv"))))
    stop("majorityRule should be: 'V' , 'D', 'v', 'd', 'dV', 'Dv', 'dv'")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  if (!is.null(alternativesIDs)){
    if(any(alternativesIDs %in% rownames(performanceTable)))
    {
      performanceTable <- performanceTable[alternativesIDs,]
      assignments <- assignments[alternativesIDs]
    }
    else
    {
      performanceTable <- NULL
      assignments <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    categoriesLowerProfiles <- categoriesLowerProfiles[,criteriaIDs]
    if (!is.null(criteriaWeights))
      criteriaWeights <- criteriaWeights[,criteriaIDs]
    if (!is.null(categoriesDictators))
      categoriesDictators <- categoriesDictators[,criteriaIDs]
    if (!is.null(categoriesVetoes))
      categoriesVetoes <- categoriesVetoes[,criteriaIDs]
    criteriaUBs <- criteriaUBs[criteriaIDs]
    criteriaLBs <- criteriaLBs[criteriaIDs]
  }
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  #if (is.null(dim(performanceTable))) 
  #  stop("less than 2 criteria or 2 alternatives")
  
  # get model assignments
  
  model.assignments <- assignments

  if (!is.null(criteriaWeights) && !is.null(majorityThreshold) && !is.null(performanceTable))
  {
    if (is.null(categoriesDictators) && is.null(categoriesVetoes))
      model.assignments <- MRSort(performanceTable, categoriesLowerProfiles, categoriesRanks,
                                  criteriaWeights, criteriaMinMax, majorityThreshold)
    else
    {
      if (is.null(majorityRule))
      {
        if (is.null(categoriesDictators) && !is.null(categoriesVetoes))
          majorityRule <- "V"
        else if (!is.null(categoriesDictators) && is.null(categoriesVetoes))
          majorityRule <- "D"
        else
          stop("majorityRule should be: 'V' , 'D', 'v', 'd', 'dV', 'Dv', 'dv'")
      }
      model.assignments <- LPDMRSort(performanceTable, categoriesLowerProfiles,
                                     categoriesRanks, criteriaWeights, criteriaMinMax,
                                     majorityThreshold,
                                     criteriaVetos=categoriesVetoes, 
                                     criteriaDictators=categoriesDictators, 
                                     majorityRule = majorityRule)
    }
  }
  
  # -------------------------------------------------------
  
  numCrit <- dim(categoriesLowerProfiles)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  if(is.null(numAlt))
    numAlt <- 0
  
  numCat <- dim(categoriesLowerProfiles)[1]
  
  # -------------------------------------------------------
  if(!is.null(performanceTable))
  {
    normalizedPerformanceTable <- matrix(nrow=numAlt,ncol=numCrit)
    
    for (j in 1:numAlt){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          normalizedPerformanceTable[j,i] <- 1-(performanceTable[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
        else
          normalizedPerformanceTable[j,i] <- (performanceTable[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
      }
    }
  }

  if (!is.null(categoriesLowerProfiles))
  {
    normalizedProfiles <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          normalizedProfiles[j,i] <- 1-(categoriesLowerProfiles[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
        else
          normalizedProfiles[j,i] <- (categoriesLowerProfiles[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
      }
    }
  }
  
  if (!is.null(categoriesDictators))
  {
    normalizedDictators <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          if(!is.null(categoriesDictators[j,i]) & !is.na(categoriesDictators[j,i]))
            normalizedDictators[j,i] <- 1-(categoriesDictators[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedDictators[j,i] <- -0.5
        else
          if(!is.null(categoriesDictators[j,i]) & !is.na(categoriesDictators[j,i]))
            normalizedDictators[j,i] <- (categoriesDictators[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedDictators[j,i] <- 1.5
        if (normalizedDictators[j,i] > 1)
          normalizedDictators[j,i] <- 1.5
        else if (normalizedDictators[j,i] < 0)
          normalizedDictators[j,i] <- -0.5
      }
    }
  }

  if (!is.null(categoriesVetoes))
  {
    normalizedVetoes <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat)
    {
      for (i in 1:numCrit)
      {
        if(criteriaMinMax[i] == "min")
          if(!is.null(categoriesVetoes[j,i]) & !is.na(categoriesVetoes[j,i]))
            normalizedVetoes[j,i] <- 1-(categoriesVetoes[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedVetoes[j,i] <- 1.2
        else
          if(!is.null(categoriesVetoes[j,i]) & !is.na(categoriesVetoes[j,i]))
            normalizedVetoes[j,i] <- (categoriesVetoes[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedVetoes[j,i] <- -0.2
        if (normalizedVetoes[j,i] > 1)
          normalizedVetoes[j,i] <- 1.2
        else if (normalizedVetoes[j,i] < 0)
          normalizedVetoes[j,i] <- -0.2
      }
    }
  }
  # color palette when number of categories outside ColorBrewer range
  col.cat <- rainbow(20)
  col.alt.lines <- rainbow(20)
  col.alt.markers <- rainbow(20)
  
  if (numCat >= 3 && numCat <= 11)
    col.cat <- brewer.pal(numCat,"Dark2")
  
  names(col.cat) <- rownames(categoriesLowerProfiles)
  
  palette(col.cat)
  
  # color palette for alternatives lines - the color of the category to which the model assigned them
  if(!is.null(model.assignments))
    col.alt.lines <- col.cat[model.assignments]

  # color palette for alternatives markers - the color of the category to which they should have been assigned
  if(!is.null(assignments))
    col.alt.markers <- col.cat[assignments]
  
  # name profiles as delimiting categories
  profiles.names <- paste(sapply(1:(numCat-1), function(x) names(categoriesRanks)[categoriesRanks == x]),sapply(2:numCat, function(x) names(categoriesRanks)[categoriesRanks == x]),sep = "-")
  
  ylim=c(-0.1, 1.1)
  
  layout(matrix(c(1:(numCat-1),rep(numCat,numCat-1)),2,numCat-1, byrow = TRUE), widths = rep(1,numCat-1), heights = c(7,1))
  
  par(mar=c(2, 2, 6, 2))
  
  # one plot for each pair of consecutive categories
  
  for(i in rev(1:(numCat-1)))
  {
    plot(1:numCrit, normalizedProfiles[i,], type="l", col="black", ylim=ylim, xlab = "weights", ylab="", xaxt="n", yaxt="n", lwd=2)
    
    # title of the two categories
    title(profiles.names[i])
    
    # criteria axes
    for (j in 1:numCrit){
      lines(c(j,j),ylim, col="gray")
    }
    
    # dictator region
    if (!is.null(categoriesDictators))
      polygon(c(0,0,1:numCrit,numCrit+1,numCrit+1),c(1.2,normalizedDictators[i,1+0.2],normalizedDictators[i,],normalizedDictators[i,numCrit]+0.2,1.2), col = "snow3", border = NA)
    
    # veto region
    if (!is.null(categoriesVetoes))
      polygon(c(0,0,1:numCrit,numCrit+1,numCrit+1),c(-0.2,normalizedVetoes[i,1]-0.2,normalizedVetoes[i,],normalizedVetoes[i,numCrit]-0.2,-0.2), col = "black", border = NA)
    
    # criteria names at the top
    axis(3,at=c(1:numCrit),labels=colnames(performanceTable))
    
    # criteria weights at the bottom
    if (!is.null(criteriaWeights))
    {
      axis(1,at=c(1:numCrit),labels=sapply(criteriaWeights,function(x) round(x,digits = 4)))
    }
    
    points(1:numCrit,normalizedProfiles[i,],type="p",col="black", pch=19, cex=1.5)
    
    # profiles values
    if (!is.null(categoriesLowerProfiles))
      text(c(1:numCrit), normalizedProfiles[i,], labels = sapply(categoriesLowerProfiles[i,],function(x) round(x,digits = 4)) , pos=3, offset = 1)
    
    if (!is.null(categoriesDictators))
      text(c(1:numCrit), normalizedDictators[i,], labels = sapply(categoriesDictators[i,],function(x) round(x,digits = 4)), pos=3, offset =1.1, font = 2)
    
    if (!is.null(categoriesVetoes))
      text(c(1:numCrit), normalizedVetoes[i,], labels = sapply(categoriesVetoes[i,],function(x) round(x,digits = 4)), col = "white", pos = 1, offset = 1.1, font = 2)
    
    # alternatives
    if (numAlt > 0)
    {
      for (j in (1:numAlt))
        points(1:numCrit,normalizedPerformanceTable[j,],type="c",pch=26, col=col.alt.lines[j], lwd=2)
      
      for (j in (1:numAlt))
        points(1:numCrit,normalizedPerformanceTable[j,],type="p",pch=c(0,2,5,3,4)[(j-1)%%5 + 1], col=col.alt.markers[j], lwd=2, cex = 2)
    }
    
  }
  
  par(mar=c(1, 3, 1, 3))
  
  plot.new()
  
  if (!is.null(majorityThreshold))
  {
    if(!is.null(performanceTable))
    {
      legend("center", c(paste("majorityThreshold =",majorityThreshold,'  '),rownames(performanceTable)), cex=1.0, col=c('black',col.alt.lines), 
         lwd=2, bty="n",pch=c(NA,rep(NA,numAlt)), lty = c(0,rep(2,numAlt)), ncol = 4)
      legend("center", c(paste("majorityThreshold =",majorityThreshold,'  '),rownames(performanceTable)), cex=1.0, col=c('black',col.alt.markers), 
             lwd=2, bty="n",pch=c(NA,rep(c(0,2,5,3,4),floor(numAlt/5) + 1)[1:numAlt]), lty = c(0,rep(0,numAlt)), ncol = 4)
    }
    else
      legend("center", c(paste("majorityThreshold =",majorityThreshold,'  ')), cex=1.2, col=c('black'), 
             lwd=2, bty="n",pch=c('.'), lty = c(0), horiz = TRUE)
  }
  else
    legend("center", c(rownames(performanceTable),rownames(categoriesLowerProfiles)), cex=0.8, col=c(col.alt.lines,col.cat), 
           lwd=2, bty="n",pch=c(mrk.cat[assignments],rep('.',numCat)), lty = c(rep(1,numAlt),rep(1,numCat)), horiz = TRUE)
}
