#############################################################################
#
# Copyright Institut Télécom-Télécom Bretagne, 2013
#
# Contributors:
#   Patrick Meyer <patrick.meyer@telecom-bretagne.eu>
#   Sebastien Bigaret <sebastien.bigaret@telecom-bretagne.eu>
#		
# This software, MCDA, is a package for the R statistical software which 
# allows to use MCDA algorithms and methods. 
# 
# This software is governed by the CeCILL license (v2) under French law
# and abiding by the rules of distribution of free software. You can
# use, modify and/ or redistribute the software under the terms of the
# CeCILL license as circulated by CEA, CNRS and INRIA at the following
# URL "http://www.cecill.info".
# 
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#		
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#		
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
##############################################################################

UTA <- function(performanceTable, alternativesRanks, criteriaMinMax, criteriaNumberOfBreakPoints, epsilon, criteriaLBs=NULL, criteriaUBs=NULL, alternativesIDs = NULL, criteriaIDs = NULL){
	
	## check the input data
  
	if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        	stop("wrong performanceTable, should be a matrix or a data frame")
	
	if (!(is.vector(alternativesRanks)))
        	stop("alternativesRanks should be a vector")
  
	if (!(is.vector(criteriaMinMax)))
	  stop("criteriaMinMax should be a vector")
  
	if (!(is.vector(criteriaNumberOfBreakPoints)))
	  stop("criteriaNumberOfBreakPoints should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
        	stop("alternativesIDs should be in a vector")
        	
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
        	stop("criteriaIDs should be in a vector")
  
	if (!(is.null(criteriaLBs) || is.vector(criteriaLBs)))
	  stop("criteriaLBs should be in a vector")
  
	if (!(is.null(criteriaUBs) || is.vector(criteriaUBs)))
	  stop("criteriaUBs should be in a vector")
	
	## filter the data according to the given alternatives and criteria
	
	if (!is.null(alternativesIDs)){
	  performanceTable <- performanceTable[alternativesIDs,]
	  alternativesRanks <- alternativesRanks[alternativesIDs]
	} 
	
	if (!is.null(criteriaIDs)){
	  criteriaMinMax <- criteriaMinMax[criteriaIDs]
	  performanceTable <- performanceTable[,criteriaIDs]
	  criteriaNumberOfBreakPoints <- criteriaNumberOfBreakPoints[criteriaIDs]
	}
  
  if (!is.null(criteriaIDs) && !is.null(criteriaUBs)){
    criteriaUBs <- criteriaUBs[criteriaIDs]
  }
  
	if (!is.null(criteriaIDs) && !is.null(criteriaLBs)){
	  criteriaLBs <- criteriaLBs[criteriaIDs]
	}
  
  # data is filtered, check for some data consistency
  
  # are the upper and lower bounds given in the function compatible with the data in the performance table ?
	if (!(is.null(criteriaUBs))){
	  if (!all(apply(performanceTable,2,max)<=criteriaUBs))
      stop("performanceTable contains higher values than criteriaUBs")
	}
  
	if (!(is.null(criteriaLBs))){
	  if (!all(apply(performanceTable,2,min)>=criteriaLBs))
	    stop("performanceTable contains lower values than criteriaLBs")
	}
  
 if (!all(criteriaNumberOfBreakPoints >= 2))
   stop("in criteriaNumberOfBreakPoints there should at least be 2 breakpoints for each criterion")
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
      stop("less than 2 criteria or 2 alternatives")
        
	# -------------------------------------------------------

	numCrit <- dim(performanceTable)[2]

  numAlt <- dim(performanceTable)[1]
  
  # -------------------------------------------------------
	
	criteriaBreakPoints <- list()
	
	for (i in 1:numCrit){
	  
    tmp<-c()
	  
    if (!is.null(criteriaLBs))
      mini <- criteriaLBs[i]
    else{
	    mini <- min(performanceTable[,i])
    }
	  
    if (!is.null(criteriaLBs))
      maxi <- criteriaUBs[i]
	  else{
      maxi <- max(performanceTable[,i])
	  }
    
    if (mini == maxi){
      # then there is only one value for that criterion, and the algorithm to build the linear interpolation
      # will not work correctly
      stop(paste("there is only one possible value left for criterion "),colnames(performanceTable)[i])
    }
    
	  alphai <- criteriaNumberOfBreakPoints[i]
	  
    for (j in 1:alphai)
	    tmp<-c(tmp,mini + (j-1)/(alphai-1) * (maxi - mini))
	  
	  # if the criterion has to be maximized, the worst value is in the first position
	  # else, we sort the vector the other way around to have the worst value in the first position
    
	  if (criteriaMinMax[i] == "min")
	    tmp<-sort(tmp,decreasing=TRUE)
	  criteriaBreakPoints <- c(criteriaBreakPoints,list(tmp))
	}
	
	names(criteriaBreakPoints) <- colnames(performanceTable)
  
	# -------------------------------------------------------
	
	# a is a matrix decomposing the alternatives in the break point space and adding the sigma columns
	
	a<-matrix(0,nrow=numAlt, ncol=(sum(criteriaNumberOfBreakPoints)+numAlt))
	
	for (n in 1:numAlt){
	  for (m in 1:numCrit){
	    if (length(which(performanceTable[n,m]==criteriaBreakPoints[[m]]))!=0){
	      # then we have a performance value which is on a breakpoint
	      j<-which(performanceTable[n,m]==criteriaBreakPoints[[m]])
	      if (m==1)
	        pos <- j
	      else
	        pos<-sum(criteriaNumberOfBreakPoints[1:(m-1)])+j
	      a[n,pos] <- 1
	    }
	    else{
	      # then we have value which needs to be approximated by a linear interpolation
	      # let us first search the lower and upper bounds of the interval of breakpoints around the value
	      if (criteriaMinMax[m] == "min"){
	        j<-which(performanceTable[n,m]>criteriaBreakPoints[[m]])[1]-1
	      }
	      else{
	        j<-which(performanceTable[n,m]<criteriaBreakPoints[[m]])[1]-1			
	      }
	      if (m==1)
	        pos <- j
	      else
	        pos<-sum(criteriaNumberOfBreakPoints[1:(m-1)])+j
	      a[n,pos] <- 1-(performanceTable[n,m]-criteriaBreakPoints[[m]][j])/(criteriaBreakPoints[[m]][j+1] - criteriaBreakPoints[[m]][j])
	      a[n,pos+1] <- (performanceTable[n,m]-criteriaBreakPoints[[m]][j])/(criteriaBreakPoints[[m]][j+1] - criteriaBreakPoints[[m]][j])
	    }
	    # and now for sigma
	    a[n,dim(a)[2]-numAlt+n] <- 1
	  }
	}
  
	# -------------------------------------------------------
	
	# the objective function : the first elements correspond to the ui's, the last one to the sigmas
	
	obj<-rep(0,sum(criteriaNumberOfBreakPoints))
	
	obj<-c(obj,rep(1,numAlt))
	
	# -------------------------------------------------------
	
	# we now build the part of the constraints matrix concerning the order given by the decision maker
	
	preferenceConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
	indifferenceConstraints <-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
  
  # determine now in which order the alternatives should be treated for the constraints
  indexOrder <- c()
  orderedAlternativesRanks <- sort(alternativesRanks)
  tmpRanks1 <- alternativesRanks
  tmpRanks2 <- alternativesRanks
  
  while (length(orderedAlternativesRanks) != 0){
    # search for the alternatives of lowest rank
    tmpIndex <- which(alternativesRanks == orderedAlternativesRanks[1])
    for (j in 1:length(tmpIndex))
      indexOrder<-c(indexOrder,tmpIndex[j])
    # remove the rank which has been dealt with now
    orderedAlternativesRanks<-orderedAlternativesRanks[-which(orderedAlternativesRanks==orderedAlternativesRanks[1])]
  }
	
	for (i in 1:(length(alternativesRanks)-1)){
	  if (alternativesRanks[indexOrder[i]] == alternativesRanks[indexOrder[i+1]]){
	    # then the alternatives are indifferent and their overall values are equal
	    indifferenceConstraints <- rbind(indifferenceConstraints, a[indexOrder[i],] - a[indexOrder[i+1],])
	  }
	  else{
	    # then the first alternative i is ranked better than the second one i+1 and i has an overall value higher than i+1
	    preferenceConstraints <- rbind(preferenceConstraints, a[indexOrder[i],] - a[indexOrder[i+1],])
	  } 
	}
	
	# add this to the constraints matrix mat
	
	mat<-rbind(preferenceConstraints,indifferenceConstraints)
	
	# right hand side of this part of mat
	
	rhs <- c()

	if (dim(preferenceConstraints)[1]!=0){
	  for (i in (1:dim(preferenceConstraints)[1]))
	    rhs<-c(rhs,epsilon)
	}
  
	if (dim(indifferenceConstraints)[1]!=0){
	  for (i in (1:dim(indifferenceConstraints)[1]))
	    rhs<-c(rhs,0)
	}
	# direction of the inequality for this part of mat
	
	dir <- c()
  
	if (dim(preferenceConstraints)[1]!=0){
	  for (i in (1:dim(preferenceConstraints)[1]))
	    dir<-c(dir,">=")
	}
  
  if (dim(indifferenceConstraints)[1]!=0){
	  for (i in (1:dim(indifferenceConstraints)[1]))
      dir<-c(dir,"==")
  }
	  
	
	# -------------------------------------------------------
	
	# now the monotonicity constraints on the value functions
	
	monotonicityConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
	
	for (i in 1:length(criteriaNumberOfBreakPoints)){
	  for (j in 1:(criteriaNumberOfBreakPoints[i]-1)){
	    tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
	    if (i==1)
	      pos <- j
	    else
	      pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+j
	    tmp[pos] <- -1
	    tmp[pos+1] <- 1
	    monotonicityConstraints <- rbind(monotonicityConstraints, tmp)
	  }
	}
	
	# add this to the constraints matrix mat
	
	mat<-rbind(mat,monotonicityConstraints)
	
	# the direction of the inequality
	
	for (i in (1:dim(monotonicityConstraints)[1]))
	  dir<-c(dir,">=")
	
	# the right hand side of this part of mat
	
	for (i in (1:dim(monotonicityConstraints)[1]))
	  rhs<-c(rhs,0)
	
	# -------------------------------------------------------
	
	# normalization constraint for the upper values of the value functions (sum = 1)
	
	tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
	
	for (i in 1:length(criteriaNumberOfBreakPoints)){
	  if (i==1)
	    pos <- criteriaNumberOfBreakPoints[i]
	  else
	    pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+criteriaNumberOfBreakPoints[i]
	  tmp[pos] <- 1
	}
	
	# add this to the constraints matrix mat
	
	mat<-rbind(mat,tmp)
	
	# the direction of the inequality
	
	dir<-c(dir,"==")
	
	# the right hand side of this part of mat
	
	rhs<-c(rhs,1)
	
	# -------------------------------------------------------
	
	# now the normalizaiton constraints for the lower values of the value functions (= 0)
	
	minValueFunctionsConstraints<-matrix(nrow=0, ncol=sum(criteriaNumberOfBreakPoints)+numAlt)
	
	for (i in 1:length(criteriaNumberOfBreakPoints)){
	  tmp<-rep(0,sum(criteriaNumberOfBreakPoints)+numAlt)
	  if (i==1)
	    pos <- i
	  else
	    pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])+1
	  tmp[pos] <- 1
	  minValueFunctionsConstraints <- rbind(minValueFunctionsConstraints,tmp)
	}
	
	# add this to the constraints matrix mat
	
	mat<-rbind(mat,minValueFunctionsConstraints)
	
	# the direction of the inequality
	
	for (i in (1:dim(minValueFunctionsConstraints)[1]))
	  dir<-c(dir,"==")
	
	# the right hand side of this part of mat
	
	for (i in (1:dim(minValueFunctionsConstraints)[1]))
	  rhs<-c(rhs,0)
	
	# -------------------------------------------------------
  
	lpSolution <- Rglpk_solve_LP(obj, mat, dir, rhs)
	
	overallValues <- a%*%lpSolution$solution
	
	rownames(overallValues) <- rownames(performanceTable)
	
	colnames(overallValues) <- c("overall values")
	
	# -------------------------------------------------------
	
	# create a structure containing the value functions
	
	valueFunctions <- list()
	
	for (i in 1:length(criteriaNumberOfBreakPoints)){
	  tmp <- c() 
	  if (i==1)
	    pos <- 0
	  else
	    pos<-sum(criteriaNumberOfBreakPoints[1:(i-1)])
	  for (j in 1:criteriaNumberOfBreakPoints[i]){
	    tmp <- c(tmp,lpSolution$solution[pos+j])
	  }
	  tmp<-rbind(criteriaBreakPoints[[i]],tmp)
    colnames(tmp)<- NULL
	  rownames(tmp) <- c("x","y")
	  valueFunctions <- c(valueFunctions,list(tmp))
	}
	
	names(valueFunctions) <- colnames(performanceTable)
	
	# -------------------------------------------------------
  
  overallValues <- as.vector(t(a[,1:sum(criteriaNumberOfBreakPoints)]%*%lpSolution$solution[1:sum(criteriaNumberOfBreakPoints)]))
  
  names(overallValues) <- rownames(performanceTable)
  
  # -------------------------------------------------------

  # the error values for each alternative (sigma)
  
  errorValues <- as.vector(lpSolution$solution[(sum(criteriaNumberOfBreakPoints)+1):length(lpSolution$solution)])
  
  names(errorValues) <- rownames(performanceTable)
  
	# -------------------------------------------------------
	
  # the ranks of the alternatives 
  
	outRanks <- rank(-overallValues, ties.method="min")
  
	# -------------------------------------------------------
	
  
  # prepare the output
  
  out <- list(optimum = lpSolution$optimum, valueFunctions = valueFunctions, overallValues = overallValues, ranks = outRanks, errors = errorValues, Kendall = Kendall(alternativesRanks,outRanks))
  
#   print(a)
#   print(criteriaBreakPoints)
#   print(mat)
#   print(dir)
#   print(rhs)
  
	return(out)
}
