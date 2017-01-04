plotPiecewiseLinearValueFunctions <- function(valueFunctions, criteriaIDs = NULL){
	
	## check the input data
  
	if (!(is.list(valueFunctions)))
        	stop("valueFunctions should be a list")
  
	if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
	  stop("criteriaIDs should be a vector")
  
	## filter the data according to the given criteria
	
	if (!is.null(criteriaIDs)){
	  valueFunctions <- valueFunctions[criteriaIDs]
	}
  
	if (is.null(valueFunctions[[1]]))
	  stop("no value functions left to plot")
  else 
    numCrit <- length(valueFunctions)
  
	# plotting symbol and color 
  
	par(pch=22, col="black")
  
	# determine how many plots per row and column
  
  if (numCrit <= 2)
    par(mfrow=c(1,2)) 
	else
	  par(mfrow=c(ceiling(log2(numCrit)),ceiling(log2(numCrit)))) 
  
  # plot the functions
    
	for(i in 1:numCrit){
	  heading = names(valueFunctions)[i]
	  plot(valueFunctions[[i]]["x",], valueFunctions[[i]]["y",], type="n", main=heading,xlab="", ylab="") 
	  lines(valueFunctions[[i]]["x",], valueFunctions[[i]]["y",], type="b") 
	}
  
}
