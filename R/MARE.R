MARE <- function(performanceTableMin, performanceTable, performanceTableMax, criteriaWeights, criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL){
	
	## check the input data is correct
	if (sum(criteriaWeights) != 1) 
        stop("criteria weights must add to 1")
	if (!((is.matrix(performanceTableMin) || (is.data.frame(performanceTableMin))))) 
        stop("performanceTableMin must be a matrix or a data frame")	
	if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("performanceTable must be a matrix or a data frame")
	if (!((is.matrix(performanceTableMax) || (is.data.frame(performanceTableMax))))) 
        stop("performanceTableMax must be a matrix or a data frame")
	if (!(length(criteriaWeights) == nrow(performanceTableMin) && length(criteriaWeights) == nrow(performanceTable) && length(criteriaWeights) == nrow(performanceTableMax))) 
		stop("the number of criteria weights must equal the number of rows in the scores matrices")
	if (missing(criteriaMinMax))
		stop("the input criteriaMinMax is required.")
	for (i in 1:ncol(performanceTable))
		{
		for (j in 1:nrow(performanceTable))
			{
				if (performanceTable[j,i] >  performanceTableMax[j,i] || performanceTableMin[j,i] >  performanceTable[j,i])
				{
					stop("performanceTableMax > performanceTable > performanceTableMin is not true.")
				}
			}
		}

	## filter the performance table and the criteria according to the given alternatives and criteria
	
	if (!is.null(alternativesIDs)) 	{
							performanceTableMin <- performanceTableMin[,alternativesIDs]
							performanceTable <- performanceTable[,alternativesIDs]
							performanceTableMax <- performanceTableMax[,alternativesIDs]
							}
	if (!is.null(criteriaIDs)) 		{
							performanceTableMin <- performanceTableMin[criteriaIDs,]
							performanceTable <- performanceTable[criteriaIDs,]
							performanceTableMax <- performanceTableMax[criteriaIDs,]
							criteriaWeights <- criteriaWeights[criteriaIDs]
							criteriaMinMax <- criteriaMinMax[criteriaIDs]
							}

	critno <- length(criteriaWeights)
	altno <- ncol(performanceTable)

	## Inverse minimising criterion scores
	for (i in 1:critno)
		{
			if (!(criteriaMinMax[i] == "max"))
			{
				formax <- performanceTableMin[i,]^-1
				performanceTable[i,] <- performanceTable[i,]^-1
				formin <- performanceTableMax[i,]^-1
				performanceTableMin[i,] <- formin
				performanceTableMax[i,] <- formax
			}
		}

	## Normalise matrices
	maxv <- c(1:critno)
	for (i in 1:critno)
		{
			maxv[i] <- max(c(max(performanceTableMin[i,]),max(performanceTable[i,]),max(performanceTableMax[i,])))
		}
	for (i in 1:critno)
		{
			performanceTableMin[i,] <- performanceTableMin[i,] / maxv[i]
			performanceTable[i,] <- performanceTable[i,] / maxv[i]
			performanceTableMax[i,] <- performanceTableMax[i,] / maxv[i]
		}

	## Calculate the results
	results <- matrix(nrow=3,ncol=altno)
	colnames(results) <- colnames(performanceTable)
	row.names(results) <- c("Minimum", "Most Likely", "Maximum")
	for (i in 1:altno)
		{
			resultmin <- 0
			result <- 0
			resultmax <- 0
			for (j in 1:critno)
			{
				resultmin <- resultmin + (performanceTableMin[j,i] * criteriaWeights[j])
				result <- result + (performanceTable[j,i] * criteriaWeights[j])
				resultmax <- resultmax + (performanceTableMax[j,i] * criteriaWeights[j])
			}
			results[1,i] <- resultmin
			results[2,i] <- result
			results[3,i] <- resultmax
		}	

	return(results)

	rm(results)
	rm(performanceTableMin)
	rm(performanceTable)
	rm(performanceTableMax)
	rm(criteriaWeights)
}

plotMARE <- function(x){

	## check the input data is correct
	if (!((is.matrix(x) || (is.data.frame(x))))) 
        stop("the results must be a matrix or a data frame")

	## plot the most likely values
	plot(1:ncol(x), x[2,], las=1, xaxt = "n", pch=19, main="MARE Results", xlab="Alternatives", ylab="Scores", ylim=c(min(x), max(x)))
	axis(1, at = 1:ncol(x), labels = colnames(x))

	## plot the minimum and maximum ranges
	arrows(1:ncol(x), x[1,], 1:ncol(x), x[3,], code=3, angle=90, length=0.1)
}
