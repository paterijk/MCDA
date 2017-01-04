TOPSIS <- function(performanceTable, criteriaWeights, criteriaMinMax, positiveIdealSolutions = NULL, negativeIdealSolutions = NULL, alternativesIDs = NULL, criteriaIDs = NULL){
	
	## check the input data

        if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
        	stop("alternatives IDs should be in a vector")
        	
        if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
        	stop("criteria IDs should be in a vector")

	if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("performanceTable must be a matrix or a data frame")

	if (!(length(criteriaWeights) == ncol(performanceTable))) 
		stop("the number of criteriaWeights must equal the number of columns in the performanceTable")

	if (missing(criteriaMinMax))
		stop("the input criteriaMinMax is required.")

	## filter the performance table and the criteria according to the given alternatives and criteria
	
	if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
	
	if (!is.null(criteriaIDs)) 	{
							performanceTable <- performanceTable[,criteriaIDs]
							criteriaWeights <- criteriaWeights[criteriaIDs]
							if (!missing(positiveIdealSolutions)) positiveIdealSolutions <- positiveIdealSolutions[criteriaIDs]
							if (!missing(negativeIdealSolutions)) negativeIdealSolutions <- negativeIdealSolutions[criteriaIDs]
							criteriaMinMax <- criteriaMinMax[criteriaIDs]
						}
	
	critno <- length(criteriaWeights)
	altno <- nrow(performanceTable)

	## Calculate the weighted normalised matrix
	divby <- c(1:critno)
	for (i in 1:critno)
		{
			divby[i] <- sqrt(sum(performanceTable[,i]^2))
		}
	normalisedm <- t(t(performanceTable) / divby)
	wnm <- t(t(normalisedm) * criteriaWeights)

	## Identify positive and negative ideal solutions
	pis <- c(1:critno)
	nis <- c(1:critno)
	if (missing(positiveIdealSolutions) || missing(negativeIdealSolutions))
		{
		for (i in 1:critno)
			{
				if (criteriaMinMax[i] == "max")
				{
					pis[i] <- max(wnm[,i])
					nis[i] <- min(wnm[,i])
				}
				else
				{
					pis[i] <- min(wnm[,i])
					nis[i] <- max(wnm[,i])
				}
			}
		}
	else
		{
		## check the input data is correct
		if (!(length(positiveIdealSolutions) == length(negativeIdealSolutions) || length(positiveIdealSolutions) == critno)) 
		stop("the number of postive and negaitve ideal solutions need to equal the number of alternaitves.")
		pis <- positiveIdealSolutions
		nis <- negativeIdealSolutions
		}

	## Identify separation from positive and negative ideal solutions
	spis <- sweep(wnm,2,pis)^2
	snis <- sweep(wnm,2,nis)^2	
	spisv <- c(1:altno)
	snisv <- c(1:altno)

	for (i in 1:altno)
			{
				spisv[i] <- sqrt(sum(spis[i,]))
				snisv[i] <- sqrt(sum(snis[i,]))
			}

	## Calculate results
	results <- c(1:altno)
	for (i in 1:altno)
			{
				results[i] <- snisv[i] / (snisv[i] + spisv[i])
			}
	names(results) <- rownames(performanceTable)
		return(results)
}