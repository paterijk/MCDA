AHP <- function(criteriaWeightsPairwiseComparisons, alternativesPairwiseComparisonsList){
	
	## check the input data is correct

	if (!((is.matrix(criteriaWeightsPairwiseComparisons) || (is.data.frame(criteriaWeightsPairwiseComparisons))))) 
        stop("criteriaWeightsPairwiseComparisons must be a matrix or a data frame")

	if (!(nrow(criteriaWeightsPairwiseComparisons) == ncol(criteriaWeightsPairwiseComparisons))) 
		stop("criteriaWeightsPairwiseComparisons must be a square matrix or a data frame")

	if(!all(criteriaWeightsPairwiseComparisons == t(1/criteriaWeightsPairwiseComparisons)))
		stop("criteriaWeightsPairwiseComparisons must be a reciprocal matrix (i.e. value on one side must = 1/value)")

	if (!(length(alternativesPairwiseComparisonsList) >= 2)) 
        stop("list alternativesPairwiseComparisonsList must contain at least 2 matrices or data frames")

	size <- 0;
	for (i in 1:length(alternativesPairwiseComparisonsList))
		{
			if (!((is.matrix(alternativesPairwiseComparisonsList[i][[1]]) || (is.data.frame(alternativesPairwiseComparisonsList[i][[1]]))))) 
        		stop("all elements in the list alternativesPairwiseComparisonsList must be a matrix or a data frame")

			if (!(nrow(alternativesPairwiseComparisonsList[i][[1]]) == ncol(alternativesPairwiseComparisonsList[i][[1]]))) 
			stop("all elements in the list alternativesPairwiseComparisonsList must be a square matrix or a data frame")

			if(!all(alternativesPairwiseComparisonsList[i][[1]] == t(1/alternativesPairwiseComparisonsList[i][[1]])))
			stop("all elements in the list scoresmatrixlist must be a reciprocal matrix (i.e. value on one side must = 1/value)")
			
			if (i == 1)
			{
				size <- nrow(alternativesPairwiseComparisonsList[i][[1]])
			}
			else
			{
				if (!(nrow(alternativesPairwiseComparisonsList[i][[1]]) == size)) 
				stop("all matrices or data frames in list alternativesPairwiseComparisonsList must be the same size.")
			}		
		}

	critno <- nrow(criteriaWeightsPairwiseComparisons)
	altno <- nrow(alternativesPairwiseComparisonsList[1][[1]])

	## Estimate the principle eigenvector of the weights matrix to 10 digits
	
	pairwisematrix <-  criteriaWeightsPairwiseComparisons  %*% criteriaWeightsPairwiseComparisons
	sumrows <- rowSums(criteriaWeightsPairwiseComparisons)
	sumtotal <- sum(sumrows)
	normalisedsumrows <- sumrows / sumtotal	
	previous <- vector()
	
	while (!identical(round(previous, digits = 10),round(normalisedsumrows, digits = 10)))
					{	
	previous <- normalisedsumrows
	pairwisematrix <-  pairwisematrix  %*% pairwisematrix
	sumrows <- rowSums(pairwisematrix)
	sumtotal <- sum(sumrows)
	normalisedsumrows <- sumrows / sumtotal
					}
	weights <- normalisedsumrows

	## Estimate the principle eigenvectors of each of the score matrices to 10 digits

	savedscores <- matrix(nrow=critno,ncol=altno)
	for (i in 1:length(alternativesPairwiseComparisonsList))
		{
			pairwisematrix <-  alternativesPairwiseComparisonsList[i][[1]]  %*% alternativesPairwiseComparisonsList[i][[1]]
			sumrows <- rowSums(alternativesPairwiseComparisonsList[i][[1]])
			sumtotal <- sum(sumrows)
			normalisedsumrows <- sumrows / sumtotal	
			previous <- vector()
	
			while (!identical(round(previous, digits = 10),round(normalisedsumrows, digits = 10)))
					{	
				previous <- normalisedsumrows
				pairwisematrix <-  pairwisematrix  %*% pairwisematrix
				sumrows <- rowSums(pairwisematrix)
				sumtotal <- sum(sumrows)
				normalisedsumrows <- sumrows / sumtotal
					}
			
			savedscores[i,] <- normalisedsumrows
		}

	## Calculate the results

	results <- matrix(nrow=1,ncol=altno)
	
	for (i in 1:altno)
		{
		altscore <- 0
	  	for (j in 1:critno)
			{
				altscore <- altscore + (weights[j] * savedscores[j,i])
			}
		results[1,i] <- altscore
		}
  results<-as.vector(results)
  names(results)<-row.names(alternativesPairwiseComparisonsList[[1]])
	return(results)
}