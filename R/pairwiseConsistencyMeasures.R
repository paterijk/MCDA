pairwiseConsistencyMeasures <- function(matrix){
	
	## check the input data is correct
	if (!is.matrix(matrix)) 
        stop("The input must be a matrix")

	if (!(nrow(matrix) == ncol(matrix))) 
	stop("The input must be a square matrix or a data frame")
		
	if(!all(matrix == t(1/matrix)))
		stop("The input must be a reciprocal matrix (i.e. value on one side must = 1/value)")		

	## Calculate CM (AHP Consistency measure) - Based on ISBN: 0-07-054371-2
	CR <- 0
	RV <- c(0,0,0.525,0.882,1.115,1.252,1.341,1.404,1.452,1.484,1.513,1.535,1.555,1.570,1.583,1.595) # Taken from DOI: 10.1016/S0377-2217(02)00255-2
	deltamax <- 0
	pairwisematrix <-  matrix  %*% matrix
	sumrows <- rowSums(matrix)
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
	for (i in 1:nrow(matrix)) { deltamax <- deltamax + (sum(matrix[,i]) * normalisedsumrows[i]) }
	CI <- ((deltamax - nrow(matrix)) / (nrow(matrix) - 1))
	CR <- CI / RV[nrow(matrix)]

	## Calculate Congruence Measure - Based on DOI: 10.1016/j.ejor.2014.10.024
	ans <- matrix(NA, nrow=nrow(matrix), ncol=nrow(matrix))
	for (i in 1:nrow(matrix))
	{
		for (j in 1:nrow(matrix))
		{
			cong <- 0
			if (i != j)
			{
				if (matrix[i,j] > 0)
				{
					b <- log(matrix[i,j])
					for (k in 1:nrow(matrix))
					{
						Aik <- matrix[i,k]
						Akj <- matrix[k,j]
						if ((j!=k) && (k!=i) && (Aik>0) && (Akj>0))
						{
							b2 <- log(Aik*Akj)
							cong <- cong + abs(b-b2)
						}
					}
				}
			}
			if (nrow(matrix) > 2)
			{
				ans[i,j] <- (cong / (nrow(matrix)-2))
			}
		}
	}
	Congruence <- (sum(ans)/(nrow(matrix)*(nrow(matrix)-1)))
	
	## Calculate Dissonance Measure - Based on DOI: 10.1016/j.ejor.2014.10.024 
	for (i in 1:nrow(matrix))
	{
		for (j in 1:nrow(matrix))
		{
			diss <- 0
			if (i!=j)
			{
				if (matrix[i,j]>0)
				{
					b <- log(matrix[i,j])
					for (k in 1:nrow(matrix))
					{
						Aik <- matrix[i,k]
						Akj <- matrix[k,j]
						if ( (j!=k) && (k!=i) && (Aik>0) && (Akj>0) )
						{
							b2 <- log(Aik*Akj)
							if ((b*b2) < 0) { diss <- diss + 1 }
						}
					}
				}
			}
			if (nrow(matrix)>2)
			{
				ans[i,j] <- (diss/(nrow(matrix)-2))
			}
		}
	}
	Dissonance <- (sum(ans)/(nrow(matrix)*(nrow(matrix)-1)))
	
	## Calculate Koczkodaj's Measure - Based on DOI: 10.1016/0895-7177(93)90059-8		
	CM <- 0
	for (i in 1:nrow(matrix))
	{
		for (j in i:nrow(matrix))
		{
			for (k in 1:nrow(matrix))
			{
				if (i!=k&&j!=k) 
				{
                    a <- matrix[i, j]
                    c <- matrix[j, k]
                    b <- matrix[i, k]
					if(a > 0 && b > 0 && c > 0)
					{
						cm_a <- (1 / a) * abs(a - (b / c))
                        cm_b <- (1 / b) * abs(b - (a * c))
                        cm_c <- (1 / c) * abs(c - (b / a))
						
						cm <- cm_a
                        if (cm > cm_b) { cm <- cm_b }
                        if (cm > cm_c) { cm <- cm_c }
                        if (cm > CM) 
						{
                            CM <- cm
                        }
					}
				}
			}
		}
	}
	
	measures <- list(CR = CR, Congruence = Congruence, Dissonance = Dissonance, Koczkodaj = CM)
	return(measures)
}