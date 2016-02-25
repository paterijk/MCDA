#############################################################################
#
# Copyright Patrick Meyer, Sebastien Bigaret, Richard Hodgett and Alexandru-Liviu Olteanu, 2015
#
# Contributors:
#   Patrick Meyer <patrick.meyer@telecom-bretagne.eu>
#   Sebastien Bigaret <sebastien.bigaret@telecom-bretagne.eu>
#   Richard Hodgett <r.e.hodgett@leeds.ac.uk>
#	Alexandru-Liviu Olteanu <al.olteanu@gmail.com>
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