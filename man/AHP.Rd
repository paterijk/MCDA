\name{AHP}
\alias{AHP}
\title{
Analytic Hierarchy Process (AHP) method
}
\description{
AHP is a multi-criteria decision analysis method which was originally developed by Thomas L. Saaty in 1970s.
}
\usage{
AHP(criteriaWeightsPairwiseComparisons, alternativesPairwiseComparisonsList)
}
\arguments{
  \item{criteriaWeightsPairwiseComparisons}{Matrix or data frame containing the pairwise comparison matrix for the criteria weights. Lines and columns are named according to the IDs of the criteria.}
  \item{alternativesPairwiseComparisonsList}{A list containing a matrix or data frame of pairwise comparisons (comparing alternatives) for each criterion. The elements of the list are named according to the IDs of the criteria. In each matrix, the lines and the columns are named according to the IDs of the alternatives.}
}
\value{
The function returns a vector containing the AHP score for each alternative.
}
\references{
The Analytic Hierarchy Process: Planning, Priority Setting (1980), ISBN 0-07-054371-2, McGraw-Hill
}
\examples{
style <- t(matrix(c(1,0.25,4,1/6,4,1,4,0.25,0.25,0.25,1,0.2,6,4,5,1),
                  nrow=4,ncol=4))
                  
colnames(style) = c("Corsa","Clio","Fiesta","Sandero")
rownames(style) = c("Corsa","Clio","Fiesta","Sandero")

reliability <- t(matrix(c(1,2,5,1,0.5,1,3,2,0.2,1/3,1,0.25,1,0.5,4,1),
                        nrow=4,ncol=4))
                        
colnames(reliability) = c("Corsa","Clio","Fiesta","Sandero")
rownames(reliability) = c("Corsa","Clio","Fiesta","Sandero")

fuel <- t(matrix(c(1,2,4,1,0.5,1,3,2,0.25,1/3,1,0.2,1,0.5,5,1),nrow=4,ncol=4))

colnames(fuel) = c("Corsa","Clio","Fiesta","Sandero")
rownames(fuel) = c("Corsa","Clio","Fiesta","Sandero")

alternativesPairwiseComparisonsList <- list(style=style, 
                                            reliability=reliability, 
                                            fuel=fuel)

criteriaWeightsPairwiseComparisons <- t(matrix(c(1,0.5,3,2,1,4,1/3,0.25,1),
                                                  nrow=3,ncol=3))
colnames(criteriaWeightsPairwiseComparisons) = c("style","reliability","fuel")
rownames(criteriaWeightsPairwiseComparisons) = c("style","reliability","fuel")

overall1 <- AHP(criteriaWeightsPairwiseComparisons, 
                  alternativesPairwiseComparisonsList)
}