\name{MARE}
\alias{MARE}
\title{
Multi-Attribute Range Evaluations (MARE)
}
\description{
MARE is a multi-criteria decision analysis method which was originally developed by Hodgett et al. in 2014.
}
\usage{
MARE(performanceTableMin, 
      performanceTable,
      performanceTableMax,
      criteriaWeights,
      criteriaMinMax,
      alternativesIDs = NULL,
      criteriaIDs = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{performanceTableMin}{Matrix or data frame containing the minimum performance table. Each column corresponds to an alternative, and each row to a criterion. Columns (resp. rows) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{performanceTable}{Matrix or data frame containing the most likely performance table. Each column corresponds to an alternative, and each row to a criterion. Columns (resp. rows) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{performanceTableMax}{Matrix or data frame containing the maximum performance table. Each column corresponds to an alternative, and each row to a criterion. Columns (resp. rows) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaWeights}{Vector containing the weights of the criteria. The elements are named according to the IDs of the criteria.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}
\value{
The function returns an element of type mare which contains the MARE scores for each alternative.
}
\references{
Richard E. Hodgett, Elaine B. Martin, Gary Montague, Mark Talford (2014). Handling uncertain decisions in whole process design. Production Planning & Control, Volume 25, Issue 12, 1028-1038.
}

\examples{
performanceTableMin <- t(matrix(c(78,87,79,19,8,68,74,8,90,89,74.5,9,20,81,30),
                  nrow=3,ncol=5, byrow=TRUE)) 
performanceTable <- t(matrix(c(80,87,86,19,8,70,74,10,90,89,75,9,33,82,30),
                              nrow=3,ncol=5, byrow=TRUE))
performanceTableMax <- t(matrix(c(81,87,95,19,8,72,74,15,90,89,75.5,9,36,84,30),
                                 nrow=3,ncol=5, byrow=TRUE))  

row.names(performanceTable) <- c("Yield","Toxicity","Cost","Separation","Odour")
colnames(performanceTable) <- c("Route One","Route Two","Route Three")
row.names(performanceTableMin) <- row.names(performanceTable)
colnames(performanceTableMin) <- colnames(performanceTable)
row.names(performanceTableMax) <- row.names(performanceTable)
colnames(performanceTableMax) <- colnames(performanceTable)

weights <- c(0.339,0.077,0.434,0.127,0.023) 
names(weights) <- row.names(performanceTable)

criteriaMinMax <- c("max", "max", "max", "max", "max")
names(criteriaMinMax) <- row.names(performanceTable)

overall1 <- MARE(performanceTableMin, 
                   performanceTable, 
                   performanceTableMax, 
                   weights, 
                   criteriaMinMax)

overall2 <- MARE(performanceTableMin, 
                    performanceTable,
                    performanceTableMax,
                    weights,
                    criteriaMinMax,
                    alternativesIDs = c("Route Two","Route Three"),
                    criteriaIDs = c("Yield","Toxicity","Cost","Separation"))
}