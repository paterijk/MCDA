\name{SURE}
\alias{SURE}
\title{
Simulated Uncertainty Range Evaluations (SURE)
}
\description{
SURE is a multi-criteria decision analysis method which was developed by Richard Hodgett and Sajid Siraj. More details on the method are available in https://doi.org/10.1016/j.eswa.2018.08.048 
}
\usage{
SURE(performanceTableMin,
      performanceTable,
      performanceTableMax,
      criteriaWeights,
      criteriaMinMax,
      alternativesIDs = NULL,
      criteriaIDs = NULL,
	  NoOfSimulations = 100000)
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
  \item{NoOfSimulations}{Integer stating the number of Simulations to use.}
}
\value{
The function returns an element of type SURE which contains the SURE simulated scores for each alternative.
}
\references{
Richard E. Hodgett, Sajid Siraj (2019). SURE: A method for decision-making under uncertainty. Expert Systems with Applications, Volume 115, 684-694.
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

criteriaWeights <- c(0.339,0.077,0.434,0.127,0.023) 
names(criteriaWeights) <- row.names(performanceTable)

criteriaMinMax <- c("max", "max", "max", "max", "max")
names(criteriaMinMax) <- row.names(performanceTable)

test1 <- SURE(performanceTableMin, 
                 performanceTable, 
                 performanceTableMax, 
                 criteriaWeights, 
                 criteriaMinMax, NoOfSimulations = 101)

summary(test1)
plotSURE(test1)
plotSURE(test1, greyScale = TRUE, separate = TRUE)

test2 <- SURE(performanceTableMin, 
              performanceTable,
              performanceTableMax,
              criteriaWeights,
              criteriaMinMax,
              alternativesIDs = c("Route Two","Route Three"),
              criteriaIDs = c("Yield","Toxicity","Separation"),
              NoOfSimulations = 101)

summary(test2)
plotSURE(test2)
plotSURE(test2, greyScale = TRUE, separate = TRUE)
}