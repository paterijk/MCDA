\name{weightedSum}
\alias{weightedSum}

\title{Weighted sum of evaluations of alternatives.}

\description{Computes the weighted sum of the evaluations of alternatives, stored in a performance table, with respect to a vector of criteria weights.}

\usage{
weightedSum(performanceTable, criteriaWeights, alternativesIDs = NULL, criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaWeights}{Vector containing the weights of the criteria. The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the performance table should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the performance table should be filtered.}
  }

\value{
  The function returns a matrix containing the weighted sum of the alternatives with respect to the criteria weights. 
}

\examples{
performanceTable <- matrix(runif(3*4), ncol=3)

row.names(performanceTable) <- c("x1","x2","x3","x4")

colnames(performanceTable) <- c("g1","g2","g3")

weights <- c(1,2,3)

names(weights) <- c("g1","g2","g3")

overall1 <- weightedSum(performanceTable, weights)

overall2 <- weightedSum(performanceTable, weights, alternativesIDs <- c("x2","x3"), criteriaIDs <- c("g2","g3"))
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
