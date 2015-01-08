\name{normalizePerformanceTable}
\alias{normalizePerformanceTable}

\title{Function to normalize (or rescale) the columns (or criteria) of a performance table.}

\description{Standardizes the range of the criteria according to a few methods : percentage of max, scale between 0 and 1, scale to 0 mean and 1 standard deviation, scale to euclidian unit length.}

\usage{
normalizePerformanceTable(performanceTable, 
                            normalizationTypes, 
                            alternativesIDs = NULL, 
                            criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{A matrix containing the performance table to be plotted. The columns are labelled according to the criteria IDs, and the rows according to the alternatives IDs.}
  \item{normalizationTypes}{Vector indicating the type of normalization that should be applied to each of the criteria. Possible values : "percentageOfMax", "rescaling" (minimum becomes 0, maximum becomes 1), "standardization" (rescale to a mean of 0 and a standard deviation of 1), "scaleToUnitLength" (scale the criteria values such that the column has euclidian length 1). Any other value (like "none") will result in no data transformation. The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  }

\examples{
library(MCDA)

performanceTable <- matrix(runif(5*9), ncol=5)

row.names(performanceTable) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9")

colnames(performanceTable) <- c("g1","g2","g3","g4", "g5")

normalizationTypes <- c("percentageOfMax","rescaling",
                        "standardization","scaleToUnitLength", "none")

names(normalizationTypes) <- c("g1","g2","g3","g4","g5")

normalizedPerformanceTable <- normalizePerformanceTable(performanceTable,
                                                        normalizationTypes)

}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
