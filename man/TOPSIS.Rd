\name{TOPSIS}
\alias{TOPSIS}

\title{Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS) method}

\description{TOPSIS is a multi-criteria decision analysis method which was originally developed by Hwang and Yoon in 1981.}

\usage{
TOPSIS(performanceTable,
         criteriaWeights,
         criteriaMinMax,
         positiveIdealSolutions = NULL,
         negativeIdealSolutions = NULL,
         alternativesIDs = NULL,
         criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaWeights}{Vector containing the weights of the criteria. The elements are named according to the IDs of the criteria.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{positiveIdealSolutions}{Vector containing the positive ideal solutions for each criteria. The elements are named according to the IDs of the criteria.}
  \item{negativeIdealSolutions}{Vector containing the negative ideal solutions for each criteria. The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}
\value{
The function returns a vector containing the TOPSIS score for each alternative.
}
\references{
Hwang, C.L.; Yoon, K. (1981). Multiple Attribute Decision Making: Methods and Applications. New York: Springer-Verlag.
http://hodgett.co.uk/topsis-in-excel/
}
\examples{
performanceTable <- matrix(c(5490,51.4,8.5,285,6500,70.6,7,
                              288,6489,54.3,7.5,290),
                              nrow=3,
                              ncol=4,
                              byrow=TRUE)

row.names(performanceTable) <- c("Corsa","Clio","Fiesta")

colnames(performanceTable) <- c("Purchase Price","Economy",
                                   "Aesthetics","Boot Capacity")

weights <- c(0.35,0.25,0.25,0.15)

criteriaMinMax <- c("min", "max", "max", "max")

positiveIdealSolutions <- c(0.179573776, 0.171636015, 0.159499658, 0.087302767)
negativeIdealSolutions <- c(0.212610118, 0.124958799, 0.131352659, 0.085797547)

names(weights) <- colnames(performanceTable)
names(criteriaMinMax) <- colnames(performanceTable)
names(positiveIdealSolutions) <- colnames(performanceTable)
names(negativeIdealSolutions) <- colnames(performanceTable)

overall1 <- TOPSIS(performanceTable, weights, criteriaMinMax)

overall2 <- TOPSIS(performanceTable,
                       weights, 
                       criteriaMinMax,
                       positiveIdealSolutions,
                       negativeIdealSolutions)

overall3 <- TOPSIS(performanceTable,
                      weights,
                      criteriaMinMax,
                      alternativesIDs = c("Corsa","Clio"),
                      criteriaIDs = c("Purchase Price","Economy","Aesthetics"))

overall4 <- TOPSIS(performanceTable, 
                    weights,
                    criteriaMinMax,
                    positiveIdealSolutions,
                    negativeIdealSolutions,
                    alternativesIDs = c("Corsa","Clio"), 
                    criteriaIDs = c("Purchase Price","Economy","Aesthetics"))
}