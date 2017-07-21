\name{SRMP}
\alias{SRMP}

\title{SRMP: a simple ranking method using reference profiles}

\description{SRMP is a ranking method that uses dominating reference profiles, in a given lexicographic ordering, in order to output a total preorder of a set of alternatives.}

\usage{
SRMP(performanceTable, referenceProfiles, lexicographicOrder, criteriaWeights,
     criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row
  corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be
  named according to the IDs of the alternatives (resp. criteria).}
  \item{referenceProfiles}{Matrix containing, in each row, the reference profiles. The columns are named according to the criteria.}
  \item{lexicographicOrder}{A vector containing the indexes of the reference profiles in a given order. This vetor needs to be of the same length as the number of rows in referenceProfiles and it has to contain a permutation of the indices of these rows.}
  \item{criteriaWeights}{Vector containing the weights of the criteria. The elements are named according to the IDs of the criteria.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria.
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}

\value{
  The function returns a list containing the alternatives placed in a descending order of preference (the first ones are the most preferred). Indifferent alternatives will be placed in vectors within this list.
}

\references{
A. Rolland. Procédures d’agrégation ordinale de préférences avec points de
référence pour l’aide a la décision. PhD thesis, Université Paris VI, 2008.
}

\examples{
# the performance table

performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
                          c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
                          c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
                          c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))

referenceProfiles <- rbind(c(5,5,5),c(10,10,10),c(15,15,15))

lexicographicOrder <- c(2,1,3)

weights <- c(0.2,0.44,0.36)

criteriaMinMax <- c("max","max","max")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24")

colnames(performanceTable) <- c("c1","c2","c3")

colnames(referenceProfiles) <- c("c1","c2","c3")

names(weights) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

expectedpreorder <- list(16,13,c(3,9),14,17,c(1,7),18,15,c(2,8),c(11,20,22),5,c(10,19,24),4,c(12,21,23),6)

colnames(assignments) <- rownames(performanceTable)

preorder<-SRMP(performanceTable, referenceProfiles, lexicographicOrder, weights, criteriaMinMax)
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
