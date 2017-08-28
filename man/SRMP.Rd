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
  The function returns a vector containing the ranks of the alternatives (the higher the better).
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

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                                "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
                                "a23","a24")

colnames(performanceTable) <- c("c1","c2","c3")

colnames(referenceProfiles) <- c("c1","c2","c3")

names(weights) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

expectedpreorder <- list('a16','a13',c('a3','a9'),'a14','a17',c('a1','a7'),'a18','a15',
                         c('a2','a8'),c('a11','a20','a22'),'a5',c('a10','a19','a24'),
                         'a4',c('a12','a21','a23'),'a6')

preorder<-SRMP(performanceTable, referenceProfiles, lexicographicOrder, weights, criteriaMinMax)
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
