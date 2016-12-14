\name{MRSortInferenceExact}
\alias{MRSortInferenceExact}

\title{Identification of profiles, weights and majority threshold for the MRSort sorting method using an exact approach.}

\description{The MRSort method, a simplification of the Electre TRI method, uses the pessimistic assignment rule,
without indifference or preference thresholds attached to criteria. 
Only a binary discordance condition is considered, i.e. a veto forbids an outranking in any
possible concordance situation, or not. 
The identification of the profiles, weights and majority threshold are done by taking into 
account assignment examples.}

\usage{
MRSortInferenceExact(performanceTable, assignments, 
            categoriesRanks, criteriaMinMax, 
            veto = FALSE, readableWeights = FALSE,
            readableProfiles = FALSE,
            alternativesIDs = NULL, criteriaIDs = NULL,
            solver = "glpk")
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{assignments}{Vector containing the assignments (IDs of the categories) of the 
  alternatives to the categories. The elements are named according to the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. 
  The elements are named according to the IDs of the categories.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. 
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{veto}{Boolean parameter indicating whether veto profiles are being used or not.}
  \item{readableWeights}{Boolean parameter indicating whether the weights are to be spaced more evenly or not.}
  \item{readableProfiles}{Boolean parameter indicating whether the profiles are to be spaced more evenly or not.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{solver}{String specifying if the glpk solver (glpk) should be used, or the cplex (cplex) solver. By default glpk. The cplex solver requires to install the cplex binary and the cplex C API, as well as the cplexAPI R package.}
}

\value{
  The function returns NULL if there is a problem, or a list structured as follows :
  \item{lambda}{The majority threshold.}
  \item{weights}{A vector containing the weights of the criteria. 
  The elements are named according to the criteria IDs.}
  \item{profilesPerformances}{A matrix containing the lower profiles of the categories. 
  The columns are named according to the criteria, whereas the rows are named according 
  to the categories. The lower profile of the lower category can be considered as 
  a dummy profile.}
  \item{vetoPerformances}{A matrix containing the veto profiles of the categories. 
  The columns are named according to the criteria, whereas the rows are named according 
  to the categories. The veto profile of the lower category can be considered as 
  a dummy profile.}
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-
satory sorting methods in MCDM, II: more than two categories. 
European Journal of Operational Research, 178(1): 246--276, 2007.
}

\examples{
performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
                          c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
                          c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
                          c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
                          c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
                          c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17))

rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
                                "a8", "a9", "a10", "a11", "a12", "a13", 
                                "a14", "a15", "a16", "a17", "a18", "a19", 
                                "a20", "a21", "a22", "a23", "a24")

colnames(performanceTable) <- c("c1","c2","c3")

assignments <-c("P", "P", "P", "F", "F", "F", "F", "F", "F", "F", "F", "F", 
                "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F")

names(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2)

names(categoriesRanks) <- c("P","F")

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

x<-MRSortInferenceExact(performanceTable, assignments, categoriesRanks, 
                         criteriaMinMax, veto = TRUE, readableWeights = TRUE, 
                         readableProfiles = TRUE, 
                         alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))

ElectreAssignments<-MRSort(performanceTable, x$profilesPerformances, 
                           x$weights, criteriaMinMax, x$lambda, 
                           criteriaVetos=x$vetoPerformances,
                           alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
