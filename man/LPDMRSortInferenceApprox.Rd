\name{LPDMRSortInferenceApprox}
\alias{LPDMRSortInferenceApprox}

\title{Identification of profiles, weights, majority threshold, veto and dictator thresholds for 
LPDMRSort using a genetic algorithm.}

\description{MRSort is a simplified ElectreTRI method that uses the pessimistic assignment rule,
without indifference or preference thresholds attached to criteria. LPDMRSort considers both a
binary discordance and a binary concordance conditions including several interactions between them. The identification of the profiles, weights, majority threshold and veto thresholds is done by taking into account assignment examples.}

\usage{
LPDMRSortInferenceApprox(performanceTable, criteriaMinMax, categoriesRanks, assignments,
                         majorityRules = c("M","V","D","v","d","dV","Dv","dv"),
                         alternativesIDs = NULL, criteriaIDs = NULL,
                         timeLimit = 60, populationSize = 20, mutationProb = 0.1)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{assignments}{Vector containing the assignments (IDs of the categories) of the alternatives to the categories. The elements are named according to the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. The elements are named according to the IDs of the categories.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{majorityRules}{A vector containing the different type of majority rules to be considered ("M", "V", "D", "v", "d", "dV", "Dv", "dv").
  "M" corresponds to using only the majority rule without vetoes or dictators, 
  "V" considers only the vetoes, "D" only the dictators, 
  "v" is like "V" only that a dictator may invalidate a veto, 
  "d" is like "D" only that a veto may invalidate a dictator, 
  "dV" is like "V" only that if there is no veto we may then consider the dictator, 
  "Dv" is like "D" only that when there is no dictator we may consider the vetoes, while finally 
  "dv" is identical to using both dictator and vetoes only that when both are active
  they invalidate each other, so the majority rule is considered in that case.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{timeLimit}{Allows to fix a time limit of the execution, in seconds (default 60).}
  \item{populationSize}{Allows to change the size of the population used by the genetic algorithm (default 20).}
  \item{mutationProb}{Allows to change the mutation probability used by the genetic algorithm (default 0.1).}
}

\value{
  The function returns a list containing:
  \item{majorityThreshold}{The inferred majority threshold (single numeric value).}
  \item{criteriaWeights}{The inferred criteria weights (a vector named with the criteria IDs).}
  \item{majorityRule}{A string corresponding to the inferred majority rule (one of "M", "V", "D", "v", "d", "dV", "Dv", "dv").}
  \item{profilesPerformances}{The inferred category limits (a matrix with the column names given by the criteria IDs and the rownames given by the upper categories each profile delimits).}
  \item{vetoPerformances}{The inferred vetoes (a matrix with the column names given by the criteria IDs and the rownames given by the categories to which each profile applies).}
  \item{dictatorPerformances}{The inferred dictators (a matrix with the column names given by the criteria IDs and the rownames given by the categories to which each profile applies).}
  \item{fitness}{The classification accuracy of the inferred model (from 0 to 1).}
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-
satory sorting methods in MCDM, II: more than two categories. 
European Journal of Operational Research, 178(1): 246--276, 2007.

no reference yet for the algorithmic approach; one should become available in 2018
}

\examples{
performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
                          c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
                          c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
                          c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))

rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11",
                                "a12", "a13", "a14", "a15", "a16", "a17", "a18", "a19", "a20",
                                "a21", "a22", "a23", "a24")

colnames(performanceTable) <- c("c1","c2","c3")

assignments <-c("P","P","P","F","F","F","F","F","F","P","P","P","P","P","P","P","P","P","F","F",
                "F","F","F","F")

names(assignments) <- rownames(performanceTable)

categoriesRanks <- c(1,2)

names(categoriesRanks) <- c("P","F")

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

set.seed(1)

x<-LPDMRSortInferenceApprox(performanceTable, criteriaMinMax, categoriesRanks, assignments,
                            majorityRules = c("M","V","D","v","d","dV","Dv","dv"),
                            populationSize = 30)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
