\name{LPDMRSort}
\alias{LPDMRSort}

\title{MRSort that takes into account large performance differences as extended by Meyer and Olteanu.}

\description{MRSort is a simplified ElectreTRI method that uses the pessimistic assignment rule,
without indifference or preference thresholds attached to criteria. LPDMRSort considers both a
binary discordance and a binary concordance conditions including several interactions between them.}

\usage{
LPDMRSort(performanceTable, categoriesLowerProfiles, criteriaWeights, 
          criteriaMinMax, majorityThreshold, criteriaVetos = NULL, 
          criteriaDictators = NULL, majorityRule = "",
          alternativesIDs = NULL, criteriaIDs = NULL, 
          categoriesIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row
  corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be
  named according to the IDs of the alternatives (resp. criteria).}
  \item{categoriesLowerProfiles}{Matrix containing, in each row, the lower profiles of the
  categories. The columns are named according to the criteria, and the rows are named 
  according to the categories. The index of the row in the matrix corresponds to the rank 
  of the category.}
  \item{criteriaWeights}{Vector containing the weights of the criteria. The elements are named according to the IDs of the criteria.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria.
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{majorityThreshold}{The cut threshold for the concordance condition. 
  Should be at least half of the sum of the weights.}
  \item{criteriaVetos}{Matrix containing in each row a vector defining the veto values for the
  lower profile of the category. NA values mean that no veto is defined. A veto threshold for
  criterion i and category k represents the performance below which an alternative is forbidden
  to outrank the lower profile of category k, and thus is forbidden to be assigned to the
  category k. The rows are named according to the categories, whereas the columns are named
  according to the criteria.}
  \item{criteriaDictators}{Matrix containing in each row a vector defining the dictator values
  for the lower profile of the category. NA values mean that no veto is defined. 
  A dictator threshold for criterion i and category k represents the performance above which
  an alternative is guaranteed to outrank the lower profile of category k, and thus may no be
  assigned below category k. The rows are named according to the categories, whereas the columns
  are named according to the criteria.}
  \item{majorityRule}{String denoting how the vetoes and dictators are combined in order to form 
  the assignment rule. 
  The values to choose from are "", "V", "D", "v", "d", "dV", "Dv", "dv". 
  "" corresponds to using only the majority rule without vetoes or dictators, 
  "V" considers only the vetoes, "D" only the dictators, 
  "v" is like "V" only that a dictator may invalidate a veto, 
  "d" is like "D" only that a veto may invalidate a dictator, 
  "dV" is like "V" only that if there is no veto we may then consider the dictator, 
  "Dv" is like "D" only that when there is no dictator we may consider the vetoes, while finally 
  "dv" is identical to using both dictator and vetoes only that when both are active
  they invalidate each other, so the majority rule is considered in that case.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{categoriesIDs}{Vector containing IDs of categories, according to which the data should be filtered.}
}

\value{
  The function returns a vector containing the assignments of the alternatives to the categories. 
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompensatory 
sorting methods in MCDM, II: more than two categories.
European Journal of Operational Research, 178(1): 246--276, 2007.

Meyer, P. and Olteanu, A-L. Integrating large positive and negative
performance differences in majority-rule sorting models.
European Journal of Operational Research, , 2015.
}

\examples{
# the performance table

performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
                          c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
                          c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
                          c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
                          c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
                          c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17))

profilesPerformances <- rbind(c(10,10,10),c(0,0,0))

vetoPerformances <- rbind(c(7,7,7),c(0,0,0))

dictatorPerformances <- rbind(c(17,17,17),c(0,0,0))

rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
                                "a8", "a9", "a10", "a11", "a12",  "a13", 
                                "a14", "a15", "a16", "a17", "a18", "a19", 
                                "a20", "a21", "a22", "a23", "a24")

rownames(profilesPerformances) <- c("P","F")

rownames(vetoPerformances) <- c("P","F")

rownames(dictatorPerformances) <- c("P","F")

colnames(performanceTable) <- c("c1","c2","c3")

colnames(profilesPerformances) <- c("c1","c2","c3")

colnames(vetoPerformances) <- c("c1","c2","c3")

colnames(dictatorPerformances) <- c("c1","c2","c3")

lambda <- 0.5

weights <- c(1/3,1/3,1/3)

names(weights) <- c("c1","c2","c3")

categoriesRanks <-c(1,2)

names(categoriesRanks) <- c("P","F")

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

assignments <-rbind(c("P","P","P","F","F","F","F","F","F","F","F","F",
                    "F","F","F","F","F","F","F","F","F","F","F","F"), 
                    c("P","P","P","F","F","F","P","P","P","P","P","P",
                    "P","P","P","P","P","P","P","P","P","P","P","P"), 
                    c("P","P","P","F","F","F","F","F","F","F","F","F",
                    "P","P","P","P","P","P","F","F","F","F","F","F"), 
                    c("P","P","P","F","F","F","P","P","P","P","P","P",
                    "P","P","P","P","P","P","F","F","F","F","F","F"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "F","F","F","F","F","F","F","F","F","F","F","F"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "P","P","P","P","P","P","P","P","P","P","P","P"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "P","P","P","P","P","P","F","F","F","F","F","F"))

colnames(assignments) <- rownames(performanceTable)

majorityRules <- c("V","D","v","d","dV","Dv","dv")

for(i in 1:7)
{
  ElectreAssignments<-LPDMRSort(performanceTable, profilesPerformances, 
                                weights, criteriaMinMax, lambda, 
                                criteriaVetos=vetoPerformances,
                                criteriaDictators=dictatorPerformances,
                                majorityRule = majorityRules[i])

  print(all(ElectreAssignments == assignments[i,]))

  stopifnot(all(assignments[i,]== ElectreAssignments))
}
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
