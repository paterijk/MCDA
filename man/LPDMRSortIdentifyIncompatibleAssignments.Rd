\name{LPDMRSortIdentifyIncompatibleAssignments}
\alias{LPDMRSortIdentifyIncompatibleAssignments}

\title{Identifies all sets of assignment examples which are incompatible with the MRSort sorting method extended to handle large performance differences.}

\description{MRSort is a simplified ElectreTRI method that uses the pessimistic assignment rule, without indifference or preference thresholds attached to criteria. LPDMRSort considers both a binary discordance and a binary concordance conditions including several interactions between them. This function outputs all (or a fixed number of) sets of incompatible assignment examples ranging in size from the minimal size and up to a given threshold. 
The retrieved sets are also not contained in each other.}

\usage{
LPDMRSortIdentifyIncompatibleAssignments(performanceTable, 
                                         assignments, 
                                         categoriesRanks,
                                         criteriaMinMax, 
                                         majorityRule = "M", 
                                         incompatibleSetsLimit = 100, 
                                         largerIncompatibleSetsMargin = 0, 
                                         alternativesIDs = NULL, 
                                         criteriaIDs = NULL,
                                         solver="glpk",
                                         cplexIntegralityTolerance = NULL,
                                         cplexThreads = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{assignments}{Vector containing the assignments (IDs of the categories) of the alternatives to the categories. 
  The elements are named according to the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. 
  The elements are named according to the IDs of the categories.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria.
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{majorityRule}{String denoting how the vetoes and dictators are combined in order to form the assignment rule. 
  The values to choose from are "M", "V", "D", "v", "d", "dV", "Dv", "dv". 
  "M" corresponds to using only the majority rule without vetoes or dictators, 
  "V" considers only the vetoes, "D" only the dictators, 
  "v" is like "V" only that a dictator may invalidate a veto, 
  "d" is like "D" only that a veto may invalidate a dictator, 
  "dV" is like "V" only that if there is no veto we may then consider the dictator, 
  "Dv" is like "D" only that when there is no dictator we may consider the vetoes, 
  while finally "dv" is identical to using both dictator and vetoes only that when both 
  are active they invalidate each other, so the majority rule is considered in that case.}
  \item{incompatibleSetsLimit}{Pozitive integer denoting the upper limit of the number of sets to be retrieved.}
  \item{largerIncompatibleSetsMargin}{Pozitive integer denoting whether sets larger than the minimal size should be retrieved, 
  and by what margin. For example, if this is 0 then only sets of the minimal size will be retrieved, 
  if this is 1 then sets also larger by 1 element will be retrieved.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
    \item{solver}{String specifying if the glpk solver (glpk) should be used, or the cplex (cplex) solver. By default glpk. The cplex solver requires to install the cplex binary and the cplex C API, as well as the cplexAPI R package.}
  \item{cplexIntegralityTolerance}{If the cplex solver is used, allows to fix a tolerance for integrality. By default NULL (which corresponds to the default value of cplex).}
  \item{cplexThreads}{If the cplex solver is used, allows to the number of threads for the calculation. By default NULL (which corresponds to the default value of cplex).}
}

\value{
  The function returns NULL if there is a problem, 
  or a list containing a list of incompatible sets of alternatives as vectors and the status of the execution (1 if OK, 0 if something went wrong).
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-satory sorting methods 
in MCDM, II: more than two categories.
European Journal of Operational Research, 178(1): 246--276, 2007.

Meyer, P. and Olteanu, A-L. Integrating large positive and negative performance 
differences in majority-rule sorting models.
European Journal of Operational Research, submitted , 2015.
}

\examples{
# the performance table

performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
                          c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
                          c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
                          c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
                          c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
                          c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17), 
                          c(7,7,7))

rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
                                "a8", "a9", "a10", "a11", "a12", "a13", 
                                "a14", "a15", "a16", "a17", "a18", "a19", 
                                "a20", "a21", "a22", "a23", "a24", "a25")

colnames(performanceTable) <- c("c1","c2","c3")

assignments <-rbind(c("P","P","P","F","F","F","F","F","F","F","F","F",
                    "F","F","F","F","F","F","F","F","F","F","F","F","P"), 
                    c("P","P","P","F","F","F","P","P","P","P","P","P",
                    "P","P","P","P","P","P","P","P","P","P","P","P","P"), 
                    c("P","P","P","F","F","F","F","F","F","F","F","F",
                    "P","P","P","P","P","P","F","F","F","F","F","F","P"), 
                    c("P","P","P","F","F","F","P","P","P","P","P","P",
                    "P","P","P","P","P","P","F","F","F","F","F","F","P"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "F","F","F","F","F","F","F","F","F","F","F","F","P"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "P","P","P","P","P","P","P","P","P","P","P","P","P"), 
                    c("P","P","P","F","F","F","F","F","F","P","P","P",
                    "P","P","P","P","P","P","F","F","F","F","F","F","P"))

colnames(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2)

names(categoriesRanks) <- c("P","F")

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

majorityRules <- c("V","D","v","d","dV","Dv","dv")

for(i in 1:1)# change to 7 in order to perform all tests
{
  incompatibleAssignmentsSets<-LPDMRSortIdentifyIncompatibleAssignments(
                                 performanceTable, assignments[i,], 
                                 categoriesRanks, criteriaMinMax,
                                 majorityRule = majorityRules[i])
  
  filteredAlternativesIDs <- setdiff(rownames(performanceTable),
                                     incompatibleAssignmentsSets[[1]][1])
  
  x<-LPDMRSortInferenceExact(performanceTable, assignments[i,], 
                             categoriesRanks, criteriaMinMax, 
                             majorityRule = majorityRules[i], 
                             readableWeights = TRUE, 
                             readableProfiles = TRUE, 
                             minmaxLPD =  TRUE, 
                             alternativesIDs = filteredAlternativesIDs)
  
  ElectreAssignments<-LPDMRSort(performanceTable, x$profilesPerformances, 
                                categoriesRanks,
                                x$weights, criteriaMinMax, x$lambda, 
                                criteriaVetos=x$vetoPerformances,
                                criteriaDictators=x$dictatorPerformances, 
                                majorityRule = majorityRules[i],
                                alternativesIDs = filteredAlternativesIDs)
  
  print(all(ElectreAssignments == assignments[i,filteredAlternativesIDs]))
}
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
