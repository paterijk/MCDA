\name{LPDMRSortIdentifyUsedDictatorProfiles}
\alias{LPDMRSortIdentifyUsedDictatorProfiles}

\title{Identify dictator profiles evaluations that have an impact on the final assignments of MRSort with large performance differences}

\description{MRSort is a simplified ELECTRE-TRI approach which assigns alternatives to a set of ordered categories using delimiting profiles evaluations. In this case, we also take into account large performance differences. This method is used to identify which dictator profiles evaluations have an impact on the final assignment of at least one of the input alternatives.}

\usage{
LPDMRSortIdentifyUsedDictatorProfiles(performanceTable, assignments,
                               categoriesRanks, criteriaMinMax,
                               majorityThreshold,
                               criteriaWeights,
                               profilesPerformances,
                               dictatorPerformances,
                               vetoPerformances = NULL,
                               majorityRule = "D",
                               alternativesIDs = NULL,
                               criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{assignments}{A vector containing the category to which each alternative is assigned. The vector needs to be named using the alternatives IDs.}
  \item{categoriesRanks}{A vector containing the ranks of the categories (1 for the best,
  with higher values for increasingly less preferred categories). The vector needs to be 
  named with the categories names, whereas the ranks need to be a range of values from 1
  to the number of categories.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. 
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{majorityThreshold}{The majority threshold needed to determine when a coalition of criteria is sufficient in order to validate an outranking relation.}
  \item{criteriaWeights}{Vector containing the weights of the criteria. 
  The elements are named according to the IDs of the criteria.}
  \item{profilesPerformances}{Matrix containing, in each row, the lower profiles of the categories. The columns are named according to the criteria, and the rows are named according to the categories. The index of the row in the matrix corresponds to the rank of the category.}
  \item{dictatorPerformances}{Matrix containing in each row a vector defining the dictator values for the lower profile of the category. NA values mean that no dictator is defined. 
  A dictator threshold for criterion i and category k represents the performance above which 
  an alternative outranks the lower profile of category k regardless of the size of the coalition of criteria in favor of this statement. 
  The rows are named according to the categories, whereas the columns are named 
  according to the criteria.}
  \item{vetoPerformances}{Matrix containing in each row a vector defining the veto values for 
  the lower profile of the category. NA values mean that no veto is defined. 
  A veto threshold for criterion i and category k represents the performance below which 
  an alternative is forbidden to outrank the lower profile of category k, and thus is 
  forbidden to be assigned to the category k. 
  The rows are named according to the categories, whereas the columns are named 
  according to the criteria. By default no veto profiles are needed.}
  \item{majorityRule}{String denoting how the vetoes and dictators are combined in order to form 
  the assignment rule. 
  The values to choose from are "D", "v", "d", "dV", "Dv", "dv". 
  "D" considers only the dictators,
  "v" is like "V" only that a dictator may invalidate a veto, 
  "d" is like "D" only that a veto may invalidate a dictator, 
  "dV" is like "V" only that if there is no veto we may then consider the dictator, 
  "Dv" is like "D" only that when there is no dictator we may consider the vetoes, while finally 
  "dv" is identical to using both dictator and vetoes only that when both are active
  they invalidate each other, so the majority rule is considered in that case.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}

\value{
  The function returns a matrix containing TRUE/FALSE inficators for each evaluation of the veto profiles. 
}

\examples{
# the performance table

performanceTable <- rbind(
  c(1,27,1),
  c(6,20,1),
  c(2,20,0),
  c(6,40,0),
  c(30,10,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# lower profiles of the categories (best category in the first position of the list)

categoriesLowerProfiles <- rbind(c(3, 11, 3),c(7, 25, 2),c(NA,NA,NA))

colnames(categoriesLowerProfiles) <- colnames(performanceTable)

rownames(categoriesLowerProfiles)<-c("Good","Medium","Bad")

# the order of the categories, 1 being the best

categoriesRanks <-c(1,2,3)

names(categoriesRanks) <- c("Good","Medium","Bad")

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# dictators

criteriaDictators <- rbind(c(1, 1, -1),c(1, 20, 0),c(NA,NA,NA))

colnames(criteriaDictators) <- colnames(performanceTable)
rownames(criteriaDictators) <- c("Good","Medium","Bad")

# vetos

criteriaVetos <- rbind(c(9, 50, 5),c(50, 50, 5),c(NA,NA,NA))

colnames(criteriaVetos) <- colnames(performanceTable)
rownames(criteriaVetos) <- c("Good","Medium","Bad")

# weights

criteriaWeights <- c(1/6,3/6,2/6)

names(criteriaWeights) <- colnames(performanceTable)

# assignments

assignments <- c("Good","Medium","Bad","Bad","Bad")


# LPDMRSortIndetifyUsedVetoProfiles

used<-LPDMRSortIdentifyUsedDictatorProfiles(performanceTable, assignments,
                                        categoriesRanks, criteriaMinMax,
                                        0.5, criteriaWeights,
                                        categoriesLowerProfiles,
                                        criteriaDictators,
                                        criteriaVetos,
                                        "dv")
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
