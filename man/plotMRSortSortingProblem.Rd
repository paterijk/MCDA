\name{plotMRSortSortingProblem}
\alias{plotMRSortSortingProblem}

\title{Plot the categories and assignments of an Electre TRI-like sorting problem (via separation profiles).}

\description{The profiles shown are the separation profiles between the classes. They are stored as the lower profiles of the categories.}

\usage{
plotMRSortSortingProblem(performanceTable, categoriesLowerProfiles, 
                          assignments, criteriaMinMax, 
                          criteriaUBs, criteriaLBs,
                          categoriesDictators = NULL, categoriesVetoes = NULL,
                          majorityRule = NULL, criteriaWeights = NULL, 
                          majorityThreshold = NULL, alternativesIDs = NULL,
                          criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{categoriesLowerProfiles}{Matrix containing, in each row, the lower profiles of the categories (the separation profiles in fact). The columns are named according to the criteria, and the rows are named according to the categories. The index of the row in the matrix corresponds to the rank of the category.}
    \item{assignments}{Vector containing the assignments (IDs of the categories) of the alternatives to the categories. The elements are named according to the alternatives.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{criteriaLBs}{Vector containing the lower bounds of the criteria to be considered for the plotting. The elements are named according to the IDs of the criteria.}
  \item{criteriaUBs}{Vector containing the upper bounds of the criteria to be considered for the plotting. The elements are named according to the IDs of the criteria.}
  \item{categoriesDictators}{Matrix containing, in each row, the lower dictator profiles of the categories. The columns are named according to the criteria, and the rows are named according to the categories. The index of the row in the matrix corresponds to the rank of the category.}
  \item{categoriesVetoes}{Matrix containing, in each row, the lower veto profiles of the categories. The columns are named according to the criteria, and the rows are named according to the categories. The index of the row in the matrix corresponds to the rank of the category.}
  \item{majorityRule}{A string containing one of the following values: 'V' , 'D', 'v', 'd', 'dV', 'Dv', 'dv'. This indicates the type of majority rule that will be used by the MRSort model. 'V' stands for MRSort with vetoes, 'D' stands for MRSort with dictators, 'v' stands for MRSort with vetoes weakened by dictators, 'd' stands for MRSort with dictators weakened by vetoes, 'dV' stands for MRSort with vetoes dominating dictators, 'Dv' stands for MRSort with dictators dominating vetoes, while 'dv' stands for MRSort with conflicting vetoes and dictators.}
  \item{criteriaWeights}{Vector containing the criteria weights. The elements are named according to the IDs of the criteria.}
  \item{majorityThreshold}{A value corresponding to the majority threshold. Along with the criteria weights, this value is used to determine when a coalition of criteria is sufficient in order to assert that an alternative is at least as good as a category profile.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}
  
\examples{
# the performance table

performanceTable <- rbind(
  c(1,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# lower profiles of the categories 
# (best category in the first position of the list)

categoriesLowerProfiles <- rbind(c(3, 11, 3),c(7, 25, 2),c(30,30,0))

colnames(categoriesLowerProfiles) <- colnames(performanceTable)

rownames(categoriesLowerProfiles)<-c("Good","Medium","Bad")

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# lower bounds of the criteria for the determination of value functions

criteriaLBs=c(0,5,0)

names(criteriaLBs) <- colnames(performanceTable)

# upper bounds of the criteria for the determination of value functions

criteriaUBs=c(50,50,4)

names(criteriaUBs) <- colnames(performanceTable)

# weights

criteriaWeights <- c(1,3,2)

names(criteriaWeights) <- colnames(performanceTable)

assignments <- assignments<-MRSort(performanceTable, 
                                          categoriesLowerProfiles, 
                                          criteriaWeights, 
                                          criteriaMinMax, 3)

names(assignments) <- rownames(performanceTable)

plotMRSortSortingProblem(performanceTable, categoriesLowerProfiles, 
                              assignments, criteriaMinMax, 
                              criteriaUBs, criteriaLBs)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
