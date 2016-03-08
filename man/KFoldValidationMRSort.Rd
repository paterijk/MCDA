\name{MRSort}
\alias{MRSort}

\title{Electre TRI-like sorting method axiomatized by Bouyssou and Marchant.}

\description{This simplification of the Electre TRI method uses the pessimistic assignment rule, without indifference or
preference thresholds attached to criteria. 
Only a binary discordance condition is considered, i.e. a veto forbids an outranking 
in any possible concordance situation, or not.}

\usage{
MRSort(performanceTable, categoriesLowerProfiles, 
            criteriaWeights, criteriaMinMax, majorityThreshold, 
            criteriaVetos = NULL, alternativesIDs = NULL, 
            criteriaIDs = NULL, categoriesIDs = NULL
		)
}

\arguments{
   \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{alternativesAssignments}{Vector containing the assignments (IDs of the categories) of the alternatives to the categories. 
  The elements are named according to the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. 
  The elements are named according to the IDs of the categories.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. 
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{veto}{Boolean parameter indicating whether veto profiles are to be used or not.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{categoriesIDs}{Vector containing IDs of categories, in which the alternative will be assigned.}
  \item{nbTest}{Integer representing the number of time that the alternativesAssignments will be divided into k parts}
  \item{k}{Integer representing the number of parts in which the alternativesAssignments will be divided in each test}
}

\value{
  The function returns a real number representing the value of the k-fold validation 
  furthermore it will print numerous informations during the execution that will allow
  the user to follow the proceeding.
}


\examples{
# the performance table

performanceTable <- rbind(
  c(1,4,2),
  c(0,1,4),
  c(1,0,1),
  c(3,1,0),
  c(0,4,4),
  c(1,2,2),
  c(3,3,3),
  c(2,3,0),
  c(0,3,1),
  c(4,2,1),
  c(4,2,3),
  c(2,3,3),
  c(4,3,4),
  c(2,1,2),
  c(4,3,0),
  c(3,3,2),
  c(4,3,3),
  c(1,4,2),
  c(2,4,3),
  c(1,3,4)
)

rownames(performanceTable) <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20")

colnames(performanceTable) <- c("Envir","Workers","Transp")

# ranks of the alternatives

alternativesAssignments <- c("C3","C3","C1","C2","C2","C2","C4","C2","C1","C3","C4","C4","C5","C2","C2","C4","C5","C3","C4","C3")

names(alternativesAssignments) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(3,3,3)

names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)

# ranks of the categories

categoriesRanks <- c(1,2,3,4,5)

names(categoriesRanks) <- c("C5","C4","C3","C2","C1")

# x<-UTADIS(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, alternativesAssignments, categoriesRanks,0.1)

#print(performanceTable)

KFold<-KFoldCrossValidationMRSort(performanceTable=performanceTable, criteriaMinMax=criteriaMinMax, 
          alternativesAssignments=alternativesAssignments, categoriesRanks=categoriesRanks,nbTest=50,k=2)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
