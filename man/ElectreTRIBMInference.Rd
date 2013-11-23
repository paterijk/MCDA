\name{ElectreTRIBMInference}
\alias{ElectreTRIBMInference}

\title{Identification of profiles, weights and majority threshold for an Electre TRI-like sorting method axiomatized by Bouyssou and Marchant.}

\description{This simplification of the Electre TRI method uses the pessimistic assignment rule, without indifference or
preference thresholds attached to criteria. Only a binary discordance condition is considered, i.e. a veto forbids an outranking in any possible concordance situation, or not. The identification of the profiles, weights and majority threshold are done by taking into account assignment examples.}

\usage{
ElectreTRIBMInference(performanceTable, assignments, 
            categoriesRanks, criteriaMinMax, 
            alternativesIDs = NULL, criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{assignments}{Vector containing the assignments (IDs of the categories) of the alternatives to the categories. The elements are named according to the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. The elements are named according to the IDs of the categories.}
    \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
      \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}

\value{
  The function returns NULL if there is a problem, or a list structured as follows :
  
  lambda = lambda, epsilon = epsilon, weights = weights, profilesPerformances = profilesPerformances)
  \item{lambda}{The majority threshold.}
  \item{epsilon}{The minimal percentage of difference between two evaluations of two consecutive profiles. This value has been maximized to separate the profiles as much as possible.}
  \item{weights}{A vector containing the weights of the criteria. The elements are named according to the criteria IDs.}
  \item{profilesPerformances}{A matrix containing the lower profiles of the categories. The columns are named according to the criteria, whereas the rows are named according to the categories. The lower profile of the lower category can be considered as a dummy profile.}
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-
satory sorting methods in MCDM, II: more than two categories. European Journal
of Operational Research, 178(1): 246--276, 2007.
}

\examples{
performanceTable <- rbind(
  c(3,3,2,3,3,3),
  c(3,2,2,2,3,3),
  c(3,2,2,3,3,3),
  c(2,3,2,2,3,3),
  c(2,3,2,3,2,3),
  c(2,3,2,2,3,2),
  c(2,3,2,3,3,2),
  c(3,2,2,3,2,3),
  c(3,1,2,1,3,1),
  c(1,1,2,3,3,3),
  c(3,3,1,3,3,3),
  c(3,3,2,1,1,3),
  c(3,3,2,3,1,1)
)

colnames(performanceTable) <- c("dcu","dep","dsf","dca","dsa","dda")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13")

assignments <-c("valide","valide","valide","valide","avecReserve","avecReserve","avecReserve","avecReserve","nonValide","nonValide","nonValide","nonValide","nonValide")

names(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2,3)

names(categoriesRanks) <- c("valide","avecReserve","nonValide")

criteriaMinMax <- c("max","max","max","max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

x<-ElectreTRIBMInference(performanceTable, assignments, categoriesRanks, criteriaMinMax, alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13"))

ElectreAssignments <- ElectreTRIBM(performanceTable, x$profilesPerformances, 
                                 x$weights, criteriaMinMax, x$lambda)

}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
