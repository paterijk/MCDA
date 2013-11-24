\name{ElectreTRIBMIdentifyIncompatibleAssignments}
\alias{ElectreTRIBMIdentifyIncompatibleAssignments}

\title{Searches assignment examples which are incompatible with the Electre TRI-like sorting method axiomatized by Bouyssou and Marchant.}

\description{This simplification of the Electre TRI method uses the pessimistic assignment rule, without indifference or
preference thresholds attached to criteria. Only a binary discordance condition is considered, i.e. a veto forbids an outranking in any possible concordance situation, or not. This function searches for a set of minimal size which can be removed from the assignment examples in order to make the problem compatible again with this simplified Electre TRI model.}

\usage{
ElectreTRIBMIdentifyIncompatibleAssignments(performanceTable, assignments, 
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
  \item{numIncompatibleAssignments}{The number of incompatible assignment examples.}
  \item{incompatibleAssignments}{A logical vector indicating which assignment examples should be removed (TRUE), and which should be kept (FALSE).}
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-
satory sorting methods in MCDM, II: more than two categories. European Journal
of Operational Research, 178(1): 246--276, 2007.
}

\examples{
# ranking some cars (from the original article on UTA by Siskos and Lagreze, 1982)

# the performance table

performanceTable <- rbind(      
  c(173, 11.4, 10.01, 10, 7.88, 49500),
  c(176, 12.3, 10.48, 11, 7.96, 46700),
  c(142, 8.2, 7.30, 5, 5.65, 32100),
  c(148, 10.5, 9.61, 7, 6.15, 39150), 
  c(178, 14.5, 11.05, 13, 8.06, 64700), 
  c(180, 13.6, 10.40, 13, 8.47, 75700),
  c(182, 12.7, 12.26, 11, 7.81, 68593), 
  c(145, 14.3, 12.95, 11, 8.38, 55000),
  c(161, 8.6, 8.42, 7, 5.11, 35200), 
  c(117, 7.2, 6.75, 3, 5.81, 24800)
)

rownames(performanceTable) <- c(
  "Peugeot505GR",
  "OpelRecord2000LS",
  "CitroenVisaSuperE",
  "VWGolf1300GLS",
  "CitroenCX2400Pallas",
  "Mercedes230",
  "BMW520",
  "Volvo244DL",
  "Peugeot104ZS",
  "CitroenDyane")

colnames(performanceTable) <- c(
  "MaximalSpeed",
  "ConsumptionTown",
  "Consumption120kmh",
  "HP",
  "Space",
  "Price")

assignments <-c("Good","Bad","Good","Bad",
          "Medium","Medium","Medium",
          "Good","Bad","Good")

names(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2,3)

names(categoriesRanks) <- c("Good","Medium","Bad")

criteriaMinMax <- c("max","min","min","max","max","min")

names(criteriaMinMax) <- colnames(performanceTable)

onoff<-ElectreTRIBMIdentifyIncompatibleAssignments(
      performanceTable, assignments, categoriesRanks, 
      criteriaMinMax, 
      criteriaIDs = c("HP","Space","Price",
      "MaximalSpeed","ConsumptionTown"))

filteredAlternativesIDs <- rownames(performanceTable)[!onoff$incompatibleAssignments]

x<-ElectreTRIBMInference(performanceTable, 
            assignments, categoriesRanks, criteriaMinMax,  
            criteriaIDs = c("HP","Space","Price",
            "MaximalSpeed","ConsumptionTown"), 
            alternativesIDs = filteredAlternativesIDs)
 
ElectreAssignments<-ElectreTRIBM(performanceTable, x$profilesPerformances, 
                         x$weights, criteriaMinMax, x$lambda, 
                         criteriaIDs = c("HP","Space","Price",
                         "MaximalSpeed","ConsumptionTown"), 
                         alternativesIDs = filteredAlternativesIDs)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
