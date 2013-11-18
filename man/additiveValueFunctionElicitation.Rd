\name{additiveValueFunctionElicitation}
\alias{additiveValueFunctionElicitation}

\title{Function to elicit a general additive value function.}

\description{Elicit a general additive value function from a ranking of alternatives.}

\usage{
additiveValueFunctionElicitation(performanceTable, alternativesRanks, criteriaMinMax, epsilon, 
    alternativesIDs = NULL, criteriaIDs = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{alternativesRanks}{Vector containing the ranks of the alternatives. The elements are named according to the IDs of the alternatives.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{epsilon}{Numeric value containing the minimal difference in value between two consecutive alternatives in the final ranking.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  }

\value{
  The function returns a list structured as follows : 
  \item{optimum}{The value of the objective function.}
  \item{valueFunctions}{A list containing the value functions which have been determined.}
  \item{overallValues}{The overall values of the input alternatives.}
  \item{ranks}{The ranks of the alternatives obtained via the elicited value functions. Ties method = "min".}
  \item{Kendall}{Kendall's tau between the input ranking and the one obtained via the elicited value functions.}
  \item{errors}{The errors (sigma) which have to be added to the overall values of the alternatives in order to respect the input ranking.}
}

\examples{

# ----------------------------------------
# ranking some cars (from the original article on UTA by Siskos and Lagreze, 1982)

# the separation threshold

epsilon <-0.01

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
  "Peugeot 505 GR",
  "Opel Record 2000 LS",
  "Citroen Visa Super E",
  "VW Golf 1300 GLS",
  "Citroen CX 2400 Pallas",
  "Mercedes 230",
  "BMW 520",
  "Volvo 244 DL",
  "Peugeot 104 ZS",
  "Citroen Dyane")

colnames(performanceTable) <- c(
  "MaximalSpeed",
  "ConsumptionTown",
  "Consumption120kmh",
  "HP",
  "Space",
  "Price")

# ranks of the alternatives

alternativesRanks <- c(1,2,3,4,5,6,7,8,9,10)

names(alternativesRanks) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("max","min","min","max","max","min")

names(criteriaMinMax) <- colnames(performanceTable)

x<-additiveValueFunctionElicitation(performanceTable, alternativesRanks, criteriaMinMax, epsilon)

}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}