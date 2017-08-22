\name{SRMPInference}
\alias{SRMPInference}

\title{Exact inference of an SRMP model given a maximum number of reference profiles}

\description{Exact inference approach from pairwise comparisons of alternatives for the SRMP ranking model. This method outputs an SRMP model that is as consistent as possible with the provided pairwise comparisons (i.e. the model - the number of profiles and their lexicographic order - that maximizes the number of fulfilled pairwise comparisons). The method will search for a model with the minimum possible number of profiles up to a given maximum value.}

\usage{
SRMPInference(performanceTable, criteriaMinMax, maxProfilesNumber, preferencePairs,
              indifferencePairs = NULL,alternativesIDs = NULL, criteriaIDs = NULL,
              solver="glpk", timeLimit = NULL, cplexIntegralityTolerance = NULL,
              cplexThreads = NULL)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row
  corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be
  named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria.
  "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{maxProfilesNumber}{A strictly pozitive numerical value which gives the highest number of reference profiles the sought SRMP model should have.}
  \item{preferencePairs}{A two column matrix containing on each row a pair of alternative names where the first alternative is considered to be strictly preferred to the second.}
    \item{indifferencePairs}{A two column matrix containing on each row a pair of alternative names the two alternatives are considered to indifferent with respect to each other.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{solver}{String specifying if the glpk solver (glpk) should be used, or the cplex (cplex) solver. By default glpk. The cplex solver requires to install the cplex binary and the cplex C API, as well as the cplexAPI R package.}
  \item{timeLimit}{Allows to fix a time limit of the execution, in seconds. By default NULL (which corresponds to no time limit).}
  \item{cplexIntegralityTolerance}{If the cplex solver is used, allows to fix a tolerance for integrality. By default NULL (which corresponds to the default value of cplex).}
  \item{cplexThreads}{If the cplex solver is used, allows to the number of threads for the calculation. By default NULL (which corresponds to the default value of cplex).}
}

\value{
  The function returns a list containing:
  \item{criteriaWeights}{The inferred criteria weights.}
  \item{referenceProfilesNumber}{The inferred reference profiles number.}
  \item{referenceProfiles}{The inferred reference profiles.}
  \item{lexicographicOrder}{The inferred lexicographic order of the profiles.}
  \item{fitness}{The percentage (0 to 1) of fulfilled pair-wise relations.}
  \item{solverStatus}{The solver status as given by glpk or cplex.}
  \item{humanReadableStatus}{A description of the solver status.}
}

\references{
A-L. OLTEANU, V. MOUSSEAU, W. OUERDANE, A. ROLLAND, Y. ZHENG, Preference Elicitation for a
Ranking Method based on Multiple Reference Profiles, forthcoming 2018.
}

\examples{
performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
                          c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
                          c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
                          c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))

criteriaMinMax <- c("max","max","max")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                                "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
                                "a23","a24")

colnames(performanceTable) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

preferencePairs <- matrix(c("a16","a13","a3","a14","a17","a1","a18","a15","a2","a11","a5",
                            "a10","a4","a12","a13","a3","a14","a17","a1","a18","a15","a2",
                            "a11","a5","a10","a4","a12","a6"),14,2)
indifferencePairs <- matrix(c("a3","a1","a2","a11","a11","a20","a10","a10","a19","a12","a12",
                              "a21","a9","a7","a8","a20","a22","a22","a19","a24","a24","a21",
                              "a23","a23"),12,2)

result<-SRMPInference(performanceTable, criteriaMinMax, 3, preferencePairs, indifferencePairs,
                      alternativesIDs = c("a1","a3","a7","a9","a13","a14","a15","a16","a17",
                      "a18"))
}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
