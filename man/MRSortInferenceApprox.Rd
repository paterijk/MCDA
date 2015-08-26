\name{MRSortInferenceApprox}
\alias{MRSortInferenceApprox}

\title{Identification of profiles, weights, majority threshold and veto thresholds for 
MRSort using a metaheuristic approach.}

\description{MRSort is a simplification of the Electre TRI method that uses the pessimistic assignment rule, without indifference or preference thresholds attached to criteria. 
Only a binary discordance condition is considered, i.e. a veto forbids an outranking in any possible concordance situation, or not. The identification of the profiles, weights, 
majority threshold and veto thresholds are done by taking into account assignment examples.}

\usage{
MRSortInferenceApprox(performanceTable, assignments, 
            categoriesRanks, criteriaMinMax, alg_total_time = 90, 
            alg_repeats = 3, alg_repeat_time = 30, 
            alg_repeat_iterations = 30, mh_max_temp_step = 0.2, 
            mh_min_temp_step = 0.02, mh_temp_step_increase = 1.25,
            mh_temp_step_decrease = 0.8, veto = FALSE, 
            alternativesIDs = NULL, criteriaIDs = NULL)
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
  \item{alg_total_time}{A strictly pozitive integer value denoting the total allowed time 
  in seconds of all algorithm executions.}
  \item{alg_repeats}{A strictly pozitive integer value denoting the number of times the 
  algorithm is executed.}
  \item{alg_repeat_time}{A strictly pozitive integer value denoting the total allowed time 
  in seconds for each algorithm execution.}
  \item{alg_repeat_iterations}{A strictly pozitive integer value denoting the maximum number of
  iterations that the algorithm will execute. 
  Each algorithm execution is stopped when either this limit is reached or when the amount of 
  time given by alg_repeat_time passes.}
  \item{mh_max_temp_step}{A value between 0 and 1 used for determining the rate at which the
  temperature of the simulated annealing algorithm decreases. 
  This parameter is the highest allowed value of this decrease. 
  Larger values make the simulated annealing algorithm perform fewer steps.}
  \item{mh_min_temp_step}{A value between 0 and 1 used for determining the rate at which the
  temperature of the simulated annealing algorithm decreases. 
  This parameter is the lowest allowed value of this decrease.
  Smaller values make the simulated annealing algorithm perform more steps.}
  \item{mh_temp_step_increase}{A value stricly above 1 used for determining the rate at which the
  temperature step increases following improvements in the overall fitness of the solution. 
  Larger values lead to a quicker reduction of the simulated annealing algorithm steps when 
  improvements are made to the solution.}
  \item{mh_temp_step_decrease}{A value between 0 and 1 used for determining the rate at which the
  temperature step decreases following non-improvements in the overall fitness of the solution.
  Smaller values lead to a quicker increase in the simulated annealing algorithm steps when
  the fitness of the solution does not increase from iteration to iteration.}
  \item{veto}{Boolean parameter indicating whether veto profiles are to be used or not.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}

\value{
  The function returns NULL if there is a problem, or a list structured as follows :
  \item{lambda}{The majority threshold.}
  \item{gamma}{Separation threshold used in the linear programs.}
  \item{weights}{A vector containing the weights of the criteria. 
  The elements are named according to the criteria IDs.}
  \item{profilesPerformances}{A matrix containing the lower profiles of the categories. 
  The columns are named according to the criteria, whereas the rows are named according to the categories. 
  The lower profile of the lower category can be considered as a dummy profile.}
}

\references{
Bouyssou, D. and Marchant, T. An axiomatic approach to noncompen-
satory sorting methods in MCDM, II: more than two categories. 
European Journal of Operational Research, 178(1): 246--276, 2007.

Olteanu, A-L. and Meyer, P. Inferring the parameters of a majority rule sorting model 
with vetoes on large datasets. DA2PL 2014 : From Multicriteria Decision Aid to Preference
Learning, 20-21 november 2014, Paris, France, 2014, pp. 87-94.
}

\examples{
performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
                          c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
                          c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
                          c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
                          c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
                          c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17))

rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
                                "a8", "a9", "a10", "a11", "a12", "a13", 
                                "a14", "a15", "a16", "a17", "a18", "a19", 
                                "a20", "a21", "a22", "a23", "a24")

colnames(performanceTable) <- c("c1","c2","c3")

assignments <-c("P", "P", "P", "F", "F", "F", "F", "F", "F", "F", "F", 
                "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", 
                "F", "F")

names(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2)

names(categoriesRanks) <- c("P","F")

criteriaMinMax <- c("max","max","max")

names(criteriaMinMax) <- colnames(performanceTable)

set.seed(1)

x<-MRSortInferenceApprox(performanceTable, assignments, categoriesRanks, 
                         criteriaMinMax, 180, 3, 30, 30, 0.2, 0.02, 1.25, 0.8, 
                         veto = TRUE,
                         alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))

print(x)

ElectreAssignments<-MRSort(performanceTable, x$profilesPerformances, 
                        x$weights, criteriaMinMax, x$lambda, 
                        criteriaVetos=x$vetoPerformances, 
                        alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
