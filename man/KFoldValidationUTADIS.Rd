\name{KFoldValidationUTADIS}
\alias{KFoldValidationUTADIS}

\title{K-Fold Validation for UTADIS method}

\description{Apply the k fold validation for UTADIS Method}



\usage{
UTADIS(performanceTable, criteriaMinMax, 
    criteriaNumberOfBreakPoints, 
    alternativesAssignments, categoriesRanks, epsilon, 
    criteriaLBs=NULL, criteriaUBs=NULL,
    alternativesIDs = NULL, criteriaIDs = NULL,
    categoriesIDs = NULL,
    nbTest,
    k
    )
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{criteriaNumberOfBreakPoints}{Vector containing the number of breakpoints of the piecewise linear value functions to be determined. Minimum 2. The elements are named according to the IDs of the criteria.}
  \item{alternativesAssignments}{Vector containing the assignments of the alternatives to categories. Minimum 2 categories. The elements of the vector are named according to the IDs of the alternatives.}
  \item{categoriesRanks}{Vector containing the ranks of the categories. Minimum 2 categories. The elements of the vector are named according to the IDs of the categories.}
  \item{epsilon}{Numeric value containing the minimal difference in value between the upper bound of a category and an alternative of that category.}
  \item{criteriaLBs}{Vector containing the lower bounds of the criteria to be considered for the elicitation of the value functions. If not specified, the lower bounds present in the performance table are taken.}
  \item{criteriaUBs}{Vector containing the upper bounds of the criteria to be considered for the elicitation of the value functions. If not specified, the upper bounds present in the performance table are taken.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{categoriesIDs}{Vector containing IDs of categories, according to which the data should be filtered.}
  \item{nbTest}{Integer representing the number of time that the alternativesAssignments will be divided into k parts}
  \item{k}{Integer representing the number of parts in which the alternativesAssignments will be divided in each test}
 }

\value{
  The function returns a real number representing the value of the k-fold validation 
  furthermore it will print numerous informations during the execution that will allow
  the user to follow the proceeding.
}


\examples{

# the separation threshold

epsilon <-0.05

# the performance table

performanceTable <- rbind(
  c(3,1,2),
  c(4,2,4),
  c(3,0,1),
  c(3,1,0),
  c(0,4,4),
  c(1,2,2),
  c(1,3,3),
  c(2,3,0),
  c(4,2,1),
  c(4,0,3),
  c(2,3,1),
  c(2,0,4),
  c(1,4,0),
  c(2,0,3),
  c(4,0,0),
  c(3,4,0),
  c(1,0,1),
  c(3,2,3),
  c(2,3,3),
  c(3,4,3)
)

rownames(performanceTable) <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20")

colnames(performanceTable) <- c("PotDes","EnvirVal","Vuln")

# ranks of the alternatives

alternativesAssignments <- c("C3","C4","C2","C2","C1","C3","C4","C3","C5","C5","C3","C3","C2","C3","C3","C4","C1","C4","C4","C5")

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

print(performanceTable)

KFold<-KFoldCrossValidationUTADIS(performanceTable=performanceTable, criteriaMinMax=criteriaMinMax, criteriaNumberOfBreakPoints=criteriaNumberOfBreakPoints,
           alternativesAssignments=alternativesAssignments, categoriesRanks=categoriesRanks,nbTest=20,k=2,epsilon=0.1)

print(KFold)
\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
