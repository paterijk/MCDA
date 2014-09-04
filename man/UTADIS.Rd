\name{UTADIS}
\alias{UTADIS}

\title{UTADIS method to elicit value functions in view of sorting alternatives in ordered categories}

\description{Elicits value functions from assignment examples, according to the UTADIS method.}



\usage{
UTADIS(performanceTable, criteriaMinMax, 
    criteriaNumberOfBreakPoints, 
    alternativesAssignments, categoriesRanks, epsilon, 
    criteriaLBs=NULL, criteriaUBs=NULL,
    alternativesIDs = NULL, criteriaIDs = NULL,
    categoriesIDs = NULL)
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
  }

\value{
  The function returns a list structured as follows : 
  \item{optimum}{The value of the objective function.}
  \item{valueFunctions}{A list containing the value functions which have been determined. Each value function is defined by a matrix of breakpoints, where the first row corresponds to the abscissa (row labelled "x") and where the second row corresponds to the ordinate (row labelled "y").}
  \item{overallValues}{A vector of the overall values of the input alternatives.}
  \item{categoriesLBs}{A vector containing the lower bounds of the considered categories.}
  \item{errors}{A list containing the errors (sigmaPlus and sigmaMinus) which have to be substracted and added to the overall values of the alternatives in order to respect the input ranking.}
}

\references{
J.M. Devaud, G. Groussaud, and E. Jacquet-Lagrèze, UTADIS : Une méthode de construction de fonctions d'utilité additives rendant compte de jugements globaux, European Working Group on Multicriteria Decision Aid, Bochum, 1980.
}



\examples{
# the separation threshold

epsilon <-0.05

# the performance table

performanceTable <- rbind(
  c(3,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# ranks of the alternatives

alternativesAssignments <- c("good","medium","medium","bad","bad")

names(alternativesAssignments) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(3,4,4)

names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)

# ranks of the categories

categoriesRanks <- c(1,2,3)

names(categoriesRanks) <- c("good","medium","bad")

x<-UTADIS(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, 
           alternativesAssignments, categoriesRanks,0.1)

# filtering out category "good" and assigment examples "RER" and "TAXI" 

y<-UTADIS(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, 
           alternativesAssignments, categoriesRanks,0.1, 
           categoriesIDs=c("medium","bad"), 
           alternativesIDs=c("METRO1","METRO2","BUS"))

# working furthermore on only 2 criteria : "Comfort" and "Time"

z<-UTADIS(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, 
            alternativesAssignments, categoriesRanks,0.1, 
            criteriaIDs=c("Comfort","Time"))
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
