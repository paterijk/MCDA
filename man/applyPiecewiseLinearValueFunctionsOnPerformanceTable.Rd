\name{applyValueFunctionsOnPerformanceTable}
\alias{applyValueFunctionsOnPerformanceTable}

\title{Applies value functions on a performance table.}

\description{Transforms a performance table via given piecewise linear value functions.}

\usage{
applyValueFunctionsOnPerformanceTable(valueFunctions, 
                                      performanceTable, 
                                      criteriaMinMax, 
                                      alternativesIDs = NULL, 
                                      criteriaIDs = NULL)
}



\arguments{
  \item{valueFunctions}{A list containing, for each criterion, the piecewise linear value functions defined by the coordinates of the break points. Each value function is defined by a matrix of breakpoints, where the first row corresponds to the abscissa (row labelled "x") and where the second row corresponds to the ordinate (row labelled "y").}
  \item{performanceTable}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the datashould be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
}

\value{
  The function returns a performance table which has been transformed through the given value functions.
}

\examples{

# the value functions

v<-list(
  Price = array(c(30, 0, 16, 0, 2, 0.0875), 
    dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
  Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
  Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))

# the performance table

performanceTable <- rbind(
    	c(3,10,1),
			c(4,20,2),
			c(2,20,0),
			c(6,40,0),
			c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# criteria to be minimized or maximized

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# the transformed performance table

applyValueFunctionsOnPerformanceTable(v,performanceTable, criteriaMinMax)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
