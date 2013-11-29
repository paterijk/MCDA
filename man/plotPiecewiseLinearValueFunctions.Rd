\name{plotPiecewiseLinearValueFunctions}
\alias{plotPiecewiseLinearValueFunctions}

\title{Function to plot piecewise linear value functions.}

\description{Plots piecewise linear value function.}

\usage{
plotPiecewiseLinearValueFunctions(valueFunctions, 
                                  criteriaIDs = NULL)
}

\arguments{
  \item{valueFunctions}{A list containing, for each criterion, the piecewise linear value functions defined by the coordinates of the break points. Each value function is defined by a matrix of breakpoints, where the first row corresponds to the abscissa (row labelled "x") and where the second row corresponds to the ordinate (row labelled "y").}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  }

\examples{

v<-list(
  Price = array(c(30, 0, 16, 0, 2, 0.0875), 
    dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
  Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
  Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))

# plot the value functions

plotPiecewiseLinearValueFunctions(v)
}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
