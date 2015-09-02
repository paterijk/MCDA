\name{plotAlternativesValuesPreorder}
\alias{plotAlternativesValuesPreorder}

\title{Function to plot a preorder of alternatives, based on some score or ranking.}

\description{Plots a preorder of alternatives as a graph, representing the ranking of the alternatives, w.r.t. some scores or ranks. A decreasing order or increasing order can be specified, w.r.t. to these scores or ranks.}

\usage{
plotAlternativesValuesPreorder(alternativesValues, 
                                decreasing = TRUE, 
                                alternativesIDs = NULL)
}

\arguments{
  \item{alternativesValues}{A vector containing some values related to alternatives, as scores or ranks. The elements of the vector are named according to the IDs of the alternatives.}
  \item{decreasing}{A boolean to indicate if the alternatives are to be sorted increasingly (FALSE) or decreasingly (TRUE) w.r.t. the alternativesValues.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  }

\examples{
library(MCDA)

alternativesValues <- c(10,1,8,3,8,3,4,4,8,5)

names(alternativesValues) <- c("x10","x1","x9","x2","x8",
                                "x3","x7","x4","x6","x5")

plotAlternativesValuesPreorder(alternativesValues, 
                                decreasing=TRUE, 
                                alternativesIDs=c("x10","x3","x7",
                                                    "x4","x6","x5"))

}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
