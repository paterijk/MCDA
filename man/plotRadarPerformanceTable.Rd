\name{plotRadarPerformanceTable}
\alias{plotRadarPerformanceTable}

\title{Function to plot radar plots of alternatives of a performance table.}

\description{Plots radar plots of alternatives contained in a performance table, either in one radar plot, or on multiple radar plots. For a given alternative, the plot shows how far above/below average (the thick black line) each of the criteria performances values are (average taken w.r.t. to the filtered performance table).}

\usage{
plotRadarPerformanceTable(performanceTable, 
                            criteriaMinMax=NULL, 
                            alternativesIDs = NULL, 
                            criteriaIDs = NULL, 
                            overlay=FALSE,
                            bw=FALSE)
}

\arguments{
  \item{performanceTable}{A matrix containing the performance table to be plotted. The columns are labelled according to the criteria IDs, and the rows according to the alternatives IDs.}
  \item{criteriaMinMax}{Vector indicating whether criteria should be minimized or maximized. If it is given, a "higher" value in the radar plot corresponds to a more preferred value according to the decision maker. "min" (resp. "max") indicates that the criterion has to be minimized (maximized). The elements are named according to the IDs of the criteria.}
  \item{alternativesIDs}{Vector containing IDs of alternatives, according to which the data should be filtered.}
  \item{criteriaIDs}{Vector containing IDs of criteria, according to which the data should be filtered.}
  \item{overlay}{Boolean value indicating if the plots should be overlayed on one plot (TRUE), or not (FALSE)}
  \item{bw}{Boolean value indicating if the plots should be in black/white (TRUE) or color (FALSE)}
  }

\examples{
library(MCDA)

performanceTable <- matrix(runif(6*9), ncol=6)

row.names(performanceTable) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9")

colnames(performanceTable) <- c("g1","g2","g3","g4","g5","g6")

criteriaMinMax <- c("min","max","min","max","min","max")

names(criteriaMinMax) <- c("g1","g2","g3","g4","g5","g6")

# plotRadarPerformanceTable(performanceTable, criteriaMinMax, overlay=TRUE)

plotRadarPerformanceTable(performanceTable, criteriaMinMax, 
                          alternativesIDs = c("x1","x2","x3","x4"), 
                          criteriaIDs = c("g1","g3","g4","g5","g6"), 
                          overlay=FALSE, bw=FALSE)

# plotRadarPerformanceTable(performanceTable, criteriaMinMax, 
#                          alternativesIDs = c("x1","x2"), 
#                          criteriaIDs = c("g1","g3","g4","g5","g6"),
#                          overlay=FALSE)

}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s)}
