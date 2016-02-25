\name{pairwiseConsistencyMeasures}
\alias{pairwiseConsistencyMeasures}
\title{
Consistency Measures for Pairwise Comparison Matrices
}
\description{
This function calculates four pairwise consistency checks: Consistency Ratio (CR) from Saaty (1980), Koczkodaj's Measure from Koczkodaj (1993) and Congruence / Dissonance Measures from Siraj et al. (2015).
}
\usage{
pairwiseConsistencyMeasures(matrix)
}
\arguments{
  \item{matrix}{A reciprocal matrix containing pairwise judgements}
}
\value{
The function returns a list of outputs for the four pairwise consistency checks
}
\references{
Thomas Saaty (1980). The Analytic Hierarchy Process: Planning, Priority Setting, ISBN 0-07-054371-2, McGraw-Hill.

W.W. Koczkodaj (1993). A new definition of consistency of pairwise comparisons. Mathematical and Computer Modelling. 18 (7).

Sajid Siraj, Ludmil Mikhailov & John A. Keane (2015). Contribution of individual judgments toward inconsistency in pairwise comparisons. European Journal of Operational Research. 242(2).
}
\examples{
examplematrix <- t(matrix(c(1,0.25,4,1/6,4,1,4,0.25,0.25,0.25,1,0.2,6,4,5,1),nrow=4,ncol=4))
pairwiseConsistencyMeasures(examplematrix)
}