\name{ELECTRE3}
\alias{ELECTRE3}
\title{
ELimination Et Choice Translating REality - ELECTRE-III
}

\description{
 ELECTRE (ELimination Et Choice Translating REality) is an outranking method proposed by Bernard Roy and his colleagues at SEMA consultancy company. This is the implementation of ELECTRE-III.
}

\usage{
ELECTRE3(scores, 
      q, 
      p, 
      v, 
      w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scores}{Matrix or data frame containing the performance table. Each column corresponds to a criterion, and each row to an alternative.}
  \item{q}{Vector containing the indifference thresholds. The elements are named according to the IDs of the criteria.}
  \item{p}{Vector containing the preference threshold on each of the criteria. The elements are named according to the IDs of the criteria.}
  \item{v}{Vector containing the veto thresholds for each criterion. The elements are named according to the IDs of the criteria.}
  \item{w}{Vector containing the weights of criteria. The elements are named according to the IDs of the criteria.}
}
\value{
The function returns the Concordance, Discordance, Credibility,  Dominance, and Scoring tables.
}
\references{
Roy, Bernard (1968). "Classement et choix en présence de points de vue multiples (la méthode ELECTRE)". La Revue d'Informatique et de Recherche Opérationelle (RIRO) (8): 57–75.
}

\examples{
library(MCDA)
scores <- matrix( c(-0.2,-2.3,-2.4,-1,3,9,10,7), 
                  nrow = 4, 
                  dimnames = list(
                    c("School-A","School-B","School-C", "School-D"), 
                    c("Location","Quality")) )

q <- c( 0.2, 1)
p <- c(   1, 2)
v <- c( 3.5, 4)
w <- c(0.25, 0.75)

res <- ELECTRE3(scores, q, p, v, w)
print(res)

}