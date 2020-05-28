\name{ELECTREIIIDistillation}
\alias{ELECTREIIIDistillation}

\title{ELECTRE III ranking}

\description{This function computes the two ELECTRE III distillations, or rankings.}

\usage{
ELECTREIIIDistillation(performanceTable,
                            criteriaWeights,
                            minMaxcriteria,
                            preferenceThresholds,
                            indifferenceThresholds,
                            vetoThresholds)
}

\arguments{
  \item{performanceTable}{Matrix or data frame containing the performance table. 
  Each row corresponds to an alternative, and each column to a criterion. 
  Rows (resp. columns) must be named according to the IDs of the alternatives
  (resp. criteria).}
  \item{criteriaWeights}{Vector containing the weights of the criteria. 
  The elements are named according to the IDs of the criteria.}
  \item{minMaxcriteria}{Vector containing the preference direction on each of 
  the criteria. "min" (resp. "max") indicates that the criterion has to be
  minimized (maximized). 
  The elements are named according to the IDs of the criteria.}
  \item{preferenceThresholds}{Vector containing preference thresholds for each
  criterion.}
  \item{indifferenceThresholds}{Vector containing indifferences thresholds for
  each criterion.}
  \item{vetoThresholds}{Vector containing veto thresholds for each criterion.}
}
  

\value{
  The function returns two lists, one for each distillation. 
}

\examples{
performanceTable <- rbind(
c(10,20,5,10,16),
c(0,5,5,16,10),
c(0,10,0,16,7),
c(20,5,10,10,13),
c(20,10,15,10,13),
c(20,10,20,13,13))
rownames(performanceTable) <-c("P1","P2","P3","P4","P5","P6")
colnames(performanceTable) <-c("CRIT1","CRIT2","CRIT3","CRIT4","CRIT5")
## vector indicating the direction of the criteria evaluation .
minMaxcriteria <-c("max","max","max","max","max")
names(minMaxcriteria) <- colnames(performanceTable)
## criteriaWeights vector
criteriaWeights <- c(3,2,3,1,1)
names(criteriaWeights) <- colnames(performanceTable)

indifferenceThresholds<-c(3,3,3,3,3)
names(indifferenceThresholds) <- colnames(performanceTable)
preferenceThresholds<-c(5,5,5,5,5)
names(preferenceThresholds) <- colnames(performanceTable)
vetoThresholds<-c(11,11,11,11,11)
names(vetoThresholds) <- colnames(performanceTable)

ELECTREIIIDistillation(performanceTable,criteriaWeights,minMaxcriteria,
                       preferenceThresholds,indifferenceThresholds,
                       vetoThresholds)
}
