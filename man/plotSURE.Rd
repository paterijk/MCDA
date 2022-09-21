\name{plotSURE}
\alias{plotSURE}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot SURE kernel density plots.
}
\description{
Plots the output of function SURE() 
}
\usage{
plotSURE(SURE,
      greyScale = FALSE,
      separate = FALSE)
}
\arguments{
  \item{SURE}{Output from function SURE().}
  \item{greyScale}{TRUE/FALSE indicating if you want the plot to be in greyscale.}
  \item{separate}{TRUE/FALSE indicating if you want the density plots to be separated.}
}
\examples{
performanceTableMin <- t(matrix(c(78,87,79,19,8,68,74,8,90,89,74.5,9,20,81,30),
                  nrow=3,ncol=5, byrow=TRUE)) 
performanceTable <- t(matrix(c(80,87,86,19,8,70,74,10,90,89,75,9,33,82,30),
                              nrow=3,ncol=5, byrow=TRUE))
performanceTableMax <- t(matrix(c(81,87,95,19,8,72,74,15,90,89,75.5,9,36,84,30),
                                 nrow=3,ncol=5, byrow=TRUE))  

row.names(performanceTable) <- c("Yield","Toxicity","Cost","Separation","Odour")
colnames(performanceTable) <- c("Route One","Route Two","Route Three")
row.names(performanceTableMin) <- row.names(performanceTable)
colnames(performanceTableMin) <- colnames(performanceTable)
row.names(performanceTableMax) <- row.names(performanceTable)
colnames(performanceTableMax) <- colnames(performanceTable)

criteriaWeights <- c(0.339,0.077,0.434,0.127,0.023) 
names(criteriaWeights) <- row.names(performanceTable)

criteriaMinMax <- c("max", "max", "max", "max", "max")
names(criteriaMinMax) <- row.names(performanceTable)

test1 <- SURE(performanceTableMin, 
                 performanceTable, 
                 performanceTableMax, 
                 criteriaWeights, 
                 criteriaMinMax,
                 NoOfSimulations = 101)

summary(test1)
plotSURE(test1)
plotSURE(test1, greyScale = TRUE, separate = TRUE)
}

