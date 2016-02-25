library(MCDA)

amatrix <- t(matrix(c(1,0.25,4,1/6,4,1,4,0.25,0.25,0.25,1,0.2,6,4,5,1),nrow=4,ncol=4))
test <- pairwiseConsistencyMeasures(amatrix)

stopifnot(round(test$CR, digits = 3) == 0.164 && round(test$Congruence, digits = 3) == 1.275 && round(test$Dissonance, digits = 3) == 0.083 && round(test$Koczkodaj, digits = 3) == 0.792)
