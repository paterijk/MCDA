library(MCDA)
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

ELECTREIIIDistillation(performanceTable,criteriaWeights,
                       minMaxcriteria,preferenceThresholds,
                       indifferenceThresholds,vetoThresholds)

