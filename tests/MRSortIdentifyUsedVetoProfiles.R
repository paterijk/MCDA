library(MCDA)

# the performance table

performanceTable <- rbind(
  c(1,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,10,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# lower profiles of the categories (best category in the first position of the list)

categoriesLowerProfiles <- rbind(c(3, 11, 3),c(7, 25, 2),c(NA,NA,NA))

colnames(categoriesLowerProfiles) <- colnames(performanceTable)

rownames(categoriesLowerProfiles)<-c("Good","Medium","Bad")

# the order of the categories, 1 being the best

categoriesRanks <-c(1,2,3)

names(categoriesRanks) <- c("Good","Medium","Bad")

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# vetos

criteriaVetos <- rbind(c(9, 50, -1),c(50, 50, 0),c(NA,NA,NA))

colnames(criteriaVetos) <- colnames(performanceTable)
rownames(criteriaVetos) <- c("Good","Medium","Bad")

# weights

criteriaWeights <- c(1/6,3/6,2/6)

names(criteriaWeights) <- colnames(performanceTable)

# assignments

assignments <- c("Good","Medium","Bad","Bad","Bad")


# MRSortIndetifyUsedVetoProfiles

used<-MRSortIdentifyUsedVetoProfiles(performanceTable, assignments, categoriesRanks, criteriaMinMax, 0.5, criteriaWeights, categoriesLowerProfiles, criteriaVetos)

stopifnot(all(as.vector(used) == c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)))
