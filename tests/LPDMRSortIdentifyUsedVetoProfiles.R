library(MCDA)

# the performance table

performanceTable <- rbind(
  c(1,27,1),
  c(6,20,1),
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

# dictators

criteriaDictators <- rbind(c(1, 1, -1),c(1, 20, 0),c(NA,NA,NA))

colnames(criteriaDictators) <- colnames(performanceTable)
rownames(criteriaDictators) <- c("Good","Medium","Bad")

# vetos

criteriaVetos <- rbind(c(9, 50, 5),c(50, 50, 5),c(NA,NA,NA))

colnames(criteriaVetos) <- colnames(performanceTable)
rownames(criteriaVetos) <- c("Good","Medium","Bad")

# weights

criteriaWeights <- c(1/6,3/6,2/6)

names(criteriaWeights) <- colnames(performanceTable)

# assignments

assignments <- c("Good","Medium","Bad","Bad","Bad")


# LPDMRSortIndetifyUsedVetoProfiles

used<-LPDMRSortIdentifyUsedVetoProfiles(performanceTable, assignments, categoriesRanks, criteriaMinMax, 0.5, criteriaWeights, categoriesLowerProfiles, criteriaVetos, criteriaDictators, majorityRule = "dv")

stopifnot(all(as.vector(used) == c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE)))
