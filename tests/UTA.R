library(MCDA)

# the separation threshold

epsilon <-0.05

# the performance table

performanceTable <- rbind(
  c(3,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# ranks of the alternatives

alternativesRanks <- c(1,2,2,3,4)

names(alternativesRanks) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(3,4,4)

names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)

x<-UTA(performanceTable, alternativesRanks, criteriaMinMax, criteriaNumberOfBreakPoints, epsilon)

stopifnot(x$Kendall$tau[1] ==1)

x<-UTA(performanceTable, alternativesRanks, criteriaMinMax, criteriaNumberOfBreakPoints, epsilon, criteriaIDs = c("Price", "Time"), alternativesIDs = c("METRO1","METRO2","TAXI"))

stopifnot(x$overallValues[1] == x$overallValues[1])
